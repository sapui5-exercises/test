CLASS zcl_customer_c_q DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      set_filter
        IMPORTING io_request      TYPE REF TO if_rap_query_request
                  io_read_request TYPE REF TO /iwbep/if_cp_request_read_list
        RAISING
                  zcx_customer_serv_cons
                  /iwbep/cx_gateway ,

      set_select_properties
        IMPORTING io_request      TYPE REF TO if_rap_query_request
                  io_read_request TYPE REF TO /iwbep/if_cp_request_read_list
        RAISING
                  /iwbep/cx_gateway,

      set_orderby
        IMPORTING io_request      TYPE REF TO if_rap_query_request
                  io_read_request TYPE REF TO /iwbep/if_cp_request_read_list
        RAISING
                  zcx_customer_serv_cons
                  /iwbep/cx_gateway.
ENDCLASS.



CLASS zcl_customer_c_q IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    """Instantiate Client Proxy
    DATA(lo_client_proxy) = zcl_customer_cp_aux=>get_client_proxy( ).

    TRY.
        """Create Read Request
        DATA(lo_read_request) = lo_client_proxy->create_resource_for_entity_set( 'D425_C_CUST20TP' )->create_request_for_read( ).

        """Request Count
        IF io_request->is_total_numb_of_rec_requested( ).
          lo_read_request->request_count( ).
        ENDIF.

        """Request Data
        IF io_request->is_data_requested( ).

          """Request Paging
          DATA(ls_paging) = io_request->get_paging( ).
          IF ls_paging->get_offset( ) >= 0.
            lo_read_request->set_skip( ls_paging->get_offset( ) ).
          ENDIF.
          IF ls_paging->get_page_size( ) <> if_rap_query_paging=>page_size_unlimited.
            lo_read_request->set_top( ls_paging->get_page_size( ) ).
          ENDIF.
        ENDIF.

        """Implement filtering
        me->set_filter(
               EXPORTING
                 io_request      = io_request
                 io_read_request = lo_read_request
             ).

        """Implement column selections
        me->set_select_properties(
               EXPORTING
                 io_request      = io_request
                 io_read_request = lo_read_request
             ).

        """Implement sorting
        me->set_orderby(
               EXPORTING
                 io_request      = io_request
                 io_read_request = lo_read_request
             ).

        """Execute the Request
        DATA(lo_response) = lo_read_request->execute( ).

        """Set Count
        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lo_response->get_count( ) ).
        ENDIF.


        """Set Data
        IF io_request->is_data_requested( ).   "überflüssig: SIEHE ZEILE 52
          DATA: lt_customer    TYPE STANDARD TABLE OF zcust20,
                lt_customer_ce TYPE STANDARD TABLE OF zi_customer_c_c,
                lt_customeradd TYPE STANDARD TABLE OF zcustomeradd.


          lo_response->get_business_data( IMPORTING et_business_data = lt_customer ).

          IF lt_customer IS NOT INITIAL.
            lt_customer_ce = CORRESPONDING #( lt_customer ).
            SELECT * FROM zcustomeradd FOR ALL ENTRIES IN @lt_customer_ce WHERE cust_id = @lt_customer_ce-id INTO TABLE @lt_customeradd.

            LOOP AT lt_customer_ce ASSIGNING FIELD-SYMBOL(<fs_customer_ce>).
              IF line_exists( lt_customeradd[ cust_id = <fs_customer_ce>-id ] ).
                <fs_customer_ce>-DiscountPct        = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-discount_pct.
                <fs_customer_ce>-DiscountAbs       = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-discount_abs.
                <fs_customer_ce>-CurrencyCode      = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-currency.


                "<fs_customer_ce>-TotalPriceWithDiscount = <fs_customer_ce>-totalprice * ( 1 - <fs_customer_ce>-discountpct / 100 ) - <fs_customer_ce>-discountabs.

                "<fs_customer_ce>-CalculatedEtag     = <fs_customer_ce>-lastchangedat && '-' && lt_customeradd[ cust_id = <fs_customer_ce>-id ]-lastchangedat.

                <fs_customer_ce>-CalculatedEtag     = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-lastchangedat.
              ELSE.
                "<fs_customer_ce>-totalpricewithdiscount = <fs_customer_ce>-totalprice.
                <fs_customer_ce>-CalculatedEtag          = '20000101120000' .  "initial value Jan 1, 2000, 12:00:00 AM
              ENDIF.
            ENDLOOP.
          ENDIF.

          io_response->set_data( lt_customer_ce ).
        ENDIF.



      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        RAISE EXCEPTION TYPE zcx_customer_serv_cons
          EXPORTING
            textid   = zcx_customer_serv_cons=>query_failed
            previous = lx_gateway.
    ENDTRY.






  ENDMETHOD.

  METHOD set_filter.
    """Request Filtering
    TRY.
        DATA(lt_filter) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_range).
        RAISE EXCEPTION TYPE zcx_customer_serv_cons
          EXPORTING
            textid   = zcx_customer_serv_cons=>no_ranges
            previous = lx_no_range.
    ENDTRY.


    LOOP AT lt_filter ASSIGNING FIELD-SYMBOL(<fs_filter>).
      IF <fs_filter>-name = 'DISCOUNTPCT' OR
         <fs_filter>-name = 'DISCOUNTABS' OR
         <fs_filter>-name = 'TOTALPRICEWITHDISCOUNT' OR
         <fs_filter>-name = 'CALCULATEDETAG' OR
         <fs_filter>-name = 'CURRENCYCODE'.
        RAISE EXCEPTION TYPE zcx_customer_serv_cons
          EXPORTING
            textid = zcx_customer_serv_cons=>filtering_failed.
      ENDIF.

      "provide currency code, if filtering on amount field
      IF <fs_filter>-name = 'TOTALPRICE'.
        IF line_exists( lt_filter[ name  = 'CURRENCYCODE' ] ).
          DATA(lv_currencycode) = VALUE waers_curc( lt_filter[ name = 'CURRENCYCODE' ]-range[ option = 'EQ' ]-low OPTIONAL ).
        ELSE.
          RAISE EXCEPTION TYPE zcx_customer_serv_cons
            EXPORTING
              textid = zcx_customer_serv_cons=>no_currencycode.
        ENDIF.
      ENDIF.

      "map element names
      DATA(lv_filter_property) = COND /iwbep/if_cp_runtime_types=>ty_property_path( WHEN <fs_filter>-name ='DESCRIPTION'
                                                                                    THEN 'MEMO'
                                                                                    ELSE <fs_filter>-name ).
      "create filter factory for read request
      DATA(lo_filter_factory) = io_read_request->create_filter_factory( ).
      "
      DATA(lo_filter_for_current_field) = lo_filter_factory->create_by_range( iv_property_path = lv_filter_property
                                                                              it_range         = <fs_filter>-range
                                                                              iv_currency_code = lv_currencycode ).
      "Concatenate filter if more than one filter element
      DATA: lo_filter            TYPE REF TO /iwbep/if_cp_filter_node.
      IF lo_filter IS INITIAL.
        lo_filter = lo_filter_for_current_field.
      ELSE.
        lo_filter = lo_filter->and( lo_filter_for_current_field ).
      ENDIF.
    ENDLOOP.

    "set filter
    IF lo_filter IS NOT INITIAL.
      io_read_request->set_filter( lo_filter ).
    ENDIF.

  ENDMETHOD.


  METHOD set_select_properties.

    """Request Elements
    DATA(lt_req_elements) = io_request->get_requested_elements( ).

    "delete local fields out of the fields to be selected via OData Client Proxy
    DELETE  lt_req_elements WHERE table_line = 'DISCOUNTPCT' OR
                                   table_line = 'DISCOUNTABS' OR
                                   table_line = 'TOTALPRICEWITHDISCOUNT' OR
                                   table_line = 'CALCULATEDETAG' OR
                                   table_line = 'CURRENCYCODE'.

    "map differing names
    LOOP AT lt_req_elements ASSIGNING FIELD-SYMBOL(<fs_req_elements>).
      DATA(lv_select_property) = COND /iwbep/if_cp_runtime_types=>ty_property_path( WHEN <fs_req_elements> ='DESCRIPTION' THEN 'MEMO'
                                                                                    ELSE <fs_req_elements> ).
      DATA: lt_select_properties TYPE /iwbep/if_cp_runtime_types=>ty_t_property_path.
      APPEND lv_select_property TO lt_select_properties.
    ENDLOOP.
    "set select properties
    IF lt_select_properties IS NOT INITIAL.
      io_read_request->set_select_properties( lt_select_properties  ).
    ENDIF.
  ENDMETHOD.

  METHOD set_orderby.

    """Request Sorting
    DATA(lt_sort) = io_request->get_sort_elements( ).

    LOOP AT lt_sort ASSIGNING FIELD-SYMBOL(<fs_sort>).
      IF <fs_sort>-element_name = 'DISCOUNTPCT' OR
         <fs_sort>-element_name = 'DISCOUNTABS' OR
         <fs_sort>-element_name = 'TOTALPRICEWITHDISCOUNT' OR
         <fs_sort>-element_name = 'CALCULATEDETAG' OR
        <fs_sort>-element_name = 'CURRENCYCODE'.
        RAISE EXCEPTION TYPE zcx_customer_serv_cons
          EXPORTING
            textid = zcx_customer_serv_cons=>sorting_failed.
      ENDIF.

      "map differing names
      DATA: lt_sort_properties TYPE /iwbep/if_cp_runtime_types=>ty_t_sort_order.
      APPEND VALUE #( property_path = COND #( WHEN <fs_sort>-element_name = 'DESCRIPTION' THEN 'MEMO'
                                              ELSE <fs_sort>-element_name )
                      descending = <fs_sort>-descending )
             TO lt_sort_properties.
    ENDLOOP.

    "set sorting properties
    IF lt_sort_properties IS NOT INITIAL.
      io_read_request->set_orderby( lt_sort_properties ).
    ENDIF.


  ENDMETHOD.

ENDCLASS.
