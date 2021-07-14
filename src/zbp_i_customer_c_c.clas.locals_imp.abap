CLASS lcl_buffer DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_buffer.

    "types used in get_data
    TYPES: BEGIN OF ts_message,
             custid TYPE zi_customer_c_c-id,
             symsg  TYPE symsg,
             fields TYPE string_table,
           END OF ts_message,
           tt_customer        TYPE STANDARD TABLE OF zi_customer_c_c,
           tt_customer_in     TYPE TABLE FOR READ IMPORT zi_customer_c_c,
           tt_customer_out    TYPE TABLE FOR READ RESULT zi_customer_c_c,
           tt_customer_failed TYPE TABLE FOR FAILED zi_customer_c_c,
           tt_message         TYPE STANDARD TABLE OF ts_message.

    "types used in put_data
    TYPES:
      tt_customer_upd    TYPE TABLE FOR UPDATE zi_customer_c_c,
      tt_customer_mapped TYPE TABLE FOR MAPPED zi_customer_c_c.

    METHODS: put_data
      IMPORTING it_customer_upd    TYPE tt_customer_upd
      EXPORTING et_customer_failed TYPE tt_customer_failed
                et_message         TYPE tt_message,

      get_data
        IMPORTING it_customer        TYPE tt_customer_in OPTIONAL
        EXPORTING et_customer        TYPE tt_customer_out
                  et_customer_failed TYPE tt_customer_failed
                  et_message         TYPE tt_message.
*        RAISING   zcx_customer_serv_cons.

  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO lcl_buffer.
    DATA: mt_customer       TYPE tt_customer.

ENDCLASS.

CLASS lcl_buffer IMPLEMENTATION.

  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD get_data.

    DATA: lt_customer        TYPE STANDARD TABLE OF zcust20.
    DATA: ls_result         LIKE LINE OF et_customer.
    DATA: lt_customer_id      TYPE STANDARD TABLE OF zi_customer_c_c-id.

    DATA: lt_filter         TYPE RANGE OF zi_customer_c_c-id.
    DATA: ls_filter         LIKE LINE OF lt_filter.
    DATA: lt_customer_ce      TYPE STANDARD TABLE OF zi_customer_c_c.
    DATA: lt_customeradd      TYPE STANDARD TABLE OF zcustomeradd.
    FIELD-SYMBOLS: <fs_customer_ce> LIKE LINE OF lt_customer_ce.

    IF it_customer IS SUPPLIED.

      LOOP AT it_customer ASSIGNING FIELD-SYMBOL(<fs_customer>).
        IF line_exists( mt_customer[ id = <fs_customer>-id ] ).
          ls_result = CORRESPONDING #( mt_customer[ id = <fs_customer>-id ] ).
          " collect from buffer for result
          APPEND ls_result TO et_customer.
        ELSE.
          " collect to retrieve from persistence
          APPEND <fs_customer>-id TO lt_customer_id.
        ENDIF.
      ENDLOOP.

      IF lt_customer_id IS NOT INITIAL.
        TRY.
            DATA(lo_client_proxy) = zcl_customer_cp_aux=>get_client_proxy( ).
            DATA(lo_request) = lo_client_proxy->create_resource_for_entity_set( 'D425_C_CUST20TP' )->create_request_for_read( ).

            lt_filter = VALUE #( FOR customer_id IN lt_customer_id
                                    ( sign = 'I' option = 'EQ' low = customer_id )
                                ).
            DATA(lo_filter) = lo_request->create_filter_factory( )->create_by_range( iv_property_path = 'ID'
                                                                                     it_range         = lt_filter ).
            lo_request->set_filter( lo_filter ).
            DATA(lo_response) = lo_request->execute( ).
            " get relevant data sets
            lo_response->get_business_data( IMPORTING et_business_data = lt_customer ).

            " add local data
            IF lt_customer IS NOT INITIAL.

              " map OData service to custom entity
              " lt_customer_ce = CORRESPONDING #( lt_customer MAPPING description = memo ).
              lt_customer_ce = CORRESPONDING #( lt_customer ).

              SELECT * FROM zcustomeradd FOR ALL ENTRIES IN @lt_customer_ce WHERE cust_id = @lt_customer_ce-id INTO TABLE @lt_customeradd.

              LOOP AT lt_customer_id ASSIGNING FIELD-SYMBOL(<fs_customer_id>).
                IF line_exists( lt_customer_ce[ id = <fs_customer_id> ] ).
                  ASSIGN lt_customer_ce[ id = <fs_customer_id> ] TO <fs_customer_ce>.

                  IF line_exists( lt_customeradd[ cust_id = <fs_customer_ce>-id ] ).
                    <fs_customer_ce>-discountpct        = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-discount_pct.
                    <fs_customer_ce>-discountabs        = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-discount_abs.
                    <fs_customer_ce>-CurrencyCode        = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-currency.
*                    <fs_customer_ce>-totalpricewithdiscount = <fs_travel_ce>-totalprice * ( 1 - <fs_travel_ce>-discountpct / 100 ) - <fs_travel_ce>-discountabs.
*                    <fs_customer_ce>-lastchange      = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-lastchangedat.
                    <fs_customer_ce>-calculatedetag         = lt_customeradd[ cust_id = <fs_customer_ce>-id ]-lastchangedat.

                  ELSE.
*                    <fs_customer_ce>-totalpricewithdiscount = <fs_customer_ce>-totalprice.
                    <fs_customer_ce>-calculatedetag = '20000101120000' .  "initial value Jan 1, 2000, 12:00:00 AM
                  ENDIF.


                  ls_result = CORRESPONDING #( <fs_customer_ce> ).
                  APPEND <fs_customer_ce> TO mt_customer.
                  APPEND ls_result           TO et_customer.
                ELSE.
                  APPEND VALUE #( id =  <fs_customer_id> ) TO et_customer_failed.
                  APPEND VALUE #( custid    = <fs_customer_id>
                                  symsg-msgty = 'E'
                                  symsg-msgid = 'ZCM_SERV_CONS'
                                  symsg-msgno = '008'
                                  symsg-msgv1 = <fs_customer_id> )
                  TO et_message.
                ENDIF.
              ENDLOOP.
            ENDIF.

          CATCH  /iwbep/cx_gateway.
            et_customer_failed = CORRESPONDING #( lt_customer_id MAPPING id = table_line ).
            et_message = CORRESPONDING #( lt_customer_id MAPPING custid = table_line ).
            LOOP AT et_message ASSIGNING FIELD-SYMBOL(<fs_message>).
              <fs_message>-symsg-msgty = 'E'.
              <fs_message>-symsg-msgid = 'ZCM_SERV_CONS'.
              <fs_message>-symsg-msgno = '008'.
              <fs_message>-symsg-msgv1 = <fs_message>-custid.
            ENDLOOP.
        ENDTRY.
      ENDIF.
    ELSE.
      et_customer = CORRESPONDING #( mt_customer ).

    ENDIF.

  ENDMETHOD.

  METHOD put_data.

    me->get_data(
      EXPORTING it_customer        = CORRESPONDING #( it_customer_upd MAPPING %key = %key EXCEPT * )
      IMPORTING et_customer        = DATA(lt_customer)
                et_customer_failed = DATA(lt_customer_failed)
                et_message       = DATA(lt_message)
    ).


    LOOP AT it_customer_upd ASSIGNING FIELD-SYMBOL(<fs_customer_upd>).
      CHECK line_exists( lt_customer[ KEY entity COMPONENTS id = <fs_customer_upd>-id ] ).
      ASSIGN lt_customer[ KEY entity COMPONENTS id = <fs_customer_upd>-id ] TO FIELD-SYMBOL(<fs_customer>).

      IF <fs_customer_upd>-%control-DiscountAbs = if_abap_behv=>mk-on.
        <fs_customer>-DiscountAbs = <fs_customer_upd>-DiscountAbs.
      ENDIF.
      IF <fs_customer_upd>-%control-DiscountPct = if_abap_behv=>mk-on.
        <fs_customer>-DiscountPct = <fs_customer_upd>-DiscountPct.
      ENDIF.
      IF <fs_customer_upd>-%control-CurrencyCode = if_abap_behv=>mk-on.
        <fs_customer>-CurrencyCode = <fs_customer_upd>-CurrencyCode.
      ENDIF.
    ENDLOOP.

    " Postprocessing
*    LOOP AT lt_customer ASSIGNING FIELD-SYMBOL(<fs_customeraddinfo>).
*      DATA(lv_totalpricediscount) = <fs_customeraddinfo>-totalprice * ( 1 - <fs_customeraddinfo>-discountpct / 100 ) - <fs_customeraddinfo>-discountabs.
*      IF lv_totalpricediscount >= 0.
*        <fs_customeraddinfo>-totalpricewithdiscount = lv_totalpricediscount.
*      ELSE.
*        APPEND VALUE #( id    =  <fs_customeraddinfo>-id ) TO et_customer_failed.
*        APPEND VALUE #( custid    = <fs_customeraddinfo>-id
*                        symsg-msgty = 'E'
*                        symsg-msgid = 'ZCM_SERV_CONS'
*                        symsg-msgno = '010'
*                        symsg-msgv1 = <fs_customeraddinfo>-id
*                        symsg-msgv2 = |{ lv_totalpricediscount NUMBER = USER }|
*                        fields      = VALUE #( ( |discountpct| )
*                                               ( |discountabs| )
*                                             )
*                      )
*        TO et_message.
*      ENDIF.
*    ENDLOOP.
    "save data in buffer
    mt_customer = CORRESPONDING #( lt_customer ) .

  ENDMETHOD.

ENDCLASS.


CLASS lhc_Customer_CE DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS update_discount FOR MODIFY
      IMPORTING it_customer_update FOR UPDATE Customer_CE.

    METHODS read_customer FOR READ
      IMPORTING it_customer_read FOR READ Customer_CE RESULT et_customeraddinfo.

    TYPES:        tt_customer_reported   TYPE TABLE FOR REPORTED zi_customer_c_c.

    METHODS: _map_messages
      IMPORTING it_message           TYPE lcl_buffer=>tt_message
      EXPORTING et_customer_reported TYPE tt_customer_reported.

ENDCLASS.

CLASS lhc_Customer_CE IMPLEMENTATION.

  METHOD update_discount.

    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

    lo_buffer->put_data(
        EXPORTING
          it_customer_upd      = it_customer_update
          IMPORTING
          et_customer_failed   = failed-customer_ce
          et_message         = DATA(lt_message)
    ).

    _map_messages(
      EXPORTING
        it_message         = lt_message
       IMPORTING
        et_customer_reported = reported-customer_ce
    ).

  ENDMETHOD.


  METHOD _map_messages.

    DATA: ls_customer_reported LIKE LINE OF et_customer_reported.
    FIELD-SYMBOLS: <fs_element> TYPE data.

    LOOP AT it_message ASSIGNING FIELD-SYMBOL(<fs_message>).

      CLEAR ls_customer_reported.
      ls_customer_reported-%msg = new_message( id       = <fs_message>-symsg-msgid
                                             number   = <fs_message>-symsg-msgno
                                             severity = if_abap_behv_message=>severity-error
                                             v1       = <fs_message>-symsg-msgv1
                                             v2       = <fs_message>-symsg-msgv2
                                             v3       = <fs_message>-symsg-msgv3
                                             v4       = <fs_message>-symsg-msgv4 ).
      IF <fs_message>-custid IS NOT INITIAL.
        ls_customer_reported-%key-id = <fs_message>-custid. "Zeile ist wahrscheinlich überflüssig
        ls_customer_reported-id      = <fs_message>-custid.
        LOOP AT <fs_message>-fields ASSIGNING FIELD-SYMBOL(<fs_field>).
          ASSIGN COMPONENT <fs_field> OF STRUCTURE ls_customer_reported-%element TO <fs_element>.
          CHECK sy-subrc = 0.
          <fs_element> = if_abap_behv=>mk-on.
        ENDLOOP.
        APPEND ls_customer_reported TO et_customer_reported.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_customer.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

    lo_buffer->get_data(
      EXPORTING
        it_customer          = it_customer_read
      IMPORTING
        et_customer   = et_customeraddinfo
        et_customer_failed   = failed-customer_ce
    ).
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZI_CUSTOMER_C_C DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZI_CUSTOMER_C_C IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
    DATA: ls_customeradd TYPE zcustomeradd.

    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

    lo_buffer->get_data(
       IMPORTING
         et_customer = DATA(lt_customer)
    ).

    LOOP AT lt_customer ASSIGNING FIELD-SYMBOL(<fs_customeraddinfo>).
      ls_customeradd = CORRESPONDING #( <fs_customeraddinfo> MAPPING cust_id    = id
                                                                 discount_pct = discountpct
                                                                 discount_abs = discountabs
                                                                 currency = CurrencyCode ).
      GET TIME STAMP FIELD ls_customeradd-lastchangedat.
      MODIFY zcustomeradd FROM @ls_customeradd.
    ENDLOOP.

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
