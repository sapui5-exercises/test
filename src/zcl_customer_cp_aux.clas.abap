CLASS zcl_customer_cp_aux DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    CLASS-METHODS:
      get_client_proxy RETURNING VALUE(ro_client_proxy) TYPE REF TO /iwbep/if_cp_client_proxy
                       RAISING   zcx_customer_serv_cons.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_customer_cp_aux IMPLEMENTATION.

  METHOD get_client_proxy.

    " Getting the destination of foreign system
    TRY.
        " Getting the destination of foreign system
        " Create http client
        " Details depend on connection settings
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
                                  cl_http_destination_provider=>create_by_cloud_destination(
                                          i_name                  = 'S4D_100'
                                         i_authn_mode = if_a4c_cp_service=>service_specific ) ).

        " Error handling
      CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
        RAISE EXCEPTION TYPE zcx_customer_serv_cons
          EXPORTING
            textid   = zcx_customer_serv_cons=>remote_access_failed
            previous = lx_http_dest_provider_error.


      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        RAISE EXCEPTION TYPE zcx_customer_serv_cons
          EXPORTING
            textid   = zcx_customer_serv_cons=>remote_access_failed
            previous = lx_web_http_client_error.

    ENDTRY.

    " Instantiation of client proxy
    TRY.
        ro_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
        EXPORTING
          iv_service_definition_name = 'ZCUSTOMER_C_A'
          io_http_client             = lo_http_client
          iv_relative_service_root   = '/sap/opu/odata/sap/S4D425_CUSTOMER20_SRV' ).

      CATCH cx_web_http_client_error INTO lx_web_http_client_error.
        RAISE EXCEPTION TYPE zcx_customer_serv_cons
          EXPORTING
            textid   = zcx_customer_serv_cons=>client_proxy_failed
            previous = lx_web_http_client_error.

      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        RAISE EXCEPTION TYPE zcx_customer_serv_cons
          EXPORTING
            textid   = zcx_customer_serv_cons=>client_proxy_failed
            previous = lx_gateway.

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
