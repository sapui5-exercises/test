@EndUserText.label: 'CE for Service Consumption'

@ObjectModel.query.implementedBy: 'ABAP:ZCL_CUSTOMER_C_Q'
define root custom entity ZI_CUSTOMER_C_C
{

      @UI.facet              : [
           { id              : 'Customer',
           purpose           : #STANDARD,
           type              : #IDENTIFICATION_REFERENCE,
           label             : 'Customer',
           position          : 10 } ]

      @UI                    : {lineItem: [ { position: 10, label: 'Customer ID', importance: #HIGH } ],
                                    selectionField: [ { position: 10 }],
                                    identification:  [{ position: 10 , label: 'Customer ID' }] }
      @EndUserText.label     : 'Travel ID'
  key id                     : abap.numc( 8 );


      @UI                    : {lineItem: [ { position: 30, label: 'Customer Name', importance: #HIGH } ],
                               selectionField: [ { position: 30 } ],
                               identification:[ { position: 30 , label: 'Customer Name'} ] }
      @EndUserText.label     : 'Customer Name'
      @OData.property.valueControl: 'name_vc'
      name                   : abap.char( 25 );
      name_vc                : rap_cp_odata_value_control;
      @OData.property.valueControl: 'form_vc'
      form                   : abap.char( 15 );
      form_vc                : rap_cp_odata_value_control;
      @OData.property.valueControl: 'street_vc'
      street                 : abap.char( 30 );
      street_vc              : rap_cp_odata_value_control;
      @OData.property.valueControl: 'postbox_vc'
      postbox                : abap.char( 10 );
      postbox_vc             : rap_cp_odata_value_control;
      @OData.property.valueControl: 'postcode_vc'
      postcode               : abap.char( 10 );
      postcode_vc            : rap_cp_odata_value_control;
      @OData.property.valueControl: 'city_vc'
      city                   : abap.char( 25 );
      city_vc                : rap_cp_odata_value_control;
      @OData.property.valueControl: 'country_vc'
      country                : abap.char( 3 );
      country_vc             : rap_cp_odata_value_control;
      @OData.property.valueControl: 'region_vc'
      region                 : abap.char( 3 );
      region_vc              : rap_cp_odata_value_control;


      @UI                    : {lineItem: [ { position: 80, label: 'Discount %', importance: #MEDIUM } ],
                                    identification:[ { position: 80, label: 'Discount %' } ] }
      DiscountPct            : abap.dec(3,1);

      @UI                    : {lineItem: [ { position: 75, label: 'Discount Absolute',  importance: #MEDIUM } ],
                                     identification:[ { position: 75, label: 'Discount Absolute' } ] }
      @Semantics.amount.currencyCode: 'CurrencyCode'

      DiscountAbs            : abap.dec(16,2);

      @Semantics.currencyCode: true
      CurrencyCode           : abap.cuky( 5 );

      @Semantics.amount.currencyCode: 'CurrencyCode'
      TotalPriceWithDiscount : abap.dec(16,2);


      CalculatedEtag         : abap.string( 0 );
}
