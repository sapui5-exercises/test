/********** GENERATED on 07/07/2021 at 16:21:24 by CB0000000012**************/
 @OData.entitySet.name: 'D425_C_Cust20TP' 
 @OData.entityType.name: 'D425_C_Cust20TPType' 
 define root abstract entity ZCUST20 { 
 key id : abap.numc( 8 ) ; 
 @Odata.property.valueControl: 'name_vc' 
 name : abap.char( 25 ) ; 
 name_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'form_vc' 
 form : abap.char( 15 ) ; 
 form_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'street_vc' 
 street : abap.char( 30 ) ; 
 street_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'postbox_vc' 
 postbox : abap.char( 10 ) ; 
 postbox_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'postcode_vc' 
 postcode : abap.char( 10 ) ; 
 postcode_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'city_vc' 
 city : abap.char( 25 ) ; 
 city_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'country_vc' 
 country : abap.char( 3 ) ; 
 country_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'region_vc' 
 region : abap.char( 3 ) ; 
 region_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 
 } 
