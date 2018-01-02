designate_small_business_eligble <- function(stage, 
                                             sheltered_workshop_flag,
                                             pop_country_code,
                                             date_signed,
                                             vendor_duns_number,
                                             contingency_ops,
                                             foreign_government,
                                             foreign_funding_desc,
                                             funding_agency_code,
                                             reason_not_competed,
                                             product_or_service_code,
                                             contracting_office_code,
                                             reference_piid,
                                             idv_reference_idv_piid)
{
  result <-  switch(stage, 
          result <- eval_jwod(),
          result <- eval_oconus_pre_100115(),
          result <- eval_unicor_duns(),
          result <- eval_american_institute_in_taiwan(),
          result <- eval_contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15(),
          result <- eval_foriegn_govts_entities_intnl_orgs(),
          result <- eval_agency_generated_sources(),
          result <- eval_acqisitionsNotCompeted_Resale(),
          result <- eval_lease_PSCs(),
          result <- eval_UtilitiesPSC_S112(),
          result <- eval_TricareDODAAC_H94001ContractingOffice(),
          result <- eval_TricareDODAAC_HT9402ContractingOffice(),
          result <- eval_CentersForMedicaidMedicareServices_00NAF_ContractingOffice(),
          result <- eval_DeptEducationNFP()
          )
    if(result == FALSE & stage <14)
    {
      result <- designate_small_business_eligble(stage+1)
    }
    
  result
     
}

eval_jwod <- function(sheltered_workshop_flag) 
{ 
  swf_return <- FALSE
  print("eval_jwod")
  if(sheltered_workshop_flag == YES) swf_return = TRUE
  swf_return
}
eval_oconus_pre_100115 <- function(pop_country_name, date_signed)
{ 
  oconus_pre_100115_return <- FALSE
  print("eval_oconus_pre_100115")
  if(pop_country_code != USA & as.Date(date_signed) < as.Date("2015-10-01"))
    {oconus_pre_100115_return}
  oconus_pre_100115_return
}
eval_unicor_duns <- function(vendor_duns_number, date_signed)
{
  unicor_duns_result <- FALSE
  print("eval_unicor_duns")
  unicor_duns <- c("014723167", "177021870", 
                   "025866133", "178845053", 
                   "027435366", "183606490", 
                   "042811430", "185501798", 
                   "056435894", "185930542", 
                   "057272486", "187651752", 
                   "060771920", "199234480", 
                   "068638951", "618879423", 
                   "072724859", "622178747", 
                   "075399977", "624770475", 
                   "085119121", "626627459", 
                   "086854069", "626979314", 
                   "088695218", "794034553", 
                   "096356964", "801970971", 
                   "098736291", "801977273", 
                   "103385519", "801983800", 
                   "118882161", "801984964", 
                   "126924018", "801991969", 
                   "127300429", "883387995", 
                   "127607732", "932169972", 
                   "129794269", "932793250", 
                   "147374714", "933631806", 
                   "148033947", "933631814", 
                   "148071236", "933631939", 
                   "152000998", "942195132", 
                   "161114251", "199221480")
  
  
  post_01OCT13_unicor_duns <- c("118575729", "196069173",
                                "198353984", "878435213", 
                                "621502173", "170419167",
                                "782184956", "806788407",
                                "139397371", "801972068", "139611631")
  
  if(vendor_duns_number %in% unicor_duns)unicor_duns_result <- TRUE
  if(vendor_duns_number %in% post_01OCT13_unicor_duns & as.Date(date_signed) >= as.Date("2013-10-01") ) unicor_duns_result <- TRUE
  unicor_duns_result
}

eval_american_institute_in_taiwan <- function(vendor_duns_number)
{
  american_institute_in_taiwan_result <- FALSE
  print("eval_american_institute_in_taiwan")
  if(vendor_duns_number == "161174503") american_institute_in_taiwan_result <- TRUE
  american_institute_in_taiwan_result
}

eval_contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15 <- function(contingency_ops, date_signed)
{
  contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15_result <- FALSE
  print("eval_contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15")
  if(contingency_ops == "A" & as.Date(date_signed) >= as.Date("2015-10-15")) 
    {contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15_result = TRUE}
  contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15_result
}
eval_foreign_govts_entities_intnl_orgs <- function(foreign_funding_desc, foreign_government)
{
  foreign_govts_entities_intnl_orgs_result <- FALSE
  print("eval_foriegn_govts_entities_intnl_orgs")
  if(foreign_government == "YES" |  foreign_funding_desc == "FOREIGN FUNDS FMS" | foreign_funding_desc == "FOREIGN FUNDS NON-FMS")
  {
    foreign_govts_entities_intnl_orgs_result = TRUE
  }
  foreign_govts_entities_intnl_orgs_result
}

eval_agency_generated_sources <- function(funding_agency_code, date_signed)
{
  agency_generated_source_result <- FALSE
  print("eval_agency_generated_sources")
  agency_generated_sources_agency_list <- c("5100", "1800", "2044", "6920", "6400", "1027", "0100", "1021", "5600", "0800", "959P", "1028", "1001", "1002", "1012")
  agency_generated_sources_agency_list_01OCT13 <- c("2041", "2046", "2047", "9594", "7100")
  agency_generated_sources_agency_list_01OCT08 <- c("6965", "7013")
  
  if(funding_agency_code %in% agency_generated_sources_agency_list) agency_generated_source_result = TRUE
  if(funding_agency_code %in% aagency_generated_sources_agency_list_01OCT13 & as.Date(date_signed) >= as.Date("2013-10-01")) agency_generated_source_result = TRUE
  if(funding_agency_code %in% aagency_generated_sources_agency_list_01OCT08 & as.Date(date_signed) >= as.Date("2008-10-01")) agency_generated_source_result = TRUE
  agency_generated_source_result
}


eval_acqisitionsNotCompeted_Resale <- function(reason_not_competed)
{
  resale_result <- FALSE
  print("eval_acqisitionsNotCompeted_Resale")
  if(reason_not_competed == "RES") resale_result <- TRUE
  resale_result
}

eval_lease_PSCs <- function(product_or_service_code)
{
  lease_PSC_result <- FALSE
  print("eval_lease_PSCs")
  if(startsWith(product_or_service_code, "X")) lease_PSC_result = TRUE
  lease_PSC_result
}
eval_UtilitiesPSC_S112 <- function(product_or_service_code)
{
  UtilitiesPSC_S112_result <- FALSE
  print("eval_UtilitiesPSC_S112")
  if(product_or_service_code == "S112") UtilitiesPSC_S112_result = TRUE
  UtilitiesPSC_S112_resultß
}


eval_TricareDODAAC_H9400_HT9402ContractingOffice <- function(contracting_office_id)
{
  DODAAC_H9400_HT9402_result <- FALSE
  print("eval_TricareDODAAC_H94001ContractingOffice")
  if(contracting_office_id == "H9400" | contracting_office_id == "HT9402") DODAAC_H9400_HT9402_result = TRUE
  DODAAC_H9400_HT9402_result
}

eval_CentersForMedicaidMedicareServices_00NAF_ContractingOffice <- function(contracting_office_id)
{
  CentersForMedicaidMedicareServices_result <- FALSE
  print("eval_CentersForMedicaidMedicareServices_00NAF_ContractingOffice")
  if(contracting_office_id == "00NAF" ) CentersForMedicaidMedicareServices_result = TRUE
  CentersForMedicaidMedicareServices_result
}
eval_DeptEducationNFP <- function(piid, reference_pid, idv_ref_idv_piid)
  {
  DeptEducationNFP_result <- FALSE
  print("eval_DeptEducationNFP")
  EducationNFP_piids <- c("EDFSA11D0012", 
                          "EDFSA12D0003", 
                          "EDFSA12D0005",  
                          "EDFSA12D0006", 
                          "EDFSA12D0007")
  if(piid %in% EducationNFP_piids | reference_pid %in% EducationNFP_piids | idv_ref_idv_piid %in% EducationNFP_piids)
  {
    DeptEducationNFP_result = TRUE
  }
  DeptEducationNFP_result
  }

