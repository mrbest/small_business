library(tictoc)
source("../CombinedAddressability/sprklyRSpark.R")
set_pre_sb_exclusions_df <- function(incoming_df)
{
  pre_sb_exclusions_df <<- incoming_df
}


designate_small_business_eligble <- function(sheltered_workshop_flag,
                                             pop_country_code,
                                             date_signed,
                                             vendor_duns_number,
                                             contingency_ops,
                                             foreign_government,
                                             foreign_funding_desc,
                                             funding_agency_code,
                                             reason_not_competed,
                                             product_or_service_code,
                                             contracting_office_id,
                                             piid, reference_piid,
                                             idv_ref_idv_piid)

{
  desig_return = "FALSE"
  if(is.na(sheltered_workshop_flag) == TRUE) sheltered_workshop_flag = "EMPTY"
  if(sheltered_workshop_flag == "YES") desig_return = "JWOD"
  
 
  us_territories <- c("USA", "ASM", "UMI", "MNP", "PRI", "VIR")
  if(is.na(pop_country_code) == TRUE) pop_country_code = "EMPTY"
  if(!pop_country_code %in% us_territories & as.Date(date_signed) < as.Date("2015-10-01"))
      {desig_return = "OCONUS_PRE_100115"}

  
  
  
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
  if(is.na(vendor_duns_number) == TRUE) vendor_duns_number <- "EMPTY"
  if(vendor_duns_number %in% unicor_duns)desig_return <- "UNICORE"
  if(vendor_duns_number %in% post_01OCT13_unicor_duns & as.Date(date_signed) >= as.Date("2013-10-01") ) desig_return <- "UNICORE_POST100113"

  
  if(is.na(vendor_duns_number) == TRUE) vendor_duns_number <- "EMPTY"
  if(vendor_duns_number == "161174503") desig_return <- "AIT"
  

  us_territories <- c("USA", "ASM", "UMI", "MNP", "PRI", "VIR")
  contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15_result <- "FALSE"
  #print("eval_contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15")
  if(is.na(funding_agency_code) == TRUE) funding_agency_code = "EMPTY"
  if(is.na(contingency_ops) == TRUE) contingency_ops = "EMPTY"
  if(contingency_ops == "A" & as.Date(date_signed) >= as.Date("2015-10-15") & funding_agency_code == "9700" & !pop_country_code %in% us_territories) 
    {desig_return = "CONT_HUM_PEACE_POST100115"}
 
 
  if(is.na(foreign_funding_desc) == TRUE) foreign_funding_desc = "EMPTY"
  if(is.na(foreign_government) == TRUE) foreign_government = "EMPTY"
  if(foreign_government == "YES" |  foreign_funding_desc == "FOREIGN FUNDS FMS" | foreign_funding_desc == "FOREIGN FUNDS NON-FMS")
  {
    desig_return = "FOREIGN_GOV_ENT_INTNL_ORG"
  }
  
 
  #print("eval_agency_generated_sources")
  agency_generated_sources_agency_list <- c("5100", "1800", "2044", "6920", "6400", "1027", "0100", "1021", "5600", "0800", "959P", "1028", "1001", "1002", "1012")
  agency_generated_sources_agency_list_01OCT13 <- c("2041", "2046", "2047", "9594", "7100")
  agency_generated_sources_agency_list_01OCT08 <- c("6965", "7013")
  
  if(is.na(funding_agency_code) == TRUE) funding_agency_code <- "EMPTY"
  if(funding_agency_code %in% agency_generated_sources_agency_list) desig_return = "AG_GEN_SRC"
  if(funding_agency_code %in% agency_generated_sources_agency_list_01OCT13 & as.Date(date_signed) >= as.Date("2013-10-01")) desig_return = "AG_GEN_SRC_P100113"
  if(funding_agency_code %in% agency_generated_sources_agency_list_01OCT08 & as.Date(date_signed) >= as.Date("2008-10-01")) desig_return = "AG_GEN_SRC_P100108"

  

  #print("eval_acqisitionsNotCompeted_Resale")
  if(is.na(reason_not_competed) == TRUE) reason_not_competed = "EMPTY"
  if(reason_not_competed == "RES") desig_return <- "ACQ_N_COMP_RESALE"
 

  if(is.na(product_or_service_code) == TRUE) product_or_service_code = "EMPTY"
  #print("eval_lease_PSCs")
  if(startsWith(product_or_service_code, "X")) desig_return = "LEASE"
 

  if(is.na(product_or_service_code) == TRUE) product_or_service_code = "EMPTY"
  if(product_or_service_code == "S112") desig_return = "UTILITIES"
 

  #print("eval_TricareDODAAC_H94001ContractingOffice")
  if(is.na(contracting_office_id) == TRUE) contracting_office_id <- "EMPTY"
  if(contracting_office_id == "H9400" | contracting_office_id == "HT9402") desig_return = "TRICARE"
 
  #print("eval_CentersForMedicaidMedicareServices_00NAF_ContractingOffice")
  if(is.na(contracting_office_id) == TRUE) contracting_office_id <- "EMPTY"
  if(contracting_office_id == "00NAF" ) desig_return = "MEDICARE"
 
  #print("eval_DeptEducationNFP")
  EducationNFP_piids <- c("EDFSA11D0012", 
                          "EDFSA12D0003", 
                          "EDFSA12D0005",  
                          "EDFSA12D0006", 
                          "EDFSA12D0007")
  if(is.na(piid) == TRUE) piid <- "EMPTY"
  if(is.na(reference_piid) == TRUE) reference_piid <- "EMPTY"
  if(is.na(idv_ref_idv_piid) == TRUE) idv_ref_idv_piid <- "EMPTY"
  if(piid %in% EducationNFP_piids | reference_piid %in% EducationNFP_piids | idv_ref_idv_piid %in% EducationNFP_piids)
  {
    desig_return = "EDU_LOAN"
  }
  
  desig_return
}


spark_small_business_goaling_report <- function(sb_exclude_src_df)
{
  sc <- sparkInit()
  print("reading in exclusion marked df")
  tic()
  sb_excluded_df <- sdf_copy_to(sc, sb_exclude_src_df, overwrite = TRUE)
  toc()
  tic()
  funding_department_list <- sb_excluded_df %>% select(funding_department_name) %>% distinct() %>% collect() %>% na.omit()%>% .$funding_department_name
  dept_length <- length(funding_department_list)
  small_business_eligible_actions <- c()
  small_business_eligble_dollars <- c()
  small_business_actions <- c()
  small_business_dollars <- c()
  small_business_percentage <- c()
  
  for(i in 1:dept_length)
  {
    small_business_eligible_actions <- append(small_business_eligible_actions, sb_excluded_df %>% 
                                               filter(funding_department_name == funding_department_list[i] & sb_exclude == "FALSE") %>% count() %>%collect()%>% .$n)
    
    small_business_eligible_obligations <- sb_excluded_df %>% 
                                               filter(funding_department_name == funding_department_list[i] & sb_exclude == "FALSE") %>% 
                                               select(dollars_obligated) %>% collect()
    
    small_business_eligble_dollars_count <- small_business_eligible_obligations %>% count() %>% .$n 
    if(small_business_eligble_dollars_count> 0 ) 
            small_business_eligble_dollars <- append(small_business_eligble_dollars,small_business_eligible_obligations %>% sum() )                                       
    else small_business_eligble_dollars <- append(small_business_eligble_dollars, c(0) )                                         
    
    
    small_business_actions <- append(small_business_actions, sb_excluded_df %>% 
                                       filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB") %>% count() %>%collect()%>% .$n)
    
    
    
    small_business_dollars_obligations <- sb_excluded_df %>% 
                                       filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB") %>% 
                                       select(dollars_obligated) %>% collect()
    
    
    
    small_business_dollars_count <- small_business_dollars_obligations %>% count() %>% .$n 
    
    if(small_business_dollars_count> 0 ) 
      small_business_dollars <- append(small_business_dollars, small_business_dollars_obligations %>% sum() )                                       
    else small_business_dollars <- append(small_business_dollars, c(0) ) 
    
    
    small_business_percentage <- append(small_business_percentage, small_business_dollars[i]/small_business_eligble_dollars[i])
    print(paste0("completed ", funding_department_list[i]))
  }
  
  result_df <- data_frame(funding_department_list, small_business_eligible_actions, small_business_eligble_dollars, small_business_actions, small_business_dollars, small_business_percentage)
  toc()
  result_df
}