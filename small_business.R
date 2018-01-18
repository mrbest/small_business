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
  #sb_excluded_df <<- sdf_copy_to(sc, sb_exclude_src_df, overwrite = TRUE)
  spark_df <<- sdf_copy_to(sc, sb_exclude_src_df, overwrite = TRUE)
  sb_excluded_df <- spark_df %>% filter(level_1_category_group == "GWCM")
  toc()
  tic()
  funding_department_list <- sb_excluded_df %>% 
    filter(funding_cfo_act_agency == "CFO")%>%
    select(funding_department_name) %>% 
    distinct() %>% 
    collect() %>% 
    na.omit()%>%
    .$funding_department_name
  
  
  dept_length <- length(funding_department_list)
  small_business_eligible_actions <- c()
  small_business_eligble_dollars <- c()
  small_business_actions <- c()
  unique_small_business_vendors <- c()
  small_business_dollars <- c()
  small_business_percentage <- c()
  small_disadvantaged_business_actions <-c()
  small_disadvantaged_business_dollars <- c()
  small_disadvantaged_business_percentage <-c()
  eight_a_procedure_actions <- c()
  eight_a_procedure_action_dollars <- c() 
  eight_a_procedure_percentage <- c()
  veterans_owned_small_business_actions <- c()
  veterans_owned_small_business_dollars <- c()
  veterans_owned_small_business_percentage <- c()
  service_disabled_veteran_owned_small_business_actions <- c()
  service_disabled_veteran_owned_small_business_dollars <- c()
  service_disabled_veteran_owned_small_business_percentage <- c()
  women_owned_small_business_actions <- c()
  women_owned_small_business_dollars <- c()
  women_owned_small_business_percentage <- c()
  certified_HUBZone_small_business_actions <- c()
  certified_HUBZone_small_business_dollars <- c()
  certified_HUBZone_small_business_percentage <- c()
  
  
  for(i in 1:dept_length)
  {
    #1. small busines eligible actions
    small_business_eligible_actions <- append(small_business_eligible_actions, sb_excluded_df %>% 
                                               filter(funding_department_name == funding_department_list[i] & sb_exclude == "FALSE") %>% count() %>%collect()%>% .$n)
    
    #2. small business eligible dollars
    small_business_eligible_obligations <- sb_excluded_df %>% 
                                               filter(funding_department_name == funding_department_list[i] & sb_exclude == "FALSE") %>% 
                                               select(dollars_obligated) %>% collect()
    
    small_business_eligble_dollars_count <- small_business_eligible_obligations %>% count() %>% .$n 
    if(small_business_eligble_dollars_count> 0 ) 
            small_business_eligble_dollars <- append(small_business_eligble_dollars,small_business_eligible_obligations %>% sum() )                                       
    else small_business_eligble_dollars <- append(small_business_eligble_dollars, c(0) )                                         
    
    #3. small business actions
    small_business_actions <- append(small_business_actions, sb_excluded_df %>% 
                                       filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>% count() %>%collect()%>% .$n)
    
    #3.5 unique small business vendors
    unique_small_business_vendors <- append(unique_small_business_vendors, sb_excluded_df %>% 
                                                 filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>%
                                                 distinct(vendor_duns_number) %>% count() %>% collect() %>% .$n)
    
    
    #4. small business dollars
    small_business_dollars_obligations <- sb_excluded_df %>% 
                                       filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>% 
                                       select(dollars_obligated) %>% collect()
    
    
    
    small_business_dollars_count <- small_business_dollars_obligations %>% count() %>% .$n 
    
    if(small_business_dollars_count> 0 ) 
      small_business_dollars <- append(small_business_dollars, small_business_dollars_obligations %>% sum() )                                       
    else small_business_dollars <- append(small_business_dollars, c(0) ) 
    
    #5. small business percentage
    small_business_percentage <- append(small_business_percentage, small_business_dollars[i]/small_business_eligble_dollars[i])
    
    #6. small business disadvantaged actions
    small_disadvantaged_business_actions <- append(small_disadvantaged_business_actions, sb_excluded_df %>% 
                                                filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>%
                                                filter(sdb == "SDB" | sdb_flag == "SDB_FLAG" | firm8a_joint_venture == "F8AJV"| eight_a_flag == "YES") %>% 
                                                count() %>%
                                                collect()%>%
                                                .$n)
    
    ####  hbcu_flag and minority_institution_flag needed for dod_nasa_uscg


    
    #7. small business disadvantaged dollars 
    small_disadvantaged_business_dollars_obligations <- sb_excluded_df %>%
                filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>%
                filter(sdb == "SDB" | sdb_flag == "SDB_FLAG" | firm8a_joint_venture == "F8AJV"| eight_a_flag == "YES") %>%
                select(dollars_obligated) %>% 
                collect()
   
    
    
    small_disadvantaged_business_dollars_count <- small_disadvantaged_business_dollars_obligations %>% count() %>% .$n 
    
    if(small_disadvantaged_business_dollars_count > 0 ) 
      small_disadvantaged_business_dollars <- append(small_disadvantaged_business_dollars, small_disadvantaged_business_dollars_obligations %>% sum() )                                       
    else small_disadvantaged_business_dollars <- append(small_disadvantaged_business_dollars, c(0) ) 
    
    #8. small business disadvantaged percentage 
    small_disadvantaged_business_percentage <- append( small_disadvantaged_business_percentage,   small_disadvantaged_business_dollars[i] / small_business_eligble_dollars[i])
    
    #9 8(a) Procedure Actions
    eight_a_procedure_actions <- append(eight_a_procedure_actions, sb_excluded_df %>% 
                                             filter(funding_department_name == funding_department_list[i] & sb_exclude == "FALSE") %>% 
                                             filter(co_bus_size_determination_code == "CO_SB") %>% #& eight_a_flag == "YES") %>%
                                             filter(type_of_set_aside == "8N" | type_of_set_aside =="HS2" | type_of_set_aside =="HS3" |type_of_set_aside == "8A") %>%
                                             count() %>%
                                             collect()%>%
                                            .$n)
    
    
    #10 8(a) Procedure Dollars:
    eight_a_procedure_action_dollars_obligations <- sb_excluded_df %>%
      filter(funding_department_name == funding_department_list[i]) %>% # & sb_exclude == "FALSE") %>% 
      filter(co_bus_size_determination_code == "CO_SB" & eight_a_flag == "YES") %>%
      #filter(type_of_set_aside == "8(a) Sole Source" | type_of_set_aside =="8(a) with HUB Zone" | type_of_set_aside == "8(a) Competed") %>%
      select(dollars_obligated) %>% 
      collect()
    
    eight_a_procedure_action_dollars_count <- eight_a_procedure_action_dollars_obligations %>% count() %>% .$n 
    
    if(eight_a_procedure_action_dollars_count > 0 ) 
      eight_a_procedure_action_dollars <- append( eight_a_procedure_action_dollars, eight_a_procedure_action_dollars_obligations %>% sum() )                                       
    else eight_a_procedure_action_dollars <- append(eight_a_procedure_action_dollars, c(0) ) 
    
    #11. 8(a) Procedure Percentage
    eight_a_procedure_percentage <- append( eight_a_procedure_percentage,   eight_a_procedure_action_dollars[i] / small_business_eligble_dollars[i])
    
    #12. Veteran Owned Small Business Actions
    veterans_owned_small_business_actions <- append(veterans_owned_small_business_actions, sb_excluded_df %>% 
      filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & veteran_owned_flag == "VO") %>% 
      count() %>%
      collect()%>%
      .$n)
    
    #13. Veteran Owned Small Business dollars
    veterans_owned_small_business_dollars_obligations <- sb_excluded_df %>%
      filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & veteran_owned_flag == "VO") %>%
      select(dollars_obligated) %>% 
      collect()
    
    veterans_owned_small_business_dollars_count <- veterans_owned_small_business_dollars_obligations %>% count() %>% .$n 
    
    if(veterans_owned_small_business_dollars_count > 0 ) 
      veterans_owned_small_business_dollars <- append(veterans_owned_small_business_dollars, veterans_owned_small_business_dollars_obligations %>% sum() )                                       
    else veterans_owned_small_business_dollars <- append(veterans_owned_small_business_dollars, c(0) ) 
    
    #14. veterans_owned_small_business_percentage
    veterans_owned_small_business_percentage <- append( veterans_owned_small_business_percentage,   veterans_owned_small_business_dollars[i] / small_business_eligble_dollars[i])
    
    #15. Service Disabled Veteran Owned Small Business Actions
    service_disabled_veteran_owned_small_business_actions <- append(service_disabled_veteran_owned_small_business_actions, sb_excluded_df %>% 
                                                                      filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & srdvob_flag == "SRDVOB") %>% 
                                                                      count() %>%
                                                                      collect()%>%
                                                                      .$n)
      
      
    #16. service_disabled_veteran_owned_small_business_dollars
    service_disabled_veteran_owned_small_business_dollars_obligations <- sb_excluded_df %>%
      filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & srdvob_flag == "SRDVOB") %>%
      select(dollars_obligated) %>% 
      collect()
    
    service_disabled_veteran_owned_small_business_dollars_count <- service_disabled_veteran_owned_small_business_dollars_obligations %>% count() %>% .$n 
    
    if(service_disabled_veteran_owned_small_business_dollars_count > 0 ) 
      service_disabled_veteran_owned_small_business_dollars <- append(service_disabled_veteran_owned_small_business_dollars, service_disabled_veteran_owned_small_business_dollars_obligations %>% sum() )                                       
    else service_disabled_veteran_owned_small_business_dollars <- append(service_disabled_veteran_owned_small_business_dollars, c(0) ) 
    
    #17. service_disabled_veteran_owned_small_business_percentage
    
    service_disabled_veteran_owned_small_business_percentage <- append( service_disabled_veteran_owned_small_business_percentage,   service_disabled_veteran_owned_small_business_dollars[i] / small_business_eligble_dollars[i])
    
    
    #18. Women Owned Small Business Actions
    women_owned_small_business_actions <- append(women_owned_small_business_actions, sb_excluded_df %>% 
                                                                      filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB") %>%
                                                                      filter(wosb_flag == "WOSB" | women_owned_flag == "WO" | jvwosb_flag == "JVWOSB" | edwosb_flag == "EDWOSB" | edjvwosb_flag == "EDJVWOSB") %>% 
                                                                      count() %>%
                                                                      collect()%>%
                                                                      .$n)
    
    #19. Women Owned Small Business dollars
    women_owned_small_business_dollars_obligations <- sb_excluded_df %>% 
      filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB") %>%
      filter(wosb_flag == "WOSB" | women_owned_flag == "WO" | jvwosb_flag == "JVWOSB" | edwosb_flag == "EDWOSB" | edjvwosb_flag == "EDJVWOSB")%>% 
      select(dollars_obligated) %>% collect()
    
    women_owned_small_business_dollars_count <- women_owned_small_business_dollars_obligations %>% count() %>% .$n 
    
    if(women_owned_small_business_dollars_count > 0 ) 
      women_owned_small_business_dollars <- append(women_owned_small_business_dollars, women_owned_small_business_dollars_obligations %>% sum() )                                       
    else women_owned_small_business_dollars <- append(women_owned_small_business_dollars, c(0) ) 
    
    #20. Women Owned Small Business percentage
    
    women_owned_small_business_percentage <- append( women_owned_small_business_percentage,   women_owned_small_business_dollars[i] / small_business_eligble_dollars[i])
    
    #21. Certified HUBZone Small Business Actions
    certified_HUBZone_small_business_actions <- append(certified_HUBZone_small_business_actions, sb_excluded_df %>% 
                                       filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & hubzone_flag == "HUBZ") %>% 
                                       count() %>%
                                       collect()%>% 
                                       .$n)
    
    #22. Certified HUBZone Small Business Dollars
    certified_HUBZone_small_business_dollars_obligations <- sb_excluded_df %>% 
      filter(funding_department_name == funding_department_list[i] & co_bus_size_determination_code == "CO_SB" & hubzone_flag == "HUBZ") %>% 
      select(dollars_obligated) %>% collect()
    
    certified_HUBZone_small_business_dollars_count <- certified_HUBZone_small_business_dollars_obligations %>% count() %>% .$n 
    
    if(certified_HUBZone_small_business_dollars_count> 0 ) 
      certified_HUBZone_small_business_dollars <- append(certified_HUBZone_small_business_dollars, certified_HUBZone_small_business_dollars_obligations %>% sum() )                                       
    else certified_HUBZone_small_business_dollars <- append(certified_HUBZone_small_business_dollars, c(0) ) 
    
    #23.  Certified HUBZone Small Business Percentage
    certified_HUBZone_small_business_percentage <- append( certified_HUBZone_small_business_percentage,   certified_HUBZone_small_business_dollars[i] / small_business_eligble_dollars[i])
    
    print(paste0("completed ", funding_department_list[i]))
    
    
  }
  
  result_df <- data_frame(funding_department_list, 
                          small_business_eligible_actions, 
                          small_business_eligble_dollars, 
                          small_business_actions, 
                          unique_small_business_vendors, 
                          small_business_dollars, 
                          small_business_percentage,
                          small_disadvantaged_business_actions,
                          small_disadvantaged_business_dollars,
                          small_disadvantaged_business_percentage,
                          eight_a_procedure_actions,
                          eight_a_procedure_action_dollars, 
                          eight_a_procedure_percentage,
                          veterans_owned_small_business_actions,
                          veterans_owned_small_business_dollars,
                          veterans_owned_small_business_percentage,
                          service_disabled_veteran_owned_small_business_actions,
                          service_disabled_veteran_owned_small_business_dollars,
                          service_disabled_veteran_owned_small_business_percentage,
                          women_owned_small_business_actions,
                          women_owned_small_business_dollars,
                          women_owned_small_business_percentage,
                          certified_HUBZone_small_business_actions,
                          certified_HUBZone_small_business_dollars,
                          certified_HUBZone_small_business_percentage)
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
  toc()
  result_df
}


bic_spark_small_business_goaling_report <- function (sb_exclude_src_df)
{
  sc <- sparkInit()
  print("reading in exclusion marked df")
  tic()
  #sb_excluded_df <<- sdf_copy_to(sc, sb_exclude_src_df, overwrite = TRUE)
  spark_df <<- sdf_copy_to(sc, sb_exclude_src_df, overwrite = TRUE)
  sb_excluded_df <- spark_df %>% filter(level_1_category_group == "GWCM")
  toc()
  tic()
  bic_list <- sb_excluded_df %>% filter(official_bic_flag == "Y") %>% select(contract_name) %>%
    distinct() %>% 
    collect() %>% 
    na.omit()%>% 
    .$contract_name
  
  bic_length <- length(bic_list)
  small_business_eligible_actions <- c()
  small_business_eligble_dollars <- c()
  small_business_actions <- c()
  unique_small_business_vendors <- c()
  small_business_dollars <- c()
  small_business_percentage <- c()
  small_disadvantaged_business_actions <-c()
  small_disadvantaged_business_dollars <- c()
  small_disadvantaged_business_percentage <-c()
  eight_a_procedure_actions <- c()
  eight_a_procedure_action_dollars <- c() 
  eight_a_procedure_percentage <- c()
  veterans_owned_small_business_actions <- c()
  veterans_owned_small_business_dollars <- c()
  veterans_owned_small_business_percentage <- c()
  service_disabled_veteran_owned_small_business_actions <- c()
  service_disabled_veteran_owned_small_business_dollars <- c()
  service_disabled_veteran_owned_small_business_percentage <- c()
  women_owned_small_business_actions <- c()
  women_owned_small_business_dollars <- c()
  women_owned_small_business_percentage <- c()
  certified_HUBZone_small_business_actions <- c()
  certified_HUBZone_small_business_dollars <- c()
  certified_HUBZone_small_business_percentage <- c()
  
  
  for(i in 1:bic_length)
  {
    #1. small busines eligible actions
    small_business_eligible_actions <- append(small_business_eligible_actions, sb_excluded_df %>% 
                                                filter(contract_name == bic_list[i] & sb_exclude == "FALSE") %>% count() %>%collect()%>% .$n)
    #2. small business eligible dollars
    small_business_eligible_obligations <- sb_excluded_df %>% 
      filter(contract_name == bic_list[i] & sb_exclude == "FALSE") %>% 
      select(dollars_obligated) %>% collect()
    
    small_business_eligble_dollars_count <- small_business_eligible_obligations %>% count() %>% .$n 
    if(small_business_eligble_dollars_count> 0 ) 
      small_business_eligble_dollars <- append(small_business_eligble_dollars,small_business_eligible_obligations %>% sum() )                                       
    else small_business_eligble_dollars <- append(small_business_eligble_dollars, c(0) )                                         
    
    #3. small business actions
    small_business_actions <- append(small_business_actions, sb_excluded_df %>% 
                                       filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>% count() %>%collect()%>% .$n)
    
    #3.5 unique small business vendors
    unique_small_business_vendors <- append(unique_small_business_vendors, sb_excluded_df %>% 
                                              filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>%
                                              distinct(vendor_duns_number) %>% count() %>% collect() %>% .$n)
    
    
    #4. small business dollars
    small_business_dollars_obligations <- sb_excluded_df %>% 
      filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>% 
      select(dollars_obligated) %>% collect()
    
    
    
    small_business_dollars_count <- small_business_dollars_obligations %>% count() %>% .$n 
    
    if(small_business_dollars_count> 0 ) 
      small_business_dollars <- append(small_business_dollars, small_business_dollars_obligations %>% sum() )                                       
    else small_business_dollars <- append(small_business_dollars, c(0) ) 
    
    #5. small business percentage
    small_business_percentage <- append(small_business_percentage, small_business_dollars[i]/small_business_eligble_dollars[i])
    
    #6. small business disadvantaged actions
    small_disadvantaged_business_actions <- append(small_disadvantaged_business_actions, sb_excluded_df %>% 
                                                     filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>%
                                                     filter(sdb == "SDB" | sdb_flag == "SDB_FLAG" | firm8a_joint_venture == "F8AJV"| eight_a_flag == "YES") %>% 
                                                     count() %>%
                                                     collect()%>%
                                                     .$n)
    
    ####  hbcu_flag and minority_institution_flag needed for dod_nasa_uscg
    
    
    
    #7. small business disadvantaged dollars 
    small_disadvantaged_business_dollars_obligations <- sb_excluded_df %>%
      filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & sb_exclude == "FALSE") %>%
      filter(sdb == "SDB" | sdb_flag == "SDB_FLAG" | firm8a_joint_venture == "F8AJV"| eight_a_flag == "YES") %>%
      select(dollars_obligated) %>% 
      collect()
    
    
    
    small_disadvantaged_business_dollars_count <- small_disadvantaged_business_dollars_obligations %>% count() %>% .$n 
    
    if(small_disadvantaged_business_dollars_count > 0 ) 
      small_disadvantaged_business_dollars <- append(small_disadvantaged_business_dollars, small_disadvantaged_business_dollars_obligations %>% sum() )                                       
    else small_disadvantaged_business_dollars <- append(small_disadvantaged_business_dollars, c(0) ) 
    
    #8. small business disadvantaged percentage 
    small_disadvantaged_business_percentage <- append( small_disadvantaged_business_percentage,   small_disadvantaged_business_dollars[i] / small_business_eligble_dollars[i])
    
    #9 8(a) Procedure Actions
    eight_a_procedure_actions <- append(eight_a_procedure_actions, sb_excluded_df %>% 
                                          filter(contract_name == bic_list[i] & sb_exclude == "FALSE") %>% 
                                          filter(co_bus_size_determination_code == "CO_SB") %>% #& eight_a_flag == "YES") %>%
                                          filter(type_of_set_aside == "8N" | type_of_set_aside =="HS2" | type_of_set_aside =="HS3" |type_of_set_aside == "8A") %>%
                                          count() %>%
                                          collect()%>%
                                          .$n)
    
    
    #10 8(a) Procedure Dollars:
    eight_a_procedure_action_dollars_obligations <- sb_excluded_df %>%
      filter(contract_name == bic_list[i])%>% # & sb_exclude == "FALSE") %>% 
      filter(co_bus_size_determination_code == "CO_SB" & eight_a_flag == "YES") %>%
      #filter(type_of_set_aside == "8(a) Sole Source" | type_of_set_aside =="8(a) with HUB Zone" | type_of_set_aside == "8(a) Competed") %>%
      select(dollars_obligated) %>% 
      collect()
    
    eight_a_procedure_action_dollars_count <- eight_a_procedure_action_dollars_obligations %>% count() %>% .$n 
    
    if(eight_a_procedure_action_dollars_count > 0 ) 
      eight_a_procedure_action_dollars <- append( eight_a_procedure_action_dollars, eight_a_procedure_action_dollars_obligations %>% sum() )                                       
    else eight_a_procedure_action_dollars <- append(eight_a_procedure_action_dollars, c(0) ) 
    
    #11. 8(a) Procedure Percentage
    eight_a_procedure_percentage <- append( eight_a_procedure_percentage,   eight_a_procedure_action_dollars[i] / small_business_eligble_dollars[i])
    
    #12. Veteran Owned Small Business Actions
    veterans_owned_small_business_actions <- append(veterans_owned_small_business_actions, sb_excluded_df %>% 
                                                      filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & veteran_owned_flag == "VO") %>% 
                                                      count() %>%
                                                      collect()%>%
                                                      .$n)
    
    #13. Veteran Owned Small Business dollars
    veterans_owned_small_business_dollars_obligations <- sb_excluded_df %>%
      filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & veteran_owned_flag == "VO") %>%
      select(dollars_obligated) %>% 
      collect()
    
    veterans_owned_small_business_dollars_count <- veterans_owned_small_business_dollars_obligations %>% count() %>% .$n 
    
    if(veterans_owned_small_business_dollars_count > 0 ) 
      veterans_owned_small_business_dollars <- append(veterans_owned_small_business_dollars, veterans_owned_small_business_dollars_obligations %>% sum() )                                       
    else veterans_owned_small_business_dollars <- append(veterans_owned_small_business_dollars, c(0) ) 
    
    #14. veterans_owned_small_business_percentage
    veterans_owned_small_business_percentage <- append( veterans_owned_small_business_percentage,   veterans_owned_small_business_dollars[i] / small_business_eligble_dollars[i])
    
    #15. Service Disabled Veteran Owned Small Business Actions
    service_disabled_veteran_owned_small_business_actions <- append(service_disabled_veteran_owned_small_business_actions, sb_excluded_df %>% 
                                                                      filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & srdvob_flag == "SRDVOB") %>% 
                                                                      count() %>%
                                                                      collect()%>%
                                                                      .$n)
    
    
    #16. service_disabled_veteran_owned_small_business_dollars
    service_disabled_veteran_owned_small_business_dollars_obligations <- sb_excluded_df %>%
      filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & srdvob_flag == "SRDVOB") %>%
      select(dollars_obligated) %>% 
      collect()
    
    service_disabled_veteran_owned_small_business_dollars_count <- service_disabled_veteran_owned_small_business_dollars_obligations %>% count() %>% .$n 
    
    if(service_disabled_veteran_owned_small_business_dollars_count > 0 ) 
      service_disabled_veteran_owned_small_business_dollars <- append(service_disabled_veteran_owned_small_business_dollars, service_disabled_veteran_owned_small_business_dollars_obligations %>% sum() )                                       
    else service_disabled_veteran_owned_small_business_dollars <- append(service_disabled_veteran_owned_small_business_dollars, c(0) ) 
    
    #17. service_disabled_veteran_owned_small_business_percentage
    
    service_disabled_veteran_owned_small_business_percentage <- append( service_disabled_veteran_owned_small_business_percentage,   service_disabled_veteran_owned_small_business_dollars[i] / small_business_eligble_dollars[i])
    
    
    #18. Women Owned Small Business Actions
    women_owned_small_business_actions <- append(women_owned_small_business_actions, sb_excluded_df %>% 
                                                   filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB") %>%
                                                   filter(wosb_flag == "WOSB" | women_owned_flag == "WO" | jvwosb_flag == "JVWOSB" | edwosb_flag == "EDWOSB" | edjvwosb_flag == "EDJVWOSB") %>% 
                                                   count() %>%
                                                   collect()%>%
                                                   .$n)
    
    #19. Women Owned Small Business dollars
    women_owned_small_business_dollars_obligations <- sb_excluded_df %>% 
      filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB") %>%
      filter(wosb_flag == "WOSB" | women_owned_flag == "WO" | jvwosb_flag == "JVWOSB" | edwosb_flag == "EDWOSB" | edjvwosb_flag == "EDJVWOSB")%>% 
      select(dollars_obligated) %>% collect()
    
    women_owned_small_business_dollars_count <- women_owned_small_business_dollars_obligations %>% count() %>% .$n 
    
    if(women_owned_small_business_dollars_count > 0 ) 
      women_owned_small_business_dollars <- append(women_owned_small_business_dollars, women_owned_small_business_dollars_obligations %>% sum() )                                       
    else women_owned_small_business_dollars <- append(women_owned_small_business_dollars, c(0) ) 
    
    #20. Women Owned Small Business percentage
    
    women_owned_small_business_percentage <- append( women_owned_small_business_percentage,   women_owned_small_business_dollars[i] / small_business_eligble_dollars[i])
    
    #21. Certified HUBZone Small Business Actions
    certified_HUBZone_small_business_actions <- append(certified_HUBZone_small_business_actions, sb_excluded_df %>% 
                                                         filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & hubzone_flag == "HUBZ") %>% 
                                                         count() %>%
                                                         collect()%>% 
                                                         .$n)
    
    #22. Certified HUBZone Small Business Dollars
    certified_HUBZone_small_business_dollars_obligations <- sb_excluded_df %>% 
      filter(contract_name == bic_list[i] & co_bus_size_determination_code == "CO_SB" & hubzone_flag == "HUBZ") %>% 
      select(dollars_obligated) %>% collect()
    
    certified_HUBZone_small_business_dollars_count <- certified_HUBZone_small_business_dollars_obligations %>% count() %>% .$n 
    
    if(certified_HUBZone_small_business_dollars_count> 0 ) 
      certified_HUBZone_small_business_dollars <- append(certified_HUBZone_small_business_dollars, certified_HUBZone_small_business_dollars_obligations %>% sum() )                                       
    else certified_HUBZone_small_business_dollars <- append(certified_HUBZone_small_business_dollars, c(0) ) 
    
    #23.  Certified HUBZone Small Business Percentage
    certified_HUBZone_small_business_percentage <- append( certified_HUBZone_small_business_percentage,   certified_HUBZone_small_business_dollars[i] / small_business_eligble_dollars[i])
    
    print(paste0("completed ", bic_list[i]))
    
    
  }
  
  result_df <- data_frame(bic_list, 
                          small_business_eligible_actions, 
                          small_business_eligble_dollars, 
                          small_business_actions, 
                          unique_small_business_vendors,
                          small_business_dollars, 
                          small_business_percentage,
                          small_disadvantaged_business_actions,
                          small_disadvantaged_business_dollars,
                          small_disadvantaged_business_percentage,
                          eight_a_procedure_actions,
                          eight_a_procedure_action_dollars, 
                          eight_a_procedure_percentage,
                          veterans_owned_small_business_actions,
                          veterans_owned_small_business_dollars,
                          veterans_owned_small_business_percentage,
                          service_disabled_veteran_owned_small_business_actions,
                          service_disabled_veteran_owned_small_business_dollars,
                          service_disabled_veteran_owned_small_business_percentage,
                          women_owned_small_business_actions,
                          women_owned_small_business_dollars,
                          women_owned_small_business_percentage,
                          certified_HUBZone_small_business_actions,
                          certified_HUBZone_small_business_dollars,
                          certified_HUBZone_small_business_percentage)
  
  
  
  
  
  
  
  
  
  
  toc()
  result_df
}


#colnames(sb_exclusions_mod %>% select(grep("veteran", colnames(sb_exclusions_mod))))