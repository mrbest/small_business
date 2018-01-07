library(parallel)
library(multidplyr)
library(dplyr)
source("small_business.R")
source("../CombinedAddressability/sprklyRSpark.R")

add_groups <- function(incoming_df)
{
cl <<- detectCores()
group <<- rep(1:cl, length.out = nrow(incoming_df))  
incoming_df <- bind_cols(tibble(group), incoming_df)
incoming_df
}

initClusteredProc <- function(df)
{
  df <- add_groups(df)
  
  cluster <<- create_cluster(core = cl)
  
  by_group <<- df %>% 
              partition(group, cluster = cluster)

    by_group %>% 
  cluster_assign_value("designate_small_business_eligble", designate_small_business_eligble) %>%
  cluster_assign_value("eval_jwod", eval_jwod) %>%
  cluster_assign_value("eval_oconus_pre_100115", eval_oconus_pre_100115)  %>%
  cluster_assign_value("eval_unicor_duns", eval_unicor_duns)  %>%
  cluster_assign_value("eval_american_institute_in_taiwan", eval_american_institute_in_taiwan)  %>%
  cluster_assign_value("eval_contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15", eval_contigency_humanitarian_peacekeeping_DoD_ocounus_post_01OCT15)  %>%
  cluster_assign_value("eval_foreign_govts_entities_intnl_orgs", eval_foreign_govts_entities_intnl_orgs)  %>%
  cluster_assign_value("eval_agency_generated_sources", eval_agency_generated_sources) %>%
  cluster_assign_value("eval_acqisitionsNotCompeted_Resale", eval_acqisitionsNotCompeted_Resale) %>%
  cluster_assign_value("eval_lease_PSCs", eval_lease_PSCs) %>%
  cluster_assign_value("eval_UtilitiesPSC_S112", eval_UtilitiesPSC_S112) %>%
  cluster_assign_value("eval_TricareDODAAC_H9400_HT9402ContractingOffice", eval_TricareDODAAC_H9400_HT9402ContractingOffice) %>%
  cluster_assign_value("eval_CentersForMedicaidMedicareServices_00NAF_ContractingOffice", eval_CentersForMedicaidMedicareServices_00NAF_ContractingOffice) %>%
  cluster_assign_value("designate_small_business_eligble", designate_small_business_eligble) %>%
  cluster_assign_value("eval_DeptEducationNFP", eval_DeptEducationNFP)
  
  
}