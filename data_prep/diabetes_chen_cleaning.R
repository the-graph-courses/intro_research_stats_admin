# Diabetes Chen data sample and clean some variables
## GRAPH Courses team
## 2025-05-26

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, openxlsx, readxl, janitor)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Main ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

diab_china_raw <- readxl::read_excel(here("data/RC Health Care Data-20180820.xlsx"))

diab_china1 <- diab_china_raw %>% 
  janitor::clean_names()
  
colnames(diab_china1) <-  c("id",                                                              
"age",                                                           
"gender",                                          
"site",                                                            
"height_cm",                                                       
"weight_kg",                                                       
"bmi",                                                       
"sbp_mm_hg",                                                       
"dbp_mm_hg",                                                       
"fpg_mmol_l",                                                      
"cholesterol_mmol_l",                                              
"triglyceride_mmol_l",                                             
"hdl_c_mmol_l",                                                    
"ldl_mmol_l",                                                      
"alt_u_l",                                                         
"ast_u_l",                                                         
"bun_mmol_l",                                                      
"ccr_umol_l",                                                      
"fpg_of_final_visit_mmol_l",                                       
"diabetes_diagnosed_during_followup",                        
"censor_of_diabetes_at_followup",                       
"year_of_followup",                                                
"smoking_status",    
"drinking_status",
"family_histroy_of_diabetes")

write_csv(diab_china1, here::here("data/diabetes_china_chen.csv"))
