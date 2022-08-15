


###   0. fetch, change company variable values and add cpi column from "Create dataset.R"
##### 1. fill in the first select properly 
###   2. get the demos correct for the second select
####  3. and then just change the names from Expanse to the actual Expanse name.
Expanse <- fetch_survey(surveyID = "SV_206cA68Eq1bA3E9",label = TRUE, force_request = TRUE)

Expanse$Company <- "Expanse"
Expanse <- add_column(Expanse, CPI = "old")
Expanse <- add_column(Expanse, All = "All")

Select <- c("ResponseID",	"StartDate",	"EndDate",	"Company",	"Industry",	"demo",	"demo_1",	"demo_2",	"demo_3",	"demo_4",	"demo_5", "All",	"cpi_vv_1",	"cpi_vv_2",	"cpi_vv_3",	"cpi_vv_4",	"cpi_vv_5",	"cpi_strat_1",	"cpi_strat_2",	"cpi_strat_3",	"cpi_strat_4",	"cpi_strat_5",	"cpi_lead_11",	"cpi_lead_12",	"cpi_lead_14",	"cpi_lead_13",	"cpi_lead_15",	"Adaptability_11",	"Adaptability_12",	"Adaptability_13",	"Adaptability_14",	"Adaptability_15",	"PerfMan_6",	"PerfMan_7",	"PerfMan_8",	"PerfMan_9",	"PerfMan_10",	"SysProc_1",	"SysProc_2",	"SysProc_3",	"SysProc_4",	"SysProc_5",	"Teamwork_1",	"Teamwork_2",	"Teamwork_3",	"Teamwork_4",	"Teamwork_5",	"TalMan_6",	"TalMan_7",	"TalMan_8",	"TalMan_9",	"TalMan_10",	"CoachDev_11",	"CoachDev_12",	"CoachDev_13",	"CoachDev_14",	"CoachDev_15",	"EmpClarFit_1",	"EmpClarFit_2",	"EmpClarFit_3",	"EmpClarFit_4",	"EmpClarFit_5",	"CustFoc_6",	"CustFoc_7",	"CustFoc_8",	"CustFoc_9",	"CustFoc_10",	"Comm_11",	"Comm_12",	"Comm_13",	"Comm_14",	"Comm_15",	"Positivity_11",	"Positivity_12",	"Positivity_13",	"Positivity_14",	"Positivity_15",	"VQ_11",	"VQ_12",	"NPS",	"CPI"
)
Expanse <- Expanse %>% select(Select)

Columns <- c("ResponseID","StartDate",	"EndDate", "Company",	"Industry",	
             "Tenure",	"Gender",	"Generation",	"Level",	"Division",	"Role", "All",
             "cpi_vv_1",	"cpi_vv_2",	"cpi_vv_3",	"cpi_vv_4",	"cpi_vv_5",	"cpi_strat_1",	"cpi_strat_2",	"cpi_strat_3",	"cpi_strat_4",	"cpi_strat_5",	"cpi_lead_1",	"cpi_lead_2",	"cpi_lead_3",	"cpi_lead_4",	"cpi_lead_5",	
             "cpi_adapt_1",	"cpi_adapt_2",	"cpi_adapt_3",	"cpi_adapt_4",	"cpi_adapt_5",	"cpi_perfman_1",	"cpi_perfman_2",	"cpi_perfman_3",	"cpi_perfman_4",	"cpi_perfman_5",	"cpi_sysproc_1",	"cpi_sysproc_2",	"cpi_sysproc_3",	"cpi_sysproc_4",	"cpi_sysproc_5",
             "cpi_team_1",	"cpi_team_2",	"cpi_team_3",	"cpi_team_4",	"cpi_team_5",	"cpi_talman_1",	"cpi_talman_2",	"cpi_talman_3",	"cpi_talman_4",	"cpi_talman_5",	"cpi_cd_1",	"cpi_cd_2",	"cpi_cd_3",	"cpi_cd_4",	"cpi_cd_5",	
             "cpi_ecf_1",	"cpi_ecf_2",	"cpi_ecf_3",	"cpi_ecf_4",	"cpi_ecf_5",	"cpi_custfoc_1",	"cpi_custfoc_2",	"cpi_custfoc_3",	"cpi_custfoc_4",	"cpi_custfoc_5",	"cpi_comm_1",	"cpi_comm_2",	"cpi_comm_3",	"cpi_comm_4",	"cpi_comm_5",	
             "positivity_1",	"positivity_2",	"positivity_3",	"positivity_4",	"positivity_5",	"satisfaction",	"effort",	
             	"nps",	"CPI")

colnames(Expanse) <- Columns
Expanse <- Expanse %>% select(ResponseID, Company, CPI, StartDate,EndDate,          cpi_vv_1:effort,nps,Tenure, Generation, Gender, All, Level, Role, Division )



# RECODE TO NUMERICAL
#### STEP 1
Expanse <- Expanse %>% mutate(
  cpi_vv_1 = recode(cpi_vv_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_vv_2 = recode(cpi_vv_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_vv_3 = recode(cpi_vv_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_vv_4 = recode(cpi_vv_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_vv_5 = recode(cpi_vv_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_1 = recode(cpi_strat_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_2 = recode(cpi_strat_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_3 = recode(cpi_strat_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_4 = recode(cpi_strat_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_5 = recode(cpi_strat_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_1 = recode(cpi_lead_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_2 = recode(cpi_lead_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_3 = recode(cpi_lead_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_4 = recode(cpi_lead_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_5 = recode(cpi_lead_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_1 = recode(cpi_adapt_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_2 = recode(cpi_adapt_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_3 = recode(cpi_adapt_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_4 = recode(cpi_adapt_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_5 = recode(cpi_adapt_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_1 = recode(cpi_perfman_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_2 = recode(cpi_perfman_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_3 = recode(cpi_perfman_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_4 = recode(cpi_perfman_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_5 = recode(cpi_perfman_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_1 = recode(cpi_sysproc_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_2 = recode(cpi_sysproc_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_3 = recode(cpi_sysproc_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_4 = recode(cpi_sysproc_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_5 = recode(cpi_sysproc_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_1 = recode(cpi_team_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_2 = recode(cpi_team_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_3 = recode(cpi_team_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_4 = recode(cpi_team_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_5 = recode(cpi_team_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_1 = recode(cpi_talman_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_2 = recode(cpi_talman_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_3 = recode(cpi_talman_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_4 = recode(cpi_talman_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_5 = recode(cpi_talman_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_1 = recode(cpi_cd_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_2 = recode(cpi_cd_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_3 = recode(cpi_cd_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_4 = recode(cpi_cd_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_5 = recode(cpi_cd_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_1 = recode(cpi_ecf_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_2 = recode(cpi_ecf_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_3 = recode(cpi_ecf_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_4 = recode(cpi_ecf_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_5 = recode(cpi_ecf_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_1 = recode(cpi_custfoc_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_2 = recode(cpi_custfoc_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_3 = recode(cpi_custfoc_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_4 = recode(cpi_custfoc_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_5 = recode(cpi_custfoc_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_1 = recode(cpi_comm_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_2 = recode(cpi_comm_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_3 = recode(cpi_comm_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_4 = recode(cpi_comm_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_5 = recode(cpi_comm_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_1 = recode(positivity_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_2 = recode(positivity_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_3 = recode(positivity_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_4 = recode(positivity_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_5 = recode(positivity_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  satisfaction = recode(satisfaction, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  effort = recode(effort, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"))


# RECODE TO NUMERICAL
#### STEP 2
Expanse <- Expanse %>% mutate(cpi_vv_1 = as.integer(cpi_vv_1),cpi_vv_2 = as.integer(cpi_vv_2),cpi_vv_3 = as.integer(cpi_vv_3),cpi_vv_4 = as.integer(cpi_vv_4),cpi_vv_5 = as.integer(cpi_vv_5),cpi_strat_1 = as.integer(cpi_strat_1),cpi_strat_2 = as.integer(cpi_strat_2),cpi_strat_3 = as.integer(cpi_strat_3),cpi_strat_4 = as.integer(cpi_strat_4),cpi_strat_5 = as.integer(cpi_strat_5),cpi_lead_1 = as.integer(cpi_lead_1),cpi_lead_2 = as.integer(cpi_lead_2),cpi_lead_3 = as.integer(cpi_lead_3),cpi_lead_4 = as.integer(cpi_lead_4),cpi_lead_5 = as.integer(cpi_lead_5),cpi_adapt_1 = as.integer(cpi_adapt_1),cpi_adapt_2 = as.integer(cpi_adapt_2),cpi_adapt_3 = as.integer(cpi_adapt_3),cpi_adapt_4 = as.integer(cpi_adapt_4),cpi_adapt_5 = as.integer(cpi_adapt_5),cpi_perfman_1 = as.integer(cpi_perfman_1),cpi_perfman_2 = as.integer(cpi_perfman_2),cpi_perfman_3 = as.integer(cpi_perfman_3),cpi_perfman_4 = as.integer(cpi_perfman_4),cpi_perfman_5 = as.integer(cpi_perfman_5),cpi_sysproc_1 = as.integer(cpi_sysproc_1),cpi_sysproc_2 = as.integer(cpi_sysproc_2),cpi_sysproc_3 = as.integer(cpi_sysproc_3),cpi_sysproc_4 = as.integer(cpi_sysproc_4),cpi_sysproc_5 = as.integer(cpi_sysproc_5),cpi_team_1 = as.integer(cpi_team_1),cpi_team_2 = as.integer(cpi_team_2),cpi_team_3 = as.integer(cpi_team_3),cpi_team_4 = as.integer(cpi_team_4),cpi_team_5 = as.integer(cpi_team_5),cpi_talman_1 = as.integer(cpi_talman_1),cpi_talman_2 = as.integer(cpi_talman_2),cpi_talman_3 = as.integer(cpi_talman_3),cpi_talman_4 = as.integer(cpi_talman_4),cpi_talman_5 = as.integer(cpi_talman_5),cpi_cd_1 = as.integer(cpi_cd_1),cpi_cd_2 = as.integer(cpi_cd_2),cpi_cd_3 = as.integer(cpi_cd_3),cpi_cd_4 = as.integer(cpi_cd_4),cpi_cd_5 = as.integer(cpi_cd_5),cpi_ecf_1 = as.integer(cpi_ecf_1),cpi_ecf_2 = as.integer(cpi_ecf_2),cpi_ecf_3 = as.integer(cpi_ecf_3),cpi_ecf_4 = as.integer(cpi_ecf_4),cpi_ecf_5 = as.integer(cpi_ecf_5),cpi_custfoc_1 = as.integer(cpi_custfoc_1),cpi_custfoc_2 = as.integer(cpi_custfoc_2),cpi_custfoc_3 = as.integer(cpi_custfoc_3),cpi_custfoc_4 = as.integer(cpi_custfoc_4),cpi_custfoc_5 = as.integer(cpi_custfoc_5),cpi_comm_1 = as.integer(cpi_comm_1),cpi_comm_2 = as.integer(cpi_comm_2),cpi_comm_3 = as.integer(cpi_comm_3),cpi_comm_4 = as.integer(cpi_comm_4),cpi_comm_5 = as.integer(cpi_comm_5),positivity_1 = as.integer(positivity_1),positivity_2 = as.integer(positivity_2),positivity_3 = as.integer(positivity_3),positivity_4 = as.integer(positivity_4),positivity_5 = as.integer(positivity_5),satisfaction = as.integer(satisfaction),effort = as.integer(effort)
)

## CALUCULATE MEANS

Expanse <- Expanse %>% rowwise() %>%  
  mutate(CPI_Index = mean(c(cpi_vv_1, 	cpi_vv_2, 	cpi_vv_3, 	cpi_vv_4, 	cpi_vv_5, 	
                            cpi_strat_1, 	cpi_strat_2, 	cpi_strat_3, 	cpi_strat_4, 	cpi_strat_5, 	
                            cpi_lead_1, 	cpi_lead_2, 	cpi_lead_3, 	cpi_lead_4, 	cpi_lead_5, 	
                            cpi_adapt_1, 	cpi_adapt_2, 	cpi_adapt_3, 	cpi_adapt_4, 	cpi_adapt_5, 	
                            cpi_perfman_1, 	cpi_perfman_2, 	cpi_perfman_3, 	cpi_perfman_4, 	cpi_perfman_5, 	
                            cpi_sysproc_1, 	cpi_sysproc_2, 	cpi_sysproc_3, 	cpi_sysproc_4, 	cpi_sysproc_5, 	
                            cpi_team_1, 	cpi_team_2, 	cpi_team_3, 	cpi_team_4, 	cpi_team_5, 	
                            cpi_talman_1, 	cpi_talman_2, 	cpi_talman_3, 	cpi_talman_4, 	cpi_talman_5, 	
                            cpi_cd_1, 	cpi_cd_2, 	cpi_cd_3, 	cpi_cd_4, 	cpi_cd_5, 	
                            cpi_ecf_1, 	cpi_ecf_2, 	cpi_ecf_3, 	cpi_ecf_4, 	cpi_ecf_5, 	
                            cpi_custfoc_1, 	cpi_custfoc_2, 	cpi_custfoc_3, 	cpi_custfoc_4, 	cpi_custfoc_5, 	
                            cpi_comm_1, 	cpi_comm_2, 	cpi_comm_3, 	cpi_comm_4, 	cpi_comm_5)),
         DIRECTION = mean(c(cpi_vv_1, 	cpi_vv_2, 	cpi_vv_3, 	cpi_vv_4, 	cpi_vv_5, 	
                            cpi_strat_1, 	cpi_strat_2, 	cpi_strat_3, 	cpi_strat_4, 	cpi_strat_5, 	
                            cpi_lead_1, 	cpi_lead_2, 	cpi_lead_3, 	cpi_lead_4, 	cpi_lead_5)),
         OPERATIONS = mean(c( cpi_adapt_1, 	cpi_adapt_2, 	cpi_adapt_3, 	cpi_adapt_4, 	cpi_adapt_5, 	
                              cpi_perfman_1, 	cpi_perfman_2, 	cpi_perfman_3, 	cpi_perfman_4, 	cpi_perfman_5, 	
                              cpi_sysproc_1, 	cpi_sysproc_2, 	cpi_sysproc_3, 	cpi_sysproc_4, 	cpi_sysproc_5)),
         PEOPLE = mean(c(cpi_team_1, 	cpi_team_2, 	cpi_team_3, 	cpi_team_4, 	cpi_team_5, 	
                         cpi_talman_1, 	cpi_talman_2, 	cpi_talman_3, 	cpi_talman_4, 	cpi_talman_5, 	
                         cpi_cd_1, 	cpi_cd_2, 	cpi_cd_3, 	cpi_cd_4, 	cpi_cd_5)),
         ENGAGEMENT = mean(c(cpi_ecf_1, 	cpi_ecf_2, 	cpi_ecf_3, 	cpi_ecf_4, 	cpi_ecf_5, 	
                             cpi_custfoc_1, 	cpi_custfoc_2, 	cpi_custfoc_3, 	cpi_custfoc_4, 	cpi_custfoc_5, 	
                             cpi_comm_1, 	cpi_comm_2, 	cpi_comm_3, 	cpi_comm_4, 	cpi_comm_5)),
         VisionValues = mean(c(cpi_vv_1, 	cpi_vv_2, 	cpi_vv_3, 	cpi_vv_4, 	cpi_vv_5)), 	
         Strategy = mean(c(           cpi_strat_1, 	cpi_strat_2, 	cpi_strat_3, 	cpi_strat_4, 	cpi_strat_5)), 	
         Leadership = mean(c(          cpi_lead_1, 	cpi_lead_2, 	cpi_lead_3, 	cpi_lead_4, 	cpi_lead_5)),
         Adaptability = mean(c( cpi_adapt_1, 	cpi_adapt_2, 	cpi_adapt_3, 	cpi_adapt_4, 	cpi_adapt_5)), 	
         PerfMan = mean(c(         cpi_perfman_1, 	cpi_perfman_2, 	cpi_perfman_3, 	cpi_perfman_4, 	cpi_perfman_5)), 	
         SysProc = mean(c(         cpi_sysproc_1, 	cpi_sysproc_2, 	cpi_sysproc_3, 	cpi_sysproc_4, 	cpi_sysproc_5)),
         Teamwork = mean(c(cpi_team_1, 	cpi_team_2, 	cpi_team_3, 	cpi_team_4, 	cpi_team_5)), 	
         TalMan = mean(c(         cpi_talman_1, 	cpi_talman_2, 	cpi_talman_3, 	cpi_talman_4, 	cpi_talman_5)), 	
         CoachDev = mean(c(            cpi_cd_1, 	cpi_cd_2, 	cpi_cd_3, 	cpi_cd_4, 	cpi_cd_5)),
         EmpClarFit = mean(c(cpi_ecf_1, 	cpi_ecf_2, 	cpi_ecf_3, 	cpi_ecf_4, 	cpi_ecf_5)), 	
         CustFoc = mean(c(                 cpi_custfoc_1, 	cpi_custfoc_2, 	cpi_custfoc_3, 	cpi_custfoc_4, 	cpi_custfoc_5)), 	
         Comm = mean(c(             cpi_comm_1, 	cpi_comm_2, 	cpi_comm_3, 	cpi_comm_4, 	cpi_comm_5)),
         Positivity = mean(c(positivity_1, positivity_2, positivity_3, positivity_4, positivity_5)))
    

## FILTER MISSING Expanse
Expanse <- filter(Expanse, Positivity >= 0)


####  WILL DO FURTHER CLEANING HERE USUALLY BUT FOR SAKE OF TABLEAU NOT FOR NOW

#### SAVE WRITE


#saveRDS(Expanse, file = "~/Dropbox/Clients/Expanse/make_plots/Expanse.rds")
#write_csv(Expanse, "~/Dropbox/Clients/Expanse/analyses/Expanse.csv")
saveRDS(Expanse, paste0(file,"Cleaned/Expanse.rds"))
write_csv(Expanse, paste0(file,"Cleaned/Expanse.csv"))

