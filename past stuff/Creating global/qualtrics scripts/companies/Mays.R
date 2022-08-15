
library(tidyverse)
library(qualtRics)
library(forcats)


## You only need to run the next few lines (qualtrics_api_credentials AND readRenviron) once per session and then you can comment it out if you have to run the script again for some reason

# qualtrics_api_credentials(api_key = "z5sbsGjc9jsc7WxAfqeLDiOmNyuYLX4IZV97TsqX",
#                           base_url = "deutser.co1.qualtrics.com",
#                           install = TRUE)
# readRenviron("~/.Renviron")

Mays <- fetch_survey(surveyID = "SV_86rE96hrjWgpwJT", start_date = 
                        "2019-09-19",label = TRUE, force_request = TRUE)

MaysColumns <- c("ResponseID",	"ResponseSet",	"IPAddress",	"StartDate",	"EndDate",	"RecipientLastName",	"RecipientFirstName",	"RecipientEmail",	"ExternalDataReference",	
             "Finished",	"Status",	"Score-sum",	"Score-weightedAvg",	"Score-weightedStdDev",	"Vision & Values-sum",	"Vision & Values-weightedAvg",	"Vision & Values-weightedStdDev",	
             "Strategy-sum",	"Strategy-weightedAvg",	"Strategy-weightedStdDev",	"Leadership-sum",	"Leadership-weightedAvg",	"Leadership-weightedStdDev",	
             "Adaptability-sum",	"Adaptability-weightedAvg",	"Adaptability-weightedStdDev",	"Performance Management-sum",	"Performance Management-weightedAvg",	"Performance Management-weightedStdDev",	
             "Systems & Processes-sum",	"Systems & Processes-weightedAvg",	"Systems & Processes-weightedStdDev",	"Coaching & Development-sum",	"Coaching & Development-weightedAvg",	"Coaching & Development-weightedStdDev",	
             "Talent Management-sum",	"Talent Management-weightedAvg",	"Talent Management-weightedStdDev",	"Team Capabilities-sum",	"Team Capabilities-weightedAvg",	"Team Capabilities-weightedStdDev",	
             "Employee Clarity & Fit-sum",	"Employee Clarity & Fit-weightedAvg",	"Employee Clarity & Fit-weightedStdDev",	"Customer Focus-sum",	"Customer Focus-weightedAvg",	"Customer Focus-weightedStdDev",	
             "Communication Effectiveness-sum",	"Communication Effectiveness-weightedAvg",	"Communication Effectiveness-weightedStdDev",	"Direction-sum",	"Direction-weightedAvg",	"Direction-weightedStdDev",	
             "Operations-sum",	"Operations-weightedAvg",	"Operations-weightedStdDev",	"People-sum",	"People-weightedAvg",	"People-weightedStdDev",	"Engagement-sum",	"Engagement-weightedAvg",	"Engagement-weightedStdDev",	
             "Company",	"Industry",	"Intro",	"Role",	"Program_2",	"Program_3",	"Program_11",	"Program_13",	"Tenure",	"Generation",	"Gender",
             "cpi_vv_1",	"cpi_vv_2",	"cpi_vv_3",	"cpi_vv_4",	"cpi_vv_5",	"cpi_strat_1",	"cpi_strat_2",	"cpi_strat_3",	"cpi_strat_4",	"cpi_strat_5",	"cpi_lead_1",	"cpi_lead_2",	"cpi_lead_3",	"cpi_lead_4",	"cpi_lead_5",	
             "cpi_adapt_1",	"cpi_adapt_2",	"cpi_adapt_3",	"cpi_adapt_4",	"cpi_adapt_5",	"cpi_perfman_1",	"cpi_perfman_2",	"cpi_perfman_3",	"cpi_perfman_4",	"cpi_perfman_5",	"cpi_sysproc_1",	"cpi_sysproc_2",	"cpi_sysproc_3",	"cpi_sysproc_4",	"cpi_sysproc_5",
             "cpi_team_1",	"cpi_team_2",	"cpi_team_3",	"cpi_team_4",	"cpi_team_5",	"cpi_talman_1",	"cpi_talman_2",	"cpi_talman_3",	"cpi_talman_4",	"cpi_talman_5",	"cpi_cd_1",	"cpi_cd_2",	"cpi_cd_3",	"cpi_cd_4",	"cpi_cd_5",	
             "cpi_ecf_1",	"cpi_ecf_2",	"cpi_ecf_3",	"cpi_ecf_4",	"cpi_ecf_5",	"cpi_custfoc_1",	"cpi_custfoc_2",	"cpi_custfoc_3",	"cpi_custfoc_4",	"cpi_custfoc_5",	"cpi_comm_1",	"cpi_comm_2",	"cpi_comm_3",	"cpi_comm_4",	"cpi_comm_5",	
             "positivity_1",	"positivity_2",	"positivity_3",	"positivity_4",	"positivity_5",	"satisfaction",	"effort",	"Characteristics_1_TEXT",	"Characteristics_2_TEXT",	"Characteristics_3_TEXT",	"Characteristics_4_TEXT",	"Characteristics_5_TEXT",	
             "Purpose_1_TEXT",	"Purpose_2_TEXT",	"Purpose_3_TEXT",	"Purpose_4_TEXT",	"Purpose_5_TEXT",	"Values_1_TEXT",	"Values_2_TEXT",	"Values_3_TEXT",	"Values_4_TEXT",	"Values_5_TEXT",	"nps",	"Change",	"Best Day",	
             "RO-BR-FL_12",	"DO-Q-Direction",	"DO-Q-Operations",	"DO-Q-People",	"DO-Q-Engagement",	"LocationLatitude",	"LocationLongitude",	"LocationAccuracy", "CPI", "All")




Mays <- add_column(Mays, CPI = "new")
Mays <- add_column(Mays, All = "All")
Mays$Company <- "Mays"
colnames(Mays) <- MaysColumns
## 1.  select vars

## The demo section here needs to be personalized
Mays <- Mays %>% select(ResponseID, Company, CPI, StartDate,EndDate, cpi_vv_1:effort,nps, Tenure, Generation, Gender, All, Role)




#### REVERSE CODED ITEMS


Mays <- Mays %>% mutate(cpi_adapt_1 = recode(cpi_adapt_1, "Strongly Disagree" = "Strongly Agree",
                                                 "Disagree" = "Agree",
                                                 "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                 "Agree" = "Disagree",
                                                 "Strongly Agree" = "Strongly Disagree"),
                            cpi_adapt_3 = recode(cpi_adapt_3, "Strongly Disagree" = "Strongly Agree",
                                                 "Disagree" = "Agree",
                                                 "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                 "Agree" = "Disagree",
                                                 "Strongly Agree" = "Strongly Disagree"),
                            cpi_sysproc_1 = recode(cpi_sysproc_1, "Strongly Disagree" = "Strongly Agree",
                                                   "Disagree" = "Agree",
                                                   "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                   "Agree" = "Disagree",
                                                   "Strongly Agree" = "Strongly Disagree"),
                            cpi_sysproc_2 = recode(cpi_sysproc_2, "Strongly Disagree" = "Strongly Agree",
                                                   "Disagree" = "Agree",
                                                   "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                   "Agree" = "Disagree",
                                                   "Strongly Agree" = "Strongly Disagree"),
                            cpi_sysproc_3 = recode(cpi_sysproc_3, "Strongly Disagree" = "Strongly Agree",
                                                   "Disagree" = "Agree",
                                                   "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                   "Agree" = "Disagree",
                                                   "Strongly Agree" = "Strongly Disagree"),
                            cpi_talman_4 = recode(cpi_talman_4, "Strongly Disagree" = "Strongly Agree",
                                                  "Disagree" = "Agree",
                                                  "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                  "Agree" = "Disagree",
                                                  "Strongly Agree" = "Strongly Disagree"),
                            cpi_ecf_2 = recode(cpi_ecf_2, "Strongly Disagree" = "Strongly Agree",
                                               "Disagree" = "Agree",
                                               "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                               "Agree" = "Disagree",
                                               "Strongly Agree" = "Strongly Disagree")
)

# RECODE TO NUMERICAL
#### STEP 1
Mays <- Mays %>% mutate(
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
Mays <- Mays %>% mutate(cpi_vv_1 = as.integer(cpi_vv_1),cpi_vv_2 = as.integer(cpi_vv_2),cpi_vv_3 = as.integer(cpi_vv_3),cpi_vv_4 = as.integer(cpi_vv_4),cpi_vv_5 = as.integer(cpi_vv_5),cpi_strat_1 = as.integer(cpi_strat_1),cpi_strat_2 = as.integer(cpi_strat_2),cpi_strat_3 = as.integer(cpi_strat_3),cpi_strat_4 = as.integer(cpi_strat_4),cpi_strat_5 = as.integer(cpi_strat_5),cpi_lead_1 = as.integer(cpi_lead_1),cpi_lead_2 = as.integer(cpi_lead_2),cpi_lead_3 = as.integer(cpi_lead_3),cpi_lead_4 = as.integer(cpi_lead_4),cpi_lead_5 = as.integer(cpi_lead_5),cpi_adapt_1 = as.integer(cpi_adapt_1),cpi_adapt_2 = as.integer(cpi_adapt_2),cpi_adapt_3 = as.integer(cpi_adapt_3),cpi_adapt_4 = as.integer(cpi_adapt_4),cpi_adapt_5 = as.integer(cpi_adapt_5),cpi_perfman_1 = as.integer(cpi_perfman_1),cpi_perfman_2 = as.integer(cpi_perfman_2),cpi_perfman_3 = as.integer(cpi_perfman_3),cpi_perfman_4 = as.integer(cpi_perfman_4),cpi_perfman_5 = as.integer(cpi_perfman_5),cpi_sysproc_1 = as.integer(cpi_sysproc_1),cpi_sysproc_2 = as.integer(cpi_sysproc_2),cpi_sysproc_3 = as.integer(cpi_sysproc_3),cpi_sysproc_4 = as.integer(cpi_sysproc_4),cpi_sysproc_5 = as.integer(cpi_sysproc_5),cpi_team_1 = as.integer(cpi_team_1),cpi_team_2 = as.integer(cpi_team_2),cpi_team_3 = as.integer(cpi_team_3),cpi_team_4 = as.integer(cpi_team_4),cpi_team_5 = as.integer(cpi_team_5),cpi_talman_1 = as.integer(cpi_talman_1),cpi_talman_2 = as.integer(cpi_talman_2),cpi_talman_3 = as.integer(cpi_talman_3),cpi_talman_4 = as.integer(cpi_talman_4),cpi_talman_5 = as.integer(cpi_talman_5),cpi_cd_1 = as.integer(cpi_cd_1),cpi_cd_2 = as.integer(cpi_cd_2),cpi_cd_3 = as.integer(cpi_cd_3),cpi_cd_4 = as.integer(cpi_cd_4),cpi_cd_5 = as.integer(cpi_cd_5),cpi_ecf_1 = as.integer(cpi_ecf_1),cpi_ecf_2 = as.integer(cpi_ecf_2),cpi_ecf_3 = as.integer(cpi_ecf_3),cpi_ecf_4 = as.integer(cpi_ecf_4),cpi_ecf_5 = as.integer(cpi_ecf_5),cpi_custfoc_1 = as.integer(cpi_custfoc_1),cpi_custfoc_2 = as.integer(cpi_custfoc_2),cpi_custfoc_3 = as.integer(cpi_custfoc_3),cpi_custfoc_4 = as.integer(cpi_custfoc_4),cpi_custfoc_5 = as.integer(cpi_custfoc_5),cpi_comm_1 = as.integer(cpi_comm_1),cpi_comm_2 = as.integer(cpi_comm_2),cpi_comm_3 = as.integer(cpi_comm_3),cpi_comm_4 = as.integer(cpi_comm_4),cpi_comm_5 = as.integer(cpi_comm_5),positivity_1 = as.integer(positivity_1),positivity_2 = as.integer(positivity_2),positivity_3 = as.integer(positivity_3),positivity_4 = as.integer(positivity_4),positivity_5 = as.integer(positivity_5),satisfaction = as.integer(satisfaction),effort = as.integer(effort)
)

## CALUCULATE MEANS

Mays <- Mays %>% rowwise() %>%  
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
    

## FILTER MISSING DATA
Mays <- filter(Mays, Positivity >= 0)
saveRDS(Mays, paste0(file,"Cleaned/Mays.rds"))
write_csv(Mays, paste0(file,"Cleaned/Mays.csv"))

