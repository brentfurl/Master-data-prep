### 1.  run the code up to set #2
library(tidyverse)
library(forcats)
library(lubridate)

####

#company <- ""
source(paste0("~/Dropbox/Clients/", company, "/CPI/scripts-materials/scripts/file names.R"))
#source(paste0("C:Users/Brent/Dropbox/Clients/", company, "/CPI/scripts:materials/scripts/file names.R"))


#source(paste0(computer, "Dropbox/", side, company, "/CPI/scripts-materials/scripts/file names.R"))
source(paste0(file.scripts.prep, "reverse_code_cpi.R"))
source(paste0(file.scripts.prep, "recode_to_integer_cpi.R"))
source(paste0(file.scripts.prep, "means_sd_cpi.R"))
source(paste0(file.scripts.prep, "cleaning_filter_cpi.R"))
source(paste0(file.scripts.prep, "num_missing.R"))
#----------------------------------------------------------------------------------------

first_participant_id <- 0

all_items <- c("The company's vision is very clear.",	"I can summarize the company's vision to someone I meet.",	"I feel inspired by the company's vision.",	"Leaders are passionate about the company's vision.",	"Leaders are guided by the company's vision.",	"I have confidence in the company's strategy.",	"I know what my company wants to achieve in the next year.",	"This company knows how to get the results it is looking for.",	"We know what our next step is for improving our capabilities.",	"Leadership effectively connects my work to the company goals.",	"I have a great deal of confidence in the leadership of this company.",	"Company leaders impact the attitude and culture of the entire company.",	"The leadership of this company is fantastic in comparison to the leadership of other organizations I've been involved with.",	"Leaders find the time to get to know the employees.",	"I have confidence in the decision making of company leaders. ",	
               "We deal with the same problems again and again.*",	"Company changes usually have a positive impact on me.",	"People look to pass the blame when something goes wrong.*",	"If an issue arises, people mostly remain calm and focused.",	"We use feedback from customers to improve our services.",	"I have real time access to business i.",	"My team uses performance measures (i.e. key performance indicators).",	"We are recognized for our successes.",	"We have a clear sense at all times whether or not we're meeting our objectives.",	"We are held accountable for our performance.",	"I often use 'workarounds' instead of the company's established systems.*",	"It often takes too long to get the information I need.*",	"Oftentimes, I have to take care of things that shouldn't be my responsibility.*",	"I know who to go to when I have concerns about our systems/processes.",	"I follow the company's standardized procedures.",	
               "Our team consistently meets our goals.",	"Our work would be better if we collaborated more.",	"Our skills complement each other well.",	"There's a real sense that I am part of a team.",	"Our company has a spirit of teamwork and cooperation.",	"My company does a good job of hiring the right people for the right jobs.",	"I am confident my performance affects my compensation.",	"I get along well with my supervisor.",	"Performance reviews are a waste of time.*",	"Based on my experience, the company does a great job of acclimating new employees.",	"I get helpful feedback from my supervisor on a regular basis.",	"I am consistently improving at my job.",	"People here are always willing to help me learn.",	"The company invests in employees learning new skills.",	"I have grown professionally during my time with this company.",	
               "My job role is always extremely clear.",	"I am very frequently overwhelmed at work.*",	"My job is a great fit for my skills and strengths.",	"I embrace the company's core values.",	"I look forward to going to work.",	"I believe in the products or services we provide.",	"I respond to customer needs as quickly as possible.",	"I build great relationships with my customers.",	"I'm disappointed if I don't meet my customers' expectations.",	"I seek feedback from customers to improve our products or services.",	"My opinions are heard and valued.",	"I am confident that leadership understands what I do.",	"I am very comfortable letting my supervisor know when I've made a mistake.",	"I trust my supervisor has my best interest in mind.",	"I feel micromanaged.*",
               "People in my organization express genuine gratitude toward one another.","I practice mindfulness by staying in the present moment while doing my work.", "People I work with generally feel optimistic about the organization's future.", "People that I work with care about my happiness and well-being.", "I feel inspired every day to do my best work.", "Taking everything into consideration, I am satisfied with my job as a whole.", "I am willing to put in a great deal of effort beyond what is normally expected in order to help the organization.")

columns <- c("ResponseId",	"StartDate",	"EndDate",	"Status",	"Contact ID",	"Legacy Comments",	"Comments",	"Language",	"Referer",	"SessionID",	
             "User Agent",	"Tags",	"IP Address",	"Longitude",	"Latitude",	"Country",	"City",	"State/Region",	"Postal", # 19 at this point
             
             "demo1_LEVEL",  #"demo2_FUNCTION", "demo3_LEVEL", "demo4_LOCATION", "demo5_GENERATION",	"demo6_GENDER",	   
             
             paste0("cpi_Direction_vision_", 1:5),	paste0("cpi_Direction_strategy_", 1:5),	paste0("cpi_Direction_leadership_", 1:5),		
             paste0("cpi_Operations_adaptability_", 1:5),	paste0("cpi_Operations_performance_", 1:5),	paste0("cpi_Operations_systems_", 1:5),
             paste0("cpi_People_teamwork_", 1:5),	paste0("cpi_People_talent_", 1:5),	paste0("cpi_People_development_", 1:5),		
             paste0("cpi_Engagement_fit_", 1:5),	paste0("cpi_Engagement_customer_", 1:5),	paste0("cpi_Engagement_climate_", 1:5),
             
            "extra_pos_positivity_1",	"extra_pos_positivity_2",	"extra_pos_positivity_3",	"extra_pos_positivity_4",	"extra_pos_positivity_5",	"extra_sat_satisfaction_1",	"extra_eff_effort_2",	
             "nps", "single_change", "best_day",
             paste0("oid_characteristics_", 1:5),
            paste0("oid_purpose_", 1:5),
             paste0("oid_values_", 1:5),
            paste0("oid_traditions_", 1:5)
            
                )

columns[21:87] <- paste0(columns[21:87], "_", all_items)


raw <- read_csv(paste0(file.i.raw, company, ".csv"))
colnames(raw)

                             ### use the next two lines to create a comparision table and view it to see if your columns check out.  
#col_compare <- tibble(raw = colnames(raw), new = columns, rep("NA", length(colnames(raw))-length(columns)))
 #view(col_compare)  #if things are lined up proceed to step 2.
 #----------------------------------------------------------------------------------------
 
 
### 2. Run the next 3 lines to put our new column names in and save the .rds file.

df_prep1_columns <- raw # this chunk sets the intial new column names.  The next two steps (df_prep2, df_prep3 change the demo datatypes and names)
colnames(df_prep1_columns) <- columns # i couldn't figure through this line yet. This step renames the columns
df_prep1.1_columns <- df_prep1_columns %>% 
   select(ResponseId:EndDate, demo1_LEVEL:nps)
          
write_csv(df_prep1.1_columns, paste0(file.i.prep, "df_prep1.1_columns.csv")) 
# saveRDS(df_prep1.1_columns, paste0(file.i.final, "df_prep1.1_columns.rds"))  # if you want to save the r data, you can use saveRDS.  
# ADD A SAVING OF OID DATA HERE.

df_prep2_test_filter_real_demo_as_factor <- df_prep1.1_columns %>%
   filter(ResponseId >= eval(first_participant_id)) %>%  # use this to filter out test data.  default is for first_participant_id is 0
  # select(ResponseId, contains("demo1"):single_change) %>% # the next two lines select the demographic variables and sets them to factor data type
   mutate_at(vars(starts_with("demo")), as.factor) # sets "demo" vars to factor data type
write_csv(df_prep2_test_filter_real_demo_as_factor, paste0(file.i.prep, "df_prep2_test_filter_real_demo_as_factor.csv")) 

#### get list of demo vars and remove "demo" prefixes and remove gender....
demo_var_names_raw <- colnames(df_prep2_test_filter_real_demo_as_factor)[grep("demo", colnames(df_prep2_test_filter_real_demo_as_factor))]   # this section could be improved but works 
demo_vars_prep <- str_remove(demo_var_names_raw, "[0-9]")                                       
demo_vars_prep2 <- str_remove(demo_vars_prep, "demo_")                       #extracted demo vars
demo_vars <- demo_vars_prep2[!grepl("GENDER", demo_vars_prep2)]              # remove gender.  we have demo_vars at this point.  demo_vars is used in loops in creating plots.  We don't plot gender
saveRDS(demo_vars, paste0(file.i.final, "demo_vars.rds"))

# #### rename the columns of the df to the new demo vars (removed prefixes)  ### may need to rework some levels here as I've done with TENURE
df_prep3_column_names_set <- df_prep2_test_filter_real_demo_as_factor # %>%
  # mutate(TENURE = fct_relevel(TENURE, "< 1 year", "1 - 4 years", "5 - 8 years", "9 - 12 years", "> 12 years"))
colnames(df_prep3_column_names_set)[grep("demo", colnames(df_prep3_column_names_set))] <- demo_vars_prep2 # at this point all column names are set
write_csv(df_prep3_column_names_set, paste0(file.i.prep, "df_prep3_column_names_set.csv"))


##### add nps_group as factor

df_prep4_nps <- df_prep3_column_names_set %>%
   mutate(nps_group = factor(nps)) %>%
   mutate(nps_group = fct_collapse(nps_group, "Detractor" = c("0","1","2","3","4","5","6"), "Passive"=c("7","8"),  "Promoter"=c("9","10"))) # here i add nps_group factor by changing nps to factor and grouping the levels
write_csv(df_prep4_nps, paste0(file.i.prep, "df_prep4_nps.csv"))

#----------------------------------------------------------------------------------------


#### 3.  now that we've set column names to those used in all of the other scripts, we can further prepare the data with the following functions.

df_prep5_reverse_coded <- reverse_code_cpi(df_prep4_nps)  ## to view number of missing for each item run num_missing(df_recoded)...from num_missing.R.
write_csv(df_prep5_reverse_coded, paste0(file.i.prep, "df_prep5_reverse_coded.csv"))                                        # y <- num_missing(df_recoded)

df_prep6_recode_to_integer <- recode_to_integer_cpi(df_prep5_reverse_coded) 
write_csv(df_prep6_recode_to_integer, paste0(file.i.prep, "df_prep6_recode_to_integer.csv"))  

df_prep7_means_sd <- means_sd_cpi(df_prep6_recode_to_integer)
write_csv(df_prep7_means_sd, paste0(file.i.prep, "df_prep7_means_sd.csv"))

############## CHECK NUM MISSING
missing <- num_missing(df_prep7_means_sd)
write_csv(missing, paste0(file.o.analyses, "stats/missing.csv"))
saveRDS(missing, paste0(file.o.analyses, "stats/missing.rds"))
############## create final datasets that are used in analysis and visualization.  i use "df" mostly for stats and "df_long" mostly for visualizations.  

 df <- df_prep7_means_sd #%>% select(ResponseId:nps, nps_group:Engagement_sd, TENURE:GENDER)
 # df <- cleaning_filter_cpi(df_prep7_means_sd) #%>% # I want to add a report for this step to determine count for reasons filtered by demographic groups
 #    select(ResponseId:nps, nps_group:Engagement_sd, TENURE:GENDER)
 
write_csv(df, paste0(file.i.final, "df.csv"))
saveRDS(df, paste0(file.i.final, "df.rds"))

df_long <- df %>%
   pivot_longer(`cpi_Direction_vision_1_The company's vision is very clear.`:`extra_eff_effort_2_I am willing to put in a great deal of effort beyond what is normally expected in order to help the organization.`,
                names_to = "quantitative") %>%
   separate(quantitative, into = c("cpi_or_not", "quadrant", "dimension", "question_number", "item"), sep = "_") %>%
   mutate(quadrant = factor(quadrant, levels = c("Direction", "Operations", "People", "Engagement", "pos", "sat", "eff")),
          dimension = factor(dimension, levels = c("vision", "strategy", "leadership", "adaptability", "performance", "systems",
                                                   "teamwork", "talent", "development", "fit", "customer", "climate", "positivity", "satisfaction", "effort")),
          question_number = as.integer(question_number)) %>%
   select(-contains("oid")) 

saveRDS(df_long, paste0(file.i.final, "df_long.rds"))
write_csv(df_long, paste0(file.i.final, "df_long.csv"))

############################ ADD TO GLOBAL #######################



####################################RUN PLOTS##################################################################
source(paste0(file.scripts.plots, "themes.R"))
source(paste0(file.scripts.plots, "all plots.R"))
source(paste0(file.scripts.plots, "CPI graph - vs Global.R"))
source(paste0(file.scripts.plots, "CPI graph.R"))
source(paste0(file.scripts.plots, "demo plots.R"))
source(paste0(file.scripts.plots, "demo quadrants.R"))
source(paste0(file.scripts.plots, "sample overview.R"))

################################## COMBINE PDF
source(paste0(file.scripts.plots, "COMBINE PDFS.R"))

################################## ANALYSES
source(paste0(file.scripts.analyses, "means and nps table.R"))
source(paste0(file.scripts.analyses, "regression tests.R"))
source(paste0(file.scripts.analyses, "nps scores.R"))
source(paste0(file.scripts.analyses, "anovas.R"))


## go to rsconnect and knit rmd file...CHANGE TITLE AND COMPANY NAME VARIABLE
## check this link https://rpubs.com/BAlanF/COMPANYNAME and send it to Peter
