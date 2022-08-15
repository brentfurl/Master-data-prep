# company <- ""
library(tidyverse)
df <- readRDS("~/Dropbox/Clients/Master data prep/Global CPI/Company datasets/Lamons.rds") %>% 
  mutate(ResponseId = as.character(ResponseId)) %>% # make sure the colnames of df align with the global dataset
  select(ResponseId,`cpi_Direction_vision_1_The company's vision is very clear.`:Engagement_sd) %>%
  rename(satisfaction_1 = `extra_sat_satisfaction_1_Taking everything into consideration, I am satisfied with my job as a whole.`,
         effort_2 = `extra_eff_effort_2_I am willing to put in a great deal of effort beyond what is normally expected in order to help the organization.`) %>%
  add_column(Company = "Lamons")

prev_global <- readRDS("~/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global_Vessel_8.6.20_2656.rds") ## PREVIOUS/LATEST GLOBAL DATASET

global_Lamons_8.21.20 <- full_join(df, prev_global, copy=TRUE) # %>%
  #mutate(t_factor = factor(ifelse(Company == "Teal", "Teal", "Global")))
global_Lamons_8.21.20_3018 <- global_Lamons_8.21.20
 
# CHECK TOTAL N
# # DOUBLE CHECK t_factor, CHANGE NAME HERE
# global_Teal_8.5.20 %>%
#       count(t_factor)
#  count


#  CHANGE NAME TWICE AND SAVE
 saveRDS(global_Lamons_8.21.20_3018, "~/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global_Lamons_8.21.20_3018.rds")
 
 
 global <- global_Lamons_8.21.20_3018
 
 means <- global %>% 
   summarise(n = n(), CPI_index = round(mean(CPI_index, na.rm = TRUE),0), 
                   Direction = round(mean(Direction, na.rm = TRUE),0), Operations = round(mean(Operations, na.rm=TRUE),0),
                   People = round(mean(People, na.rm = TRUE),0), Engagement = round(mean(Engagement, na.rm=TRUE),0),
                   Vision = round(mean(Vision, na.rm = TRUE),0), Strategy = round(mean(Strategy, na.rm=TRUE),0),
                   Leadership = round(mean(Leadership, na.rm = TRUE),0), Adaptability = round(mean(Adaptability, na.rm=TRUE),0),
                   Performance = round(mean(Performance, na.rm = TRUE),0), Systems = round(mean(Systems, na.rm=TRUE),0),
                   Teamwork = round(mean(Teamwork, na.rm = TRUE),0), Talent = round(mean(Talent, na.rm=TRUE),0),
                   Development = round(mean(Development, na.rm = TRUE),0), Fit = round(mean(Fit, na.rm=TRUE),0),
                   Customer = round(mean(Customer, na.rm = TRUE),0), Climate = round(mean(Climate, na.rm=TRUE),0),
                   Positivity = round(mean(Positivity, na.rm = TRUE),0), Satisfaction = round(mean(satisfaction_1, na.rm=TRUE),0),
                   Effort = round(mean(effort_2, na.rm=TRUE),0), 
                   nps = round(mean(nps, na.rm=TRUE),0)) 
 
 write_csv(means, "~/Dropbox/Clients/Master data prep/Global CPI/global means/new.current/global_Lamons_8.21.20_3018.csv")

# GO TO FOLDER AND TAKE THE PREVIOUS CURRENT ONE AND MOVE IT TO PAST