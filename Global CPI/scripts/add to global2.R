library(tidyverse)
library(forcats)

#NEXT TIME: make changes to fix computer (windows or mac prefixes).  got it started last time.
#          automate a bit more.  move previous files automatically.  See if I can automate changing names of files 


computer <- "~"
#computer <- "C:/Users/Brent"
company <- "UT FAS"
surv_iteration <- 1
items <- read_csv("Global CPI/global means/new.current/items.csv")

# CHECK TO SEE IF YOU NEED TO ADD ALL OR CHANGE THE COLUMNS NAMES
df <- readRDS(paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/Company datasets/", company,".rds"))  %>% 
  mutate(ResponseId = as.character(ResponseId)) # make sure the colnames of df align with the global dataset

if (("ALL" %in% colnames(df)) == FALSE) {
  df <- df %>% 
    add_column(ALL = "ALL")
}

df <- df %>% 
  relocate(ALL, .after = last_col())

#c(colnames(df)[1], colnames(df)[length(df)]) # check the first and last columns for responseId and ALL
colnames(df)

colnames(df)[c(2:66, 72:73)]
colnames(df)[c(2:66, 72:73)] <- items[[1]][1:67]
# colnames(df)[2:68]
# colnames(df)[2:68] <- items[[1]][1:67]


df <- df %>%  #select(ResponseId:ALL) %>%
   # rename(satisfaction_1 = `extra_sat_satisfaction_1_Taking everything into consideration, I am satisfied with my job as a whole.`,
   #        effort_2 = `extra_eff_effort_2_I am willing to put in a great deal of effort beyond what is normally expected in order to help the organization.`) %>%
  add_column(Company = company) %>%
  add_column(iteration = surv_iteration) %>%
  mutate_if(is.factor, as.character) 



#prev_global <- readRDS("~/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global_Vessel_8.6.20_2656.rds") ## PREVIOUS/LATEST GLOBAL DATASET
prev_global <- readRDS(paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global.rds")) #%>% 
  # select(-(`cpi_Direction_leadership_5_I have confidence in the decision making of company leaders. `)) %>% 
  # filter(Company != "HISD")



new_global <- full_join(df, prev_global, copy=TRUE) # %>%
saveRDS(new_global, paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/global sets/past/global_", company, "_", Sys.Date(), "_", nrow(new_global), ".rds")) # basically, "global sets/past" will contain all of the past versions as well as the newly created current one
saveRDS(new_global, paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global.rds")) # this saves over the previous global
 
#saveRDS(global_HISD_2.5.21_7712, paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global.rds")) # this saves 
# CHECK TOTAL N
# # DOUBLE CHECK t_factor, CHANGE NAME HERE
# global_Teal_8.5.20 %>%
#       count(t_factor)
#  count


#  CHANGE NAME TWICE AND SAVE
# saveRDS(global_North_Star_9.2.20_3803, "~/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global_North_Star_9.2.20_3803.rds")
 #saveRDS(global_HISD_2.5.21_7712, paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global_HISD_2.5.21_7712.rds"))
 
# global <- global_HISD_2.5.21_7712
 
 latest_means <- new_global %>% 
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
 
 write_csv(latest_means, paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/global means/new.current/latest means.csv"))
 write_csv(latest_means, paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/global means/previous/", company, "_", Sys.Date(), "_", nrow(new_global), ".csv"))
 
 #write_csv(as_tibble(colnames(global)), paste0(computer, "/Dropbox/Clients/Master data prep/Global CPI/global means/new.current/items.csv"))
# GO TO FOLDER AND TAKE THE PREVIOUS CURRENT ONE AND MOVE IT TO PAST
 
 
