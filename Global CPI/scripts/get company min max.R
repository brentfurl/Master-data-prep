library(tidyverse)
library(forcats)
library(lubridate)
library(googledrive)
library(googlesheets4)

df_company_means <- df %>%
  group_by(Company) %>%
    summarise(n = n(),CPI_index = round(mean(CPI_index, na.rm = TRUE),0), 
              Direction = round(mean(Direction, na.rm = TRUE),0), Operations = round(mean(Operations, na.rm=TRUE),0),
              People = round(mean(People, na.rm = TRUE),0), Engagement = round(mean(Engagement, na.rm=TRUE),0),
              Vision = round(mean(Vision, na.rm = TRUE),0), Strategy = round(mean(Strategy, na.rm=TRUE),0),
              Leadership = round(mean(Leadership, na.rm = TRUE),0), Adaptability = round(mean(Adaptability, na.rm=TRUE),0),
              Performance = round(mean(Performance, na.rm = TRUE),0), Systems = round(mean(Systems, na.rm=TRUE),0),
              Teamwork = round(mean(Teamwork, na.rm = TRUE),0), Talent = round(mean(Talent, na.rm=TRUE),0),
              Development = round(mean(Development, na.rm = TRUE),0), Fit = round(mean(Fit, na.rm=TRUE),0),
              Customer = round(mean(Customer, na.rm = TRUE),0), Climate = round(mean(Climate, na.rm=TRUE),0),
              Positivity = round(mean(Positivity, na.rm = TRUE),0), nps = round(mean(nps, na.rm=TRUE),2)
      ) %>%
  ungroup() %>%
  filter(Company != "Ergon")

df_subcompany_means <- df %>%
  group_by(SUB_COMPANY) %>%
  summarise(n = n(),CPI_index = round(mean(CPI_index, na.rm = TRUE),0), 
            Direction = round(mean(Direction, na.rm = TRUE),0), Operations = round(mean(Operations, na.rm=TRUE),0),
            People = round(mean(People, na.rm = TRUE),0), Engagement = round(mean(Engagement, na.rm=TRUE),0),
            Vision = round(mean(Vision, na.rm = TRUE),0), Strategy = round(mean(Strategy, na.rm=TRUE),0),
            Leadership = round(mean(Leadership, na.rm = TRUE),0), Adaptability = round(mean(Adaptability, na.rm=TRUE),0),
            Performance = round(mean(Performance, na.rm = TRUE),0), Systems = round(mean(Systems, na.rm=TRUE),0),
            Teamwork = round(mean(Teamwork, na.rm = TRUE),0), Talent = round(mean(Talent, na.rm=TRUE),0),
            Development = round(mean(Development, na.rm = TRUE),0), Fit = round(mean(Fit, na.rm=TRUE),0),
            Customer = round(mean(Customer, na.rm = TRUE),0), Climate = round(mean(Climate, na.rm=TRUE),0),
            Positivity = round(mean(Positivity, na.rm = TRUE),0), nps = round(mean(nps, na.rm=TRUE),2)
  ) %>%
  ungroup() %>%
  filter(SUB_COMPANY != "NA") %>%
  rename(Company = colnames(.)[1]) 

min <- full_join(df_company_means, df_subcompany_means) %>%
  summarise(across(CPI_index:nps, ~min(.x, na.rm = TRUE))) %>%
  add_column(parameter = "minimum company mean", .before = 1)

max <- full_join(df_company_means, df_subcompany_means) %>%
  summarise(across(CPI_index:nps, ~max(.x, na.rm = TRUE))) %>%
  add_column(parameter = "maximum company mean", .before = 1)

min_max_company_means <- bind_rows(min, max)

drive_trash("minimum and maximum company means")
gs4_create(name = "minimum and maximum company means", sheets = list("minimum and maximum company means" = min_max_company_means))
drive_mv("minimum and maximum company means", path = "~/CPI reports/Clients/Global stats/")
