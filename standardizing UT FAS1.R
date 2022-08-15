library(googlesheets4)
library(googledrive)
library(tidyverse)


positivity_com <- global %>% 
  group_by(Company) %>% 
  summarise(CPI_index = round(mean(CPI_index, na.rm = TRUE),0), 
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
            nps = round(mean(nps, na.rm=TRUE),0), n = n()) %>% 
  arrange(desc(Positivity)) %>% 
  print(n=23)

positivity_subcom <- global %>% 
  group_by(SUB_COMPANY) %>% 
  summarise(CPI_index = round(mean(CPI_index, na.rm = TRUE),0), 
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
    nps = round(mean(nps, na.rm=TRUE),0), n = n()) %>% 
  arrange(desc(Positivity)) %>% 
  filter(!is.na(SUB_COMPANY)) %>% 
  rename(Company = "SUB_COMPANY")

positivity_by_company <- bind_rows(positivity_com, positivity_subcom) %>% 
  arrange(desc(Positivity)) %>% 
  print(n=31)

count <- global %>% 
  group_by(Company) %>% count() %>% arrange(desc(n)) %>% print(n=24)


gs4_create(name = "Positivity by company", sheets = list("means" = positivity_by_company))


dimensions_df <- global %>% select(Company, CPI_index:Positivity, satisfaction_1, effort_2)
dimensions <- colnames(dimensions_df)[2:length(dimensions_df)]


dims_loop1t <- list()
for (d in 1:length(dimensions)){

  global_mean <- round(mean(global %>% filter(Company != "UT FAS") %>% 
                        pull(dimensions[d]), na.rm=TRUE),1)
  FAS_mean <- round(mean(dimensions_df %>% filter(Company == "UT FAS") %>% 
                     pull(dimensions[d]), na.rm=TRUE),1)
  percentile <- round(t.test(dimensions_df %>% filter(Company == "UT FAS") %>% 
         pull(dimensions[d]), mu=global_mean, alternative="less")[[3]]*100,0)
  
  dims_loop1t[[d]] <- c(dimensions[d],as.character(percentile), as.character(global_mean), as.character(FAS_mean))

}

percentiles1 <- do.call(rbind, dims_loop1t) %>% 
  as_tibble() %>% 
  mutate(V2 = as.integer(V2), V3 = as.numeric(V3), V4 = as.numeric(V4)) %>% 
  rename(dimension = "V1", percentile = "V2", global_mean = "V3", FAS_mean = "V4") %>% 
  arrange(desc(percentile))


dims_loop2t <- list()
for (d in 1:length(dimensions)){
  
  global_mean <- round(mean(dimensions_df %>% filter(Company != "UT FAS") %>% 
                              pull(dimensions[d]), na.rm=TRUE),1)
  FAS_mean <- round(mean(dimensions_df %>% filter(Company == "UT FAS") %>% 
                           pull(dimensions[d]), na.rm=TRUE),1)
  
  percentile <- round(t.test(
                              dimensions_df %>% filter(Company == "UT FAS") %>% 
                               pull(dimensions[d]), 
                              dimensions_df %>% filter(Company != "UT FAS") %>% 
                                pull(dimensions[d]), alternative="less", var.equal = FALSE)[[3]]*100,0)
  
  dims_loop2t[[d]] <- c(dimensions[d],as.character(percentile), as.character(global_mean), as.character(FAS_mean))
  
}

percentiles2vF <- do.call(rbind, dims_loop2t) %>% 
  as_tibble() %>% 
  mutate(V2 = as.integer(V2), V3 = as.numeric(V3), V4 = as.numeric(V4)) %>% 
  rename(dimension = "V1", percentile = "V2", global_mean = "V3", FAS_mean = "V4") %>% 
  arrange(desc(percentile))

gs4_create(name = "FAS percentiles", sheets = list("means" = percentiles2vF))
################# ITEMS

items_df <- global %>% select(Company, `cpi_Direction_vision_1_The company's vision is very clear.`:`extra_pos_positivity_5_I feel inspired every day to do my best work.`, satisfaction_1, effort_2)
items <- colnames(items_df)[2:length(items_df)]
items_loop1t <- list()
for (d in 1:length(items)){
  
  global_mean <- round(mean(items_df %>% filter(Company != "UT FAS") %>% 
                              pull(items[d]), na.rm=TRUE),1)
  FAS_mean <- round(mean(items_df %>% filter(Company == "UT FAS") %>% 
                           pull(items[d]), na.rm=TRUE),1)
  percentile <- round(t.test(items_df %>% filter(Company == "UT FAS") %>% 
                               pull(items[d]), mu=global_mean, alternative="less")[[3]]*100,0)
  
  items_loop1t[[d]] <- c(items[d],as.character(percentile), as.character(global_mean), as.character(FAS_mean))
  
}

percentilesItems <- do.call(rbind, items_loop1t) %>% 
  as_tibble() %>% 
  mutate(V2 = as.integer(V2), V3 = as.numeric(V3), V4 = as.numeric(V4)) %>% 
  rename(item = "V1", percentile = "V2", global_mean = "V3", FAS_mean = "V4") %>% 
  arrange(desc(percentile))

gs4_create(name = "FAS item percentiles", sheets = list("means" = percentilesItems))
