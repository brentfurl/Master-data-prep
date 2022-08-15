dims_loopZ <- list()
for (d in 1:length(dimensions)){
  
  global_mean <- round(mean(dimensions_df %>% filter(Company != "UT FAS") %>% 
                              pull(dimensions[d]), na.rm=TRUE),1)
  global_sd <- round(sd(dimensions_df %>% filter(Company != "UT FAS") %>% 
                          pull(dimensions[d]), na.rm=TRUE),1)
  FAS_mean <- round(mean(dimensions_df %>% filter(Company == "UT FAS") %>% 
                           pull(dimensions[d]), na.rm=TRUE),1)
  
  FAS_Z <- round(pnorm((FAS_mean-global_mean)/global_sd),2) * 100
  
  # percentile <- round(t.test(
  #   dimensions_df %>% filter(Company == "UT FAS") %>% 
  #     pull(dimensions[d]), 
  #   dimensions_df %>% filter(Company != "UT FAS") %>% 
  #     pull(dimensions[d]), alternative="less", var.equal = FALSE)[[3]]*100,0)
  
  dims_loopZ[[d]] <- c(dimensions[d],as.character(FAS_Z), as.character(global_mean), as.character(FAS_mean))
  
}

percentiles_dimZ <- do.call(rbind, dims_loopZ) %>% 
  as_tibble() %>% 
  mutate(V2 = as.integer(V2), V3 = as.numeric(V3), V4 = as.numeric(V4)) %>% 
  rename(dimension = "V1", percentile = "V2", global_mean = "V3", FAS_mean = "V4") %>% 
  arrange(desc(percentile))

gs4_create(name = "FAS Z dim percentiles", sheets = list("means" = percentiles_dimZ))
################# ITEMS

items_df <- global %>% select(Company, `cpi_Direction_vision_1_The company's vision is very clear.`:`extra_pos_positivity_5_I feel inspired every day to do my best work.`, satisfaction_1, effort_2)
items <- colnames(items_df)[2:length(items_df)]
items_loopZ <- list()
for (d in 1:length(items)){
  
  global_mean <- round(mean(items_df %>% filter(Company != "UT FAS") %>% 
                              pull(items[d]), na.rm=TRUE),1)
  global_sd <- round(sd(items_df %>% filter(Company != "UT FAS") %>% 
                          pull(items[d]), na.rm=TRUE),1)
  FAS_mean <- round(mean(items_df %>% filter(Company == "UT FAS") %>% 
                           pull(items[d]), na.rm=TRUE),1)
  
  FAS_Z <- round(pnorm((FAS_mean-global_mean)/(global_sd/(sqrt(20)))),2) * 100
  
  items_loopZ[[d]] <- c(items[d],as.character(FAS_Z), as.character(global_mean), as.character(FAS_mean))
  
}

percentilesItemsZ <- do.call(rbind, items_loopZ) %>% 
  as_tibble() %>% 
  mutate(V2 = as.integer(V2), V3 = as.numeric(V3), V4 = as.numeric(V4)) %>% 
  rename(item = "V1", percentile = "V2", global_mean = "V3", FAS_mean = "V4") %>% 
  arrange(desc(percentile)) %>% print(n=67)

gs4_create(name = "FAS Z item percentiles", sheets = list("means" = percentilesItems))
