global_Rooster_Teeth_11.5.20_4393 <- global_Rooster_Teeth_11.5.20_4393 %>%
  select(-(ALL)) %>%
  add_column(ALL = "ALL") %>%
  mutate(ALL = factor(ALL))

saveRDS(global_Rooster_Teeth_11.5.20_4393,"~/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global_Rooster_Teeth_11.5.20_4393.2.rds")


df2 <- global_Rooster_Teeth_11.5.20_4393.2 %>% 
  mutate(SUB_COMPANY = as.character(SUB_COMPANY)) %>%
  mutate(company_all = ifelse(Company == "Ergon", paste0("Ergon - ",SUB_COMPANY), Company)) %>%
  mutate(company_all = factor(company_all))




ggplot(df2, aes(x = Vision)) + #geom_col()
  geom_histogram(aes(color = ALL, fill = ALL),
                 position = "identity", bins = 20, alpha = .4)