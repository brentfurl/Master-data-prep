company <- "North Star"

df_pop <- df #%>%
 # filter(Company != company)
df_sample <- df %>%
  filter(Company == company)

pop_vision <- df_pop$Vision
sample_vision <- df_sample$Vision
pop_sd <- sd(pop_vision)*sqrt((length(pop_vision)-1)/(length(pop_vision)))
pop_mean <- mean(pop_vision)
sample_mean <- mean(sample_vision)

z <- (sample_mean - pop_mean) / pop_sd
z_perc <- pnorm(z) * 100



df_pop_companies_vision <- df %>%
  group_by(Company) %>%
  summarise(mean(Vision)) %>%
  filter(Company != "Ergon")

df_pop_subcompanies_vision <- df %>%
  group_by(SUB_COMPANY) %>%
  summarise(mean(Vision)) %>%
  filter(SUB_COMPANY != "NA") %>%
  rename(Company = colnames(.)[1]) 

df_pop_all_companies_vision <- full_join(df_pop_companies_vision, df_pop_subcompanies_vision) %>%
  rename(Vision = colnames(.)[2])


z <- (sample_mean - company_pop_vision_mean) / company_pop_vision_sd
