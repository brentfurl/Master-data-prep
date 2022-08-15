#bind all qualtrics and sg scripts

#skip whatever takes too long and come back after an adequate set is created.

global_Rooster_Teeth_11.5.20_4393.3 <- global_Rooster_Teeth_11.5.20_4393.2 %>%
  filter(Company != "Envoy") %>%
  full_join(., Envoy)