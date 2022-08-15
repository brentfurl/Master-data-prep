recode_to_integer_cpi <- function(df) {
  
  df <- df %>%mutate_at(vars(contains("cpi_")), funs(recode(., 
                                              "Strongly Disagree" = "0", 
                                              "Disagree" = "25", 
                                              "Neither Agree nor Disagree" = "50", 
                                              "Neither Agree Nor Disagree" = "50", 
                                              "Agree" = "75", 
                                              "Strongly Agree" = "100"))) %>%
  mutate_at(vars(contains("extra_")), funs(recode(., 
                                                  "Strongly Disagree" = "0", 
                                                  "Disagree" = "25", 
                                                  "Neither Agree nor Disagree" = "50", 
                                                  "Neither Agree Nor Disagree" = "50", 
                                                  "Agree" = "75", 
                                                  "Strongly Agree" = "100"))) %>%
  mutate_at(vars(contains("cpi_")), funs(as.integer)) %>%
  mutate_at(vars(contains("extra_")), funs(as.integer))
return(df)
}