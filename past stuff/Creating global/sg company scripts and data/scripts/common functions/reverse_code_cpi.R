
reverse_code_cpi <- function(df) {
  
            df <- df %>% mutate_at(vars(contains("cpi_operations_adaptability_1")), 
                                   funs(recode(.,"Strongly Disagree" = "Strongly Agree",
                                               "Disagree" = "Agree",
                                               "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                               "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                               "Agree" = "Disagree",
                                               "Strongly Agree" = "Strongly Disagree"))) %>%
              mutate_at(vars(contains("cpi_operations_adaptability_3")), 
                        funs(recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree"))) %>%
              mutate_at(vars(contains("cpi_operations_systems_1")), 
                        funs(recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree"))) %>%
              mutate_at(vars(contains("cpi_operations_systems_2")), 
                        funs(recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree"))) %>%
              mutate_at(vars(contains("cpi_operations_systems_3")), 
                        funs(recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree"))) %>%
              mutate_at(vars(contains("cpi_people_talent_4")), 
                        funs(recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree"))) %>%
              mutate_at(vars(contains("cpi_engagement_fit_2")), 
                        funs(recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree"))) %>%
              mutate_at(vars(contains("cpi_engagement_climate_5")), 
                        funs(recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree"))) 
             
return(df)
}



