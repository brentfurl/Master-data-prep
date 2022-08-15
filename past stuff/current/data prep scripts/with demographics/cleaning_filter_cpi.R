cleaning_filter_cpi <- function(df) {df <- df %>%
  filter(`extra_pos_positivity_1_People in my organization express genuine gratitude toward one another.` != "NA" 
         & df$Direction_sd > 0
         & df$Direction_sd < 40
         & df$Operations_sd > 0
         & df$Operations_sd < 40
         & df$People_sd > 0
         & df$People_sd < 40
         & df$Engagement_sd > 0
         & df$Engagement_sd < 40)
  return(df)
}


