library(tidyverse)

Ergon_data <- readRDS("C:/Users/Brent/Dropbox/Clients/Master data prep/Global CPI/Industrial/data.rds")

df_indust <- Ergon_data %>%
  filter(DIVISION == "Refining + Marketing  |  DIVISION == Construction + Real Estate (i.e., Alliant Construction, ISO Services, Ergon Maintenance Services, etc.)" 
         |  DIVISION == "Oil + Gas (i.e., Lampton-Love Inc., Ergon Exploration, Ergon Production, etc.)"  |  DIVISION ==  "Asphalt + Emulsions (i.e., Ergon Asphalt, Paragon Technical Services, etc.)"
         |  DIVISION == "Midstream + Logistics (i.e., Terminaling, Oil Purchasing, Trucking, Magnolia Marine, EMIS)" |  DIVISION == "Crafco" |  DIVISION == "Specialty Chemicals (Resinall)")


saveRDS(df_indust, "C:/Users/Brent/Dropbox/Clients/Master data prep/Global CPI/Industrial/global_indust.rds")


df <- readRDS("C:/Users/Brent/Dropbox/Clients/Lamons/CPI/input/final/df.rds") %>% 
  select(ResponseId:GENERATION, Vision:Climate)  # df is from the loop for Ergon business units
quadrants <- list(list("DIRECTION",c("Vision", "Strategy","Leadership")), 
                  list("OPERATIONS",c("Adaptability", "Performance", "Systems")), 
                  list("PEOPLE",c("Teamwork", "Talent", "Development")), 
                  list("ENGAGEMENT",c("Fit", "Customer", "Climate")))

triangles  <- df %>% gather(Dimension, Dim_score, Vision:Climate) %>%
  mutate(Dimension = factor(Dimension, levels = c(eval(quadrants[[1]][[2]]), eval(quadrants[[2]][[2]]), eval(quadrants[[3]][[2]]), eval(quadrants[[4]][[2]]))),
         Quadrant = fct_collapse(Dimension, Direction = c("Vision", "Strategy","Leadership"), Operations = c("Adaptability", "Performance", "Systems"), 
                                 People = c("Teamwork", "Talent", "Development"), Engagement = c("Fit", "Customer", "Climate"))) %>%
  group_by(Dimension, Quadrant) %>% 
  summarise(mean(Dim_score, na.rm = TRUE)) 
colnames(triangles) <- c("Dimension", "Quadrant", "means")

idx_dup <- rep(1:nrow(triangles), 3)
triangles <- triangles[idx_dup,] %>% arrange(Dimension)

lamons_means_tidy <- triangles %>%
  group_by(Dimension) %>%
  summarise(mean = round(mean(means),0))


write_csv(lamons_means_tidy, "C:/Users/Brent/Dropbox/Clients/Master data prep/Global CPI/Industrial/lamons_means_tidy.csv")