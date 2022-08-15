library(tidyverse)
library(qualtRics)
library(forcats)
library(lubridate)
file <- "~/Dropbox/Clients/Positivity/"

Ergon <- readRDS("~/Dropbox/Clients/Ergon/Ergon business units.Font.Red_line/data.rds")

Ergon <- add_column(Ergon, CPI = "new")
Ergon <- add_column(Ergon, Company = "Ergon")
colnames(Ergon)[colnames(Ergon)=="DIVISION"] <- "SubCompany"
colnames(Ergon)[colnames(Ergon)=="TENURE"] <- "Tenure"
colnames(Ergon)[colnames(Ergon)=="ROLE"] <- "Role"
colnames(Ergon)[colnames(Ergon)=="GENERATION"] <- "Generation"
colnames(Ergon)[colnames(Ergon)=="ResponseId"] <- "ResponseID"
colnames(Ergon)[colnames(Ergon)=="satisfaction_1"] <- "satisfaction"
colnames(Ergon)[colnames(Ergon)=="effort_2"] <- "effort"

Ergon <- filter(Ergon, Positivity >= 0 && CPI_Index >= 0)

saveRDS(Ergon, paste0(file,"Cleaned/Ergon.rds"))
write_csv(Ergon, paste0(file,"Cleaned/Ergon.csv"))

