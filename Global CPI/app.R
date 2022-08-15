library(shiny)
library(DT)
library(shinydashboard)
library(tidyverse)
library(forcats)
#library(lubridate)
library(qualtRics)

df <- readRDS("~/Dropbox/Clients/Master data prep/Global CPI/global sets/current/global_HISD_2.5.21_7712.rds") %>% 
  mutate(SUB_COMPANY = as.character(SUB_COMPANY)) %>%
  mutate(company_all = ifelse(Company == "Ergon", paste0("Ergon - ",SUB_COMPANY), Company))
           # case_when(Company == "Ergon" ~ SUB_COMPANY,
           #           Company != "Ergon"))


ui <- dashboardPage(
  dashboardHeader(title = "Global CPI"),
  dashboardSidebar(
    sidebarMenu(
      selectizeInput("company","Companies", choices = unique(df$company_all),
      selectizeInput("cpi_version","CPI version", choices = c("1", "2"),              
      )
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("company_count"),
      valueBoxOutput("total_count"),
      valueBoxOutput("total_cpi.v2")
    ),
    fluidRow(
      valueBoxOutput("count")
    ),
  )
)

server <- function(input, output) {
  
  output$company_count <- renderValueBox({
    
    df_box_comp <- df %>% 
      filter(company_all == input$company) %>%
      group_by(ALL) %>%  
      summarise(count=n()) %>% 
      filter(ALL != "NA") %>%
      select(count)  %>% 
      valueBox(subtitle = "company count")
    
  })
  
  output$total_count <- renderValueBox({
    
      df_box_tot <- df %>% 
        group_by(ALL) %>%  
        summarise(count=n()) %>% 
        filter(ALL != "NA") %>%
        select(count)  %>% 
        valueBox(subtitle = "total count")
  })
  
}

shinyApp(ui = ui, server = server)
  