app_data <- function(df){
  
  df <- readRDS(paste0(file.i.final, "df.rds"))
  colnames(df)[4:70] <- items_no_pref
  df <- df %>%
    add_column(ALL = "ALL") 
  demo_vars <- readRDS(paste0(file.i.final, "demo_vars.rds"))
  demo_vars_plus_all <- c("ALL", demo_vars)
  means_loop <- list()
  demo_graph_loop <- list()
  #####################################
  for (d in 1:length(demo_vars_plus_all)){
    
    
    # d<-1
     ####### NPS breakdown for ALL #################
  if (demo_vars_plus_all[d] == "ALL") {
    
     all_nps_breakdown <- df %>% 
       group_by(ALL, nps_group) %>% 
       count(nps_group, .drop = FALSE) %>% 
       filter(nps_group != "NA") %>%
       spread(nps_group, n)  %>%
       filter(ALL != "NA") %>%
       add_column(demo = eval(demo_vars_plus_all[d]), .before=1) %>%
       rename(demo = colnames(.)[1], level = colnames(.)[2])
  } 
     
    ########### NPS scores ####################
    #d<-2
    nps_scores <- df %>%
      
     # mutate(nps = fct_collapse(nps, "Detractor" = c("0","1","2","3","4","5","6"), "Passive"=c("7","8"),  "Promoter"=c("9","10"))) %>%
      group_by(get(demo_vars_plus_all[d])) %>%
      count(nps_group, .drop = FALSE) %>% 
      filter(nps_group != "NA") %>%
      spread(nps_group, n)  %>%
      mutate(nps_score = round(((Promoter/sum(c(Detractor, Passive, Promoter), na.rm = TRUE))-(Detractor/sum(c(Detractor, Passive, Promoter), na.rm = TRUE))) *100,0)) %>%
      add_column(demo = eval(demo_vars_plus_all[d]), .before=1) %>%
      rename(level = colnames(.)[2]) %>%
      select(demo, level, nps_score) %>%
      mutate(level = factor(level, order = FALSE)) %>%
      filter(level != "NA")
    
     ######### MEANS and n#################
    
    means <- df %>%  
      group_by(get(demo_vars_plus_all[d])) %>%
       summarise_at(
         vars('The company\'s vision is very clear.':Positivity), mean, na.rm = TRUE) %>%
      mutate_if(is.numeric, round,0) %>%
      ungroup() %>%
      add_column(demo = eval(demo_vars_plus_all[d]), .before=1) %>%
      rename(level = colnames(.)[2]) %>%
      filter(level != "NA") %>%
      mutate(level = factor(level, order = FALSE))
       
    n <- df %>%  
      group_by(get(demo_vars_plus_all[d])) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      add_column(demo = eval(demo_vars_plus_all[d]), .before=1) %>%
      rename(level = colnames(.)[2]) %>%
      filter(level != "NA") %>%
      mutate(level = factor(level, order = FALSE))
    
   
    
  
   
  
                
  #########################################################             
               #all_nps_breakdown, nps_scores, means, n 
              
if (demo_vars_plus_all[d] == "ALL") {
    graph_vars <- full_join(n, means) %>%
      full_join(., nps_scores) %>%
      full_join(., all_nps_breakdown) 
    } else {
      graph_vars <- full_join(n, means) %>%
              full_join(., nps_scores)
    }
    means_loop[[d]] <- graph_vars
    
    
  } # end demo loop
  
  means_all_demos <- do.call(dplyr::bind_rows, means_loop) %>%
    filter(n > 4) %>%
    select(-c('n', 'nps', 'nps_group', 'CPI_index'))
  write_csv(means_all_demos, paste0(file.i.final, "app_means_data.csv"))
  ###########################################
  
  for (d in 1:length(demo_vars)){
  
  n_demo_graphs <- df %>%  
    group_by(get(demo_vars[d])) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    add_column(demo = eval(demo_vars[d]), .before=1) %>%
    rename(level = colnames(.)[2]) %>%
    filter(level != "NA") %>%
    mutate(level = factor(level, order = FALSE)) %>%
    mutate(percentage = round(n/sum(n)*100, 1)) 
  
  demo_graph_loop[[d]] <- n_demo_graphs
  }
  
  demo_graph_nums <- do.call(dplyr::bind_rows, demo_graph_loop)
  write_csv(demo_graph_nums, paste0(file.i.final, "app_demo_n.csv"))
  
} # end function
  