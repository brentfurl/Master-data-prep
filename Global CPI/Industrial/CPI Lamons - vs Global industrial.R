library(tidyverse)
library(ggplot2)

df <- readRDS("C:/Users/Brent/Dropbox/Clients/Lamons/CPI/input/final/df.rds") %>% 
  select(ResponseId:GENERATION, Vision:Climate)  # df is from the loop for Ergon business units
quadrants <- list(list("DIRECTION",c("Vision", "Strategy","Leadership")), 
                  list("OPERATIONS",c("Adaptability", "Performance", "Systems")), 
                  list("PEOPLE",c("Teamwork", "Talent", "Development")), 
                  list("ENGAGEMENT",c("Fit", "Customer", "Climate")))

#demo_vars <- readRDS(paste0(file.i.final, "demo_vars.rds"))


triangles  <- df %>% gather(Dimension, Dim_score, Vision:Climate) %>%
  mutate(Dimension = factor(Dimension, levels = c(eval(quadrants[[1]][[2]]), eval(quadrants[[2]][[2]]), eval(quadrants[[3]][[2]]), eval(quadrants[[4]][[2]]))),
         Quadrant = fct_collapse(Dimension, Direction = c("Vision", "Strategy","Leadership"), Operations = c("Adaptability", "Performance", "Systems"), 
                                 People = c("Teamwork", "Talent", "Development"), Engagement = c("Fit", "Customer", "Climate"))) %>%
  group_by(Dimension, Quadrant) %>% 
  summarise(mean(Dim_score, na.rm = TRUE)) 
colnames(triangles) <- c("Dimension", "Quadrant", "means")

idx_dup <- rep(1:nrow(triangles), 3)
triangles <- triangles[idx_dup,] %>% arrange(Dimension)

triangles_local <- triangles %>% add_column(x =c(0, -tan(.2618)*triangles$means[1], -tan(.7854)*triangles$means[1], 0,tan(.2618)*triangles$means[4], -tan(.2618)*triangles$means[4], 0, tan(.2618)*triangles$means[7], tan(.7854)*triangles$means[7], 
                                       0, triangles$means[10], triangles$means[10], 0, triangles$means[13], triangles$means[13], 0, triangles$means[16], triangles$means[16],
                                       0, tan(.2618)*triangles$means[19], tan(.7854)*triangles$means[19], 0,tan(.2618)*triangles$means[22], -tan(.2618)*triangles$means[22], 0, -tan(.2618)*triangles$means[25], -tan(.7854)*triangles$means[25], 
                                       0, -triangles$means[28], -triangles$means[28], 0, -triangles$means[31], -triangles$means[31], 0, -triangles$means[34], -triangles$means[34]),
                                  
                                  y = c(0, triangles$means[1], triangles$means[1], 0, triangles$means[4], triangles$means[4], 0, triangles$means[7], triangles$means[7],
                                        0, tan(.2618)*triangles$means[10], tan(.7854)*triangles$means[10], 0,tan(.2618)*triangles$means[13], -tan(.2618)*triangles$means[13], 0, -tan(.2618)*triangles$means[16], -tan(.7854)*triangles$means[16],
                                        0, -triangles$means[19], -triangles$means[19], 0, -triangles$means[22], -triangles$means[22], 0, -triangles$means[25], -triangles$means[25],
                                        0, -tan(.2618)*triangles$means[28], -tan(.7854)*triangles$means[28], 0,tan(.2618)*triangles$means[31], -tan(.2618)*triangles$means[31], 0, tan(.2618)*triangles$means[34], tan(.7854)*triangles$means[34]))

global_means <- readRDS("C:/Users/Brent/Dropbox/Clients/Master data prep/Global CPI/Industrial/global_indust.rds") %>%
  gather(Dimension, Dim_score, Vision:Climate) %>%
  mutate(Dimension = factor(Dimension, levels = c(eval(quadrants[[1]][[2]]), eval(quadrants[[2]][[2]]), eval(quadrants[[3]][[2]]), eval(quadrants[[4]][[2]]))),
         Quadrant = fct_collapse(Dimension, Direction = c("Vision", "Strategy","Leadership"), Operations = c("Adaptability", "Performance", "Systems"), 
                                 People = c("Teamwork", "Talent", "Development"), Engagement = c("Fit", "Customer", "Climate"))) %>%
  group_by(Dimension, Quadrant) %>% 
  summarise(mean(Dim_score, na.rm = TRUE)) 
colnames(global_means) <- c("Dimension", "Quadrant", "means")
 

idx_dup_gl <- rep(1:nrow(global_means), 3)
global_means <- global_means[idx_dup_gl,] %>% arrange(Dimension)

global_means_tidy <- global_means %>%
  group_by(Dimension) %>%
  summarise(mean = round(mean(means),0))


write_csv(global_means_tidy, "C:/Users/Brent/Dropbox/Clients/Master data prep/Global CPI/Industrial/global_indust_means.csv")
# gs4_create(name = "global_indust vs lamons means", sheets = global_means) #list(nps_and_means = means_all_demos, anovas = anovas_sig_no_nests_basic, regression = model))
# drive_mv("global_indust vs lamons means", path = "~/Clients/Lamons/analyses/")
  
  # c(71.0647570527483,	71.0647570527483,	71.0647570527483,	68.3322055659659,	68.3322055659659,	68.3322055659659,	66.8538628196999,	66.8538628196999,	66.8538628196999,	67.3097885626644,	
  #                  67.3097885626644,	67.3097885626644,	67.8192294002491,	67.8192294002491,	67.8192294002491,	63.39549786395,	63.39549786395,	63.39549786395,	74.4061898834446,	74.4061898834446,	
  #                  74.4061898834446,	66.110513604287,	66.110513604287,	66.110513604287,	66.8218361485848,	66.8218361485848,	66.8218361485848,	75.935866058516,	75.935866058516,	75.935866058516,	
  #                  68.5325945362513,	68.5325945362513,	68.5325945362513,	67.0287471661649,	67.0287471661649,	67.0287471661649)

triangles_global <- global_means 
#triangles_global$means <- global_means

triangles_global <- triangles_global %>%  add_column(x =c(0, -tan(.2618)*triangles_global$means[1], -tan(.7854)*triangles_global$means[1], 0,tan(.2618)*triangles_global$means[4], -tan(.2618)*triangles_global$means[4], 0, tan(.2618)*triangles_global$means[7], tan(.7854)*triangles_global$means[7], 
                                                          0, triangles_global$means[10], triangles_global$means[10], 0, triangles_global$means[13], triangles_global$means[13], 0, triangles_global$means[16], triangles_global$means[16],
                                                          0, tan(.2618)*triangles_global$means[19], tan(.7854)*triangles_global$means[19], 0,tan(.2618)*triangles_global$means[22], -tan(.2618)*triangles_global$means[22], 0, -tan(.2618)*triangles_global$means[25], -tan(.7854)*triangles_global$means[25], 
                                                          0, -triangles_global$means[28], -triangles_global$means[28], 0, -triangles_global$means[31], -triangles_global$means[31], 0, -triangles_global$means[34], -triangles_global$means[34]),
                                                     
                                                     y = c(0, triangles_global$means[1], triangles_global$means[1], 0, triangles_global$means[4], triangles_global$means[4], 0, triangles_global$means[7], triangles_global$means[7],
                                                           0, tan(.2618)*triangles_global$means[10], tan(.7854)*triangles_global$means[10], 0,tan(.2618)*triangles_global$means[13], -tan(.2618)*triangles_global$means[13], 0, -tan(.2618)*triangles_global$means[16], -tan(.7854)*triangles_global$means[16],
                                                           0, -triangles_global$means[19], -triangles_global$means[19], 0, -triangles_global$means[22], -triangles_global$means[22], 0, -triangles_global$means[25], -triangles_global$means[25],
                                                           0, -tan(.2618)*triangles_global$means[28], -tan(.7854)*triangles_global$means[28], 0,tan(.2618)*triangles_global$means[31], -tan(.2618)*triangles_global$means[31], 0, tan(.2618)*triangles_global$means[34], tan(.7854)*triangles_global$means[34])) %>% 
                                                  add_column(all = "All") %>% 
                                                  mutate(all = factor(all)) 
#################### MAJOR DIAGONALS - DATA ########################
grey_square_coords <- tibble(
  x = c(-100, -100, 100, 100),
  y = c(-100, 100, 100, -100),
)

diagonals <- tibble(
  names = c("tll", "tll", "tll", "tll", "tll", "tll", 
            "xbll", "xbll", "xbll", "xbll", "xbll", "xbll"),
  x = c(
        -100, -100, -97.5,  100,  100, 97.5,         
        -100, -100, -97.5, 100,  100, 97.5    ),
  
  y = c(
        97.5,   100,   100,-97.5,  -100, -100,
        -97.5, -100, -100, 97.5, 100, 100  
  ))

################################ FRAMING TRAPEZOIDS - DATA ##########################

trapezoids <- tibble(
  names = c("d", "d", "d", "d", "o", "o", "o", "o", "p","p","p","p", "e", "e", "e", "e"),
  x = c(-110, -100, 100, 110, 
        110, 100, 100, 110,
        110, 100, -100, -110,
        -110, -100, -100, -110   
  ),
  
  y = c(110, 100, 100, 110,   
        110, 100,  -100, -110,  
        -110, -100, -100, -110,
        110, 100, -100, -110
  ))

######################## SEGMENTS - DATA #############################
segxb <- c(-25, 25, -25, -25,-50, 50, -50, -50,-75, 75, -75, -75,     0,   0, 0,0,0,0,0,0)
segyb <- c(-25, -25, 25, -25,-50, -50, 50, -50,-75, -75, 75, -75,     0,   0,0,0,0,0,0,0)
segxe <- c(-25, 25, 25, 25,-50, 50, 50, 50,-75, 75, 75, 75,       -26.79,  26.79, 100, 100, -26.79,  26.79, -100, -100)
segye <- c(25, 25, 25, -25,50, 50, 50, -50,75, 75, 75, -75,        100,    100,-26.79,  26.79,-100,    -100,-26.79,  26.79)

segments <- tibble(x= segxb, y=segyb, xend=segxe, yend=segye)

######################### PLOT ###############################

p <- ggplot() + 
  geom_polygon(data= grey_square_coords, aes(x=x, y = y), fill = "#F1F1F1") +
  
  geom_polygon(data=triangles_local, aes(x=x,y=y, group = Dimension, fill = factor(Quadrant)), show.legend = FALSE) +
  
  geom_polygon(data=triangles_global, aes(x=x,y=y, group = Dimension, fill = factor(all), alpha = .01), show.legend = FALSE) +

  geom_polygon(data=diagonals, aes(x=x, y=y, group = names, fill = factor(names)), show.legend = FALSE) +
  geom_polygon(data=trapezoids, aes(x=x, y=y, group = names, fill = factor(names)), show.legend = FALSE) +
  geom_segment(data = segments, aes(x=segxb, y=segyb, xend=segxe, yend=segye), color="white") +
  coord_cartesian(ylim = c(-120, 120), xlim = c(-120, 120))  + theme_classic() +
  
  scale_fill_manual(values = c("#75787B", "#C65C3D","#C65C3D","#B17E4A", "#B17E4A","#556B59", "#556B59", "#4A6B7D","#4A6B7D", "white", "white")) +
  theme(axis.title = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +

  ######## SCORE LABELS ################
annotate("text", x = (triangles_local$x[2]+triangles_local$x[3])/2, y = triangles_local$means[1]+5, label = round(triangles_local$means[1],0),  color = "#C65C3D", size = 4.5) +
  annotate("text", x = 0, y = triangles_local$means[4]+5, label = round(triangles_local$means[4],0),  color = "#C65C3D", size = 4.5 ) +
  annotate("text", x = (triangles_local$x[8]+triangles_local$x[9])/2, y = triangles_local$means[7]+5, label = round(triangles_local$means[7],0),  color = "#C65C3D", size = 4.5 ) +
  
  annotate("text", x = triangles_local$means[10]+5, y = (triangles_local$y[11]+triangles_local$y[12])/2, label = round(triangles_local$means[10],0),  color = "#556B59", size = 4.5 ) +
  annotate("text", x = triangles_local$means[13]+5, y = 0, label = round(triangles_local$means[13],0),  color = "#556B59", size = 4.5 ) +
  annotate("text", x = triangles_local$means[16]+5, y = (triangles_local$y[17]+triangles_local$y[18])/2, label = round(triangles_local$means[16],0),  color = "#556B59", size = 4.5 ) +
  
  annotate("text", x = (triangles_local$x[20]+triangles_local$x[21])/2, y = -triangles_local$means[19]-5, label = round(triangles_local$means[19],0),  color = "#4A6B7D", size = 4.5) +
  annotate("text", x = 0, y = -triangles_local$means[22]-5, label = round(triangles_local$means[22],0),  color = "#4A6B7D", size = 4.5 ) +
  annotate("text", x = (triangles_local$x[26]+triangles_local$x[27])/2, y = -triangles_local$means[25]-5, label = round(triangles_local$means[25],0),  color = "#4A6B7D", size = 4.5 ) +
  
  annotate("text", x = -triangles_local$means[28]-5, y = (triangles_local$y[29]+triangles_local$y[30])/2, label = round(triangles_local$means[28],0),  color = "#B17E4A", size = 4.5 ) +
  annotate("text", x = -triangles_local$means[31]-5, y = 0, label = round(triangles_local$means[31],0),  color = "#B17E4A", size = 4.5 ) +
  annotate("text", x = -triangles_local$means[34]-5, y = (triangles_local$y[35]+triangles_local$y[36])/2, label = round(triangles_local$means[34],0),  color = "#B17E4A", size = 4.5 ) +
  
  #################### QUAD LABELS #####################
  annotate("text", x = 0, y = 115, label = "D   I   R   E   C   T   I   O   N",  color = "#edcec4", size = 4.5) +
  annotate("text", x = -115, y = 0, label = "E   N   G   A   G   E   M   E   N   T", angle = 90,  color = "#E7D8C8", size = 4.5) +
  annotate("text", x = 0, y = -115, label = "P   E   O   P   L   E",  color = "#C8D2D8", size = 4.5) +
  annotate("text", x = 115, y = 0, label = "O   P   E   R   A   T   I   O   N   S", angle = 270,  color = "#CCD2CD", size = 4.5) +
  
  ########################### DIMENSION LABELS ################################
annotate("text", x = -105, y = -65.6, label = "F I T", angle = 90,  color = "white", size = 3.25) +
  annotate("text", x = -105, y = 0, label = "C U S T O M E R", angle = 90,  color = "white", size = 3.25) +
  annotate("text", x = -105, y = 62.6, label = "C L I M A T E", angle = 90,  color = "white", size = 3.25) +
  
  annotate("text", x = -65.6, y = 105, label = "V I S I O N", angle = 0,  color = "white", size = 3.25) +
  annotate("text", x = 0, y = 105, label = "S T R A T E G Y", angle = 0,  color = "white", size = 3.25) +
  annotate("text", x = 62.6, y = 105, label = "L E A D E R S H I P", angle = 0,  color = "white", size = 3.25) +
  
  annotate("text", x = 105, y = 65.6, label = "A D A P T A B I L I T Y", angle = 270,  color = "white", size = 3.25) +
  annotate("text", x = 105, y = 0, label = "P E R F O R M A N C E", angle = 270,  color = "white", size = 3.25) +
  annotate("text", x = 105, y = -62.6, label = "S Y S T E M S", angle = 270,  color = "white", size = 3.25) +
  
  annotate("text", x = -65.6, y = -105, label = "D E V E L O P M E N T", angle = 0,  color = "white", size = 3.25) +
  annotate("text", x = 0, y = -105, label = "T A L E N T", angle = 0,  color = "white", size = 3.25) +
  annotate("text", x = 62.6, y = -105, label = "T E A M W O R K", angle = 0,  color = "white", size = 3.25) +
  
  
  ############### QUARTILE LABELS###########################################
  
  annotate("text", x = -23, y = 23, label = "1", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = -48, y = 48, label = "2", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = -73, y = 73, label = "3", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = -98, y = 98, label = "4", angle = 0,  color = "BLACK", size = 2.2) +
  
  annotate("text", x = 23, y = 23, label = "1", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = 48, y = 48, label = "2", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = 73, y = 73, label = "3", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = 98, y = 98, label = "4", angle = 0,  color = "BLACK", size = 2.2) +
  
  annotate("text", x = -23, y = -23, label = "1", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = -48, y = -48, label = "2", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = -73, y = -73, label = "3", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = -98, y = -98, label = "4", angle = 0,  color = "BLACK", size = 2.2) +
  
  annotate("text", x = 23, y = -23, label = "1", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = 48, y = -48, label = "2", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = 73, y = -73, label = "3", angle = 0,  color = "BLACK", size = 2.2) +
  annotate("text", x = 98, y = -98, label = "4", angle = 0,  color = "BLACK", size = 2.2) 
  
  
   
# Dir_trap, Dir_tris, Eng_trap, Eng_tris, O-trap, O-tris, P-trap, P-tris
p

ggsave("C:/Users/Brent/Dropbox/Clients/Master data prep/Global CPI/Industrial/cpi_Lamons_vs_global_industrial.pdf", 
       plot = p, width = 8.5, height = 8.5)

