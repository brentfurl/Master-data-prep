### so I think the way to go is to enter the data into the global set first and let them be part of the population and then calculate
## use the population mean and sd, along with the sample mean and sample n to get the z score and then pnorm it.
## so the task now is to use North Star to create a report from start to finish.  Also, use the same sort of data sheet that I'll send to Sebastian.




scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

dfz <- df %>% 
  mutate(CPI_index_z2 = scale(CPI_index))
  
  
  
  map(~ df %>% 
        select(CPI_index:Positivity), ~{
          sd(.)*sqrt((length(.)-1)/(length(.)))
        })
  
  z <- (. - pop_mean) / pop_sd
  
  
  