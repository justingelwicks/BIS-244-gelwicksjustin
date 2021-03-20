#Justin  Gelwicks

# Clear console
rm(list=ls(all=TRUE))
cat("\014")

library(gapminder)
library(here)
library(tidyverse)

AAPL <- read.csv("~/BIS244/BIS-244-gelwicksjustin/AAPL.csv")

l <- length(AAPL$Adj.Close)

AAPL$incr_cases <- 1

for (i in 2:l) {
  AAPL$incr_cases[i] <- (AAPL$Adj.Close[i]-AAPL$Adj.Close[i-1]) 
}
  



for (i in 1:n) {
  if(AAPL$incr_cases[i]>=0) {
    AAPL$a[i] <- AAPL$incr_cases[i]
  } 
  
 # else if(AAPL$incr_cases[i]==0){
  #  AAPL$z[i] <- AAPL$incr_cases[i]  }
  
  else{
    AAPL$b[i] <- AAPL$incr_cases[i]
  }
}




  g <- ggplot(data = AAPL,
              mapping = aes(x = Date,
                            y = incr_cases))
  
  g + geom_point() +
    geom_point(data = AAPL, aes(x = Date, y = a), color = "#56B4E9") +
    #geom_point(data = AAPL, aes(x = Date, y = z), color = "gray") +
    
    geom_point(data = AAPL, aes(x = Date, y = b), color = "#FF0000") +
    
    labs(
         title = "Change in AAPL Daily Prices (Past Year)",
         x = "03/19/2020 through 03/18/2021",
         y = "Change in Adjusted Closing Price",
         caption = "Exam 1",
         subtitle = "Justin Gelwicks",
         col="UpDown"
         
         )
  
  #
  