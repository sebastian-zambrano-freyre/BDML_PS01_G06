# install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)

# require/install packages on this session
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, # Correlation Plots 
       stargazer,        # tables/output to TEX.
       dplyr,
       tibble,
       fixest,
       boot,
       ggplot2)

require(pacman) 

p_load(tidyverse, 
       rvest) # Librería de web scraping.