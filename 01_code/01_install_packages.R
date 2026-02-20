# Instalamos en caso de que no estén en el equipo y luego llamamos todos los paquetes necesarios
if(!require(pacman)) install.packages("pacman") ; require(pacman)
if(!require(gt)) install.packages("gt") ; require(gt)
if(!require(webshot2)) install.packages("webshot2") ; require(webshot2)

p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, # Correlation Plots 
       stargazer, # tables/output to TEX.
       dplyr,
       tibble,
       fixest,
       boot,
       ggplot2,
       caret,
       modelsummary,
       gt,
       webshot2,
       rvest,    # Librería de web scraping.
       patchwork)