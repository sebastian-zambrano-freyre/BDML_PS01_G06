################Tabla Seccion 1
extra_rows <- data.frame(
  term = "Edad pico con Bootstrap",
  
  `Modelo 1` = sprintf("%.2f [%.2f , %.2f]",
                       edad_pico_m1,
                       ci_m1[1],
                       ci_m1[2]),
  
  `Modelo 2` = sprintf("%.2f [%.2f , %.2f]",
                       edad_pico_m2,
                       ci_m2[1],
                       ci_m2[2])
)

tabla_1 <- modelsummary(
  list("Modelo 1" = m1,
       "Modelo 2" = m2),
  
  stars = c('*' = .1, '**' = .05, '***' = .01),
  
  gof_map = c("nobs", "r.squared"),
  
  add_rows = extra_rows,
  
  output = "gt"
)

tabla_1 <- tabla_1 |>
  tab_header(
    title = "Resultados de regresión del perfil de ingresos por edad",
    subtitle = "Edad pico por Bootstrap para ambos modelos"
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_source_note(
    source_note = "Errores estándar entre paréntesis. Intervalos Bootstrap al 95%"
  )

tabla_1

gtsave(tabla_1,
       "02_outputs/tables/tabla_1.png",
       dpi = 300
)

gtsave(tabla_1, "02_outputs/tables/tabla_1.pdf")

# De la tabla del cuaderno, para los 12 modelos
scores<- data.frame( Model= c(1, 2, 3, 4),
                     RMSE_vsa= c(score1a, score2a, score3a, score4a), 
                     RMSE_kfold= c(score1b, score2b, score3b, score4b),
                     RMSE_loocv= c(score1c, score2c, score3c, score4c)
)

head(scores)

