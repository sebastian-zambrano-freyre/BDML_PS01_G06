# Tabla de Comparación de los modelos 1 y 2
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
       "02_outputs/tables/tabla_1_m1_m2.png",
       zoom = 3
)

##Tabla comparativa modelos 3 y 4 (section 2)

tabla_2 <- modelsummary(
  list(
    "Modelo 3" = m3.1,
    "Modelo 4" = m4.1
  ),
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|Within",
  output = "gt"
)

gtsave(tabla_2,
       "02_outputs/tables/tabla_2_m3_m4.png",
       zoom = 3
)

##Tabla FWL con error estandar bootstrap

# Extraer valores
coef_est <- coef(modelo4_fwl)["x_tilde"]
se_conv  <- summary(modelo4_fwl)$coefficients["x_tilde","Std. Error"]
se_boot  <- se_boot_m4

# data frame de parametros
tabla_df <- data.frame(
  Variable = "Mujer (FWL residualizada)",
  Coeficiente = round(coef_est, 4),
  `EE Convencional` = round(se_conv, 4),
  `EE Bootstrap` = round(se_boot, 4)
)

# Tabla final
tabla_3 <- tabla_df |>
  gt() |>
  tab_header(
    title = "Brecha salarial asociada a ser mujer",
    subtitle = "Modelo FWL con efectos fijos por oficio"
  ) |>
  cols_align(align = "center", columns = -Variable) |>
  tab_source_note(
    source_note = "Controles: edad, edad², universidad, horas usuales, formalidad, tamaño firma, mujer×edad y mujer×edad²."
  ) |>
  tab_source_note(
    source_note = "Errores estándar convencionales y bootstrap (500 réplicas)."
  )

gtsave(tabla_3,
       "02_outputs/tables/tabla_3_m4_se_bootstrap.png",
       zoom = 3
)

# Tabla para comparar los scores de predicción de los modelos

tabla_modelos <- data.frame(
  Modelo = paste("Modelo", 1:12),
  RMSE = rmse_scores,
  Descripción = c(
    rep("De la Sección 1", 2),
    rep("De la Sección 2", 2),
    rep("Mayor complejidad", 8)
  ),
  Selección = ifelse(1:12 == 11, "Modelo ganador", "")
)

tabla_RMSE <- datasummary_df(
  tabla_modelos,
  fmt = 6,
  title = "Comparación de Modelos: RMSE",
  output = "gt"
) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  cols_width(
    Modelo ~ px(150),
    Descripción ~ px(250),
    RMSE ~ px(150),
    Selección ~ px(200)
  ) |>
  tab_options(
    heading.align = "center",
    table.width = px(750)
  )

gtsave(
  tabla_RMSE,
  filename = "02_outputs/tables/tabla_rmse.png",
  zoom = 3
)