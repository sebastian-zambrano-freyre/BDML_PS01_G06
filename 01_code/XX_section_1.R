##Tomamos la base ya limpia y armada
db <- bind_rows(lista_tablas)
db <- db %>% filter(age>=18,
                    ocu == 1,                 # Solo ocupados
                    y_salary_m > 0,           # Salario positivo
                    !is.na(sex))

#Agregar logaritmo del salario
db_1 <- db %>% mutate(
  log_salary = log(y_salary_m),
  age2 = age^2
)

#Modelo 1
m1 <- lm(
  log_salary ~ age + age2,
  data = db_1
)

summary(m1)
stargazer(m1, type = "text")


#Modelo 2
m2 <- lm(
  log_salary ~ age + age2 +
    totalHoursWorked + relab,
  data = db_1
)

summary(m2)
stargazer(m2, type = "text") 


##Calculo de la edad pico con BOOTSTRAP para el Modelo 1
boot_fn <- function(data, indices) {
  
  d <- data[indices, ]   # remuestreo
  
  model <- lm(
    log_salary ~ age + age2,
    data = d
  )
  
  beta_2 <- coef(model)["age"]
  beta_3 <- coef(model)["age2"]
  
  edad_pico <- (-1 * beta_2) / (2 * beta_3)
  
  return(edad_pico)
  
}

set.seed(777)

boot_results <- boot(
  data = db_1,
  statistic = boot_fn,
  R = 2000   #Inicialmente usemos 500, si todo corre bien lo podemos subir a 1000 como sugiere la literatura
)

##Edad Pico e Intervalo de confianza
edad_pico_m1 <- mean(boot_results$t)
edad_pico_m1
boot.ci(boot_results, type = "perc")
ci_m1 <- quantile(boot_results$t, c(0.025, 0.975))

##Calculo de la edad pico con BOOTSTRAP para el Modelo 2
boot_fn_2 <- function(data, indices) {
  
  d <- data[indices, ]   # remuestreo
  
  model <- lm(
    log_salary ~ age + age2 + totalHoursWorked + relab,
    data = d
  )
  
  beta_2 <- coef(model)["age"]
  beta_3 <- coef(model)["age2"]
  
  edad_pico <- (-1 * beta_2) / (2 * beta_3)
  
  return(edad_pico)
  
}

set.seed(777)

boot_results <- boot(
  data = db_1,
  statistic = boot_fn_2,
  R = 2000   #Inicialmente usemos 500, si todo corre bien lo podemos subir a 1000 como sugiere la literatura
)

##Edad Pico e Intervalo de confianza
edad_pico_m2 <- mean(boot_results$t)
edad_pico_m2
boot.ci(boot_results, type = "perc")
ci_m2 <- quantile(boot_results$t, c(0.025, 0.975))


#Modelo 2 hecho con FWL
reg_y <- lm(log_salary ~ totalHoursWorked + relab, data = db_1)
db_1$y_tilde <- resid(reg_y)

reg_age <- lm(age ~ totalHoursWorked + relab, data = db_1)
db_1$age_tilde <- resid(reg_age)

reg_age2 <- lm(age2 ~ totalHoursWorked + relab, data = db_1)
db_1$age2_tilde <- resid(reg_age2)

modelo_fwl <- lm(y_tilde ~ age_tilde + age2_tilde - 1, data = db_1)
coef_fwl <- coef(modelo_fwl)

stargazer(modelo_fwl, type = "text")

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

##################################
db_1$predicciones1 <- predict(m1)

coef_m2 <- coef(m2)

# Crear predicción manual usando solo age y age2
db_1$predicciones2 <- coef_m2["(Intercept)"] +
  coef_m2["age"] * db_1$age +
  coef_m2["age2"] * db_1$age2

mean_hours <- mean(db_1$totalHoursWorked, na.rm = TRUE)
mean_relab <- mean(db_1$relab, na.rm = TRUE)

db_1$predicciones2_1 <- coef_m2["(Intercept)"] +
  coef_m2["age"] * db_1$age +
  coef_m2["age2"] * db_1$age2 +
  coef_m2["totalHoursWorked"] * mean_hours +
  coef_m2["relab"] * mean_relab

db_1$predicciones2_2 <- predict(m2)

db_1$pred_tilde <- predict(modelo_fwl)
db_1$pred_controles <- predict(reg_y)
#db_1$predicciones2_3 <- db_1$pred_tilde + db_1$pred_controles
db_1$predcciones2_3 <- coef_fwl["age_tilde"] * db_1$age +
  coef_fwl["age2_tilde"] * db_1$age2
#aqui es necesario usar listas de valores para las dos variables


stargazer(m1, m2, modelo_fwl, type = "text")

ggplot(db_1, aes(x = age)) +
  #geom_point(aes(y = log_salary), color = "black") +          # Datos reales
  geom_line(aes(y = predicciones1, color = "Modelo 1"), linewidth = 1.2) +  # Predicción 1
  #geom_line(aes(y = predicciones2), color = "orange", linewidth = 0.5) +  # Predicción 2
  geom_line(aes(y = predicciones2_1, color = "Modelo 2"), linewidth = 1.2) +  # Predicción 2.1
  #geom_line(aes(y = predicciones2_3), color = "green", linewidth = 0.5) +  # Predicción 2.1
  scale_color_manual(
    values = c("Modelo 1" = "#1F4E79",
              "Modelo 2" = "#8B0000")
  ) +
  labs(title = "Perfil Edad - Ingreso con 2 modelos",
       x = "Edad",
       y = "Log del salario",
       color = NULL
       ) +
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank()
)

