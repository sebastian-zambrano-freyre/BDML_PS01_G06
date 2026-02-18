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

ggplot(db_1, aes(x = age)) +
  #geom_point(aes(y = log_salary), color = "black") +          # Datos reales
  geom_line(aes(y = predicciones1), color = "blue", linewidth = 1) +  # Predicción 1
  geom_line(aes(y = predicciones2), color = "red", linewidth = 0.5) +  # Predicción 2
  geom_line(aes(y = predicciones2_1), color = "orange", linewidth = 0.5) +  # Predicción 2.1
  geom_line(aes(y = predicciones2_2), color = "green", linewidth = 0.5) +  # Predicción 2.1
  labs(title = "Predicciones del modelo",
       x = "age",
       y = "log_salary") +
  theme_minimal()

##Calculo de la edad pico con BOOTSTRAP
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
  R = 500   #Inicialmente usemos 500, si todo corre bien lo podemos subir a 1000 como sugiere la literatura
)

##Intervalo de confianza

boot.ci(boot_results, type = "perc")

stargazer(m1, m2, type = "text")