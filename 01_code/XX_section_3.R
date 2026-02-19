#Separemos las bases en train y test
db_train <- bind_rows(lista_tablas[1:7])
db_test <- bind_rows(lista_tablas[8:10])

db_train <- db_train %>% filter(age>=18,
                        ocu == 1,                 # Solo ocupados
                        y_salary_m > 0,           # Salario positivo
                        !is.na(sex))

db_test <- db_test %>% filter(age>=18,
                                ocu == 1,                 # Solo ocupados
                                y_salary_m > 0,           # Salario positivo
                                !is.na(sex))

db_train <- db_train %>% mutate(
  log_salary = log(y_salary_m),
  female = 1 - sex,
  age2 = age^2
)

db_test <- db_test %>% mutate(
  log_salary = log(y_salary_m),
  female = 1 - sex,
  age2 = age^2
)

set.seed(777)

modelo_1 <- log_salary ~ age + age2
m1 <- lm(modelo_1,
         data = db_train)

predictions <- predict(object=m1, newdata=db_test)
score1 <- RMSE(pred = predictions, obs = db_test$log_salary)
score1

modelo_2 <- log_salary ~ age + age2 + totalHoursWorked + relab
m2 <- lm(modelo_2,
         data = db_train)

predictions <- predict(object=m2, newdata=db_test)
score2 <- RMSE(pred = predictions, obs = db_test$log_salary)
score2

modelo_3 <- log_salary ~ female
m3 <- lm(modelo_3,
         data = db_train)

predictions <- predict(object=m3, newdata=db_test)
score3 <- RMSE(pred = predictions, obs = db_test$log_salary)
score3

modelo_4 <- log_salary ~ female + age + age2 + college + hoursWorkUsual + formal + sizeFirm + oficio
m4 <- lm(modelo_4,
         data = db_train)

predictions <- predict(object=m4, newdata=db_test)
score4 <- RMSE(pred = predictions, obs = db_test$log_salary)
score4

score1
score2
score3
score4

db_train <- db_train %>% mutate(
  femage = female * age,
  femage2 = female * age2
)

db_test <- db_test %>% mutate(
  femage = female * age,
  femage2 = female * age2
)


##Nuevos modelos
modelo_5 <- log_salary ~ female + age + age2 + femage + college + hoursWorkUsual + formal + sizeFirm + oficio
modelo_6 <- log_salary ~ female + age + age2 + femage + femage2 + college + hoursWorkUsual + formal + sizeFirm + oficio
modelo_7 <- log_salary ~ female + age + age2 + femage + p6050 + college + hoursWorkUsual + formal + sizeFirm + oficio
modelo_8 <- log_salary ~ female + age + age2 + maxEducLevel + femage + hoursWorkUsual + formal + sizeFirm + oficio
modelo_9 <- log_salary ~ female + age + age2 + maxEducLevel + femage + femage2 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_10 <- log_salary ~ female + age + age2 + maxEducLevel + p7040 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_11 <- log_salary ~ female + age + age2 + maxEducLevel + p7040 + femage + hoursWorkUsual + formal + sizeFirm + oficio
modelo_12 <- log_salary ~ female + age + age2 + maxEducLevel + p7040 + femage + femage2 + hoursWorkUsual + formal + sizeFirm + oficio



modelos <- list()
predicciones <- list()
rmse_scores <- numeric(12)  # vector para RMSE

# Loop de 5 a 12
for (i in 5:12) {
  
  # Construir el nombre de la fórmula
  formula_name <- paste0("modelo_", i)
  
  # Obtener la fórmula usando get()
  formula_i <- get(formula_name)
  
  # Ajustar modelo con lm()
  modelos[[i]] <- lm(formula_i, data = db_train)
  
  # Predecir sobre db_test
  predicciones[[i]] <- predict(modelos[[i]], newdata = db_test)
  
  # Calcular RMSE
  rmse_scores[i] <- RMSE(pred = predicciones[[i]], obs = db_test$log_salary)
  
  # Opcional: imprimir resultado
  cat("Modelo", i, "RMSE =", rmse_scores[i], "\n")
}

# RMSE finales
rmse_scores[5:12]

