#Separemos las bases en train y test
db_train <- bind_rows(lista_tablas[1:7])
db_test <- bind_rows(lista_tablas[8:10])
db_total <- bind_rows(lista_tablas)

db_train <- db_train %>% filter(age>=18,
                        ocu == 1,                 # Solo ocupados
                        y_salary_m > 0,           # Salario positivo
                        !is.na(sex))

db_test <- db_test %>% filter(age>=18,
                                ocu == 1,                 # Solo ocupados
                                y_salary_m > 0,           # Salario positivo
                                !is.na(sex))

db_total <- db_total %>% filter(age>=18,
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

db_total <- db_total %>% mutate(
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

db_total <- db_total %>% mutate(
  femage = female * age,
  femage2 = female * age2
)

##Nuevos modelos
modelo_5 <-  log_salary ~ female + age + age2 + college + femage + hoursWorkUsual + formal + sizeFirm + oficio
modelo_6 <-  log_salary ~ female + age + age2 + college + femage + femage2 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_7 <-  log_salary ~ female + age + age2 + college + femage + p6050 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_8 <-  log_salary ~ female + age + age2 + college + femage + femage2 + p6050 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_9 <-  log_salary ~ female + age + age2 + college + femage + femage2 + p7040 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_10 <- log_salary ~ female + age + age2 + college + p6050 + p7040 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_11 <- log_salary ~ female + age + age2 + college + p6050 + p7040 + femage + hoursWorkUsual + formal + sizeFirm + oficio
modelo_12 <- log_salary ~ female + age + age2 + college + p6050 + p7040 + femage + femage2 + hoursWorkUsual + formal + sizeFirm + oficio



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
rmse_scores[1] <- score1
rmse_scores[2] <- score2
rmse_scores[3] <- score3
rmse_scores[4] <- score4
rmse_scores[1:12]

which.min(rmse_scores)
rmse_scores[which.min(rmse_scores)]

#Nos quedamos con el modelo 11 :)
# Ajustar modelo
m11 <- lm(modelo_11, data = db_total)

# Residuos
e <- resid(m11)

# Leverage (diagonal de la hat matrix)
h <- hatvalues(m11)

# LOOCV RMSE
rmse_loocv <- sqrt(mean((e / (1 - h))^2))

rmse_loocv


loo_error <- resid(m11) / (1 - hatvalues(m11))

head(order(abs(loo_error), decreasing = TRUE), 10)

residuos <- resid(m11)

# Observaciones con mayor error absoluto
head(order(abs(residuos), decreasing = TRUE), 10)


# Matriz de diseño
X <- model.matrix(m11)

# Residuos
e <- resid(m11)

# Leverage
h <- hatvalues(m11)

# (X'X)^(-1)
XtX_inv <- solve(t(X) %*% X)

# Multiplicador escalar por observación
mult <- e / (1 - h)

# Calcular todas las diferencias beta(-i) - beta en una sola operación
# t(X) %*% diag(mult)  equivale a multiplicar cada fila de X por mult_i
beta_diff <- XtX_inv %*% t(X * mult)

# Ahora cada columna es el vector beta(-i) - beta
# Calculamos norma euclidiana por observación

influence_beta <- sqrt(colSums(beta_diff^2))
#Normalización de la influencia
influence_beta <- influence_beta / sqrt(ncol(X))
# Observaciones más influyentes
head(order(influence_beta, decreasing = TRUE), 10)