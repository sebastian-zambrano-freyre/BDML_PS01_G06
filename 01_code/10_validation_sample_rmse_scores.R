# Separemos las bases en train y test
db_train <- db %>% filter(chunk < 8)
db_test <- db %>% filter(chunk > 7)

# Establecemos el seed
set.seed(777)

# Performance del modelo 1
m1 <- lm(modelo_1,
         data = db_train)

predictions <- predict(object=m1, newdata=db_test)
score1 <- RMSE(pred = predictions, obs = db_test$log_salary)

# Performance del modelo 2
m2 <- lm(modelo_2,
         data = db_train)

predictions <- predict(object=m2, newdata=db_test)
score2 <- RMSE(pred = predictions, obs = db_test$log_salary)

# Performance del modelo 3
m3 <- lm(modelo_3,
         data = db_train)

predictions <- predict(object=m3, newdata=db_test)
score3 <- RMSE(pred = predictions, obs = db_test$log_salary)

# Performance del modelo 4
m4 <- lm(modelo_4,
         data = db_train)

predictions <- predict(object=m4, newdata=db_test)
score4 <- RMSE(pred = predictions, obs = db_test$log_salary)

# Performance de los modelos del 5 al 12
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

}

# RMSE finales
rmse_scores[1] <- score1
rmse_scores[2] <- score2
rmse_scores[3] <- score3
rmse_scores[4] <- score4
rmse_scores[1:12]

which.min(rmse_scores)
rmse_scores[which.min(rmse_scores)]

best_score <- rmse_scores[11]