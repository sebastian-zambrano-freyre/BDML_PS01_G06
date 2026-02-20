#Separemos las bases en train y test
db_train <- bind_rows(lista_tablas[1:7]) ##CAMBIAR ESTO
db_test <- bind_rows(lista_tablas[8:10]) ##CAMBIAR ESTO
db_total <- db

set.seed(777)

m1 <- lm(modelo_1,
         data = db_train)

predictions <- predict(object=m1, newdata=db_test)
score1 <- RMSE(pred = predictions, obs = db_test$log_salary)

m2 <- lm(modelo_2,
         data = db_train)

predictions <- predict(object=m2, newdata=db_test)
score2 <- RMSE(pred = predictions, obs = db_test$log_salary)

m3 <- lm(modelo_3,
         data = db_train)

predictions <- predict(object=m3, newdata=db_test)
score3 <- RMSE(pred = predictions, obs = db_test$log_salary)

m4 <- lm(modelo_4,
         data = db_train)

predictions <- predict(object=m4, newdata=db_test)
score4 <- RMSE(pred = predictions, obs = db_test$log_salary)


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