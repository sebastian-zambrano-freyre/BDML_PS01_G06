# Nos quedamos con el modelo 11 :)
# Ajustar modelo
m11 <- lm(modelo_11, data = db_total)

# Vamos a usar la forma cerrada para calcular el error con
# LOOCV (usar el leverage)

# Residuos
e <- resid(m11)

# Leverage (diagonal de la hat matrix)
h <- hatvalues(m11)

# LOOCV RMSE
rmse_loocv <- sqrt(mean((e / (1 - h))^2))

rmse_loocv