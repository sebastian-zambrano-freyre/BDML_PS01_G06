

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