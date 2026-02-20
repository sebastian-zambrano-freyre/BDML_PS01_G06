# Debemos encontrar indicadores para medir el error de
# predicción. Se presentan 2
# Error derivado del LOOCV -> Observaciones impredecibles
loo_error <- resid(m11) / (1 - hatvalues(m11))
head(order(abs(loo_error), decreasing = TRUE), 10)

# Observaciones con mayor error de predicción absoluto
residuos <- resid(m11)
head(order(abs(residuos), decreasing = TRUE), 10)

# En ambos casos las observaciones más impredecibles
# son las mismas

# Encontrar las observaciones más influyentes en
# el modelo seleccionado

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
db
head(order(influence_beta, decreasing = TRUE), 10)

# Ahora hermos un análisis directo por grupos diferenciados por edad,
# género, formalidad y quintil de ingreso

db$loo_error <- (resid(m11) / (1 - hatvalues(m11)))
db$abs_loo_error <- abs(resid(m11) / (1 - hatvalues(m11)))
db$influence <- influence_beta

# Analisis por edad
db <- db %>%
  mutate(
    grupo_edad = cut(age,
                     breaks = c(18,25,34,49,64,90),
                     labels = c("18-25","26-34","35-49","50-64","65-90"),
                     include.lowest = TRUE)
  )

edad_stats <- db %>%
  group_by(grupo_edad) %>%
  summarise(
    error = mean(abs_loo_error, na.rm = TRUE),
    influencia = mean(influence, na.rm = TRUE),
    sesgo = mean(loo_error)
  )


# Analisis por género
db <- db %>%
  mutate(
    genero = ifelse(female==1,"Mujer","Hombre")
  )

genero_stats <- db %>%
  group_by(genero) %>%
  summarise(
    error=mean(abs_loo_error,na.rm=TRUE),
    influencia=mean(influence,na.rm=TRUE),
    sesgo = mean(loo_error)
  )

# Analisis por formalidad
db <- db %>%
  mutate(
    formalidad = ifelse(formal==1,"Formal","Informal")
  )

formal_stats <- db %>%
  group_by(formalidad) %>%
  summarise(
    error=mean(abs_loo_error,na.rm=TRUE),
    influencia=mean(influence,na.rm=TRUE),
    sesgo = mean(loo_error)
  )

# Analisis por quintil de ingresos
db <- db %>%
  mutate(
    quintil_ingreso = cut(log_salary,
                          breaks = quantile(log_salary,
                                            probs = seq(0,1,0.2),
                                            na.rm=TRUE),
                          include.lowest=TRUE,
                          labels=c("Q1","Q2","Q3","Q4","Q5"))
  )

ingreso_stats <- db %>%
  group_by(quintil_ingreso) %>%
  summarise(
    error=mean(abs_loo_error,na.rm=TRUE),
    influencia=mean(influence,na.rm=TRUE),
    sesgo = mean(loo_error)
  )