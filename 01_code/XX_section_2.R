###Section 2

db <- bind_rows(lista_tablas)
db <- db %>% filter(age>=18,
                    ocu == 1,                 # Solo ocupados
                    y_salary_m > 0,           # Salario positivo
                    !is.na(sex))

#Agregar logaritmo del salario y edad al cuadrado

db_2 <- db %>% mutate(
    female = 1 - sex,         # 1 = mujer
    log_salary = log(y_salary_m),
    age2 = age^2
  )

##Modelo 3
m3 <- feols(
  log_salary ~ female,
  data = db_2
)

summary(m3)

#Modelo 4

m4 <- feols(
  log_salary ~ female +
    age + age2 +
    college +
    hoursWorkUsual +
    formal +
    sizeFirm
  | oficio,
  data = db_2
)

summary(m4)

#Los datos son unicamente de Bogota, no es necesario aplicar EF de departamento
#unique(db_2$depto)

##Tabla comparativa
etable(m3, m4)

##errores estandar analiticos

se_an <- se(m4)["female"]
se_an

##errores estandar bootstrap

boot_fn <- function(data, indices) {
  
  d <- data[indices, ]   # remuestreo
  
  model <- feols(
    log_salary ~ female +
      age + age2 +
      college +
      hoursWorkUsual +
      formal +
      sizeFirm +
      cuentaPropia
    | oficio,
    data = d
  )
  
  coef(model)["female"]
}

set.seed(777)

boot_results <- boot(
  data = db_2,
  statistic = boot_fn,
  R = 500   #Inicialmente usemos 500, si todo corre bien lo podemos subir a 1000 como sugiere la literatura
)

se_boot <- sd(boot_results$t)
se_boot

##Intervalo de confianza

boot.ci(boot_results, type = "perc")

##Prediccion del perfil edad-salario

age_seq <- seq(min(db_2$age), max(db_2$age), by = 1)

##Función que genera la predicción promedio
avg_profile <- function(age_value, female_value, model, data){
  
  newdata <- data
  
  newdata$age <- age_value
  newdata$female <- female_value
  
  preds <- predict(model, newdata = newdata)
  
  mean(preds, na.rm = TRUE)
}

##Construir perfiles

profile_df <- expand.grid(
  age = age_seq,
  female = c(0,1)
)

profile_df$log_pred <- mapply(
  avg_profile,
  age_value = profile_df$age,
  female_value = profile_df$female,
  MoreArgs = list(model = m4, data = db_2)
)

##profile_df$wage_pred <- exp(profile_df$log_pred) ### Se puede usar para
##graficar en niveles, pero la gráfica no es tan facil de interpretar.

ggplot(profile_df, aes(x = age, y = log_pred, color = factor(female))) +
  geom_line(size = 1.2) +
  labs(
    x = "Edad",
    y = "Ingreso laboral predicho",
    color = "Sexo",
    title = "Perfiles edad–ingreso (Average Predictions)"
  ) +
  scale_color_manual(
    labels = c("Hombres", "Mujeres"),
    values = c("blue", "red")
  ) +
  theme_minimal()

###

b2 <- coef(m4)["age"]
b3 <- coef(m4)["age2"]

peak_age <- -b2 / (2*b3)
peak_age

# Coeficientes
b2 <- coef(m4)["age"]
b3 <- coef(m4)["age2"]

# Matriz var-cov
V <- vcov(m4)

# Gradiente
g1 <- -1 / (2*b3)
g2 <- b2 / (2*b3^2)

# Varianza delta
var_peak <- 
  g1^2 * V["age","age"] +
  g2^2 * V["age2","age2"] +
  2*g1*g2 * V["age","age2"]

se_peak <- sqrt(var_peak)

# Intervalo 95%
ci_lower <- peak_age - 1.96*se_peak
ci_upper <- peak_age + 1.96*se_peak

c(Peak = peak_age,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper)

profile_df$age_component <- 
  coef(m4)["age"]*profile_df$age +
  coef(m4)["age2"]*(profile_df$age^2)

ggplot(profile_df, aes(age, age_component)) +
  geom_line()


#############################################################
###Alternativa, ceteris paribus en medias


# Secuencia de edades
# Valores promedio para controles continuos
means <- db_2 %>%
  summarise(
    college = mean(college, na.rm = TRUE),
    hoursWorkUsual = mean(hoursWorkUsual, na.rm = TRUE),
    formal = mean(formal, na.rm = TRUE),
    sizeFirm = mean(sizeFirm, na.rm = TRUE),
    cuentaPropia = mean(cuentaPropia, na.rm = TRUE),
    age2 = mean(age2, na.rm = TRUE)
  )

# Crear dataset de predicción
pred_data <- expand.grid(
  age = age_seq,
  female = c(0,1)
)

pred_data <- pred_data %>%
  mutate(
    college = means$college,
    hoursWorkUsual = means$hoursWorkUsual,
    formal = means$formal,
    sizeFirm = means$sizeFirm,
    cuentaPropia = means$cuentaPropia,
    age2 = means$age2,
    oficio = db_2$oficio[1]
  )

pred_data$log_pred <- predict(m4, newdata = pred_data)

# Si quieres salario en niveles:
pred_data$wage_pred <- exp(pred_data$log_pred)

library(ggplot2)

ggplot(pred_data, aes(x = age, y = wage_pred, color = factor(female))) +
  geom_line(size = 1.2) +
  labs(
    x = "Edad",
    y = "Ingreso laboral predicho",
    color = "Sexo",
    title = "Perfiles edad–ingreso predichos por sexo"
  ) +
  scale_color_manual(labels = c("Hombres", "Mujeres"),
                     values = c("blue", "red")) +
  theme_minimal()
