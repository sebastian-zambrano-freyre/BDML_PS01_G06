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
  data = db,
  statistic = boot_fn,
  R = 500   #Inicialmente usemos 500, si todo corre bien lo podemos subir a 1000 como sugiere la literatura
)

##Edad Pico e Intervalo de confianza
edad_pico_m1 <- mean(boot_results$t)
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
  data = db,
  statistic = boot_fn_2,
  R = 500   #Inicialmente usemos 500, si todo corre bien lo podemos subir a 1000 como sugiere la literatura
)

##Edad Pico e Intervalo de confianza
edad_pico_m2 <- mean(boot_results$t)
ci_m2 <- quantile(boot_results$t, c(0.025, 0.975))