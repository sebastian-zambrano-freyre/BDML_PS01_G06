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
    age2 = age^2,
    femage = female*age,
    femage2 = female*age2
  )

##Modelo 3
m3 <- feols(
  log_salary ~ female,
  data = db_2
) 

summary(m3)

#Modelo 4 interacciones femage sin efectos fijos de oficio

m4 <- feols(
  log_salary ~ female +
    age + age2 +
    college +
    hoursWorkUsual +
    formal +
    sizeFirm +
  femage +
    femage2,
  data = db_2
)
summary(m4)

##Modelo 4.1, femage con efectos fijos de oficio

m4.1 <- feols(
  log_salary ~ female +
    age + age2 +
    college +
    hoursWorkUsual +
    formal +
    sizeFirm +
    femage +
    femage2
  | oficio,
  data = db_2
)
summary(m4.1)

#Los datos son unicamente de Bogota, no es necesario aplicar EF de departamento
#unique(db_2$depto)

##Tabla comparativa
etable(m3.1, m4.1)


##Modelo estimado con FWL
res_y <- feols(
  log_salary ~ age + age2 + college +
    hoursWorkUsual + formal + sizeFirm +
    femage + femage2 | oficio,
  data = db_2
)

res_female <- feols(
  female ~ age + age2 + college +
    hoursWorkUsual + formal + sizeFirm +
    femage + femage2 | oficio,
  data = db_2
)

y_tilde <- resid(res_y)
x_tilde <- resid(res_female)

modelo_fwl <- lm(y_tilde ~ x_tilde)

summary(modelo_fwl)

##errores estandar analiticos

se_an <- se(m4.1)["female"]
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
      cuentaPropia +
      femage +
      femage2
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

##Coeficientes
b_age <- coef(m4.1)["age"]
b_age2 <- coef(m4.1)["age2"]
b_femage <- coef(m4.1)["femage"]
b_femage2 <- coef(m4.1)["femage2"]
b_fem <- coef(m4.1)["female"]

age_seq <- seq(min(db_2$age), max(db_2$age), length.out = 100)

curvas <- data.frame(
  age = age_seq,
  hombres = b_age * age_seq +
    b_age2 * age_seq^2,
  mujeres = b_fem +
    (b_age + b_femage) * age_seq +
    (b_age2 + b_femage2) * age_seq^2
)

curvas_long <- pivot_longer(curvas,
                            cols = c("hombres", "mujeres"),
                            names_to = "sexo",
                            values_to = "log_salary_hat")

ggplot(db_2, aes(x = age, y = log_salary)) +
  geom_line(data = curvas_long,
            aes(x = age, y = log_salary_hat, color = sexo),
            linewidth = 1.2) +
  labs(title = "Perfil Edad-Salario por Género",
       x = "Edad",
       y = "Log salario",
       color = "Grupo") +
  theme_minimal()

##Edades pico

edad_pico_hombres <- - b_age / (2 * b_age2)
edad_pico_hombres

edad_pico_mujeres <- - (b_age + b_femage) /
  (2 * (b_age2 + b_femage2))

edad_pico_mujeres

###Intervalos de confianza edades pico

calcular_picos <- function(data){
  
  modelo <- feols(
    log_salary ~ female +
      age + age2 +
      college +
      hoursWorkUsual +
      formal +
      sizeFirm +
      femage +
      femage2
    | oficio,
    data = data
  )
  
  b <- coef(modelo)
  
  pico_h <- - b["age"] / (2 * b["age2"])
  
  pico_m <- - (b["age"] + b["femage"]) /
    (2 * (b["age2"] + b["femage2"]))
  
  return(c(pico_h, pico_m))
}

##Cluster por oficio

set.seed(777)

B <- 500   # número de repeticiones
clusters <- unique(db_2$oficio)

resultados <- matrix(NA, nrow = B, ncol = 2)

for (b in 1:B){
  
  # remuestreo de clusters
  clusters_boot <- sample(clusters,
                          size = length(clusters),
                          replace = TRUE)
  
  # construir base bootstrap
  db_boot <- do.call(rbind,
                     lapply(clusters_boot,
                            function(cl){
                              db_2[db_2$oficio == cl, ]
                            }))
  
  resultados[b, ] <- calcular_picos(db_boot)
}

colnames(resultados) <- c("hombres", "mujeres")

##IC
IC_hombres  <- quantile(resultados[,1], c(0.025, 0.975))
IC_mujeres  <- quantile(resultados[,2], c(0.025, 0.975))

IC_hombres
IC_mujeres

##Media
mean(resultados[,1])
mean(resultados[,2])


###

# Data frame en formato largo
df_boot <- data.frame(
  edad_pico = c(resultados[,1], resultados[,2]),
  grupo = rep(c("Hombres", "Mujeres"),
              each = nrow(resultados))
)

# Medias
mean_h <- mean(resultados[,1])
mean_m <- mean(resultados[,2])

# IC
low_h <- IC_hombres[1]
up_h  <- IC_hombres[2]

low_m <- IC_mujeres[1]
up_m  <- IC_mujeres[2]

ggplot(df_boot, aes(x = edad_pico, fill = grupo)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  
  # Medias
  geom_vline(xintercept = mean_h, linewidth = 1, color = "red") +
  geom_vline(xintercept = mean_m, linewidth = 1, color= "blue") +
  
  # IC hombres
  geom_vline(xintercept = low_h, linetype = "dashed", linewidth = 1, color = "red") +
  geom_vline(xintercept = up_h,  linetype = "dashed", linewidth = 1 , color= "red") +
  
  # IC mujeres
  geom_vline(xintercept = low_m, linetype = "dashed", linewidth = 1, color ="blue") +
  geom_vline(xintercept = up_m,  linetype = "dashed", linewidth = 1, color ="blue") +
  
  labs(title = "Distribución Bootstrap de la Edad Pico",
       x = "Edad pico",
       y = "Frecuencia") +
  theme_minimal()


