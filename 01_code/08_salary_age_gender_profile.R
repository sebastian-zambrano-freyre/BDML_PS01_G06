##Prediccion del perfil edad-salario

##Coeficientes
b_age <- coef(m4.1)["age"]
b_age2 <- coef(m4.1)["age2"]
b_femage <- coef(m4.1)["femage"]
b_femage2 <- coef(m4.1)["femage2"]
b_fem <- coef(m4.1)["female"]

age_seq <- seq(min(db$age), max(db$age), length.out = 100)

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


##Edades pico

edad_pico_hombres <- - b_age / (2 * b_age2)
edad_pico_hombres

edad_pico_mujeres <- - (b_age + b_femage) /
  (2 * (b_age2 + b_femage2))

edad_pico_mujeres
