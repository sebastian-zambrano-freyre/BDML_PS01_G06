###FWL

##Se debe residualizar Y y nuestras X de interés (para la sección 1 y 2 serían
##age y age2)

##Residualizar Y

vars_modelo <- c("log_salary", "age", "age2", "female",
                 "college", "hoursWorkUsual",
                 "formal", "sizeFirm", "oficio", "femage", "femage2")

db_fwl <- db_2 %>%
  select(all_of(vars_modelo)) %>%
  na.omit()

# 2. Residualizar salario respecto a todos los controles
res_y <- lm(log_salary ~ age +
              age2 +
              college +
              hoursWorkUsual +
              formal +
              sizeFirm +
              femage +
              femage2
            | oficio,
            data = db_fwl)

db_fwl$resid_y <- resid(res_y)

# 3. Residualizar female
res_female <- lm(female ~ age +
                age2 +
                college +
                hoursWorkUsual +
                formal +
                sizeFirm +
                femage +
                femage2
              | oficio,
              data = db_fwl)

db_fwl$resid_female <- resid(res_female)

#Segunda etapa FWL
modelo_fwl <- lm(resid_y ~ resid_female, data = db_fwl)

summary(modelo_fwl)

# Extraer coeficientes del modelo FWL
b1 <- coef(modelo_fwl)["resid_age"]
b2 <- coef(modelo_fwl)["resid_age2"]

# Crear secuencia suave de edad
age_seq <- seq(min(db_fwl$age),
               max(db_fwl$age),
               length.out = 200)

# Construir curva predicha
curva_original <- data.frame(
  age = age_seq,
  log_salary_hat = b1 * age_seq + b2 * age_seq^2
)

# Gráfico
ggplot(db_fwl, aes(x = age, y = log_salary)) +
  geom_line(data = curva_original,
            aes(x = age, y = log_salary_hat),
            linewidth = 1.2) +
  labs(title = "Perfil Edad-Salario (coeficientes FWL)",
       x = "Edad",
       y = "Log salario") +
  theme_minimal()








####
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

