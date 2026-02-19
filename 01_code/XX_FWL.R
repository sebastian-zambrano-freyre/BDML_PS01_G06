###FWL

m4 <- lm(
  log_salary ~ female +
    age + age2 +
    college +
    hoursWorkUsual +
    formal +
    sizeFirm +
    oficio,
  data = db_2
)

summary(m4)

##Se debe residualizar Y y nuestras X de interés (para la sección 1 y 2 serían
##age y age2)

##Residualizar Y

# 1. Crear base solo con observaciones completas en todas las variables relevantes
vars_modelo <- c("log_salary", "age", "age2", "female",
                 "college", "hoursWorkUsual",
                 "formal", "sizeFirm", "oficio")

db_fwl <- db_2 %>%
  select(all_of(vars_modelo)) %>%
  na.omit()

# 2. Residualizar salario respecto a todos los controles (excepto age y age2)
res_y <- lm(log_salary ~ female +
              college +
              hoursWorkUsual +
              formal +
              sizeFirm +
              factor(oficio),
            data = db_fwl)

db_fwl$resid_y <- resid(res_y)

# 3. Residualizar age
res_age <- lm(age ~ female +
                college +
                hoursWorkUsual +
                formal +
                sizeFirm +
                factor(oficio),
              data = db_fwl)

db_fwl$resid_age <- resid(res_age)

# 4. Residualizar age2
res_age2 <- lm(age2 ~ female +
                 college +
                 hoursWorkUsual +
                 formal +
                 sizeFirm +
                 factor(oficio),
               data = db_fwl)

db_fwl$resid_age2 <- resid(res_age2)

# 5. Segunda etapa FWL: solo relación salario–edad
modelo_fwl <- lm(resid_y ~ resid_age + resid_age2, data = db_fwl)

summary(modelo_fwl)

##
ggplot(db_fwl, aes(x = resid_age, y = resid_y)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "blue") +
  labs(
    x = "Edad (residualizada)",
    y = "Log salario (residualizado)",
    title = "Relación parcial edad–salario (FWL)"
  ) +
  theme_minimal()


