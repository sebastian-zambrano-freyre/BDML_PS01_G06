# En este script vamos a definir todos los modelos a usar y guardarlos como modelos

# Modelo 1
modelo_1 <- log_salary ~ age + age2
m1 <- lm(
  log_salary ~ age + age2,
  data = db
)

# Modelo 2
modelo_2 <- log_salary ~ age + age2 + totalHoursWorked + relab
m2 <- lm(
  log_salary ~ age + age2 +
    totalHoursWorked + relab,
  data = db
)

# Modelo 2 hecho con FWL
reg_y <- lm(log_salary ~ totalHoursWorked + relab, data = db)
db$y_tilde <- resid(reg_y)

reg_age <- lm(age ~ totalHoursWorked + relab, data = db)
db$age_tilde <- resid(reg_age)

reg_age2 <- lm(age2 ~ totalHoursWorked + relab, data = db)
db$age2_tilde <- resid(reg_age2)

m2_fwl <- lm(y_tilde ~ age_tilde + age2_tilde - 1, data = db)

# Modelo 3 (version section 3)
modelo_3 <- log_salary ~ female
m3 <- lm(
  log_salary ~ female,
  data = db
)

# Modelo 4 (version section 3)
modelo_4 <- log_salary ~ female + age + age2 + college + hoursWorkUsual + formal + sizeFirm + oficio
m4 <- lm(
  log_salary ~ female +
    age + age2 +
    college +
    hoursWorkUsual +
    formal +
    sizeFirm +
    oficio,
  data = db
)

##Modelo 3 (section 2)
m3.1 <- feols(
  log_salary ~ female,
  data = db
)

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
  data = db
)
summary(m4.1)

# Modelo 4 hecho con FWL

res_y <- feols(
  log_salary ~ age + age2 + college +
    hoursWorkUsual + formal + sizeFirm +
    femage + femage2 | oficio,
  data = db
)

res_female <- feols(
  female ~ age + age2 + college +
    hoursWorkUsual + formal + sizeFirm +
    femage + femage2 | oficio,
  data = db
)

y_tilde <- resid(res_y)
x_tilde <- resid(res_female)

modelo4_fwl <- lm(y_tilde ~ x_tilde)

# Modelos del 5 al 12
modelo_5 <-  log_salary ~ female + age + age2 + college + femage + hoursWorkUsual + formal + sizeFirm + oficio
modelo_6 <-  log_salary ~ female + age + age2 + college + femage + femage2 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_7 <-  log_salary ~ female + age + age2 + college + femage + p6050 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_8 <-  log_salary ~ female + age + age2 + college + femage + femage2 + p6050 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_9 <-  log_salary ~ female + age + age2 + college + femage + femage2 + p7040 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_10 <- log_salary ~ female + age + age2 + college + p6050 + p7040 + hoursWorkUsual + formal + sizeFirm + oficio
modelo_11 <- log_salary ~ female + age + age2 + college + p6050 + p7040 + femage + hoursWorkUsual + formal + sizeFirm + oficio
modelo_12 <- log_salary ~ female + age + age2 + college + p6050 + p7040 + femage + femage2 + hoursWorkUsual + formal + sizeFirm + oficio



