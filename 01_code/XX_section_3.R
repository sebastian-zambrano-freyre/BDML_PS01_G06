#Separemos las bases en train y test
db_train <- bind_rows(lista_tablas[1:7])
db_test <- bind_rows(lista_tablas[8:10])

db_train <- db_train %>% filter(age>=18,
                        ocu == 1,                 # Solo ocupados
                        y_salary_m > 0,           # Salario positivo
                        !is.na(sex))

db_test <- db_test %>% filter(age>=18,
                                ocu == 1,                 # Solo ocupados
                                y_salary_m > 0,           # Salario positivo
                                !is.na(sex))

db_train <- db_train %>% mutate(
  log_salary = log(y_salary_m),
  female = 1 - sex,
  age2 = age^2
)

db_test <- db_test %>% mutate(
  log_salary = log(y_salary_m),
  female = 1 - sex,
  age2 = age^2
)

set.seed(777)

modelo_1 <- log_salary ~ age + age2
m1 <- lm(modelo_1,
         data = db_train)

predictions <- predict(object=m1, newdata=db_test)
score1 <- RMSE(pred = predictions, obs = db_test$log_salary)
score1

modelo_2 <- log_salary ~ age + age2 + totalHoursWorked + relab
m2 <- lm(modelo_2,
         data = db_train)

predictions <- predict(object=m2, newdata=db_test)
score2 <- RMSE(pred = predictions, obs = db_test$log_salary)
score2

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

modelo_3 <- log_salary ~ female
m3 <- lm(modelo_3,
         data = db_train)

predictions <- predict(object=m3, newdata=db_test)
score3 <- RMSE(pred = predictions, obs = db_test$log_salary)
score3

modelo_4 <- log_salary ~ female + age + age2 + college + hoursWorkUsual + formal + sizeFirm + oficio
m4 <- lm(modelo_4,
         data = db_train)

predictions <- predict(object=m4, newdata=db_test)
score4 <- RMSE(pred = predictions, obs = db_test$log_salary)
score4

score1
score2
score3
score4
