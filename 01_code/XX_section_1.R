##Tomamos la base ya limpia y armada
db <- bind_rows(lista_tablas)
db <- db %>% filter(age>=18,
                    ocu == 1,                 # Solo ocupados
                    y_salary_m > 0,           # Salario positivo
                    !is.na(sex))

#Agregar logaritmo del salario
db_1 <- db %>% mutate(
  log_salary = log(y_salary_m),
  age2 = age^2
)

#Modelo 1
m1 <- lm(
  log_salary ~ age + age2,
  data = db_1
)

#summary(m1)
stargazer(m1, type = "text")


db_1$predicciones <- predict(m1, newdata = db_1)

ggplot(db_1, aes(x = age)) +
  geom_point(aes(y = log_salary), color = "black") +          # Datos reales
  geom_line(aes(y = predicciones), color = "blue", linewidth = 1) +  # Predicción
  labs(title = "Datos reales vs Predicciones del modelo",
       x = "age",
       y = "log_salary") +
  theme_minimal()

#Modelo 2
m2 <- lm(
  log_salary ~ age + age2,
  data = db_1
)

#summary(m2)
stargazer(m2, type = "text") 
