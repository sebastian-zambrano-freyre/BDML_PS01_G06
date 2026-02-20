# Vamos a predecir los perfiles de salario y edad con los modelos 1 y 2
db$predicciones1 <- predict(m1)

coef_m2 <- coef(m2)
#coef_fwl <- coef(m2_fwl)

db <- db %>%
  group_by() %>%
  mutate(
    mean_hours_age = mean(totalHoursWorked, na.rm = TRUE),
    mean_relab_age = mean(relab, na.rm = TRUE)
  ) %>%
  ungroup()

db$predicciones2 <- coef_m2["(Intercept)"] +
  coef_m2["age"] * db$age +
  coef_m2["age2"] * db$age2 +
  coef_m2["totalHoursWorked"] * db$mean_hours_age +
  coef_m2["relab"] * db$mean_relab_age