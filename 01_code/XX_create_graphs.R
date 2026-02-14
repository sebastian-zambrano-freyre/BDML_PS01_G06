data <- data.frame(
  edad = c(22, 25, 28, 30, 35, 40, 45, 50, 55, 60),
  salario = c(25, 28, 32, 35, 50, 55, 58, 60, 57, 42)
)

write.csv(data,
          "02_outputs/tables/tabla_salario_edad.csv",
          row.names = FALSE)

write.csv(data,
          "03_temp/tabla_salario_edad.csv",
          row.names = FALSE)

png("02_outputs/figures/grafico_salario_edad.png",
    width = 800, height = 600)

plot(data$edad, data$salario,
     main = "Relación entre Edad y Salario",
     xlab = "Edad",
     ylab = "Salario (miles de $)",
     pch = 19,
     col = "blue")

modelo <- lm(salario ~ edad, data = data)

abline(modelo, col = "red", lwd = 2)

dev.off()