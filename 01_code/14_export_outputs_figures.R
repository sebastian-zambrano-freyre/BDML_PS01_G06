# Gráfico Sección 1

ggplot(db, aes(x = age)) +
  geom_line(aes(y = predicciones1, color = "Modelo 1"), linewidth = 1.2) +  # Predicción 1
  geom_line(aes(y = predicciones2, color = "Modelo 2"), linewidth = 1.2) +  # Predicción 2.1
  #geom_smooth(aes(y = predicciones2), method = "lm", formula = y ~ x + I(x^2), color = "green") +
  scale_color_manual(
    values = c("Modelo 1" = "#1F4E79",
               "Modelo 2" = "#8B0000")
  ) +
  labs(title = "Perfil Edad - Ingreso con 2 modelos",
       x = "Edad",
       y = "Log del salario",
       color = NULL
  ) +
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )