# Gráfico del perfil de Edad - Ingreso

figure_1 <- ggplot(db, aes(x = age)) +
  geom_line(aes(y = predicciones1, color = "Modelo 1, Incondicional"), linewidth = 1.2) +  # Predicción 1
  geom_line(aes(y = predicciones2, color = "Modelo 2, Condicional"), linewidth = 1.2) +  # Predicción 2
  scale_color_manual(
    values = c("Modelo 1, Incondicional" = "#1F4E79",
               "Modelo 2, Condicional" = "#8B0000")
  ) +
  labs(title = "Perfil Edad - Ingreso",
       x = "Edad",
       y = "Log del salario",
       color = NULL
  ) +
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "02_outputs/figures/grafico_perfil_edad_ingreso.png",
  plot = figure_1,
  width = 8,
  height = 6,
  dpi = 300
)

# Análisis de error LOOCV y Validation Set para el modelo elegido
df_error <- data.frame(
  Metodo = c("Error con Grupo de Validación", "Error con LOOCV"),
  RMSE = c(best_score, rmse_loocv)
)

grafico_error <- ggplot(df_error, aes(x = Metodo, y = RMSE, fill = Metodo)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = sprintf("%.6f", RMSE)),
    vjust = -0.6,
    size = 5
  ) +
  coord_cartesian(ylim = c(0.4, max(df_error$RMSE) + 0.02)) +
  labs(
    title = "Comparación de Error de Predicción",
    x = "",
    y = "RMSE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

ggsave(
  "02_outputs/figures/grafico_comparacion_rmse.png",
  plot = grafico_error,
  width = 7,
  height = 5,
  dpi = 300
)

# Analisis de error de predicción y de influencia de las observaciones, por diversos criterios
# Edad

# Gráfico 1: Error de predicción
g1 <- ggplot(edad_stats,
             aes(x = grupo_edad, y = error, fill = "Error LOOCV")) +
  geom_col(width = 0.6) +
  labs(
    title = "Error de predicción por grupo de edad",
    x = "Grupo de edad",
    y = "Error promedio",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Gráfico 2: Influencia
g2 <- ggplot(edad_stats,
             aes(x = grupo_edad, y = influencia, fill = "Influencia")) +
  geom_col(width = 0.6) +
  labs(
    title = "Influencia por grupo de edad",
    x = "Grupo de edad",
    y = "Influencia promedio",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

figure_edad <- g1 | g2

ggsave(
  "02_outputs/figures/grafico_edad.png",
  plot = figure_edad,
  width = 10,
  height = 5,
  dpi = 300
)

# Género
# Gráfico 1: Error de predicción
g1 <- ggplot(genero_stats,
             aes(x = genero, y = error, fill = "Error LOOCV")) +
  geom_col(width = 0.6) +
  labs(
    title = "Error de predicción por Género",
    x = "Género",
    y = "Error promedio",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Gráfico 2: Influencia
g2 <- ggplot(genero_stats,
             aes(x = genero, y = influencia, fill = "Influencia")) +
  geom_col(width = 0.6) +
  labs(
    title = "Influencia por Género",
    x = "Género",
    y = "Influencia promedio",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

figure_genero <- g1 | g2

ggsave(
  "02_outputs/figures/grafico_genero.png",
  plot = figure_genero,
  width = 10,
  height = 5,
  dpi = 300
)


# Formalidad
# Gráfico 1: Error de predicción
g1 <- ggplot(formal_stats,
             aes(x = formalidad, y = error, fill = "Error LOOCV")) +
  geom_col(width = 0.6) +
  labs(
    title = "Error de predicción por Formalidad",
    x = "Formalidad",
    y = "Error promedio",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Gráfico 2: Influencia
g2 <- ggplot(formal_stats,
             aes(x = formalidad, y = influencia, fill = "Influencia")) +
  geom_col(width = 0.6) +
  labs(
    title = "Influencia por Formalidad",
    x = "Formalidad",
    y = "Influencia promedio",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

figure_formalidad <- g1 | g2

ggsave(
  "02_outputs/figures/grafico_formalidad.png",
  plot = figure_formalidad,
  width = 10,
  height = 5,
  dpi = 300
)

# Quintiles de ingreso
# Gráfico 1: Error de predicción
g1 <- ggplot(ingreso_stats,
             aes(x = quintil_ingreso, y = error, fill = "Error LOOCV")) +
  geom_col(width = 0.6) +
  labs(
    title = "Error de predicción por Ingreso",
    x = "Quintil de Ingreso",
    y = "Error promedio",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Gráfico 2: Influencia
g2 <- ggplot(ingreso_stats,
             aes(x = quintil_ingreso, y = influencia, fill = "Influencia")) +
  geom_col(width = 0.6) +
  labs(
    title = "Influencia por Ingreso",
    x = "Quintil de Ingreso",
    y = "Influencia promedio",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

figure_ingreso <- g1 | g2

ggsave(
  "02_outputs/figures/grafico_ingreso.png",
  plot = figure_ingreso,
  width = 10,
  height = 5,
  dpi = 300
)

# Sesgo VS Error para grupos de edad
grafico_edad_bias <- ggplot(edad_stats,
                            aes(x = error, y = sesgo, label = grupo_edad)) +
  geom_point(size = 4, color = "#1F4E79") +
  geom_text(
    nudge_y = 0.003,
    size = 4
  ) +
  labs(
    title = "Sesgo vs Error por Grupo de Edad",
    x = "Error promedio",
    y = "Sesgo promedio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(
  "02_outputs/figures/sesgo_error_edad.png",
  plot = grafico_edad_bias,
  width = 7,
  height = 5,
  dpi = 300
)

# Sesgo VS Error para quintiles de ingreso
grafico_ingreso_bias <- ggplot(ingreso_stats,
                               aes(x = error, y = sesgo, label = quintil_ingreso)) +
  geom_point(size = 4, color = "#1F4E79") +
  geom_text(
    nudge_y = 0.05,
    size = 4
  ) +
  labs(
    title = "Sesgo vs Error por Quintil de Ingreso",
    x = "Error promedio",
    y = "Sesgo promedio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(
  "02_outputs/figures/sesgo_error_ingreso.png",
  plot = grafico_ingreso_bias,
  width = 7,
  height = 5,
  dpi = 300
)