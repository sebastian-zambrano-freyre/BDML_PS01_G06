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
ggplot(edad_stats,
       aes(x=grupo_edad,y=error))+
  geom_col()+
  labs(title="Error de predicción por grupo de edad",
       x="Grupo de edad",
       y="Error LOOCV promedio")+
  theme_minimal()

ggplot(edad_stats,
       aes(x=grupo_edad,y=influencia))+
  geom_col()+
  labs(title="Influencia por grupo de edad",
       x="Grupo de edad",
       y="Influencia promedio")+
  theme_minimal()

# Genero
ggplot(genero_stats,
       aes(x=genero,y=error))+
  geom_col()+
  labs(title="Error de predicción por género",
       x="Género",
       y="Error LOOCV promedio")+
  theme_minimal()

ggplot(genero_stats,
       aes(x=genero,y=influencia))+
  geom_col()+
  labs(title="Influencia por género",
       x="Género",
       y="Influencia promedio")+
  theme_minimal()

# Formalidad
ggplot(formal_stats,
       aes(x=formalidad,y=error))+
  geom_col()+
  labs(title="Error de predicción por formalidad",
       x="Formalidad",
       y="Error LOOCV promedio")+
  theme_minimal()

ggplot(formal_stats,
       aes(x=formalidad,y=influencia))+
  geom_col()+
  labs(title="Influencia por formalidad",
       x="Formalidad",
       y="Influencia promedio")+
  theme_minimal()

# Quintiles de ingreso
ggplot(ingreso_stats,
       aes(x=quintil_ingreso,y=error))+
  geom_col()+
  labs(title="Error de predicción por quintil de ingreso",
       x="Quintil ingreso",
       y="Error LOOCV promedio")+
  theme_minimal()

ggplot(ingreso_stats,
       aes(x=quintil_ingreso,y=influencia))+
  geom_col()+
  labs(title="Influencia por quintil de ingreso",
       x="Quintil ingreso",
       y="Influencia promedio")+
  theme_minimal()

# Sesgo VS Error para grupos de edad
ggplot(edad_stats,
       aes(x=error,y=sesgo,label=grupo_edad))+
  geom_point(size=4)+
  geom_text(nudge_y=0.005)+
  labs(
    x="Error promedio",
    y="Sesgo promedio"
  )

# Sesgo VS Error para quintiles de ingreso
ggplot(ingreso_stats,
       aes(x=error,y=sesgo,label=quintil_ingreso))+
  geom_point(size=4)+
  geom_text(nudge_y=0.1)+
  labs(
    x="Error promedio",
    y="Sesgo promedio"
  )