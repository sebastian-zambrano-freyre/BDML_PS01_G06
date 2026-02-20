################Tabla
extra_rows <- data.frame(
  term = "Edad pico con Bootstrap",
  
  `Modelo 1` = sprintf("%.2f [%.2f , %.2f]",
                       edad_pico_m1,
                       ci_m1[1],
                       ci_m1[2]),
  
  `Modelo 2` = sprintf("%.2f [%.2f , %.2f]",
                       edad_pico_m2,
                       ci_m2[1],
                       ci_m2[2])
)

tabla_1 <- modelsummary(
  list("Modelo 1" = m1,
       "Modelo 2" = m2),
  
  stars = c('*' = .1, '**' = .05, '***' = .01),
  
  gof_map = c("nobs", "r.squared"),
  
  add_rows = extra_rows,
  
  output = "gt"
)

tabla_1 <- tabla_1 |>
  tab_header(
    title = "Resultados de regresión del perfil de ingresos por edad",
    subtitle = "Edad pico por Bootstrap para ambos modelos"
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_source_note(
    source_note = "Errores estándar entre paréntesis. Intervalos Bootstrap al 95%"
  )

tabla_1

gtsave(tabla_1,
       "02_outputs/tables/tabla_1.png",
       dpi = 300
       )

gtsave(tabla_1, "02_outputs/tables/tabla_1.pdf")

# Gráfico

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