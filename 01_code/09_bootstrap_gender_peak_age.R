###Intervalos de confianza edades pico

calcular_picos <- function(data){
  
  modelo <- feols(
    log_salary ~ female +
      age + age2 +
      college +
      hoursWorkUsual +
      formal +
      sizeFirm +
      femage +
      femage2
    | oficio,
    data = data
  )
  
  b <- coef(modelo)
  
  pico_h <- - b["age"] / (2 * b["age2"])
  
  pico_m <- - (b["age"] + b["femage"]) /
    (2 * (b["age2"] + b["femage2"]))
  
  return(c(pico_h, pico_m))
}

##Cluster por oficio

set.seed(777)

B <- 500   # número de repeticiones
clusters <- unique(db_2$oficio)

resultados <- matrix(NA, nrow = B, ncol = 2)

for (b in 1:B){
  
  # remuestreo de clusters
  clusters_boot <- sample(clusters,
                          size = length(clusters),
                          replace = TRUE)
  
  # construir base bootstrap
  db_boot <- do.call(rbind,
                     lapply(clusters_boot,
                            function(cl){
                              db_2[db_2$oficio == cl, ]
                            }))
  
  resultados[b, ] <- calcular_picos(db_boot)
}

colnames(resultados) <- c("hombres", "mujeres")

##IC
IC_hombres  <- quantile(resultados[,1], c(0.025, 0.975))
IC_mujeres  <- quantile(resultados[,2], c(0.025, 0.975))

IC_hombres
IC_mujeres

##Media
mean(resultados[,1])
mean(resultados[,2])

###

# Data frame en formato largo para grafico

df_boot <- data.frame(
  edad_pico = c(resultados[,1], resultados[,2]),
  grupo = rep(c("Hombres", "Mujeres"),
              each = nrow(resultados))
)

# Medias
mean_h <- mean(resultados[,1])
mean_m <- mean(resultados[,2])

# IC
low_h <- IC_hombres[1]
up_h  <- IC_hombres[2]

low_m <- IC_mujeres[1]
up_m  <- IC_mujeres[2]
