##errores estandar analiticos

se_an_m4 <- se(modelo4_fwl)["x_tilde"]
se_an_m4

##errores estandar bootstrap

boot_fn_m4 <- function(data, indices) {
  
  d <- data[indices, ]   # remuestreo
  
  model <- feols(
    log_salary ~ female +
      age + age2 +
      college +
      hoursWorkUsual +
      formal +
      sizeFirm +
      cuentaPropia +
      femage +
      femage2
    | oficio,
    data = d
  )
  
  coef(model)["female"]
}

set.seed(777)

boot_results_m4 <- boot(
  data = db,
  statistic = boot_fn_m4,
  R = 500   #Inicialmente usemos 500, si todo corre bien lo podemos subir a 1000 como sugiere la literatura
)

se_boot_m4 <- sd(boot_results_m4$t)
se_boot_m4

boot.ci(boot_results_m4, type = "perc")
