# Primer filtro de edad
summary(db$age)
db <- db %>% filter(age>=18) ## Se descartan 7,609 observaciones

# Filtro por condicion de ocupacion
summary(db$ocu)
db <- db %>% filter(ocu==1) ## Se descartan 8,026 observaciones

# Filtro por salario mayor a cero. Se decide no usar las observaciones sin salario
# ni imputar promedios porque el objetivo es predecir e imputar nos quita poder
summary(db$y_salary_m)
db <- db %>% filter(y_salary_m > 0) ## Se descartan 6,650 observaciones (eran NA)

# Filtro por registro de género
summary(db$sex)
db <- db %>% filter(!is.na(sex)) ## No se descartan observaciones

# Nos quedamos con 9,892 para toda la base

# Análisis de variables con muchos missing values -> la mayoría tiene relación con el desempleo
db_miss <- skim(db) %>% select( skim_variable, n_missing)
Nobs <- nrow(db) 
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
db_miss <- db_miss %>% arrange(-n_missing)
db_miss<- db_miss %>% filter(n_missing!= 0)
head(db_miss, 20)

# Las siguientes variables están vacías, además se descartan porque están relacionadas a individuos que no se encuentran ocupados:
db <- db[, !names(db) %in% c("p7350", "p7422", "p7422s1", "p7472", "p7472s1", "p7310", "ina", "inac", "imdi", "imdies", "cclasnr5")]
rm(db_miss)
rm(lista_tablas)

# Creación de variables útiles en las siguientes secciones

db <- db %>% mutate(
  log_salary = log(y_salary_m), # Agregar logaritmo del salario
  age2 = age^2,                 # Agregar edad al cuadrado
  female = 1 - sex,             # 1 = mujer
  femage = female * age,        # Interacción de género con edad
  femage2 = female * age2       # Interacción de género con edad al cuadrado
)

# Finalmente, nos quedamos solo con las variables de utilidad

db <- db %>% select(log_salary,
                    age,
                    age2,
                    female,
                    femage,
                    femage2,
                    totalHoursWorked,
                    relab,
                    college,
                    hoursWorkUsual,
                    formal,
                    sizeFirm,
                    cuentaPropia,
                    oficio,
                    p6050,
                    p7040
                    )