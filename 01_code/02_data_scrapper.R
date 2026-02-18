###Loop para importar todas las paginas de la base de datos
pages <- 1:10

lista_tablas <- lapply(pages, function(i) {
  
  url <- paste0(
    "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",
    i,
    ".html"
  )
  
  tabla <- read_html(url) %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  # Eliminar la primera columna (que enumera las observaciones para cada pagina html)
  tabla <- tabla[, names(tabla) != ""]
  
  tabla
})

db <- bind_rows(lista_tablas)

###
skim(db) %>% head()

summary(db$age)


#Analisis de edad y horas trabajadas
db_menores <- db %>% filter(age<19, age>13)
summary(db_menores$totalHoursWorked)

g1 <- ggplot(data=db_menores, aes(x = age, y = totalHoursWorked)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal()
g1

#Primer filtro de edad
db <- db %>% filter(age>18)

#Analisis de situacion de empleo
summary(db$ocu)
summary(db$dsi)
g2 <- ggplot(data=db, aes(x = age, y = ocu)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal()
g2

#Relacion entre "ocu y variable "dsi"
summary(db$dsi)
g3 <- ggplot(data=db, aes(x = dsi, y = ocu)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal()
g3

#Filtro por condicion de ocupacion
db <- db %>% filter(pea==1)

g4 <- ggplot(data=db, aes(x = dsi, y = ocu)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal()
g4

summary(db$pet)
summary(db$wap)

ggplot(data=db, mapping = aes(x = age, y = y_total_m_ha)) + geom_point(col = "red", size = 0.5)
ggplot(data=db, mapping = aes(x = age, y = totalHoursWorked)) + geom_point(col = "red", size = 0.5)

db_miss <- skim(db) %>% select( skim_variable, n_missing)
Nobs <- nrow(db) 
Nobs
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
head(db_miss)
db_miss <- db_miss %>% arrange(-n_missing)
db_miss<- db_miss %>% filter(n_missing!= 0)
#db_miss<- db_miss %>% filter(p_missing < 0.99)
head(db_miss, 20)

#Seleccion de variables
skim(db) %>% head() 
summary(db$orden)
# Las siguientes variables están vacías, además se descartan porque están relacionadas a individuos que no se encuentran ocupados:
db <- db[, !names(db) %in% c("p7350", "p7422", "p7422s1", "p7472", "p7472s1", "p7310", "ina", "inac", "imdi", "imdies", "cclasnr5")]

ggplot(data=db, mapping = aes(x = oficio, y = y_total_m_ha)) + geom_point(col = "red", size = 0.5)

base_final <- db

write.csv(base_final, "02_outputs/tables/db_preliminar.csv", row.names = FALSE)