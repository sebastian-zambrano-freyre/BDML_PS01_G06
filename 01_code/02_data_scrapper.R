#Loop para importar todas las paginas de la base de datos
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
  
  # Etiquetar los chunks para cuando se quiera hacer en validation set approach
  tabla <- tabla %>% mutate(
    chunk = paste0(i)
    )
  
  tabla
})

db <- bind_rows(lista_tablas)