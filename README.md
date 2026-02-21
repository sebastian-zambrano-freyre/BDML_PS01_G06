# BDML_PS01_G06
## Repositorio del Problem Set 01

Sebastian Zambrano

Juan David Bonilla

Big Data y Machine Learning para Economía Aplicada – Universidad de los Andes – 2026

Para replicar todos los resultados correr de la siguiente manera:

Desde una sesión de R: source("01_code/00_rundirectory.R") o desde una línea de comando: R CMD BATCH 01_code/00_rundirectory.R

Descripción de cada script:
# Paso 01: Instalar y llamar a los paquetes necesarios
source("01_code/01_install_packages.R")

# Paso 02: Descargar el dataset con webscrapping
source("01_code/02_data_scrapper.R")

# Paso 03: Limpiar base y construir nuevas variables necesarias
source("01_code/03_db_cleaning_and_construction.R")

# Paso 04: Definir todos los modelos que usaremos en el PS01
source("01_code/04_define_models.R")

# Paso 05: Encontrar edades pico - S01
source("01_code/05_bootstrap_peak_age.R")

# Paso 06: Definir perfil salario - edad en general - S01
source("01_code/06_salary_age_profile.R")

# Paso 07: Estimar los errores estándar - S02
source("01_code/07_standard_errors.R")

# Paso 08: Definir perfil salario - edad por género - S02
source("01_code/08_salary_age_gender_profile.R")

# Paso 09: Encontrar edades pico por género - S02
source("01_code/09_bootstrap_gender_peak_age.R")

# Paso 10: Validation set approach -> RMSE - S03
source("01_code/10_validation_sample_rmse_scores.R")

# Paso 11: Leave-One-Out-Cross-Validation + FWL - S03
source("01_code/11_loocv.R")

# Paso 12: Examinar dificultad para predecir y la influencia - S03
source("01_code/12_prediction_failure_and_obs_influence.R")

# Paso 13: Exportar tablas
source("01_code/13_export_outputs_tables.R")

# Paso 14: Exportar gráficos
source("01_code/14_export_outputs_figures.R")
