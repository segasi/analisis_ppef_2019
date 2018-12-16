### Paquetes ----
library(pacman)
p_load(ggrepel, scales, readxl, tidyverse, treemapify)

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

### Datos ----

# Los datos los obtuve de las secciones ¿PARA QUÉ SE GASTA? y ¿QUIÉN GASTA? en esta liga: https://www.transparenciapresupuestaria.gob.mx/es/PTP/infografia_ppef2019#page3

# Datos de ¿Para qué se gasta?
bd_para_que <- read_xlsx("01_datos/bd_para_que_se_gasta.xlsx")

# Datos de ¿Quién gasta?
bd_quien <- read_xlsx("01_datos/bd_quien_gasta.xlsx")


