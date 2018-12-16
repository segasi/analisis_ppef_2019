### Paquetes ----
library(pacman)
p_load(ggrepel, janitor, scales, readxl, tidyverse, treemapify)

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))

### Datos ----

# Los datos los obtuve de las secciones ¿PARA QUÉ SE GASTA? y ¿QUIÉN GASTA? en esta liga: https://www.transparenciapresupuestaria.gob.mx/es/PTP/infografia_ppef2019#page3

# PPEF 2019 completo
ppef <- read_csv("01_datos/PPEF_2019.csv", locale = locale(encoding = "latin1"))

# Datos de ¿Para qué se gasta?
bd_para_que <- read_xlsx("01_datos/bd_para_que_se_gasta.xlsx")

# Datos de ¿Quién gasta?
bd_quien <- read_xlsx("01_datos/bd_quien_gasta.xlsx")



### Transformar datos ----

bd_funcion <- bd_para_que %>% 
  mutate(var_real = var_real*100,
         color_lineas = ifelse(var_real >= 0, "aumento", "disminuyo")) %>%
  gather(key = fuente, 
         value = valor,
         -c(funcion, tipo, var_real, color_lineas)) %>% 
  mutate(año = ifelse(fuente == "proyecto_2019", 2019, 2018),
         var_real = ifelse(año == 2018, 0, var_real)) 

bd_ramo <- 
  bd_quien %>% 
  mutate(var_real = var_real*100,
         color_lineas = ifelse(var_real >= 0, "aumento", "disminuyo")) %>%
  gather(key = fuente, 
         value = valor,
         -c(ramo, tipo, var_real, color_lineas)) %>% 
  mutate(año = ifelse(fuente == "proyecto_2019", 2019, 2018),
         var_real = ifelse(año == 2018, 0, var_real),
         ramo_acronimo = case_when(str_detect(ramo, "Estadística") ~ "INEGI", 
                            str_detect(ramo, "Seguro Social") ~ "IMSS", 
                            str_detect(ramo, "Petróleos") ~ "PEMEX",
                            str_detect(ramo, "Electricidad") ~ "CFE",
                            str_detect(ramo, "Servicios Sociales") ~ "ISSSTE",
                            str_detect(ramo, "Educación P") ~ "SEP",
                            str_detect(ramo, "Defensa") ~ "SEDENA",
                            str_detect(ramo, "Comunicaciones") ~ "SCT",
                            str_detect(ramo, "Gobernaci") ~ "SEGOB",
                            str_detect(ramo, "Poder Judicial") ~ "PJF",
                            str_detect(ramo, "Consejo de la Judi") ~ "CJF",
                            str_detect(ramo, "Agricultura y D") ~ "SADER",
                            str_detect(ramo, "Suprema Corte de Justicia de la Nación") ~ "SCJN",
                            str_detect(ramo, "Trabajo y Previsión Social") ~ "STPS",
                            str_detect(ramo, "Tribunal Electoral Federal") ~ "TEPJF",
                            str_detect(ramo, "Tribunal Federal de Justicia Administrativa") ~ "TFJA",
                            str_detect(ramo, "Oficina de la Presidencia de la República") ~ "Presidencia",
                            str_detect(ramo, "Instituto Nacional Electoral") ~ "INE",
                            str_detect(ramo, "Instituto Federal de Telecomunicaciones") ~ "IFT",
                            str_detect(ramo, "Instituto Nacional de Transparencia, Acceso") ~ "INAI",
                            str_detect(ramo, "Instituto Nacional para la Evaluación de") ~ "INEE",
                            str_detect(ramo, "Medio Ambiente y Recursos Naturales") ~ "SEMARNAT",
                            str_detect(ramo, "Relaciones Exteriores") ~ "SRE",
                            str_detect(ramo, "Hacienda y Crédito Público") ~ "SHCP",
                            TRUE ~ ramo)) %>% 
  count(ramo_acronimo) %>% 
  print(n = Inf)




