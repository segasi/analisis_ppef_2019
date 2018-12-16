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

# PEF 2018. Fuente: https://www.transparenciapresupuestaria.gob.mx
pef_18 <- read_csv("01_datos/presupuesto_mexico__2018.csv", locale = locale(encoding = "latin1"))

# PPEF 2019 completo
ppef_19 <- read_csv("01_datos/PPEF_2019.csv", locale = locale(encoding = "latin1"))

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
                            TRUE ~ ramo))


### Gráfica: Presupuesto de cada ramo de acuerdo con PPEF 2019 y el PEF de 2018 ----
bd_ramo %>% 
  mutate(tipo = str_wrap(str_to_upper(tipo), width = 30),
         ramo_etiqueta = ifelse(valor > 57000 & año == 2019, str_wrap(ramo_acronimo, width = 20), ""),
         var_real_etiqueta = ifelse(var_real > 20 | var_real < -600, paste("\n(", var_real, "%)", sep = ""), "")) %>% 
  ggplot(aes(año, valor, color = color_lineas)) +
  geom_line(aes(group = ramo), size = 1, alpha = 0.7) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = paste(ramo_etiqueta,
                                    # var_real_etiqueta,
                                    sep = "")), 
                  fontface = "bold", 
                  nudge_x = 0.2,
                  nudge_y = 2,
                  force = 3,
                  color = "grey40") +
  scale_x_continuous(limits = c(2017.9, 2019.4), breaks = c(2018, 2019)) +
  scale_y_continuous(breaks = seq(0, 800000, 100000), label = comma) +
  scale_color_manual(values = c("#74c476", "tomato")) +
  facet_wrap(~ tipo, ncol = 4) +
  labs(title = str_wrap(str_to_upper("presupuesto de cada ramo de acuerdo con PPEF 2019 y PEF 2018"), width = 70), 
       subtitle = "Cifras en términos reales y millones de pesos",
       x = NULL, 
       y = "Millones de pesos\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: bit.ly/PPEF2019") +
  tema +
  theme(strip.background = element_rect(color = "grey70", fill = "grey70"),
        strip.text = element_text(color = "white", size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

ggsave(filename = "presupuesto_ramos_2018_2019.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)  


### Gráfica: Presupuesto de los ramos del Poder Ejecutivo de acuerdo con PPEF 2019 y el PEF de 2018 ----
bd_ramo %>% 
  filter(tipo == "Poder Ejecutivo") %>% 
  mutate(tipo = str_wrap(str_to_upper(tipo), width = 30),
         ramo_etiqueta = ifelse(valor > 40000 & año == 2019, str_wrap(ramo_acronimo, width = 20), ""),
         var_real_etiqueta = ifelse(var_real > 20 | var_real < -600, paste("\n(", var_real, "%)", sep = ""), "")) %>% 
  ggplot(aes(año, valor, color = color_lineas)) +
  geom_line(aes(group = ramo), size = 1, alpha = 0.7) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = paste(ramo_etiqueta,
                                    # var_real_etiqueta,
                                    sep = "")), 
                  fontface = "bold", 
                  nudge_x = 0.1,
                  nudge_y = 2,
                  force = 1,
                  color = "grey40") +
  scale_x_continuous(limits = c(2018, 2019.01), breaks = c(2018, 2019)) +
  scale_y_continuous(breaks = seq(0, 800000, 100000), label = comma) +
  scale_color_manual(values = c("#74c476", "tomato")) +
  labs(title = str_wrap(str_to_upper("presupuesto de los ramos del poder ejecutivo de acuerdo con PPEF 2019 y PEF 2018"), width = 70), 
       subtitle = "Cifras en términos reales y millones de pesos",
       x = NULL, 
       y = "Millones de pesos\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: bit.ly/PPEF2019") +
  tema +
  theme(strip.background = element_rect(color = "grey70", fill = "grey70"),
        strip.text = element_text(color = "white", size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

ggsave(filename = "presupuesto_ramos_poder_ejecutivo_2018_2019.png", path = "03_graficas/", width = 15, height = 10, dpi = 100) 


### Gráfica: Presupuesto de los ramos de los Poder y órganos autónomos de acuerdo con PPEF 2019 y el PEF de 2018 ----
bd_ramo %>% 
  filter(tipo == "Poderes y órganos autónomos") %>% 
  mutate(tipo = str_wrap(str_to_upper(tipo), width = 30),
         ramo_etiqueta = ifelse(valor > 5000 & año == 2019, str_wrap(ramo_acronimo, width = 20), ""),
         var_real_etiqueta = ifelse(var_real > 20 | var_real < -600, paste("\n(", var_real, "%)", sep = ""), "")) %>% 
  ggplot(aes(año, valor, color = color_lineas)) +
  geom_line(aes(group = ramo), size = 1, alpha = 0.7) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = paste(ramo_etiqueta,
                                    # var_real_etiqueta,
                                    sep = "")), 
                  fontface = "bold", 
                  nudge_x = 0.2,
                  nudge_y = 2,
                  force = 1,
                  color = "grey40") +
  scale_x_continuous(limits = c(2018, 2019.01), breaks = c(2018, 2019)) +
  scale_y_continuous(breaks = seq(0, 80000, 5000), label = comma) +
  scale_color_manual(values = c("#74c476", "tomato")) +
  labs(title = str_wrap(str_to_upper("presupuesto de los ramos de los poderes y órganos autónomos de acuerdo con PPEF 2019 y PEF 2018"), width = 70), 
       subtitle = "Cifras en términos reales y millones de pesos",
       x = NULL, 
       y = "Millones de pesos\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: bit.ly/PPEF2019") +
  tema +
  theme(strip.background = element_rect(color = "grey70", fill = "grey70"),
        strip.text = element_text(color = "white", size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

ggsave(filename = "presupuesto_ramos_poderes_org_autonomos_2018_2019.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


### Gráfica: Variación % del presupuesto de los ramos, PPEF 2019 vs. PEF 2018 ----
bd_ramo %>% 
  mutate(tipo = str_wrap(str_to_upper(tipo), width = 30),
         ramo_acronimo = ifelse(str_detect(ramo, "Estadística"), "INEGI", ramo),
         ramo_etiqueta = ifelse(var_real > 20, str_wrap(ramo_acronimo, width = 30), ""),
         var_real_etiqueta = ifelse(var_real > 20 | var_real < -600, paste("\n(", var_real, "%)", sep = ""), "")) %>% 
  ggplot(aes(año, var_real)) +
  geom_line(aes(group = ramo, color = color_lineas), size = 1, alpha = 0.7) +
  geom_point(aes(color = color_lineas), size = 3) +
  geom_text_repel(aes(label = paste(ramo_etiqueta, var_real_etiqueta, sep = "")), 
                  fontface = "bold", 
                  nudge_x = -0.2,
                  nudge_y = 0.2, 
                  force = 2) +
  scale_x_continuous(limits = c(2017.9, 2019.1), breaks = c(2018, 2019)) +
  scale_y_continuous(limits = c(-50, 1020), breaks = c(0, seq(-100, 1100, 100)), label = comma) +
  scale_color_manual(values = c("#74c476", "tomato")) +
  facet_wrap(~ tipo, ncol = 4) +
  labs(title = str_wrap(str_to_upper("variación % del presupuesto de los ramos, PPEF 2019 vs. PEF 2018"), width = 70), 
       subtitle = "Variación es en términos reales",
       x = NULL, 
       y = "Variación porcentual\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: bit.ly/PPEF2019") +
  tema +
  theme(strip.background = element_rect(color = "grey70", fill = "grey70"),
        strip.text = element_text(color = "white", size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

ggsave(filename = "variacion_ramos.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)  


### Gráfica: Presupuesto clasificación funcional de acuerdo con PPEF 2019 y el PEF de 2018 ----
bd_funcion %>% 
  # count(funcion) %>% print(n = Inf)
  mutate(etiqueta = ifelse(funcion == "Recreación, Cultura y Otras Manifestaciones Sociales" & año == 2019, "Recreación y Cultura", funcion)) %>% 
  mutate(etiqueta = ifelse(valor > 100000 & año == 2019 | funcion == "Ciencia, Tecnología e Innovación" & año == 2019 | etiqueta == "Recreación y Cultura" & año == 2019 , str_wrap(etiqueta, width = 25), ""),
         tipo = str_wrap(str_to_upper(tipo), width = 30)) %>%
  ggplot(aes(año, valor)) +
  geom_line(aes(group = funcion, color = color_lineas), size = 2, alpha = 0.7) +
  geom_point(aes(color = color_lineas), size = 3) +
  geom_text_repel(aes(label = etiqueta), fontface = "bold", nudge_y = 2, force = 2, color = "grey30") +
  scale_x_continuous(limits = c(2017.9, 2019.1), breaks = c(2018, 2019)) +
  scale_y_continuous(breaks = seq(0, 2000000, 250000), label = comma) +
  scale_color_manual(values = c("#74c476", "tomato")) +
  facet_wrap(~ tipo, ncol = 4) +
  labs(title = str_wrap(str_to_upper("presupuesto PPEF 2019 vs. PEF 2018, clasificación funcional"), width = 70), 
       subtitle = "Cifras en términos reales y millones de pesos",
       x = NULL, 
       y = "Millones de pesos\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: bit.ly/PPEF2019") +
  tema +
  theme(strip.background = element_rect(color = "grey70", fill = "grey70"),
        strip.text = element_text(color = "white", size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "none")


ggsave(filename = "presupuesto_funcional_2018_2019.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)  

