library(tidyverse)
library(knitr)
library(kableExtra)

# Leer el archivo CSV
datos <- read_delim("GOB-INT-SanJuan.csv", delim = ";")

# Calcular los totales de votos por ETIQUETA_ZONA, ETIQUETA_BLOQUE y VOTACION
tabla_votos <- datos %>%
 group_by(ETIQUETA_ZONA, ETIQUETA_BLOQUE) %>%
 summarise(GOB = sum(VOTOS[VOTACION == "GOB"]), INT = sum(VOTOS[VOTACION == "INT"])) %>%
 ungroup()

# Agregar filas con los totales de GOB e INT por ETIQUETA_ZONA
tabla_totales <- tabla_votos %>%
 group_by(ETIQUETA_ZONA) %>%
 summarise(GOB = sum(GOB), INT = sum(INT)) %>%
 mutate(ETIQUETA_BLOQUE="TOTAL")

 tabla_final<-bind_rows(tabla_votos,tabla_totales) %>% arrange(ETIQUETA_ZONA) %>%
  filter(ETIQUETA_BLOQUE %in% c("San Juan por Todos", "Unidos por San Juan", "TOTAL")) %>%
  mutate(GOBvsINT=GOB-INT)

 GOBVSINT <- tabla_final %>%
  group_by(ETIQUETA_BLOQUE) %>%
 filter(ETIQUETA_BLOQUE %in% c("San Juan por Todos", "Unidos por San Juan")) %>%
 summarize(GOBvsINT=sum(GOBvsINT))

 # Convertir la tabla filtrada a formato HTML
 tabla_html <- tabla_final %>%
  kable(format = "html")

 # Imprimir la tabla HTML
 tabla_print<-tabla_final
 tabla_print$ETIQUETA_BLOQUE = cell_spec(tabla_print$ETIQUETA_BLOQUE, background = ifelse(tabla_print$ETIQUETA_BLOQUE == "San Juan por Todos", "cyan", ifelse(tabla_print$ETIQUETA_BLOQUE == "Unidos por San Juan", "yellow", "none")))
 tabla_print$GOBvsINT = cell_spec(tabla_print$GOBvsINT, color = ifelse(tabla_print$GOBvsINT < 0, "red", "blue"))
 htmltable<-kbl(tabla_print, escape = F) %>% kableExtra::kable_paper(full_width = F)
 save_kable(htmltable,file="tabla_por_jurisdiccion.html")

 GOBVSINT$ETIQUETA_BLOQUE = cell_spec(GOBVSINT$ETIQUETA_BLOQUE, background = ifelse(GOBVSINT$ETIQUETA_BLOQUE == "San Juan por Todos", "cyan", ifelse(GOBVSINT$ETIQUETA_BLOQUE == "Unidos por San Juan", "yellow", "none")))
 GOBVSINT$GOBvsINT = cell_spec(GOBVSINT$GOBvsINT, color = ifelse(GOBVSINT$GOBvsINT < 0, "red", "blue"))
 TOTALHTML<-kbl(GOBVSINT, escape = F) %>% kableExtra::kable_paper(full_width = F)
 save_kable(TOTALHTML,file="totales.html")

