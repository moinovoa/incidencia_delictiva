# Librerías ----
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(tseries)
library(forecast)
library(prophet)

# Creación base inicial ----
ruta <- "C:\\Users\\mois_\\OneDrive\\Servicio social\\Fuero_federal_2012-2026_feb2026.xlsx"
excel_sheets(ruta)
datos <-read_excel(ruta)
datos_narcotrafico = datos %>%
  filter(CONCEPTO == "CONTRA LA SALUD")
conteo_ordenado <- datos_narcotrafico %>% 
  count(TIPO, sort = TRUE)
datos_narcotrafico <- datos_narcotrafico %>%
  pivot_longer(
    cols = ENERO:DICIEMBRE, 
    names_to = "MES",      
    values_to = "CONTEO"    
  ) 
datos_narcotrafico <- datos_narcotrafico %>% mutate(
    MES_NUM = match(MES, c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", 
                           "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", 
                           "NOVIEMBRE", "DICIEMBRE")),
    FECHA = as.Date(paste(AÑO, MES_NUM, "01", sep = "-"))
  )
datos_narcotrafico <- datos_narcotrafico[, c("AÑO", "MES_NUM", "MES", "FECHA", 
                                             "INEGI", "ENTIDAD", "LEY", "CONCEPTO",
                                             "TIPO", "CONTEO")]

# Top 10 estados con más incidencia ----
top_estados <- datos_narcotrafico %>%
  group_by(ENTIDAD) %>% 
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE)) %>%
  arrange(desc(TOTAL)) %>%
  slice(1:10)

ggplot(top_estados, aes(x = reorder(ENTIDAD, TOTAL), y = TOTAL)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Estados: Delitos (narcotráfico)", 
       x = "Estado", y = "Número de delitos")

# Evolución anual total ----
evolucion_anual <- datos_narcotrafico %>%
  group_by(AÑO) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))

ggplot(evolucion_anual, aes(x = AÑO, y = TOTAL)) +
  geom_line(color = "red", size = 1) +
  geom_point() +
  labs(title = "Evolución Nacional de Delitos Federales (Narcotráfico)", 
       x = "Año", y = "Total de Incidencias")

# Estadísticas----
descriptivas <- datos_narcotrafico %>%
  group_by(ENTIDAD) %>%
  summarize(
    MEDIA_MENSUAL = mean(CONTEO, na.rm = TRUE),
    MEDIANA_MENSUAL = median(CONTEO, na.rm = TRUE),
    DESV_EST = sd(CONTEO, na.rm = TRUE),
    MAXIMO = max(CONTEO, na.rm = TRUE),
    TOTAL_HIST = sum(CONTEO, na.rm = TRUE)
  )
descriptivas

# Series de tiempo x estado ----
conteo_estado <- datos_narcotrafico %>%
  group_by(ENTIDAD) %>%
  summarize(SUMA = sum(CONTEO, na.rm = TRUE))

## Aguascalientes ----
aguascalientes <- datos_narcotrafico %>%
  filter(ENTIDAD == "AGUASCALIENTES") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(aguascalientes, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Aguascalientes)",
       x="Fecha", y="Total de incidencias")

## Baja California ----
baja_calif <- datos_narcotrafico %>%
  filter(ENTIDAD == "BAJA CALIFORNIA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(baja_calif, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Baja California)",
       x="Fecha", y="Total de incidencias")

## Baja California Sur ----
baja_calif_s <- datos_narcotrafico %>%
  filter(ENTIDAD == "BAJA CALIFORNIA SUR") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(baja_calif_s, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Baja California Sur)",
       x="Fecha", y="Total de incidencias")

## Campeche ----
campeche <- datos_narcotrafico %>%
  filter(ENTIDAD == "CAMPECHE") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(campeche, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Campeche)", 
       x="Fecha", y="Total de incidencias")

## Chiapas ----
chiapas <- datos_narcotrafico %>%
  filter(ENTIDAD == "CHIAPAS") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(chiapas, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Chiapas)", 
       x="Fecha", y="Total de incidencias")

## Chihuahua ----
chihuahua <- datos_narcotrafico %>%
  filter(ENTIDAD == "CHIHUAHUA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(chihuahua , aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Chihuahua)",
       x="Fecha", y="Total de incidencias")

## CDMX ----
cdmx <- datos_narcotrafico %>%
  filter(ENTIDAD == "CIUDAD DE MEXICO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(cdmx, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (CDMX)", 
       x="Fecha", y="Total de incidencias")

## Coahuila ----
coahuila <- datos_narcotrafico %>%
  filter(ENTIDAD == "COAHUILA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(coahuila, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Coahuila)", 
       x="Fecha", y="Total de incidencias")

## Colima ----
colima <- datos_narcotrafico %>%
  filter(ENTIDAD == "COLIMA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(colima, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Colima)", 
       x="Fecha", y="Total de incidencias")

## Durango ----
durango <- datos_narcotrafico %>%
  filter(ENTIDAD == "DURANGO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(durango, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Durango)", 
       x="Fecha", y="Total de incidencias")

## Guanajuato ----
guanajuato <- datos_narcotrafico %>%
  filter(ENTIDAD == "GUANAJUATO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(guanajuato, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Guanajuato)", 
       x="Fecha", y="Total de incidencias")

## Guerrero ----
guerrero <- datos_narcotrafico %>%
  filter(ENTIDAD == "GUERRERO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(guerrero, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Guerrero)", 
       x="Fecha", y="Total de incidencias")

## Hidalgo ----
hidalgo <- datos_narcotrafico %>%
  filter(ENTIDAD == "HIDALGO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(hidalgo, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Hidalgo)", 
       x="Fecha", y="Total de incidencias")

## Jalisco ----
jalisco<- datos_narcotrafico %>%
  filter(ENTIDAD == "JALISCO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(jalisco, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Jalisco)", 
       x="Fecha", y="Total de incidencias")

## México (Edomex) ----
edomex <- datos_narcotrafico %>%
  filter(ENTIDAD == "MEXICO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(edomex, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Estado de México)", 
       x="Fecha", y="Total de incidencias")

## Michoacán ----
michoacan<- datos_narcotrafico %>%
  filter(ENTIDAD == "MICHOACAN") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(michoacan, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Michoacán)", 
       x="Fecha", y="Total de incidencias")

## Morelos ----
morelos<- datos_narcotrafico %>%
  filter(ENTIDAD == "MORELOS") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(morelos, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Morelos)", 
       x="Fecha", y="Total de incidencias")

## Nayarit ----
nayarit <- datos_narcotrafico %>%
  filter(ENTIDAD == "NAYARIT") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(nayarit, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Nayarit)", 
       x="Fecha", y="Total de incidencias")

## Nuevo León ----
nuevo_leon <- datos_narcotrafico %>%
  filter(ENTIDAD == "NUEVO LEON") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(nuevo_leon, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Nuevo León)", 
       x="Fecha", y="Total de incidencias")

## Oaxaca ----
oaxaca <- datos_narcotrafico %>%
  filter(ENTIDAD == "OAXACA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(oaxaca, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Oaxaca)", 
       x="Fecha", y="Total de incidencias")

## Puebla ----
puebla <- datos_narcotrafico %>%
  filter(ENTIDAD == "PUEBLA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(puebla, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Puebla)", 
       x="Fecha", y="Total de incidencias")

## Querétaro ----
queretaro<- datos_narcotrafico %>%
  filter(ENTIDAD == "QUERETARO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(queretaro, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Querétaro)", 
       x="Fecha", y="Total de incidencias")

## Quintana Roo ----
quintana_roo <- datos_narcotrafico %>%
  filter(ENTIDAD == "QUINTANA ROO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(quintana_roo, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Quintana Roo)", 
       x="Fecha", y="Total de incidencias")

## San Luis Potosí ----
san_luis <- datos_narcotrafico %>%
  filter(ENTIDAD == "SAN LUIS POTOSI") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(san_luis, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (San Luis Potosí)", 
       x="Fecha", y="Total de incidencias")

## Sinaloa ----
sinaloa <- datos_narcotrafico %>%
  filter(ENTIDAD == "SINALOA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(sinaloa, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Sinaloa)", 
       x="Fecha", y="Total de incidencias")

## Sonora ----
sonora <- datos_narcotrafico %>%
  filter(ENTIDAD == "SONORA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(sonora, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Sonora)", 
       x="Fecha", y="Total de incidencias")

## Tabasco ----
tabasco <- datos_narcotrafico %>%
  filter(ENTIDAD == "TABASCO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(tabasco, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Tabasco)", 
       x="Fecha", y="Total de incidencias")

## Tamaulipas ----
tamaulipas <- datos_narcotrafico %>%
  filter(ENTIDAD == "TAMAULIPAS") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(tamaulipas, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Tamaulipas)", 
       x="Fecha", y="Total de incidencias")

## Tlaxcala ----
tlaxcala <- datos_narcotrafico %>%
  filter(ENTIDAD == "TLAXCALA") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(tlaxcala, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Tlaxcala)", 
       x="Fecha", y="Total de incidencias")

## Veracruz ----
veracruz <- datos_narcotrafico %>%
  filter(ENTIDAD == "VERACRUZ") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(veracruz, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Veracruz)", 
       x="Fecha", y="Total de incidencias")

## Yucatán ----
yucatan <- datos_narcotrafico %>%
  filter(ENTIDAD == "YUCATAN") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(yucatan, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Yucatán)", 
       x="Fecha", y="Total de incidencias")

## Zacatecas ----
zacatecas <- datos_narcotrafico %>%
  filter(ENTIDAD == "ZACATECAS")%>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(zacatecas, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolucion Mensual (Zacatecas)",
       x="Año", y="Total de incidencias")

## Todos ----
serie_estado <- datos_narcotrafico %>%
  group_by(ENTIDAD, FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE), .groups = "drop")%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(serie_estado, aes(FECHA, TOTAL)) +
  geom_line() +
  facet_wrap(~ENTIDAD, scales="free_y") +
  labs(title="Serie mensual completa por estado",
       x="Fecha", y="Total")

# ARIMA x estado ----
## Aguascalientes ----
lambda <- BoxCox.lambda(aguascalientes$TOTAL)
print(lambda)

arima <- aguascalientes %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)

par(mfrow = c(1, 2))
acf(arima$TOTAL, main = "ACF de Delitos")
pacf(arima$TOTAL, main = "PACF de Delitos")

modelo <- Arima(arima$TOTAL, 
                order = c(2, 0, 0),
                lambda = 0.5)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(2,0,1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

aguascalientes$PREDICCIONES <- exp(fitted(modelo))-1
aguascalientes$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(aguascalientes, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Aguascalientes",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(arima$TOTAL, stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

#modelo

## Baja California ----
lambda <- BoxCox.lambda(baja_calif$TOTAL)
print(lambda)

arima <- baja_calif %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
#diff_arima = diff(arima$TOTAL)
#adf.test(diff_arima)
#kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(arima$TOTAL, main = "ACF de Delitos")
pacf(arima$TOTAL, main = "PACF de Delitos")

modelo <- arima(arima$TOTAL,
                c(2,0,0)) 
modelo_2 <- arima(arima$TOTAL,
                  c(3,0,0))

summary(modelo)
summary(modelo_2)

baja_calif$PREDICCIONES <- exp(fitted(modelo))-1
baja_calif$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(baja_calif, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Baja California",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(arima$TOTAL, stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

#modelo

## Baja California Sur ----
lambda <- BoxCox.lambda(baja_calif_s$TOTAL)
print(lambda)

arima <- baja_calif_s %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(baja_calif_s$TOTAL, 
                order = c(1, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(baja_calif_s$TOTAL, 
                  order = c(0, 1, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

baja_calif_s$PREDICCIONES <- fitted(modelo)
baja_calif_s$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(baja_calif_s, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Baja California Sur",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(baja_calif_s$TOTAL, seasonal = TRUE, stationary = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Campeche ----
lambda <- BoxCox.lambda(campeche$TOTAL)
print(lambda)

arima <- campeche %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(campeche$TOTAL, 
                order = c(1, 1, 0),
                lambda = 0.5)
modelo_2 <- Arima(campeche$TOTAL, 
                  order = c(1,1,1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

campeche$PREDICCIONES <- fitted(modelo)
campeche$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(campeche, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Campeche",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(campeche$TOTAL, seasonal = TRUE, 
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# auto

## Chiapas ---- 
lambda <- BoxCox.lambda(chiapas$TOTAL)
print(lambda)

arima <- chiapas %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(chiapas$TOTAL, 
                order = c(2, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(chiapas$TOTAL, 
                  order = c(1,1,1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

chiapas$PREDICCIONES <- fitted(modelo)
chiapas$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(chiapas, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Chiapas",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(chiapas$TOTAL, stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

#modelo_2

## Chihuahua ----
lambda <- BoxCox.lambda(chihuahua$TOTAL)
print(lambda)

arima <- chihuahua %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
#diff_arima = diff(arima$TOTAL)
#adf.test(diff_arima)
#kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(arima$TOTAL, main = "ACF de Delitos")
pacf(arima$TOTAL, main = "PACF de Delitos")

modelo <- Arima(chihuahua$TOTAL, 
                order = c(1, 0, 1),
                lambda = 0.5)
modelo_2 <- Arima(chihuahua$TOTAL, 
                  order = c(1, 0, 0),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

chihuahua$PREDICCIONES <- fitted(modelo)
chihuahua$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(chihuahua, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Chihuahua",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(chihuahua$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

## CDMX ----
lambda <- BoxCox.lambda(cdmx$TOTAL)
print(lambda)

arima <- cdmx %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(cdmx$TOTAL, 
                order = c(1, 0, 0),
                lambda = 0.5)
modelo_2 <- Arima(cdmx$TOTAL, 
                  order = c(1, 0, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

cdmx$PREDICCIONES <- fitted(modelo)
cdmx$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(cdmx, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: CDMX",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(cdmx$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Coahuila ----
lambda <- BoxCox.lambda(coahuila$TOTAL)
print(lambda)

#arima <- coahuila %>%
#  mutate(TOTAL=sqrt(TOTAL))

adf.test(coahuila$TOTAL)
kpss.test(coahuila$TOTAL)
diff_arima = diff(coahuila$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(coahuila$TOTAL, 
                order = c(1, 1, 1))
modelo_2 <- Arima(coahuila$TOTAL, 
                  order = c(2, 1, 1))
summary(modelo)
summary(modelo_2)

coahuila$PREDICCIONES <- fitted(modelo)
coahuila$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(coahuila, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Coahuila",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(coahuila$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Colima ----
lambda <- BoxCox.lambda(colima$TOTAL)
print(lambda)

arima <- colima %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(arima$TOTAL, 
                order = c(2, 1, 1))
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1))
summary(modelo)
summary(modelo_2)

colima$PREDICCIONES <- exp(fitted(modelo))-1
colima$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(colima, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Colima",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(arima$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Durango ----
lambda <- BoxCox.lambda(durango$TOTAL)
print(lambda)

arima <- durango %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima <- diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(durango$TOTAL, 
                order = c(2, 1, 3),
                lambda = 0.5)
modelo_2 <- Arima(durango$TOTAL, 
                  order = c(2, 1, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

durango$PREDICCIONES <- fitted(modelo)
durango$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(durango, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Durango",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(durango$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto) 

# modelo

## Guanajuato ----
lambda <- BoxCox.lambda(guanajuato$TOTAL)
print(lambda)

arima <- guanajuato %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima <- diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(guanajuato$TOTAL, 
                order = c(1, 1, 1))
modelo_2 <- Arima(guanajuato$TOTAL, 
                  order = c(0, 1, 1))
summary(modelo)
summary(modelo_2)

guanajuato$PREDICCIONES <- fitted(modelo)
guanajuato$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(guanajuato, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Guanajuato",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(guanajuato$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

#modelo_2

## Guerrero ----
lambda <- BoxCox.lambda(guerrero$TOTAL)
print(lambda)

arima <- guerrero %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima <- diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)
#diff_arima = diff(diff_arima, lag = 12)
#par(mfrow = c(1, 2))
#acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
#pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(guerrero$TOTAL, 
                order = c(1, 1, 0), 
                seasonal = list(order = c(1, 0, 0), period = 12),
                lambda = 0.5)
modelo_2 <- Arima(guerrero$TOTAL, 
                  order = c(1, 1, 1), 
                  seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

guerrero$PREDICCIONES <- fitted(modelo)
guerrero$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(guerrero, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Guerrero",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(guerrero$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE, lambda = 0.5)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Hidalgo ----
lambda <- BoxCox.lambda(hidalgo$TOTAL)
print(lambda)

arima <- hidalgo %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(hidalgo$TOTAL, 
                order = c(1, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(hidalgo$TOTAL, 
                  order = c(2, 1, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

hidalgo$PREDICCIONES <- fitted(modelo)
hidalgo$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(hidalgo, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Hidalgo",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(hidalgo$TOTAL, seasonal = TRUE, 
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

#modelo_2

## Jalisco ----
lambda <- BoxCox.lambda(jalisco$TOTAL)
print(lambda)

arima <- jalisco %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(arima$TOTAL, 
                order = c(2, 1, 1))
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(1, 1, 1))
summary(modelo)
summary(modelo_2)

jalisco$PREDICCIONES <- exp(fitted(modelo))-1
jalisco$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(jalisco, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Jalisco",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(arima$TOTAL, seasonal = TRUE, 
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# auto

## Mexico (Edomex) ----
lambda <- BoxCox.lambda(edomex$TOTAL)
print(lambda)

arima <- edomex %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(arima$TOTAL, 
                order = c(2, 1, 1))
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1))
summary(modelo)
summary(modelo_2)

edomex$PREDICCIONES <- exp(fitted(modelo))-1
edomex$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(edomex, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Estado de México",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(edomex$TOTAL, seasonal = TRUE, 
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Michoacan -----
lambda <- BoxCox.lambda(michoacan$TOTAL)
print(lambda)

arima <- michoacan %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima <- diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 1))
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(2, 1, 1))
summary(modelo)
summary(modelo_2)

michoacan$PREDICCIONES <- exp(fitted(modelo))-1
michoacan$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(michoacan, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Michoacan",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(arima$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

## Morelos ----
lambda <- BoxCox.lambda(morelos$TOTAL)
print(lambda)

arima <- morelos %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(morelos$TOTAL, 
                order = c(2, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(morelos$TOTAL, 
                  order = c(2, 1, 0),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

morelos$PREDICCIONES <- fitted(modelo)
morelos$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(morelos, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Morelos",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(morelos$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

## Nayarit ----
lambda <- BoxCox.lambda(nayarit$TOTAL)
print(lambda)

arima <- nayarit %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(nayarit$TOTAL, 
                order = c(1, 1, 2),
                lambda = 0.5)
modelo_2 <- Arima(nayarit$TOTAL, 
                  order = c(1, 1, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

nayarit$PREDICCIONES <- fitted(modelo)
nayarit$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(nayarit, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Nayarit",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(nayarit$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Nuevo Leon ----
lambda <- BoxCox.lambda(nuevo_leon$TOTAL)
print(lambda)

arima <- nuevo_leon %>%
  mutate(TOTAL = log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 1),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(1, 1, 0),
                  lambda = 1)
summary(modelo)
summary(modelo_2)

nuevo_leon$PREDICCIONES <- exp(fitted(modelo))-1
nuevo_leon$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(nuevo_leon, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Nuevo León",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(arima$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# auto

## Oaxaca ----
lambda <- BoxCox.lambda(oaxaca$TOTAL)
print(lambda)

arima <- oaxaca %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(oaxaca$TOTAL, 
                order = c(1, 1, 0),
                lambda = 0.5)
modelo_2 <- Arima(oaxaca$TOTAL, 
                  order = c(0, 1, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

oaxaca$PREDICCIONES <- fitted(modelo)
oaxaca$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(oaxaca, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Oaxaca",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(oaxaca$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Puebla ----
lambda <- BoxCox.lambda(puebla$TOTAL)
print(lambda)

arima <- puebla %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(puebla$TOTAL, 
                order = c(2, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(puebla$TOTAL, 
                  order = c(1, 1, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

puebla$PREDICCIONES <- fitted(modelo)
puebla$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(puebla, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Puebla",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(puebla$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

## Queretaro ----
lambda <- BoxCox.lambda(queretaro$TOTAL)
print(lambda)

arima <- queretaro %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(queretaro$TOTAL, 
                order = c(1, 1, 2),
                lambda = 0.5)
modelo_2 <- Arima(queretaro$TOTAL, 
                  order = c(1, 1, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

queretaro$PREDICCIONES <- fitted(modelo)
queretaro$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(queretaro, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Querétaro",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(queretaro$TOTAL, seasonal = TRUE, 
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Quintana Roo ----
lambda <- BoxCox.lambda(quintana_roo$TOTAL)
print(lambda)

arima <- queretaro %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(quintana_roo$TOTAL, 
                order = c(2, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(quintana_roo$TOTAL, 
                  order = c(1, 1, 0),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

quintana_roo$PREDICCIONES <- fitted(modelo)
quintana_roo$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(quintana_roo, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Quintana Roo",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(quintana_roo$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

## San Luis Potosí ----
lambda <- BoxCox.lambda(san_luis$TOTAL)
print(lambda)

arima <- san_luis %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(san_luis$TOTAL, 
                order = c(1, 1, 1),
                #seasonal = list(order = c(1, 0, 0), period = 12),
                lambda = 0.5)
modelo_2 <- Arima(san_luis$TOTAL, 
                  order = c(1, 1, 0),
                  #seasonal = list(order = c(0, 0, 1), period = 12),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

san_luis$PREDICCIONES <- fitted(modelo)
san_luis$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(san_luis, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: San Luis Potosí",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(san_luis$TOTAL, seasonal = TRUE, 
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# auto

## Sinaloa ----
lambda <- BoxCox.lambda(sinaloa$TOTAL)
print(lambda)

arima <- sinaloa %>%
  mutate(TOTAL = log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
#diff_arima = diff(arima$TOTAL)
#adf.test(diff_arima)
#kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos")
pacf(diff_arima, main = "PACF de Delitos")

modelo <- Arima(arima$TOTAL, 
                order = c(1, 0, 1))
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(1, 0, 0))
summary(modelo)
summary(modelo_2)

sinaloa$PREDICCIONES <- exp(fitted(modelo))-1
sinaloa$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(sinaloa, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Sinaloa",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(arima$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Sonora ----
lambda <- BoxCox.lambda(sonora$TOTAL)
print(lambda)

arima <- sonora %>%
  mutate(TOTAL = log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 0), 
                seasonal = list(order = c(0, 0, 1), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), 
                  seasonal = list(order = c(0, 0, 1), period = 12),
                  lambda = 1)
summary(modelo)
summary(modelo_2)

sonora$PREDICCIONES <- exp(fitted(modelo))-1
sonora$PREDICCIONES_2 <- exp(fitted(modelo_2))-1

ggplot(sonora, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Sonora",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(arima$TOTAL, seasonal = TRUE, 
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo_2

## Tabasco ----
lambda <- BoxCox.lambda(tabasco$TOTAL)
print(lambda)

arima <- tabasco %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
#diff_arima = diff(arima$TOTAL)
#adf.test(diff_arima)
#kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(arima$TOTAL, main = "ACF de Delitos", lag.max = 36)
pacf(arima$TOTAL, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(tabasco$TOTAL, 
                order = c(0, 0, 1),
                lambda = 0.5)
modelo_2 <- Arima(tabasco$TOTAL, 
                  order = c(1, 0, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

tabasco$PREDICCIONES <- fitted(modelo)
tabasco$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(tabasco, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Tabasco",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(tabasco$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# auto

## Tamaulipas ----
lambda <- BoxCox.lambda(tamaulipas$TOTAL)
print(lambda)

arima <- tamaulipas %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(tamaulipas$TOTAL, 
                order = c(2, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(tamaulipas$TOTAL, 
                  order = c(2, 1, 0),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

tamaulipas$PREDICCIONES <- fitted(modelo)
tamaulipas$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(tamaulipas, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Tamaulipas",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(tamaulipas$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

#modelo_2

## Tlaxcala ----
lambda <- BoxCox.lambda(tlaxcala$TOTAL)
print(lambda)

arima <- tlaxcala %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(tlaxcala$TOTAL, 
                order = c(1, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(tlaxcala$TOTAL, 
                  order = c(1, 1, 0),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

tlaxcala$PREDICCIONES <- fitted(modelo)
tlaxcala$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(tlaxcala, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Tlaxcala",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(tlaxcala$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

## Veracruz ----
lambda <- BoxCox.lambda(veracruz$TOTAL)
print(lambda)

arima <- veracruz %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(veracruz$TOTAL, 
                order = c(2, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(veracruz$TOTAL, 
                  order = c(2, 1, 0),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

veracruz$PREDICCIONES <- fitted(modelo)
veracruz$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(veracruz, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Veracruz",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(veracruz$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

## Yucatan ----
lambda <- BoxCox.lambda(yucatan$TOTAL)
print(lambda)

arima <- yucatan %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(arima$TOTAL, main = "ACF de Delitos", lag.max = 36)
pacf(arima$TOTAL, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(yucatan$TOTAL, 
                order = c(1, 0, 1),
                lambda = 0.5)
                #method = "ML",
                #include.drift = TRUE)
modelo_2 <- Arima(yucatan$TOTAL, 
                  order = c(0, 1, 1),
                  lambda = 0.37)
                  #method = "ML",
                  #include.drift = TRUE)
summary(modelo)
summary(modelo_2)

yucatan$PREDICCIONES <- fitted(modelo)
yucatan$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(yucatan, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Yucatan",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(yucatan$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

## Zacatecas ----
lambda <- BoxCox.lambda(zacatecas$TOTAL)
print(lambda)

arima <- zacatecas %>%
  mutate(TOTAL = sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(zacatecas$TOTAL, 
                order = c(1, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(yucatan$TOTAL, 
                  order = c(2, 1, 1),
                  lambda = 0.5)
summary(modelo)
summary(modelo_2)

zacatecas$PREDICCIONES <- fitted(modelo)
zacatecas$PREDICCIONES_2 <- fitted(modelo_2)

ggplot(zacatecas, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", "Modelo ARIMA 2" = "red")) +
  labs(title = "Comparación de Incidencia Delictiva: Zacatecas",
       subtitle = "Valores observados vs. Ajuste del modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")
checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

auto <- auto.arima(zacatecas$TOTAL, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE)
arimaorder(auto)
checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")
AIC(modelo, modelo_2, auto)

# modelo

# Test x estado ----
n_test <- 12
n_total <- nrow(aguascalientes)-12
n_train <- n_total-n_test
## Aguascalientes ----
train_data <- aguascalientes[1:n_train, ]
test_data  <- aguascalientes[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(2, 0, 0),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Aguascalientes",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)

## Baja California ----
train_data <- baja_calif[1:n_train, ]
test_data  <- baja_calif[(n_train+1):n_total, ]

modelo_final <- arima(log(train_data$TOTAL+1),
                      c(2,0,0))
pronostico <- forecast(modelo_final, h = n_test)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = exp(as.numeric(pronostico$mean))-1, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Baja California",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)
## Baja California Sur ----
train_data <- baja_calif_s[1:n_train, ]
test_data  <- baja_calif_s[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(0, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"),
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Baja California Sur", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%")
       , x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Campeche ----
train_data <- campeche[1:n_train, ]
test_data  <- campeche[(n_train+1):n_total, ]

modelo_final <- auto.arima(train_data$TOTAL, seasonal = TRUE, 
                           stepwise = FALSE, approximation = FALSE)

pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Campeche", 
       subtitle = paste("MAE:", round(metricas[1, "MAE"], 2)), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Chiapas ----
train_data <- chiapas[1:n_train, ]
test_data  <- chiapas[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1,1,1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Chiapas", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Chihuahua ----
train_data <- chihuahua[1:n_train, ]
test_data  <- chihuahua[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1, 0, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Chihuahua", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## CDMX ----
train_data <- cdmx[1:n_train, ]
test_data  <- cdmx[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1, 0, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: CDMX", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Coahuila ----
train_data <- coahuila[1:n_train, ]
test_data  <- coahuila[(n_train+1):n_total, ]

modelo_final <- modelo <- Arima(train_data$TOTAL, 
                                order = c(2, 1, 1))
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Coahuila", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Colima ----
train_data <- colima[1:n_train, ]
test_data  <- colima[(n_train+1):n_total, ]

modelo_final <- Arima(log(train_data$TOTAL+1), 
                      order = c(0, 1, 1))
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(exp(as.numeric(pronostico$mean))-1, test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = exp(as.numeric(pronostico$mean))-1, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Colima", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Durango ----
train_data <- durango[1:n_train, ]
test_data  <- durango[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(2, 1, 3),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Durango", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Guanajuato ----
train_data <- guanajuato[1:n_train, ]
test_data  <- guanajuato[(n_train+1):n_total, ]

modelo_final <- Arima(log(train_data$TOTAL+1), 
                      order = c(0, 1, 1))
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(exp(as.numeric(pronostico$mean))-1, test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = exp(as.numeric(pronostico$mean))-1, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Guanajuato", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Guerrero ----
train_data <- guerrero[1:n_train, ]
test_data  <- guerrero[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1, 1, 1),
                      seasonal = list(order = c(1, 0, 0), period = 12),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Guerrero", 
       subtitle = paste("MAE:", round(metricas[1, "MAE"], 2)), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Hidalgo ----
train_data <- hidalgo[1:n_train, ]
test_data  <- hidalgo[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(2, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Hidalgo", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Jalisco ----
train_data <- jalisco[1:n_train, ]
test_data  <- jalisco[(n_train+1):n_total, ]

modelo_final <- auto.arima(train_data$TOTAL, seasonal = TRUE, 
                           stepwise = FALSE, approximation = FALSE)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Jalisco", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## México (Edomex) ----
train_data <- edomex[1:n_train, ]
test_data  <- edomex[(n_train+1):n_total, ]

modelo_final <- Arima(log(train_data$TOTAL+1), 
                      order = c(0, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(exp(as.numeric(pronostico$mean))-1, test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = exp(as.numeric(pronostico$mean))-1, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Estado de México", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Michoacan ----
train_data <- michoacan[1:n_train, ]
test_data  <- michoacan[(n_train+1):n_total, ]
modelo_final <- Arima(log(train_data$TOTAL+1), 
                      order = c(1, 1, 1),
                      lambda = 1)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(exp(as.numeric(pronostico$mean))-1, test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = exp(as.numeric(pronostico$mean))-1, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Michoacán", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Morelos ----
train_data <- morelos[1:n_train, ]
test_data  <- morelos[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(2, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Morelos", 
       subtitle = paste("MAE:", round(metricas[1, "MAE"], 2)), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Nayarit ----
train_data <- nayarit[1:n_train, ]
test_data  <- nayarit[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Nayarit", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Nuevo León ----
train_data <- nuevo_leon[1:n_train, ]
test_data  <- nuevo_leon[(n_train+1):n_total, ]

modelo_final <- auto.arima(train_data$TOTAL, seasonal = TRUE,
                           stepwise = FALSE, approximation = FALSE)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Nuevo León", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Oaxaca ----
train_data <- oaxaca[1:n_train, ]
test_data  <- oaxaca[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(0, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Oaxaca", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Puebla ----
train_data <- puebla[1:n_train, ]
test_data  <- puebla[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(2, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Puebla", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Querétaro ----
train_data <- queretaro[1:n_train, ]
test_data  <- queretaro[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Querétaro", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Quintana Roo ----
train_data <- quintana_roo[1:n_train, ]
test_data  <- quintana_roo[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(2, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Quintana Roo", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## San Luis ----
train_data <- san_luis[1:n_train, ]
test_data  <- san_luis[(n_train+1):n_total, ]

modelo_final <- auto.arima(train_data$TOTAL, seasonal = TRUE, 
                           stepwise = FALSE, approximation = FALSE)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: SLP", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Sinaloa ----
train_data <- sinaloa[1:n_train, ]
test_data  <- sinaloa[(n_train+1):n_total, ]

modelo_final <- Arima(log(train_data$TOTAL+1), 
                      order = c(1, 0, 0))
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(exp(as.numeric(pronostico$mean))-1, test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = exp(as.numeric(pronostico$mean))-1, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Sinaloa", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Sonora ----
train_data <- sonora[1:n_train, ]
test_data  <- sonora[(n_train+1):n_total, ]

modelo_final <- Arima(log(train_data$TOTAL+1), 
                      order = c(0, 1, 1), 
                      seasonal = list(order = c(1, 0, 0), period = 12),
                      lambda = 1)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(exp(as.numeric(pronostico$mean))-1, test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = exp(as.numeric(pronostico$mean))-1, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Sonora", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Tabasco ----
train_data <- tabasco[1:n_train, ]
test_data  <- tabasco[(n_train+1):n_total, ]

modelo_final <- auto.arima(tabasco$TOTAL, seasonal = TRUE,
                           stepwise = FALSE, approximation = FALSE)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Tabasco", 
       subtitle = paste("MAE:", round(metricas[1, "MAE"], 2)), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Tamaulipas ----
train_data <- tamaulipas[1:n_train, ]
test_data  <- tamaulipas[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(2, 1, 0),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Tamaulipas", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2), "%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Tlaxcala ----
train_data <- tlaxcala[1:n_train, ]
test_data  <- tlaxcala[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Tlaxcala", 
       subtitle = paste("MAE:", round(metricas[1, "MAE"], 2)), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Veracruz ----
train_data <- veracruz[1:n_train, ]
test_data  <- veracruz[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(2, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Veracruz", 
       subtitle = paste("MAE:", round(metricas[1, "MAE"], 2)), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Yucatán ----
train_data <- yucatan[1:n_train, ]
test_data  <- yucatan[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1, 0, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Yucatán", 
       subtitle = paste("MAE:", round(metricas[1, "MAE"], 2)), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

## Zacatecas ----
train_data <- zacatecas[1:n_train, ]
test_data  <- zacatecas[(n_train+1):n_total, ]

modelo_final <- Arima(train_data$TOTAL, 
                      order = c(1, 1, 1),
                      lambda = 0.5)
pronostico <- forecast(modelo_final, h = n_test)

metricas <- accuracy(as.numeric(pronostico$mean), test_data$TOTAL)
print(metricas)

ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = as.numeric(pronostico$mean), color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), fill = "steelblue", alpha = 0.2) +
  labs(title = "ARIMA: Zacatecas", 
       subtitle = paste("MAPE:", round(metricas[1, "MAPE"], 2),"%"), 
       x = "Fecha", y = "Delitos", color = "Serie") + 
  theme_minimal()

# Prophet x estado ----
# Aguascalientes ----
datos_prophet <- aguascalientes %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  train_data,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE
)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)

forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data,
            aes(x = ds, y = y, color = "Train"),
            size = 1) +
  geom_line(data = test_data,
            aes(x = ds, y = y, color = "Real"),
            size = 1) +
  geom_line(data = forecast_test,
            aes(x = ds, y = yhat, color = "Predicción"),
            linetype = "dashed",
            size = 1) +
  geom_ribbon(data = forecast_test,
              aes(x = ds,
                  ymin = yhat_lower,
                  ymax = yhat_upper),
              alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Aguascalientes",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Baja California ----
datos_prophet <- baja_calif %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)

future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Baja California",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Baja California Sur ----
datos_prophet <- baja_calif_s %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Baja California Sur",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Campeche ----
datos_prophet <- campeche %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Campeche",
       subtitle = paste("MAE en Test Set:", round(metricas[1, "MAE"], 2)),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Chiapas ----
datos_prophet <- chiapas %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Chiapas",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Chihuahua ----
datos_prophet <- chihuahua %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Chihuahua",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# CDMX ----
datos_prophet <- cdmx %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)

future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: CDMX",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Coahuila ----
datos_prophet <- coahuila %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Coahuila",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Colima ----
datos_prophet <- colima %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Colima",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
# Durango ----
datos_prophet <- durango %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Durango",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
# Guanajuato ----
datos_prophet <- guanajuato %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Guanajuato",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
# Guerrero ----
datos_prophet <- guerrero %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Guerrero",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
# Hidalgo ----
datos_prophet <- hidalgo %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Hidalgo",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
# Jalisco ----
datos_prophet <- jalisco %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)

future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Jalisco",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# México (Edomex) ----
datos_prophet <- edomex %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)

future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Estado de México",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Michoacán ----
datos_prophet <- michoacan %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Michoacán",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
# Morelos ----
datos_prophet <- morelos %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Morelos",
       subtitle = paste("MAE en Test Set:", round(metricas[1, "MAE"], 2)),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
# Nayarit ----
datos_prophet <- nayarit %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
matricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Nayarit",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
# Nuevo León ----
datos_prophet <- nuevo_leon %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)

future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Nuevo León",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Oaxaca ----
datos_prophet <- oaxaca %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Oaxaca",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Puebla ----
datos_prophet <- puebla %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Puebla",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Querétaro ----
datos_prophet <- queretaro %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Querétaro",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Quintana Roo ----
datos_prophet <- quintana_roo %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Quintana Roo",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# San Luis Potosí ----
datos_prophet <- san_luis%>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: San Luis Potosí",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Sinaloa ----
datos_prophet <- sinaloa %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Sinaloa",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Sonora ----
datos_prophet <- sonora %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Sonora",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Tabasco ----
datos_prophet <- tabasco %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Tabasco",
       subtitle = paste("MAE en Test Set:", round(metricas[1, "MAE"], 2)),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Tamaulipas ----
datos_prophet <- tamaulipas %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Tamaulipas",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Tlaxcala ----
datos_prophet <- tlaxcala %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Tlaxcala",
       subtitle = paste("MAE en Test Set:", round(metricas[1, "MAE"], 2)),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Veracruz ----
datos_prophet <- veracruz %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Veracruz",
       subtitle = paste("MAE en Test Set:", round(metricas[1, "MAE"], 2)),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Yucatán ----
datos_prophet <- yucatan %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Yucatán",
       subtitle = paste("MAE en Test Set:", round(metricas[1, "MAE"], 2)),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# Zacatecas ----
datos_prophet <- zacatecas %>%
  select(ds = FECHA, y = TOTAL)

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(modelo, periods = nrow(test_data), freq = "month")
forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))
metricas <- accuracy(predicciones, test_data$y)
metricas
forecast_test <- tail(forecast_prophet, nrow(test_data))

ggplot() +
  geom_line(data = train_data, aes(x = ds, y = y, color = "Train"), size = 1) +
  geom_line(data = test_data, aes(x = ds, y = y, color = "Real"), size = 1) +
  geom_line(data = forecast_test, aes(x = ds, y = yhat, color = "Predicción"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_test, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Zacatecas",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
