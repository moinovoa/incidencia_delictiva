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

# Tipo de delito ----
composicion <- datos_narcotrafico %>%
  group_by(TIPO) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE)) %>%
  mutate(PORCENTAJE = (TOTAL / sum(TOTAL)) * 100)

plot_ly(composicion, labels = ~TIPO, values = ~TOTAL, type = 'pie') %>%
  layout(title = 'Tipo de delito (narcotráfico)')

## Estadísticas ----
descriptivas <- datos_narcotrafico %>%
  group_by(TIPO) %>%
  summarize(
    MEDIA_MENSUAL = mean(CONTEO, na.rm = TRUE),
    MEDIANA_MENSUAL = median(CONTEO, na.rm = TRUE),
    DESV_EST = sd(CONTEO, na.rm = TRUE),
    MAXIMO = max(CONTEO, na.rm = TRUE),
    TOTAL_HIST = sum(CONTEO, na.rm = TRUE)
  )
descriptivas

# Series de tiempo x delito ----
conteo_delito <- datos_narcotrafico %>%
  group_by(TIPO) %>%
  summarize(SUMA = sum(CONTEO, na.rm = TRUE))
## Posesion ----
posesion <- datos_narcotrafico %>%
  filter(TIPO == "POSESION") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(posesion, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Posesion)",
       x="Fecha", y="Total de incidencias")

## Otros ----
otros <- datos_narcotrafico %>%
  filter(TIPO == "OTROS") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(otros, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Otros)",
       x="Fecha", y="Total de incidencias")
otros <- otros %>%
  filter(FECHA >= "2020-01-01")

ggplot(otros, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Otros)",
       x="Fecha", y="Total de incidencias")

## Transporte ----
transporte <- datos_narcotrafico %>%
  filter(TIPO == "TRANSPORTE") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(transporte, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Transporte)",
       x="Fecha", y="Total de incidencias")

## Tráfico ----
trafico <- datos_narcotrafico %>%
  filter(TIPO == "TRAFICO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(trafico, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Tráfico)",
       x="Fecha", y="Total de incidencias")

trafico <- trafico %>%
  filter(FECHA >= "2020-01-01")

ggplot(trafico, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Tráfico)",
       x="Fecha", y="Total de incidencias")

## Comercio ----
comercio <- datos_narcotrafico %>%
  filter(TIPO == "COMERCIO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(comercio, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Comercio)",
       x="Fecha", y="Total de incidencias")

## Producción ----
produccion <- datos_narcotrafico %>%
  filter(TIPO == "PRODUCCION") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(produccion, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Producción)",
       x="Fecha", y="Total de incidencias")

produccion <- produccion %>%
  filter(FECHA >= "2020-01-01")

ggplot(produccion, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Producción)",
       x="Fecha", y="Total de incidencias")

## Suministro ----
suministro <- datos_narcotrafico %>%
  filter(TIPO == "SUMINISTRO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(suministro, aes(x=FECHA, y=TOTAL))+
  geom_line(color = "black", size = 1) +
  geom_point() +
  labs(title = "Evolución Mensual (Suministro)",
       x="Fecha", y="Total de incidencias")

## Todos ----
serie_tipo <- datos_narcotrafico %>%
  group_by(TIPO, FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE), .groups = "drop")%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

ggplot(serie_tipo, aes(FECHA, TOTAL)) +
  geom_line() +
  facet_wrap(~TIPO, scales="free_y") +
  labs(title="Serie mensual completa por tipo de delito",
       x="Fecha", y="Total")

# ARIMA x delito ----
n_test <- 12
n_total <- nrow(posesion)-12
n_train <- n_total-n_test

## Posesion ----
lambda <- BoxCox.lambda(posesion$TOTAL)
print(lambda)

arima <- posesion %>%
  mutate(TOTAL=log(TOTAL+1))

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
                seasonal = list(order = c(1, 0, 1), period = 12))
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), 
                  seasonal = list(order = c(1, 0, 1), period = 12))

serie_mensual <- ts(arima$TOTAL, start = c(2013, 1), frequency = 12)
auto <- auto.arima(serie_mensual, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE, lambda = 1)

summary(modelo)
summary(modelo_2)
summary(auto)

posesion$PREDICCIONES <- exp(fitted(modelo))-1
posesion$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
posesion$PREDICCIONES_AUTO <-  exp(fitted(auto))-1

ggplot(posesion, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_AUTO, color = "Modelo ARIMA AUTO"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", 
                                "Modelo ARIMA 2" = "red", "Modelo ARIMA AUTO" = "green")) +
  labs(title = "Comparación de Incidencia Delictiva: Posesión",
       subtitle = "Valores observados vs. Ajustes de los modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

#auto

### Test ----
train_data <- posesion[1:n_train, ]
test_data  <- posesion[(n_train+1):n_total, ]

pronostico <- forecast(auto, h = n_test)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Posesión",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)

## Otros ----
n_total <- nrow(otros)-12
n_train <- n_total-n_test

lambda <- BoxCox.lambda(otros$TOTAL)
print(lambda)

arima <- otros %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)
#diff_arima = diff(diff_arima, lag = 12)
#par(mfrow = c(1, 2))
#acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
#pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 1), 
                #seasonal = list(order = c(1, 1, 1), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), 
                  seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

serie_mensual <- ts(arima$TOTAL, start = c(2013, 1), frequency = 12)
auto <- auto.arima(serie_mensual, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE, lambda = 1)

summary(modelo)
summary(modelo_2)
summary(auto)

otros$PREDICCIONES <- exp(fitted(modelo))-1
otros$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
otros$PREDICCIONES_AUTO <-  exp(fitted(auto))-1

ggplot(otros, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_AUTO, color = "Modelo ARIMA AUTO"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", 
                                "Modelo ARIMA 2" = "red", "Modelo ARIMA AUTO" = "green")) +
  labs(title = "Comparación de Incidencia Delictiva: Otros",
       subtitle = "Valores observados vs. Ajustes de los modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# auto

### Test ----
train_data <- otros[1:n_train, ]
test_data  <- otros[(n_train+1):n_total, ]

pronostico <- forecast(auto, h = n_test)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Otros",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)

## Transporte ----
n_total <- nrow(transporte)-12
n_train <- n_total-n_test

lambda <- BoxCox.lambda(transporte$TOTAL)
print(lambda)

arima <- transporte %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(transporte$TOTAL, 
                order = c(0, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(transporte$TOTAL, 
                  order = c(1, 1, 1),
                  lambda = 0.5)
serie_mensual <- ts(transporte$TOTAL, start = c(2013, 1), frequency = 12)
auto <- auto.arima(serie_mensual, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE, lambda = 1)

summary(modelo)
summary(modelo_2)
summary(auto)

transporte$PREDICCIONES <- fitted(modelo)
transporte$PREDICCIONES_2 <- fitted(modelo_2)
transporte$PREDICCIONES_AUTO <-  fitted(auto)

ggplot(transporte, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_AUTO, color = "Modelo ARIMA AUTO"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", 
                                "Modelo ARIMA 2" = "red", "Modelo ARIMA AUTO" = "green")) +
  labs(title = "Comparación de Incidencia Delictiva: Transporte",
       subtitle = "Valores observados vs. Ajustes de los modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

#modelo

### Test ----
train_data <- transporte[1:n_train, ]
test_data  <- transporte[(n_train+1):n_total, ]

pronostico <- forecast(modelo, h = n_test)

reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Transporte",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)

## Tráfico ----
n_total <- nrow(trafico)-12
n_train <- n_total-n_test

lambda <- BoxCox.lambda(trafico$TOTAL)
print(lambda)

#arima <- trafico %>%
#  mutate(TOTAL=sqrt(TOTAL))

adf.test(trafico$TOTAL)
kpss.test(trafico$TOTAL)
diff_arima = diff(trafico$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(trafico$TOTAL, 
                order = c(1, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(trafico$TOTAL, 
                  order = c(1, 1, 0),
                  lambda = 0.5)
serie_mensual <- ts(trafico$TOTAL, start = c(2013, 1), frequency = 12)
auto <- auto.arima(serie_mensual, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE, lambda = 0.5)

summary(modelo)
summary(modelo_2)
summary(auto)

trafico$PREDICCIONES <- fitted(modelo)
trafico$PREDICCIONES_2 <- fitted(modelo_2)
trafico$PREDICCIONES_AUTO <-  fitted(auto)

ggplot(trafico, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_AUTO, color = "Modelo ARIMA AUTO"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", 
                                "Modelo ARIMA 2" = "red", "Modelo ARIMA AUTO" = "green")) +
  labs(title = "Comparación de Incidencia Delictiva: Tráfico",
       subtitle = "Valores observados vs. Ajustes de los modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# modelo

### Test ----
train_data <- trafico[1:n_train, ]
test_data  <- trafico[(n_train+1):n_total, ]

pronostico <- forecast(auto, h = n_test)

reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Tráfico",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)

## Comercio ----
n_total <- nrow(comercio)-12
n_train <- n_total-n_test

lambda <- BoxCox.lambda(comercio$TOTAL)
print(lambda)

arima <- comercio %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 36)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(comercio$TOTAL, 
                order = c(0, 1, 1),
                lambda = 0.5)
modelo_2 <- Arima(comercio$TOTAL, 
                  order = c(1, 1, 1),
                  lambda = 0.5)
serie_mensual <- ts(comercio$TOTAL, start = c(2013, 1), frequency = 12)
auto <- auto.arima(serie_mensual, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE, lambda = 0.5)

summary(modelo)
summary(modelo_2)
summary(auto)

comercio$PREDICCIONES <- fitted(modelo)
comercio$PREDICCIONES_2 <- fitted(modelo_2)
comercio$PREDICCIONES_AUTO <-  fitted(auto)

ggplot(comercio, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_AUTO, color = "Modelo ARIMA AUTO"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", 
                                "Modelo ARIMA 2" = "red", "Modelo ARIMA AUTO" = "green")) +
  labs(title = "Comparación de Incidencia Delictiva: Comercio",
       subtitle = "Valores observados vs. Ajustes de los modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# auto

### Test ----
train_data <- comercio[1:n_train, ]
test_data  <- comercio[(n_train+1):n_total, ]

pronostico <- forecast(auto, h = n_test)

reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = as.numeric(pronostico$lower[,2]), 
                  ymax = as.numeric(pronostico$upper[,2])), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Comercio",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)

## Producción ----
n_total <- nrow(produccion)-12
n_train <- n_total-n_test

lambda <- BoxCox.lambda(produccion$TOTAL)
print(lambda)

arima <- produccion %>%
  mutate(TOTAL=sqrt(TOTAL))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
#diff_arima = diff(arima$TOTAL)
#adf.test(diff_arima)
#kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(arima$TOTAL, main = "ACF de Delitos", lag.max = 36)
pacf(arima$TOTAL, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(arima$TOTAL, 
                order = c(0, 1, 0),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(2, 1, 1),
                  lambda = 1)
serie_mensual <- ts(produccion$TOTAL, start = c(2020, 1), frequency = 12)
auto <- auto.arima(serie_mensual, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE, lambda = 0.5)

summary(modelo)
summary(modelo_2)
summary(auto)

produccion$PREDICCIONES <- fitted(modelo)
produccion$PREDICCIONES_2 <- fitted(modelo_2)
produccion$PREDICCIONES_AUTO <-  fitted(auto)

ggplot(produccion, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_AUTO, color = "Modelo ARIMA AUTO"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", 
                                "Modelo ARIMA 2" = "red", "Modelo ARIMA AUTO" = "green")) +
  labs(title = "Comparación de Incidencia Delictiva: Producción",
       subtitle = "Valores observados vs. Ajustes de los modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# modelo_2

### Test ----
train_data <- produccion[1:n_train, ]
test_data  <- produccion[(n_train+1):n_total, ]

pronostico <- forecast(modelo_2, h = n_test)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Producción",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)

## Suministro ----
n_total <- nrow(suministro)-12
n_train <- n_total-n_test

lambda <- BoxCox.lambda(suministro$TOTAL)
print(lambda)

arima <- suministro %>%
  mutate(TOTAL=log(TOTAL+1))

adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
#diff_arima = diff(arima$TOTAL)
#adf.test(diff_arima)
#kpss.test(diff_arima)

par(mfrow = c(1, 2))
acf(arima$TOTAL, main = "ACF de Delitos", lag.max = 36)
pacf(arima$TOTAL, main = "PACF de Delitos", lag.max = 36)

modelo <- Arima(arima$TOTAL, 
                order = c(1, 0, 1),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 0, 1),
                  lambda = 1)
serie_mensual <- ts(arima$TOTAL, start = c(2013, 1), frequency = 12)
auto <- auto.arima(serie_mensual, seasonal = TRUE,
                   stepwise = FALSE, approximation = FALSE, lambda = 1)

summary(modelo)
summary(modelo_2)
summary(auto)

suministro$PREDICCIONES <- exp(fitted(modelo))-1
suministro$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
suministro$PREDICCIONES_AUTO <-  exp(fitted(auto))-1

ggplot(suministro, aes(x = FECHA)) +
  geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
  geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PREDICCIONES_AUTO, color = "Modelo ARIMA AUTO"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", 
                                "Modelo ARIMA 2" = "red", "Modelo ARIMA AUTO" = "green")) +
  labs(title = "Comparación de Incidencia Delictiva: Suministro",
       subtitle = "Valores observados vs. Ajustes de los modelo ARIMA",
       x = "Mes / Año",
       y = "Número total de delitos",
       color = "Referencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2) 
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# auto

### Test ----
train_data <- suministro[1:n_train, ]
test_data  <- suministro[(n_train+1):n_total, ]

pronostico <- forecast(auto, h = n_test)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, test_data$TOTAL)
print(metricas)

plot_validación <- ggplot() +
  geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
  geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
  geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
            size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = test_data$FECHA, 
                  ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                  ymax = exp(as.numeric(pronostico$upper[,2]))-1), 
              fill = "steelblue", alpha = 0.2) +
  labs(title = "Validación del Modelo ARIMA: Suministro",
       subtitle = paste("MAE en Test Set:", round(metricas[1, "MAE"], 2)),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

print(plot_validación)


# Prophet ----
## Posesión ----
datos_prophet <- posesion %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  train_data,
  changepoint.prior.scale = 0.02,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive"
)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

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
  labs(title = "Validación del Modelo Prophet: Posesión",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## Otros ----
datos_prophet <- otros %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  train_data,
  changepoint.prior.scale = 0.5,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive"
)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

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
  labs(title = "Validación del Modelo ARIMA: Otros",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## Transporte ----
datos_prophet <- transporte %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  train_data,
  changepoint.prior.scale = 0.07,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive"
)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

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
  labs(title = "Validación del Modelo Prophet: Transporte",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## Tráfico ----
datos_prophet <- trafico %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  train_data,
  changepoint.prior.scale = 0.06,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive"
)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

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
  labs(title = "Validación del Modelo ARIMA: Tráfico",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## Comercio ----
datos_prophet <- comercio %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

bache_evento <- data.frame(
  holiday = 'bache_comercio',
  ds = seq(as.Date("2018-04-01"), as.Date("2019-01-01"), by = "month"),
  lower_window = -3,
  upper_window = 3
)

modelo <- prophet(
  train_data,
  holidays = bache_evento,
  changepoint.prior.scale = 0.5,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive"
)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)
prophet_plot_components(modelo, forecast_prophet)

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
  labs(title = "Validación del Modelo Prophet: Comercio",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## Producción ----
datos_prophet <- produccion %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  train_data,
  changepoint.prior.scale = 0.0095,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive"
)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)
prophet_plot_components(modelo, forecast_prophet)

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
  labs(title = "Validación del Modelo Prophet: Producción",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## Suministro ----
datos_prophet <- suministro %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

bache_evento <- data.frame(
  holiday = 'bache_suministro',
  ds = seq(as.Date("2019-04-01"), as.Date("2019-06-01"), by = "month"),
  lower_window = -3,
  upper_window = 3
)

modelo <- prophet(
  train_data,
  holidays = bache_evento,
  changepoint.prior.scale = 0.5,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive"
)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)
prophet_plot_components(modelo, forecast_prophet)

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
  labs(title = "Validación del Modelo Prophet: Suministro",
       subtitle = paste("MAE en Test Set:", round(metricas[1, "MAE"], 2)),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
