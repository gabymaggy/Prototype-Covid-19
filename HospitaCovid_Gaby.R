
hospitales <- read.csv("hosp.csv")

View(hospitales)
str(hospitales)

hospitales <- mutate(hospitales, fecha = as.Date(fecha, "%Y-%m-%d"))
str(hospitales)

hospitales.ts <- ts(hospitales[,5], start = 2020, freq=365.25)
hospitales.ts

start(hospitales.ts); frequency(hospitales.ts)

summary(hospitales.ts)

plot(hospitales.ts, ylab = "Personas hospitalizadas", xlab = "2020", 
     main = "Personas Hospitalizadas por covid 19 en la ZMVM")

boxplot(hospitales.ts ~ cycle(hospitales.ts),
        xlab = "Boxplot de personas infectadas",
        main = "Personas Hospitalizadas ZMVM")



plot(hospitales.ts, xlab = "", ylab = "")
title(main = "Serie de Hospitalizacion de ZMVM",
      ylab = "Hospitalización",
      xlab = "Tiempo")


plot(log(hospitales.ts), xlab = "", ylab = "")
title(main = "Log de Serie de Hospitalización",
      ylab = "Log de Hospitalización",
      xlab = "Tiempo")





hist(hospitales$dia, breaks = (seq(0,40)), 
     main = "Hospitalizados de Covid por día en ZMVM",
     xlab = "Día del Mes",
     ylab = "Frecuencia",
     col= "purple")

mediaH <-mean(hospitales$dia)
ggplot(hospitales, aes(dia))+ 
  geom_histogram(binwidth = 2, col="black", fill = "purple") + 
  ggtitle("Hospitalizados de Covid por día", paste("Media=",mediaH)) +
  ylab("Frecuencia") +
  xlab("Día del Mes") +
  geom_vline(xintercept =  mediaH, col = "red", lwd = 1.5, lty =2)+
  theme_light()




p <- ggplot(hospitales, aes(x=fecha, y=camas_intubados_totales)) + 
  geom_line( color="blue") + 
  geom_point() +
  labs(x = "Fecha", 
       y = "Acumulado de casos intubados",
       title = paste("Intubados de COVID-19 en ZMVM:", 
                     format(Sys.time(), 
                            tz="America/Mexico_City", 
                            usetz=TRUE))) +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas
p <- p  + scale_x_date(labels = date_format("%d-%m-%Y")) # paquete scales

p <- p +
  theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
  annotate("text", x = hospitales$fecha[round(dim(hospitales)[1]*0.4)], y = max(hospitales$camas_intubados_totales), colour = "blue", size = 5, label = paste("Última actualización: ", hospitales$camas_intubados_totales[dim(hospitales)[1]]))
p


cdmx <- ggplot(hospitales, aes(x=fecha, y=camas_intubados_cdmx)) + 
  geom_line(stat = "identity") + 
  labs(x = "Fecha", y = "Incidencia (Número de intibados nuevos)",
       title = paste("Intubados de COVID-19 en CDMX:", 
                     format(Sys.time(), 
                            tz="America/Mexico_City", usetz=TRUE))) +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))  # color, Ángulo y estilo de las abcisas y ordenadas

cdmx <- cdmx  + scale_x_date(labels = date_format("%d-%m-%Y")) # paquete scales
cdmx


edomex <- ggplot(hospitales, aes(x=fecha, y=camas_intubados_edomex)) + 
  geom_line(stat = "identity") + 
  labs(x = "Fecha", y = "Incidencia (Número de intubados nuevos)",
       title = paste("Intubados de COVID-19 en EdoMex:", 
                     format(Sys.time(), 
                            tz="America/Mexico_City", usetz=TRUE))) +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))  # color, Ángulo y estilo de las abcisas y ordenadas

edomex <- edomex  + scale_x_date(labels = date_format("%d-%m-%Y")) # paquete scales
edomex




acf(hospitales.ts)

pacf(hospitales.ts)




arima560 = auto.arima(hospitales.ts, stepwise = F,
                      approximation = F,
                      trace = T)


print(arima560)

proy = forecast(arima560,d=1,D=1, h = 12, level = c(12))

plot(proy)
autoplot(proy)

checkresiduals(proy)




