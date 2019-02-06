library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
Data1 <- read_excel("Data/DataServicio.xlsx")
Data = data.frame (Data1)

attach(Data)


# Recodificación de valor 9 en NA
Data$neto_SerGen [Data$neto_SerGen == 9] <-NA
Data$NetoAtencion[Data$NetoAtencion == 9] <-NA
Data$NetoDocente [Data$NetoDocente == 9] <-NA
Data$NetoInfra [Data$NetoInfra  == 9] <-NA
Data$NetoBiblio [Data$NetoBiblio  == 9] <-NA
Data$NetoEbook[Data$NetoEbook == 9] <-NA
Data$NetoLab [Data$NetoLab == 9] <-NA
Data$NetoSClases [Data$NetoSClases  == 9] <-NA
Data$NetoBanos [Data$NetoBanos  == 9] <-NA
Data$NetoCafeteria [Data$NetoCafeteria == 9] <-NA
Data$NetoIEBVirtual [Data$NetoIEBVirtual == 9] <-NA
Data$NetoWifi [Data$NetoWifi == 9] <-NA
Data$NetoServSolici [Data$NetoServSolici == 9] <-NA
Data$NetoCajas [Data$NetoCajas == 9] <-NA
Data$Recomendar_IGS [Data$Recomendar_IGS  == 9] <-NA
Data$Recomendar_IGS [Data$Recomendar_IGS  == "S/I"] <-NA

# Recodificación de valor no valido en variables de Satisfacion Total (6 y 7)
Data$Ind_NotaSerGen [Data$Ind_NotaSerGen > 1] <-NA
Data$Ind_NotaDocentes[Data$Ind_NotaDocentes > 1] <-NA
Data$Ind_NotaAtencion [Data$Ind_NotaAtencion > 1] <-NA
Data$Ind_NotaInfra [Data$Ind_NotaInfra  > 1] <-NA
Data$Ind_NotaBiblio [Data$Ind_NotaBiblio   > 1] <-NA
Data$Ind_NotaEbook[Data$Ind_NotaEbook  > 1] <-NA
Data$Ind_NotaLab [Data$Ind_NotaLab  > 1] <-NA
Data$Ind_NotaSalaClases [Data$Ind_NotaSalaClases > 1] <-NA
Data$Ind_NotaBanos [Data$Ind_NotaBanos  > 1] <-NA
Data$NetoCafeteria [Data$NetoCafeteria  > 1] <-NA
Data$Ind_NotaCafeteria [Data$Ind_NotaCafeteria > 1] <-NA
Data$Ind_IEBVirtual [Data$Ind_IEBVirtual > 1] <-NA
Data$Ind_NotaWIFI [Data$Ind_NotaWIFI > 1] <-NA
Data$Ind_NotaCajas [Data$Ind_NotaCajas > 1] <-NA 
Data$Ind_NotaServSolicitudes [Data$Ind_NotaServSolicitudes > 1] <-NA

#DATA FRAME DE SATISFACCION TOTAL (% DE ALUMNOS EN NOTA 6 Y 7)

SatisTot=aggregate(Data[c("Ind_NotaSerGen","Ind_NotaAtencion","Ind_NotaDocentes","Ind_NotaInfra","Ind_NotaBiblio","Ind_NotaEbook",
                          "Ind_NotaLab","Ind_NotaSalaClases","Ind_NotaBanos","Ind_NotaCafeteria","Ind_IEBVirtual",
                          "Ind_NotaWIFI","Ind_NotaCajas","Ind_NotaServSolicitudes")],Data["medicion"], na.rm=TRUE, mean, data=Data)

detach(Data)

attach(SatisTot)
SatisTot=mutate(SatisTot, PorSerGen = Ind_NotaSerGen * 100,PorDocente=Ind_NotaDocentes*100 ,PorAtencion=Ind_NotaAtencion*100,
                PorInfra=Ind_NotaInfra*100,PorBiblio=Ind_NotaBiblio*100,PorEbook=Ind_NotaEbook*100,
                PorLab=Ind_NotaLab*100,PorSClases=Ind_NotaSalaClases*100,PorBanos=Ind_NotaBanos*100,
                PorCafeteria=Ind_NotaCafeteria*100,PorIEBVirtual=Ind_IEBVirtual*100,PorWifi=Ind_NotaWIFI*100,
                PorCajas=Ind_NotaCajas*100,PorSolicitudes=Ind_NotaServSolicitudes*100)
SatisTot

#Redondear Valores en Porcentajes
SatisTot=SatisTot %>% mutate_at(vars(starts_with("Por")), funs(round(., 1)))
SatisTot

#Asignar NA donde no hay medición
SatisTot$PorLab [SatisTot$PorLab == 0] <-NA
Satistot$PorSClases [SatisTot$PorSClases == 0] <-NA
SatisTot$PorBanos [SatisTot$PorBanos == 0] <-NA
SatisTot$PorCafeteria [SatisTot$PorCafeteria == 0] <-NA
SatisTot$PorIEBVirtual [SatisTot$PorIEBVirtual == 0] <-NA
SatisTot$PorCajas [SatisTot$PorCajas == 0] <-NA
SatisTot$PorSolicitudes [SatisTot$PorSolicitudes == 0] <-NA

#Se añade nueva columna con tipo de Satisfacción
SatisTot=cbind(SatisTot,TipoSat="Satisfacción (6-7)")

#Nueva Data FRame
SatisTot2 <- select (SatisTot, -c(Ind_NotaSerGen:Ind_NotaServSolicitudes))

detach(SatisTot)

#DATA FRAME DE SATIFACCION NETA (Dif entre % DE ALUMNOS con NOTA 6 Y 7 menos aquellos con notas 1 a 4)

attach(Data)
SatisNeta=aggregate(Data[c("neto_SerGen","NetoAtencion","NetoDocente","NetoInfra","NetoBiblio","NetoEbook",
                           "NetoLab","NetoSClases","NetoBanos","NetoCafeteria","NetoIEBVirtual",
                           "NetoWifi","NetoCajas","NetoServSolici")],Data["medicion"], na.rm=TRUE, mean, data=Data)

SatisNeta=mutate(SatisNeta, PorSerGen = neto_SerGen * 100,PorAtencion=NetoAtencion*100,PorDocente=NetoDocente*100,
                 PorInfra=NetoInfra*100,PorBiblio=NetoBiblio*100,PorEbook=NetoEbook*100,
                 PorLab=NetoLab*100,PorSClases=NetoSClases*100,PorBanos=NetoBanos*100,
                 PorCafeteria=NetoCafeteria*100,PorIEBVirtual=NetoIEBVirtual*100,PorWifi=NetoWifi*100,
                 PorCajas=NetoCajas*100,PorSolicitudes=NetoServSolici*100)

detach(Data)

#Redondear Valors en Porcentajes
SatisNeta=SatisNeta %>% mutate_at(vars(starts_with("Por")), funs(round(., 1)))

#Asignar NA en años donde no hay medición
SatisNeta$PorLab [SatisNeta$PorLab == -100] <-NA
SatisNeta$PorSClases [SatisNeta$PorSClases == -100] <-NA
SatisNeta$PorBanos [SatisNeta$PorBanos == -100] <-NA
SatisNeta$PorCafeteria [SatisNeta$PorCafeteria == -100] <-NA
SatisNeta$PorIEBVirtual [SatisNeta$PorIEBVirtual == -100] <-NA
SatisNeta$PorCajas [SatisNeta$PorCajas == -100] <-NA
SatisNeta$PorSolicitudes [SatisNeta$PorSolicitudes == -100] <-NA

#Agregar nueva columna con valor por defecto
SatisNeta=cbind(SatisNeta,TipoSat="Satisfacción Neta")
#Nueva Data FRame
SatisNeta2 <- select (SatisNeta, -c(neto_SerGen:NetoServSolici))

#rm(Sergen)

DataSatis=rbind(SatisNeta2,SatisTot2)
save(DataSatis, file = "Data/DataSatis.RData")


#contar NA por Variable
#sapply(DataSatis, function(x) sum(is.na(x)))

DF.Infra=select(SatisNeta2,medicion,PorInfra,PorBiblio,PorLab,PorSClases,PorBanos,PorCafeteria)
colnames(DF.Infra)<-c("medicion","Infraestructura","Biblioteca","Laboratorio","Sala Clases","Baños","Cafetería")

#sapply(DF.Infra2, function(x) sum(is.na(x)))

DF.Infra2=DF.Infra %>%
  gather(Tipo, Valor, Infraestructura:Cafetería)

#Filtro de base segun valor diferente a NA
#DF.Infra2$valor [DF.Infra2$valor == 0] <-NA
DF.Infra3=DF.Infra2[!is.na(DF.Infra2$Valor),]

write.csv2(DF.Infra3,file="Data/Infra1.csv")

DF.Digit=select(SatisNeta2,medicion,PorIEBVirtual,PorEbook,PorWifi)
colnames(DF.Digit)<-c("medicion","IEBVirtual","Ebook","Wifi")

DF.Digit2=DF.Digit %>%
  gather(Tipo, Valor, IEBVirtual:Wifi)

DF.Digit3=DF.Digit2[!is.na(DF.Digit2$Valor),]
DF.Digit3=mutate(DF.Digit3, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(DF.Digit3,file="Data/Digit.csv")


DF.OtrosServ=select(SatisNeta2,medicion,PorCajas,PorSolicitudes)
colnames(DF.OtrosServ)<-c("medicion","Cajas","Serv.Solicitudes")
DF.OtrosServ2=DF.OtrosServ %>%
  gather(Tipo, Valor, Cajas:Serv.Solicitudes)
DF.OtrosServ3=DF.OtrosServ2[!is.na(DF.OtrosServ2$Valor),]
write.csv2(DF.OtrosServ3,file="Data/OtrosServ.csv")

# VARIABLE DISPOSICIÓN DE RECOMENDAR

# GRÁFICO 1
Data2=subset(Data, Recomendar_IGS > 0)
DataRecom=Data[!is.na(Data$Recomendar_IGS),]

table(DataRecom$Recomendar_IGS)

t.Recom = data.frame (DataRecom)

t.Recom1= table (t.Recom$Recomendar_IGS ,t.Recom$medicion )

t.Recom2=prop.table(t.Recom1,2)
t.Recom3 = data.frame (t.Recom2)
t.Recom3=mutate(t.Recom3,Valor=Freq*100)
t.Recom3=mutate(t.Recom3,Valor2 = paste(round(Valor,digits=0),sep ="","%"))


t.Recom3=mutate(t.Recom3, colores = ifelse(Var1 %in% "Probablemente SI lo recomendaría", "olivedrab3",
                                           ifelse(Var1 %in% "Probablemente NO lo recomendaría", "orangered1",
                                                  ifelse(Var1 %in% "Definitivamente SI lo recomendaría", "olivedrab4",
                                                         ifelse(Var1 %in% "Definitivamente NO lo recomendaría", "orangered3", "yellow2")))))

t.Recom3$colores <- factor(t.Recom3$colores)
t.Recom3=mutate(t.Recom3, Var1 = ifelse(Var1 %in% "Probablemente SI lo recomendaría", "Probablemente SI",
                                        ifelse(Var1 %in% "Probablemente NO lo recomendaría", "Probablemente NO",
                                               ifelse(Var1 %in% "Definitivamente SI lo recomendaría", "Definitivamente SI",
                                                      ifelse(Var1 %in% "Definitivamente NO lo recomendaría", "Definitivamente NO", "NO sé")))))

head(t.Recom3)

write.csv2(t.Recom3,file="Data/t.Recom1.csv")

# GRÁFICO 2 de RECOMENDACION

t.Recom3b=select(Data,medicion,Sede,Recomendar_IGS) 

t.Recom3c = data.frame (t.Recom3b)
t.Recom3c=mutate(t.Recom3c, CatRecom =ifelse(t.Recom3c$Recomendar_IGS %in% "Probablemente SI lo recomendaría", 1,
                                             ifelse(t.Recom3c$Recomendar_IGS %in% "Probablemente NO lo recomendaría",0,                                               
                                                    ifelse(t.Recom3c$Recomendar_IGS %in% "Definitivamente SI lo recomendaría", 1,
                                                           ifelse(t.Recom3c$Recomendar_IGS %in% "Definitivamente SI lo recomendaría",0,0)))))



t.Recom3d=aggregate(t.Recom3c$CatRecom ~ t.Recom3c$medicion+Sede, data=t.Recom3c, mean, na.rm=TRUE)
t.Recom3d
colnames(t.Recom3d)<-c("medicion","Sede","Valor")

#t.Recom3d=mutate(t.Recom3d,t.Recom3d$CatRecom=NetoServSolici*100)
t.Recom2=t.Recom3d %>% filter(t.Recom3d$Valor > 0)
write.csv2(t.Recom2,file="Data/t.Recom2.csv")

GrafRecom2= ggplot(t.Recom2, aes(t.Recom2$Valor, t.Recom2$medicion, label = paste(round(t.Recom2$Valor*100, 0), "%"))) +
  geom_segment(aes(x = 0, y = t.Recom2$medicion, xend = t.Recom2$Valor, yend = t.Recom2$medicion), color = rgb(1, 0, 0, 0.4), size=3) +
  geom_point(color = rgb(1, 0, 0, 0.6) , size = 13) +
  geom_text(nudge_x = 0 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Indicadores Globales de Satisfacción General",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafRecom2 + facet_grid( .~ t.Recom2$Sede )



# ATENCION POR SEDE MÁS MEDICION

SedeAtencion=aggregate(Data$NetoAtencion ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeAtencion
colnames(SedeAtencion)<-c("medicion","Sede","Valor")
write.csv2(SedeAtencion,file="Data/SedeAtencion.csv")

GrafAtencion=ggplot(SedeAtencion, aes(x=medicion, y=Valor, group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1.0)  + 
  geom_text(nudge_x = 0 , color= "black" ,size=3) +
  geom_point( size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Atención por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafAtencion + facet_grid(. ~ Sede )

# SERVICIO DE GLOBAL POR SEDE MÁS MEDICION 

SedeGlobal=aggregate(Data$neto_SerGen ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeGlobal
colnames(SedeGlobal)<-c("medicion","Sede","Valor")
write.csv2(SedeGlobal,file="Data/SedeGlobal.csv")

GrafGlobal=ggplot(SedeGlobal, aes(x=medicion, y=Valor, group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1.0)  + 
  geom_text(nudge_x = 1,nudge_y = 0.01 , color= "black" ,size=3) +
  geom_point( size=0.5, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Ev.Global por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafGlobal + facet_grid(. ~ Sede )

# EVAL DOCENTES POR SEDE MÁS MEDICION 

SedeDocente=aggregate(Data$NetoDocente ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeDocente
colnames(SedeDocente)<-c("medicion","Sede","Valor")
write.csv2(SedeDocente,file="Data/SedeDocente.csv")

GrafDocente=ggplot(SedeDocente, aes(x=medicion, y=Valor, group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1.0)  + 
  geom_text(nudge_x = 0, nudge_y = 0.03 , color= "black" ,size=3) +
  geom_point( size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Docentes por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafDocente + facet_grid(. ~ Sede )

# INFRAESTRUCTURA POR SEDE MÁS MEDICION

SedeInfra=aggregate(Data$NetoInfra ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeInfra
colnames(SedeInfra)<-c("medicion","Sede","Valor")
SedeInfra=mutate(SedeInfra, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeInfra,file="Data/SedeInfra.csv")

GrafInfra=ggplot(SedeInfra, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.03 , color= SedeInfra$colores, size=3) +
  geom_point( size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Infraestructura por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafInfra + facet_grid(. ~ Sede )







# SALAS DE CLASES POR SEDE MÁS MEDICION

SedeSClases=aggregate(Data$NetoSClases ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeSClases
colnames(SedeSClases)<-c("medicion","Sede","Valor")
SedeSClases2=filter(SedeSClases, Valor != -1)
SedeSClases2=mutate(SedeSClases2, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeSClases2,file="Data/SedeSClases.csv")

GrafSClases=ggplot(SedeSClases2, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.03 , color= SedeSClases2$colores, size=3) +
  geom_point( size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Sala de Clases por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafSClases + facet_grid(. ~ Sede )

# BIBLIOTECA POR SEDE MÁS MEDICION

SedeBiblio=aggregate(Data$NetoBiblio ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeBiblio
colnames(SedeBiblio)<-c("medicion","Sede","Valor")
SedeBiblio=filter(SedeBiblio, Valor != 0)
SedeBiblio=mutate(SedeBiblio, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeBiblio,file="Data/SedeBiblio.csv")

GrafBiblio=ggplot(SedeBiblio, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.03 , color= SedeBiblio$colores, size=3) +
  geom_point( size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Biblioteca por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafBiblio + facet_grid(. ~ Sede )

# BAÑOS POR SEDE MÁS MEDICION

SedeBanos=aggregate(Data$NetoBanos ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeBanos
colnames(SedeBanos)<-c("medicion","Sede","Valor")
SedeBanos=filter(SedeBanos, Valor != -1)
SedeBanos=mutate(SedeBanos, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeBanos,file="Data/SedeBanos.csv")

GrafBanos=ggplot(SedeBanos, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.03 , color= SedeBanos$colores, size=3) +
  geom_point( size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Baños por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafBanos + facet_grid(. ~ Sede )

# LABORATORIO POR SEDE MÁS MEDICION

SedeLab=aggregate(Data$NetoLab ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeLab
colnames(SedeLab)<-c("medicion","Sede","Valor")
SedeLab=filter(SedeLab, Valor != -1)
SedeLab=mutate(SedeLab, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeLab,file="Data/SedeLab.csv")

GrafLab=ggplot(SedeLab, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.03 , color= SedeLab$colores, size=3) +
  geom_point( size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Laboratorio por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafLab + facet_grid(. ~ Sede )

# CAFETERIA POR SEDE MÁS MEDICION

SedeCaf=aggregate(Data$NetoCafeteria ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeCaf
colnames(SedeCaf)<-c("medicion","Sede","Valor")
SedeCaf=filter(SedeCaf, Valor != -1)
SedeCaf=mutate(SedeCaf, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeCaf,file="Data/SedeCaf.csv")

GrafCaf=ggplot(SedeCaf, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.03 , color= SedeCaf$colores, size=3) +
  geom_point(size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Cafetería por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafCaf + facet_grid(. ~ Sede )