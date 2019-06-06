library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
Data1 <- read_excel("Data/DataServicio.xlsx")
Data = data.frame (Data1)
Data1$Jornada <- gsub("DIURNO", "Diurno", Data1$Jornada)
Data1$Jornada <- gsub("VESPERTINA", "Vespertina", Data1$Jornada)
names(Data1$Jornada)
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
                                                         ifelse(Var1 %in% "Definitivamente NO lo recomendaría", "orangered3", "SkyBlue")))))

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


# IEB VIRTUAL POR SEDE MÁS MEDICION

SedeIEBV=aggregate(Data$NetoIEBVirtual ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeIEBV
colnames(SedeIEBV)<-c("medicion","Sede","Valor")
SedeIEBV=filter(SedeIEBV, Valor != -1)
SedeIEBV=mutate(SedeIEBV, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeIEBV,file="Data/SedeIEBV.csv")

GrafIEBV=ggplot(SedeIEBV, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.01 , color= SedeIEBV$colores, size=3) +
  geom_point(size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Cafetería por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafIEBV + facet_grid(. ~ Sede )

# ebook POR SEDE MÁS MEDICION

SedeEbook=aggregate(Data$NetoEbook ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeEbook
colnames(SedeEbook)<-c("medicion","Sede","Valor")
SedeEbook=mutate(SedeEbook, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeEbook,file="Data/SedeEbook.csv")

GrafEbook=ggplot(SedeEbook, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.01 , color= SedeEbook$colores, size=3) +
  geom_point(size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Cafetería por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafEbook + facet_grid(. ~ Sede )

# WIFI POR SEDE MÁS MEDICION

SedeWifi=aggregate(Data$NetoWifi ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeWifi
colnames(SedeWifi)<-c("medicion","Sede","Valor")
SedeWifi=mutate(SedeWifi, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeWifi,file="Data/SedeWifi.csv")

GrafWifi=ggplot(SedeWifi, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.01 , color= SedeWifi$colores, size=3) +
  geom_point(size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción con Cafetería por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafWifi + facet_grid(. ~ Sede )

# SERVICIO DE SOLICITUDES POR SEDE Y MEDICION

SedeServSol=aggregate(Data$NetoServSolici ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeServSol
colnames(SedeServSol)<-c("medicion","Sede","Valor")
SedeServSol=mutate(SedeServSol, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeServSol,file="Data/SedeServSol.csv")

GrafSolicit=ggplot(SedeServSol, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.01 , color= SedeServSol$colores, size=3) +
  geom_point(size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción Neta con Serv.Solicitudes por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafSolicit + facet_grid(. ~ Sede )

# CAJAS POR SEDE Y MEDICION

SedeCajas=aggregate(Data$NetoCajas ~ medicion+Sede, data=Data, mean, na.rm=TRUE)
SedeCajas
colnames(SedeCajas)<-c("medicion","Sede","Valor")
SedeCajas=filter(SedeCajas, Valor != -1)
SedeCajas=mutate(SedeCajas, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(SedeCajas,file="Data/SedeCajas.csv")

GrafCajas=ggplot(SedeCajas, aes(x=medicion, y=Valor,group = Sede, colour=Sede, label = paste(round(Valor*100, 0),"%"))) +
  geom_line(size=1)  + 
  geom_text(nudge_x = 0, nudge_y = 0.01 , color= SedeCajas$colores, size=3) +
  geom_point(size=2, shape=21, fill="white") + 
  theme (axis.text.x = element_text (angle = 90, vjust = 0.5))+
  labs(
    x = "Mediciones",
    y = "% de Satisfaccion Neta",
    title = "Gráfico XX: Satisfacción Neta con Cajas por Sede y Medición",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafCajas + facet_grid(. ~ Sede )

#_________________________________________________________________________________________________
#Mejora docente 2018-2

MejDocente=select(Data, medicion, Sede, MejDocente_Metodologia, MejDocente_Vocacion, MejDocente_Comunicacion, 
                  MejDocente_Empatia, MejDocente_Contenidos)
colnames(MejDocente)<-c("medicion","Sede", "Metodología", "Vocación", "Comunicación", 
                        "Empatía", "Contenidos")

MejDocente2=filter(MejDocente, medicion == "2018-2")
MejDocente3=MejDocente2 %>%
  gather(Aspecto, Valor, Metodología:Contenidos)
MejDocente3=filter(MejDocente3, Valor != "N/A")

MejDocente3=mutate(MejDocente3, Valor2 =ifelse(MejDocente3$Valor %in% "Sí", 1,0))                                      
colnames(MejDocente3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejDocente4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejDocente3, mean, na.rm=TRUE)
colnames(MejDocente4)<-c("medicion","Sede","Aspecto","Valor")
MejDocente4
write.csv2(MejDocente4,file="Data/MejDocente4.csv")

GrafMejDoc= ggplot(MejDocente4, aes(MejDocente4$Valor, MejDocente4$Aspecto, label = paste(round(MejDocente4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejDocente4$Aspecto, xend = MejDocente4$Valor, yend = MejDocente4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas en desempeño docente",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejDoc + facet_grid( .~ MejDocente4$Sede )

#Mejora INFRAESTRUCTURA 2018-2

MejInfra=select(Data, medicion, Sede, MejInfra_Banos,MejInfra_AseoBanos,MejInfra_SalaClases,MejInfra_Ascensores,MejInfra_Cafeteria, MejInfra_EspComunes)
colnames(MejInfra)<-c("medicion","Sede", "Baños","AseoBaños","SalaClases","Ascensores","Cafetería", "EspaciosComunes")

MejInfra2=filter(MejInfra, medicion == "2018-2")
MejInfra3=MejInfra2 %>%
  gather(Aspecto, Valor, Baños:EspaciosComunes)
MejInfra3=filter(MejInfra3, Valor != "N/A")

MejInfra3=mutate(MejInfra3, Valor2 =ifelse(MejInfra3$Valor %in% "Sí", 1,0))                                      
colnames(MejInfra3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejInfra4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejInfra3, mean, na.rm=TRUE)
colnames(MejInfra4)<-c("medicion","Sede","Aspecto","Valor")
MejInfra4
write.csv2(MejInfra4,file="Data/MejInfra4.csv")  

GrafMejInfra= ggplot(MejInfra4, aes(MejInfra4$Valor, MejInfra4$Aspecto, label = paste(round(MejInfra4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejInfra4$Aspecto, xend = MejInfra4$Valor, yend = MejInfra4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas en Infraestructura",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejInfra + facet_grid( .~ MejInfra4$Sede )


#Mejora BIBLIOTECA 2018-2

MejBiblio=select(Data, medicion, Sede, MejBiblio_MasVariedad,MejBiblio_Infraestructura,MejBiblio_Horarios)
colnames(MejBiblio)<-c("medicion","Sede", "MásVariedad","Infraestructura","Horarios")

MejBiblio2=filter(MejBiblio, medicion == "2018-2")
MejBiblio3=MejBiblio2 %>%
  gather(Aspecto, Valor, MásVariedad:Horarios)
MejBiblio3=filter(MejBiblio3, Valor != "N/A")

MejBiblio3=mutate(MejBiblio3, Valor2 =ifelse(MejBiblio3$Valor %in% "Sí", 1,0))                                      
colnames(MejBiblio3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejBiblio4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejBiblio3, mean, na.rm=TRUE)
colnames(MejBiblio4)<-c("medicion","Sede","Aspecto","Valor")
MejBiblio4
write.csv2(MejBiblio4,file="Data/MejBiblio4.csv") 

GrafMejBiblio= ggplot(MejBiblio4, aes(MejBiblio4$Valor, MejBiblio4$Aspecto, label = paste(round(MejBiblio4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejBiblio4$Aspecto, xend = MejBiblio4$Valor, yend = MejBiblio4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas en Biblioteca",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejBiblio + facet_grid( .~ MejBiblio4$Sede )

#Mejora BIBLIOTECA VIRTUAL (EBOOK)2018-2

MejEbook=select(Data, medicion, Sede, MejEbook_VariedadTxT, MejEbook_Usabilidad, MejEbook_MasDescargas)
colnames(MejEbook)<-c("medicion","Sede", "MásVariedad","Usabilidad","MásMaterialDescargable")

MejEbook2=filter(MejEbook, medicion == "2018-2")

MejEbook3=MejEbook2 %>%
  gather(Aspecto, Valor, MásVariedad:MásMaterialDescargable)

MejEbook3=filter(MejEbook3, Valor != "N/A")

MejEbook3=mutate(MejEbook3, Valor2 =ifelse(MejEbook3$Valor %in% "Sí", 1,0))                                      
colnames(MejEbook3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejEbook4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejEbook3, mean, na.rm=TRUE)
colnames(MejEbook4)<-c("medicion","Sede","Aspecto","Valor")
MejEbook4
write.csv2(MejEbook4,file="Data/MejEbook4.csv") 

GrafMejEbook= ggplot(MejEbook4, aes(MejEbook4$Valor, MejEbook4$Aspecto, label = paste(round(MejEbook4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejEbook4$Aspecto, xend = MejEbook4$Valor, yend = MejEbook4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas en Biblioteca",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejEbook + facet_grid( .~ MejEbook4$Sede)


#Mejora LABORATORIO 2018-2

MejLab=select(Data, medicion, Sede, MejLabo_EstadoPCs, MejLabo_NumPCs, MejLab_Espacio, MejLab_MasProgramas, MejLab_Amabilidad)
colnames(MejLab)<-c("medicion","Sede", "EstadoPCs","NúmeroPCs","MásEspacio","MásProgramas","MásAmabilidad")

MejLab2=filter(MejLab, medicion == "2018-2")

MejLab3=MejLab2 %>%
  gather(Aspecto, Valor, EstadoPCs:MásAmabilidad)

MejLab3=filter(MejLab3, Valor != "N/A")

MejLab3=mutate(MejLab3, Valor2 =ifelse(MejLab3$Valor %in% "Sí", 1,0))                                      
colnames(MejLab3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejLab4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejLab3, mean, na.rm=TRUE)
colnames(MejLab4)<-c("medicion","Sede","Aspecto","Valor")
MejLab4
write.csv2(MejLab4,file="Data/MejLab4.csv") 

GrafMejLab= ggplot(MejLab4, aes(MejLab4$Valor, MejLab4$Aspecto, label = paste(round(MejLab4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejLab4$Aspecto, xend = MejLab4$Valor, yend = MejLab4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas en Biblioteca",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejLab + facet_grid( .~ MejLab4$Sede )

#Mejora SALA DE CLASES 2018-2

MejSClases=select(Data, medicion, Sede, MejSalas_AireAcond, MejSalas_Espacio, MejSalas_Sillas, MejSalas_Mesas, MejSalas_Insumos, MejSalas_Pizarra)
colnames(MejSClases)<-c("medicion","Sede", "AireAcondicionado","MásEspacio","Sillas","Mesas","Insumos","Pizarras")

MejSClases2=filter(MejSClases, medicion == "2018-2")

MejSClases3=MejSClases2 %>%
  gather(Aspecto, Valor, AireAcondicionado:Pizarras)

MejSClases3=filter(MejSClases3, Valor != "N/A")

MejSClases3=mutate(MejSClases3, Valor2 =ifelse(MejSClases3$Valor %in% "Sí", 1,0))                                      
colnames(MejSClases3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejSClases4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejSClases3, mean, na.rm=TRUE)
colnames(MejSClases4)<-c("medicion","Sede","Aspecto","Valor")
MejSClases4
write.csv2(MejSClases4,file="Data/MejSClases4.csv") 

GrafMejSClases= ggplot(MejSClases4, aes(MejSClases4$Valor, MejSClases4$Aspecto, label = paste(round(MejSClases4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejSClases4$Aspecto, xend = MejSClases4$Valor, yend = MejSClases4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas en Salas de Clases",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejSClases + facet_grid( .~ MejSClases4$Sede )


#Mejora BAÑOS 2018-2

MejBanos=select(Data, medicion, Sede, MejBanos_Espacio, MejBanos_Aseo, MejBanos_Cantidad, MejBanos_Insumos, MejBanos_Cerraduras, MejBanos_Ventilacion, 
                MejBanos_iluminacion)
colnames(MejBanos)<-c("medicion","Sede", "MásEspacio","AseoBaños","Cantidad","Insumos","Cerraduras","Ventilación","Iluminación")

MejBanos2=filter(MejBanos, medicion == "2018-2")

MejBanos3=MejBanos2 %>%
  gather(Aspecto, Valor, MásEspacio:Iluminación)

MejBanos3=filter(MejBanos3, Valor != "N/A")

MejBanos3=mutate(MejBanos3, Valor2 =ifelse(MejBanos3$Valor %in% "Sí", 1,0))                                      
colnames(MejBanos3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejBanos4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejBanos3, mean, na.rm=TRUE)
colnames(MejBanos4)<-c("medicion","Sede","Aspecto","Valor")
MejBanos4
write.csv2(MejBanos4,file="Data/MejBanos4.csv") 

GrafMejBanos= ggplot(MejBanos4, aes(MejBanos4$Valor, MejBanos4$Aspecto, label = paste(round(MejBanos4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejBanos4$Aspecto, xend = MejBanos4$Valor, yend = MejBanos4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas en Baños",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejBanos + facet_grid( .~ MejBanos4$Sede )


#Mejora CAFETERÍA 2018-2

MejCaf=select(Data, medicion, Sede, MejCafet_Variedad, MejCafet_Precios, MejCafet_Amplitud, MejCafet_Temperatura, MejCafet_Disponibilidad)
colnames(MejCaf)<-c("medicion","Sede", "MásVariedad","MejoresPrecios","Amplitud","Temperatura","StockProductos")

MejCaf2=filter(MejCaf, medicion == "2018-2")

MejCaf3=MejCaf2 %>%
  gather(Aspecto, Valor, MásVariedad:StockProductos)

MejCaf3=filter(MejCaf3, Valor != "N/A")

MejCaf3=mutate(MejCaf3, Valor2 =ifelse(MejCaf3$Valor %in% "Sí", 1,0))                                      
colnames(MejCaf3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejCaf4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejCaf3, mean, na.rm=TRUE)
colnames(MejCaf4)<-c("medicion","Sede","Aspecto","Valor")
MejCaf4
write.csv2(MejCaf4,file="Data/MejCaf4.csv")

GrafMejCaf= ggplot(MejCaf4, aes(MejCaf4$Valor, MejCaf4$Aspecto, label = paste(round(MejCaf4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejCaf4$Aspecto, xend = MejCaf4$Valor, yend = MejCaf4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "Porcentaje de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas en Cafetería",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejCaf + facet_grid( .~ MejCaf4$Sede )


#Mejora WIFI 2018-2

MejWifi=select(Data, medicion, Sede, MejWIFI_EstaSenal, MejWIFI_Velocidad, MejWIFI_Cobertura)
colnames(MejWifi)<-c("medicion","Sede", "MejorSeñal","MejorVelocidad","MayorCobertura")

MejWifi2=filter(MejWifi, medicion == "2018-2")

MejWifi3=MejWifi2 %>%
  gather(Aspecto, Valor, MejorSeñal:MayorCobertura)

MejWifi3=filter(MejWifi3, Valor != "N/A")

MejWifi3=mutate(MejWifi3, Valor2 =ifelse(MejWifi3$Valor %in% "Sí", 1,0))                                      
colnames(MejWifi3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejWifi4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejWifi3, mean, na.rm=TRUE)
colnames(MejWifi4)<-c("medicion","Sede","Aspecto","Valor")
MejWifi4
write.csv2(MejWifi4,file="Data/MejWifi4.csv")

GrafMejWifi= ggplot(MejWifi4, aes(MejWifi4$Valor, MejWifi4$Aspecto, label = paste(round(MejWifi4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejWifi4$Aspecto, xend = MejWifi4$Valor, yend = MejWifi4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "% de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas sobre servicio WIFI",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejWifi + facet_grid( .~ MejWifi4$Sede )

#Mejora CAJAS 2018-2

MejCaja=select(Data, medicion, Sede, MejCaja_TiempoEspera,MejCaja_TiempoAtencion,MejCaja_Amabilidad,MejCaja_Informacion,
               MejCaja_InfodePagos,MejCaja_Horarios,MejCaja_PagosInternet)
colnames(MejCaja)<-c("medicion","Sede","TiempoEspera","TiempoAtención","MayorAmabilidad","MásInformación","InfoSobrePagos","Horarios","PagosporInternet")

MejCaja2=filter(MejCaja, medicion == "2018-2")

MejCaja3=MejCaja2 %>%
  gather(Aspecto, Valor, TiempoEspera:PagosporInternet)

MejCaja3=filter(MejCaja3, Valor != "N/A")

MejCaja3=mutate(MejCaja3, Valor2 =ifelse(MejCaja3$Valor %in% "Sí", 1,0))                                      
colnames(MejCaja3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejCaja4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejCaja3, mean, na.rm=TRUE)
colnames(MejCaja4)<-c("medicion","Sede","Aspecto","Valor")
MejCaja4
write.csv2(MejCaja4,file="Data/MejCaja4.csv")

GrafMejCaja= ggplot(MejCaja4, aes(MejCaja4$Valor, MejCaja4$Aspecto, label = paste(round(MejCaja4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejCaja4$Aspecto, xend = MejCaja4$Valor, yend = MejCaja4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "% de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas sobre Cajas",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejCaja + facet_grid( .~ MejCaja4$Sede )


#Mejora SERVICIO DE SOLICITUDES 2018-2

MejServSolic=select(Data, medicion, Sede,MejServ_Rapidez, MejServ_Usabilidad, MejServ_MasInfo, MejServ_CalidadRespuestas)
colnames(MejServSolic)<-c("medicion","Sede","MayorRapidez","Usabilidad","MayorInformación","CalidadRespuestas")

MejServSolic2=filter(MejServSolic, medicion == "2018-2")

MejServSolic3=MejServSolic2 %>%
  gather(Aspecto, Valor, MayorRapidez:CalidadRespuestas)

MejServSolic3=filter(MejServSolic3, Valor != "N/A")

MejServSolic3=mutate(MejServSolic3, Valor2 =ifelse(MejServSolic3$Valor %in% "Sí", 1,0))                                      
colnames(MejServSolic3)<-c("medicion","Sede","Aspecto","Respuesta","Valor")

MejServSolic4=aggregate(Valor ~ medicion+Sede+Aspecto, data=MejServSolic3, mean, na.rm=TRUE)
colnames(MejServSolic4)<-c("medicion","Sede","Aspecto","Valor")
MejServSolic4
write.csv2(MejServSolic4,file="Data/MejServSolic4.csv")

GrafMejServSolic= ggplot(MejServSolic4, aes(MejServSolic4$Valor, MejServSolic4$Aspecto, label = paste(round(MejServSolic4$Valor*100, 0), sep="","%"))) +
  geom_segment(aes(x = 0, y = MejServSolic4$Aspecto, xend = MejServSolic4$Valor, yend = MejServSolic4$Aspecto), color = "SkyBlue", size=3) +
  geom_point(color = "RoyalBlue4" , size = 10) +
  geom_text(nudge_x = 0.02 , color= "white",size=3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(
    x = "% de Satisfacción",
    y = "Mediciones",
    title = "Gráfico 1: Mejoras sugeridas sobre Cajas",
    subtitle = "Encuesta de Servicios IGS",
    caption = "\nUnidad de Análisis Institucional"
  )
GrafMejServSolic + facet_grid( .~ MejServSolic4$Sede )









