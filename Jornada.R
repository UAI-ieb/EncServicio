
Data$Jornada <- gsub("DIURNO", "Diurno", Data$Jornada)
Data$Jornada <- gsub("VESPERTINA", "Vespertina", Data$Jornada)
Data$Jornada <- gsub("VESPERTINO", "Vespertina", Data$Jornada)
Data$Jornada <- gsub("Vespertino", "Vespertina", Data$Jornada)

# SERVICIO GLOBAL POR JORNADA MÁS MEDICION

JorGlobal=aggregate(Data$neto_SerGen ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorGlobal
colnames(JorGlobal)<-c("medicion","Sede","Jornada","Valor")
write.csv2(JorGlobal,file="Data/JorGlobal.csv")

# ATENCION POR JORNADA MÁS MEDICION

JorAtencion=aggregate(Data$NetoAtencion ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorAtencion
colnames(JorAtencion)<-c("medicion","Sede","Jornada","Valor")
write.csv2(JorAtencion,file="Data/JorAtencion.csv")

# EVAL.DOCENTES POR JORNADA MÁS MEDICION 

JorDocente=aggregate(Data$NetoDocente ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorDocente
colnames(JorDocente)<-c("medicion","Sede","Jornada","Valor")
write.csv2(JorDocente,file="Data/JorDocente.csv")

# INFRAESTRUCTURA POR JORNADA MÁS MEDICION

JorInfra=aggregate(Data$NetoInfra ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorInfra
colnames(JorInfra)<-c("medicion","Sede","Jornada","Valor")
JorInfra=mutate(JorInfra, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorInfra,file="Data/JorInfra.csv")

# SALAS DE CLASES POR JORNADA MÁS MEDICION

JorSClases=aggregate(Data$NetoSClases ~ medicion+Sede+Jornada,data=Data,mean, na.rm=TRUE)
JorSClases
colnames(JorSClases)<-c("medicion","Sede","Jornada","Valor")
JorSClases=filter(JorSClases, Valor != -1)
JorSClases=mutate(JorSClases2, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorSClases,file="Data/JorSClases.csv")

# BAÑOS POR JORNADA MÁS MEDICION

JorBanos=aggregate(Data$NetoBanos ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorBanos
colnames(JorBanos)<-c("medicion","Sede","Jornada","Valor")
JorBanos=filter(JorBanos, Valor != -1)
JorBanos=mutate(JorBanos, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorBanos,file="Data/JorBanos.csv")

# BIBLIOTECA POR JORNADA MÁS MEDICION

JorBiblio=aggregate(Data$NetoBiblio ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorBiblio
colnames(JorBiblio)<-c("medicion","Sede","Jornada","Valor")
JorBiblio=filter(JorBiblio, Valor != 0)
JorBiblio=mutate(JorBiblio, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorBiblio,file="Data/JorBiblio.csv")

# LABORATORIO POR SEDE MÁS MEDICION

JorLab=aggregate(Data$NetoLab ~ medicion+Sede+Jornada,data=Data, mean, na.rm=TRUE)
JorLab
colnames(JorLab)<-c("medicion","Sede","Jornada","Valor")
JorLab=filter(JorLab, Valor != -1)
JorLab=mutate(JorLab, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorLab,file="Data/JorLab.csv")

# CAFETERIA POR JORNADA MÁS MEDICION

JorCaf=aggregate(Data$NetoCafeteria ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorCaf
colnames(JorCaf)<-c("medicion","Sede","Jornada","Valor")
JorCaf=filter(JorCaf, Valor != -1)
JorCaf=mutate(JorCaf, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorCaf,file="Data/JorCaf.csv")

# IEB VIRTUAL POR JORNADA MÁS MEDICION

JorIEBV=aggregate(Data$NetoIEBVirtual ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorIEBV
colnames(JorIEBV)<-c("medicion","Sede","Jornada","Valor")
JorIEBV=filter(JorIEBV, Valor != -1)
JorIEBV=mutate(JorIEBV, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorIEBV,file="Data/JorIEBV.csv")

# EBOOK POR JORNADA MÁS MEDICION

JorEbook=aggregate(Data$NetoEbook ~ medicion+Sede+Jornada, data=Data,mean,na.rm=TRUE)
JorEbook
colnames(JorEbook)<-c("medicion","Sede","Jornada","Valor")
JorEbook=mutate(JorEbook, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorEbook,file="Data/JorEbook.csv")

# WIFI POR JORNADA MÁS MEDICION

JorWifi=aggregate(Data$NetoWifi ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorWifi
colnames(JorWifi)<-c("medicion","Sede","Jornada","Valor")
JorWifi=mutate(JorWifi, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorWifi,file="Data/JorWifi.csv")

# SERVICIO DE SOLICITUDES POR JORNADA Y MEDICION

JorServSol=aggregate(Data$NetoServSolici ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorServSol
colnames(JorServSol)<-c("medicion","Sede","Jornada","Valor")
JorServSol=mutate(JorServSol, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorServSol,file="Data/JorServSol.csv")

# CAJAS POR JORNADA Y MEDICION

JorCajas=aggregate(Data$NetoCajas ~ medicion+Sede+Jornada, data=Data, mean, na.rm=TRUE)
JorCajas
colnames(JorCajas)<-c("medicion","Sede","Jornada","Valor")
JorCajas=filter(JorCajas, Valor != -1)
JorCajas=mutate(JorCajas, colores = ifelse(Valor < 0, "Red","Black"))
write.csv2(JorCajas,file="Data/JorCajas.csv")
