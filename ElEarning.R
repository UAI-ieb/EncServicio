library(readxl)
library(tidyverse)
library(dplyr)
library(plyr)
library(scales)
library(tidyr)
library(ggplot2)

df_virtual1 <- read_excel("Data/Encuesta de Satisfaccion (E-Learning).xlsx",sheet = "Df1")

df_virtual2 <- read_excel("Data/Encuesta de Satisfaccion (E-Learning).xlsx",sheet = "Df2")



# GRAFICO P2
graf_P2=ggplot(data=df_virtual1[df_virtual1$N_Preg =="P2",], aes(x=as.factor(Nota), y=Porc2))+
  geom_bar(stat = "identity", fill="Orange")+
  geom_text(aes(label=paste0(round(Porc2*100,digits = 1),"%",sep=""))
            ,position=position_stack(vjust=.5),vjust=0.03, color="white", size=3)

graf_P2 + facet_wrap(. ~  Preguntas, ncol=2)

# GRAFICO P5
graf_P5=ggplot(data=df_virtual1[df_virtual1$N_Preg =="P5",], aes(x=as.factor(Nota), y=Porc2))+
  geom_bar(stat = "identity", fill="Orange")+
  geom_text(aes(y=pos,label=paste0(round(Porc2*100,digits = 1),"%",sep=""))
            ,position=position_stack(vjust=.5),vjust=0.03, color="white", size=3)

graf_P5 + facet_grid(. ~Preguntas)

# GRAFICO P6
graf_P6=ggplot(data=df_virtual1[df_virtual1$N_Preg =="P6",], aes(x=as.factor(Nota), y=Porc2))+
  geom_bar(stat = "identity", fill="Orange")+
  geom_text(aes(label=paste0(round(Porc2*100,digits = 1),"%",sep=""))
            ,position=position_stack(vjust=.5),vjust=0.03, color="white", size=3)

graf_P6 + facet_grid(. ~Preguntas)

# GRAFICO P7
graf_P7=ggplot(data=df_virtual1[df_virtual1$N_Preg =="P7" & df_virtual1$Interfaz =="Virtual" ,], aes(x=as.factor(Nota), y=Porc2))+
  geom_bar(stat = "identity", fill="Orange")+
  geom_text(aes(label=paste0(round(Porc2*100,digits = 1),"%",sep=""))
            ,position=position_stack(vjust=.5),vjust=0.03, color="white", size=3)

graf_P7 + facet_wrap(. ~  Preguntas, ncol=2)

# GRAFICO P8
graf_P8=ggplot(data=df_virtual1[df_virtual1$N_Preg =="P8" & df_virtual1$Interfaz =="Virtual" ,], aes(x=as.factor(Nota), y=Porc2))+
  geom_bar(stat = "identity", fill="Orange")+
  geom_text(aes(label=paste0(round(Porc2*100,digits = 1),"%",sep=""))
            ,position=position_stack(vjust=.5),vjust=0.03, color="white", size=3)

graf_P8 + facet_wrap(. ~  Preguntas, ncol=2)

# GRAFICO INDICADORES
graf_Ind=ggplot(data=df_virtual2[df_virtual2$Interfaz =="Total",], aes(x=Indicador, y=Valor,fill=Preguntas))+
  geom_bar(stat = "identity", fill="Orange")+
  geom_text(aes(label=paste0(round(Valor*100,digits = 0),"%",sep=""))
            ,position=position_stack(vjust=.5),vjust=0.03, color="white", size=3)

graf_Ind + facet_grid(. ~ Preguntas)