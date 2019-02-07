

GrafMejInfra=ggplot() +
  geom_point(data = MejInfra4, aes(x = Por_Valor2, y=Aspecto,color = Aspecto), size = 0) +
  geom_text(label = MejInfra4$Por_Valor2, nudge_x = 0.5, nudge_y = 0 ,size=3) +
  geom_segment(aes(x = 0, y = MejInfra4$Aspecto, xend = MejInfra4$Por_Valor2, yend = MejInfra4$Aspecto, color = MejInfra4$Aspecto), size=5 ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(
    x = "Valores",
    y = "Aspectos",
    title = "Aspectos que deben mejorar Docentes",
    subtitle = "Respuesta Múltiple (% de Sí)",
    caption = "\nPie de linea, para explicar lo que se vea conveniente"
  )
GrafMejInfra + facet_grid( MejInfra4$Sede ~.)





y=paste(round(MejDocente4$Valor*100, 0),"%")                                   
mejdoc=ggplot(MejDocente4, aes(Valor, fill=Sede)) + geom_bar(position="dodge")
mejdoc + facet_grid(. ~ MejDocente4$Aspecto )