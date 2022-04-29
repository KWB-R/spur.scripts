library('reshape2')

catchments<- c('Wuhle', 'Flughafensee')
extends <- c(0, 10, 20, 50)

#Einlesen aller modellierten Frachten
for (catchment in catchments) {
  for (extend in extends) {
assign(paste0(catchment, '_', extend), read.csv(paste0('data_output/calculation_load_reduction/loads_', catchment,'_measure_extend_',extend,'%.csv')))
    
  }
}

#Berechnung des Reduktionspotenzials
#plotten der Potenzials
potencials<- c(10, 20, 50)

for (catchment in catchments) {
  for (potencial in potencials) {
    assign(paste0('rp',potencial,'_', catchment), (1-eval(parse(text = paste0(catchment,'_',potencial)))/eval(parse(text = paste0(catchment,'_0'))))*100)
    assign(paste0('rp',potencial,'_', catchment), melt(eval(parse(text=paste0('rp',potencial,'_', catchment)))))
  
    print(ggplot(data=eval(parse(text=paste0('rp',potencial,'_', catchment))), aes(x= variable, y=value))+
      stat_boxplot(geom = 'errorbar', width= 0.5, alpha=1)+
      geom_boxplot( fill= 'blue', alpha= 1)
    )
    }
  
}


#plotten der Reduktionspotenziale
for (catchment in catchments) {
  for (potencial in potencials) {

  print(ggplot(data= eval(parse(text=paste0('rp',potencial,'_',catchment))), aes(x= variable, y=value))+
  stat_boxplot(geom = 'errorbar', width= 0.5, alpha=1)+
  geom_boxplot( fill= '#5b6fc2', alpha= 1)+
  theme_minimal()+
  xlab("")+
  ylab("Frachtreduktion [%]")+
  ggtitle(paste0('Kombinierte Maßnahmen'), subtitle = paste0(catchment,' ',potencial,'%'))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  theme(plot.title = element_text(size=14),
        axis.title.y= element_text(hjust = 0.5, size = 12), 
        axis.text = element_text(size = 12)))
  }
} 


