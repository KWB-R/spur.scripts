library(tidyverse)

simulated_loads<-read.csv('data/simulated_loads.csv')
Diuron_load <- read.csv('data/load_Diuron.csv')
Mecoprop_load <- read.csv('data/load_Mecoprop.csv')
Terbutryn_load <- read.csv('data/load_Terbutryn.csv')
Benzothiazol_load <- read.csv('data/load_Benzothiazol.csv')
Zn_load <- read.csv('data/load_Zn.csv')
Cu_load <- read.csv('data/load_Cu.csv')


mean(Cu_load$load_total)

substances <-c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')




for (substance in substances) {
  index_substance<- which(substances==substance)
  boxplot(simulated_loads[,index_substance], 
          main = substance,  
          ylab = 'load [kg/a]',
          
          outline = FALSE, 
          cex.axis = 1.1, 
          cex.lab = 1.3, 
          col = 3)
  
}


boxplot(simulated_loads, log = 'y',  ylab= 'load [kg/a]', outline=FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')


boxplot(Diuron_load, ylab = 'load [kg/a)', las=1, main = 'Diuron', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Mecoprop_load, ylab = 'load [kg/a)', las=1, main = 'Mecoprop', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Terbutryn_load, ylab = 'load [kg/a)', las=1, main = 'Terbutryn', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Benzothiazol_load, ylab = 'load [kg/a)', las=1, main = 'Benzothiazol', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Zn_load, ylab = 'load [kg/a)', main = 'Zn', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Cu_load, ylab = 'load [kg/a)', main = 'Cu', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')

 
DMZ_osm_simulated_loads <- read.csv('data/osm_simulated_loads(DMZ).csv')
DMZ_osm_simulated_loads <- mutate(DMZ_osm_simulated_loads, 'measurement' = 'diuronfreie Farbe')

DMZ_filtered_simulated_loads <- read.csv('data/filtered_simulated_loads(DMZ).csv')
DMZ_filtered_simulated_loads <- mutate(DMZ_filtered_simulated_loads, 'measurement' = ' Filter')

DMZ_simulated_loads <- read.csv('data/simulated_loads(DMZ).csv')
DMZ_simulated_loads <- mutate(DMZ_simulated_loads, 'measurement' = 'keine')


DMZ <- as.data.frame(rbind(DMZ_filtered_simulated_loads,DMZ_osm_simulated_loads,DMZ_simulated_loads))

boxplot(DMZ$Diuron ~DMZ$measurement, ylab= '', xlab = '', las=1, main= 'Diuron', outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = c('lightblue','blue','darkblue'))
title(ylab = 'load [kg/a]', line = 2.5, cex.lab=1.3)
boxplot(DMZ$Mecoprop~DMZ$measurement , xlab = '', ylab= '', las=1, main= 'Mecoprop', outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = c('lightblue','blue','darkblue'))
title(ylab = 'load [kg/a]', line = 2.5, cex.lab=1.3)
boxplot(DMZ$Zn~DMZ$measurement, xlab = '', ylab= '', main= 'Zn', outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = c('lightblue','blue','darkblue'))
title(ylab = 'load [kg/a]', line = 2.5, cex.lab=1.3)

