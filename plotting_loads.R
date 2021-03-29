library(tidyverse)

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

# Plots Berlin
# Daten einlesen
simulated_loads<-read.csv('data/simulated_loads.csv')
Diuron_load <- read.csv('data/load_Diuron.csv')
Mecoprop_load <- read.csv('data/load_Mecoprop.csv')
Terbutryn_load <- read.csv('data/load_Terbutryn.csv')
Benzothiazol_load <- read.csv('data/load_Benzothiazol.csv')
Zn_load <- read.csv('data/load_Zn.csv')
Cu_load <- read.csv('data/load_Cu.csv')
# Box-plots erstellen
boxplot(simulated_loads, log = 'y',  ylab= 'load [kg/a]', outline=FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Diuron_load, ylab = 'load [kg/a)', las=1, main = 'Diuron', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Mecoprop_load, ylab = 'load [kg/a)', las=1, main = 'Mecoprop', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Terbutryn_load, ylab = 'load [kg/a)', las=1, main = 'Terbutryn', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Benzothiazol_load, ylab = 'load [kg/a)', las=1, main = 'Benzothiazol', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Zn_load, ylab = 'load [kg/a)', main = 'Zn', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Cu_load, ylab = 'load [kg/a)', main = 'Cu', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')



# plots Berlin DMZ only
# Daten einlesen
DMZ_osm_simulated_loads <- read.csv('data/osm_simulated_loads(DMZ).csv')
DMZ_osm_simulated_loads <- mutate(DMZ_osm_simulated_loads, 'measurement' = 'diuronfreie Farbe')
DMZ_filtered_simulated_loads <- read.csv('data/filtered_simulated_loads(DMZ).csv')
DMZ_filtered_simulated_loads <- mutate(DMZ_filtered_simulated_loads, 'measurement' = ' Filter')
DMZ_simulated_loads <- read.csv('data/simulated_loads(DMZ).csv')
DMZ_simulated_loads <- mutate(DMZ_simulated_loads, 'measurement' = 'keine')
# Datensätze zusammenführen (zeilenweise)
DMZ <- as.data.frame(rbind(DMZ_filtered_simulated_loads,DMZ_osm_simulated_loads,DMZ_simulated_loads))
# Box-plots erstellen
boxplot(DMZ$Diuron ~DMZ$measurement, ylab= '', xlab = '', las=1, main= 'Diuron', outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = c('lightblue','blue','darkblue'))
title(ylab = 'load [kg/a]', line = 2.5, cex.lab=1.3)
boxplot(DMZ$Mecoprop~DMZ$measurement , xlab = '', ylab= '', las=1, main= 'Mecoprop', outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = c('lightblue','blue','darkblue'))
title(ylab = 'load [kg/a]', line = 2.5, cex.lab=1.3)
boxplot(DMZ$Zn~DMZ$measurement, xlab = '', ylab= '', main= 'Zn', outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = c('lightblue','blue','darkblue'))
title(ylab = 'load [kg/a]', line = 2.5, cex.lab=1.3)



# Plots Wuhle
# Daten einlesen
Wuhle_loads<-read.csv('data/Wuhle_simulated_loads.csv')
Wuhle_Diuron<- read.csv('data/Wuhle_load_Diuron.csv')
Wuhle_Mecoprop <- read.csv('data/Wuhle_load_Mecoprop.csv')
Wuhle_Terbutryn <- read.csv('data/Wuhle_load_Terbutryn.csv')
Wuhle_Benzothiazol <- read.csv('data/Wuhle_load_Benzothiazol.csv')
Wuhle_Zn <- read.csv('data/Wuhle_load_Zn.csv')
Wuhle_Cu <- read.csv('data/Wuhle_load_Cu.csv')
# Box-plots erstellen
boxplot(Wuhle_loads, log = 'y',  ylab= 'load [kg/a]', outline=FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Wuhle_Diuron, ylab = 'load [kg/a)', las=1, main = 'Diuron', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Wuhle_Mecoprop, ylab = 'load [kg/a)', las=1, main = 'Mecoprop', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Wuhle_Terbutryn, ylab = 'load [kg/a)', las=1, main = 'Terbutryn', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Wuhle_Benzothiazol, ylab = 'load [kg/a)', las=1, main = 'Benzothiazol', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Wuhle_Zn, ylab = 'load [kg/a)', main = 'Zn', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Wuhle_Cu, ylab = 'load [kg/a)', main = 'Cu', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')

# Plots catchment area analysis
 catchments <- c('Wuhle')
 substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
 OgRe_types <- c("ALT", "NEU", "EFH", "GEW","AND")

 
 
 for(catchment in catchments){
  for(substance in substances){
    
    data <- read.csv(paste0('data_output/catchment_area_analysis/',catchment,'_',substance,'.csv'), sep = ',')
    load_fraction <- c(data[7,1], data[7,2], data[7,3], data[7,4], data[7,5])/sum(data[7,])*100

    names(load_fraction)<- c('ALT','NEU','EFH','GEW','AND')
    
    barplot(load_fraction, ylim = c(0, 100), main =substance , sub = catchment)
  }  
 }
 
 for(catchment in catchments){
   for(substance in substances){
     for(OgRe_type in OgRe_types){
       data <-  read.csv(paste0('data_output/catchment_area_analysis/',catchment,'_',substance,'.csv'), sep = ',')
       index_OgRe_type <- which(OgRe_types==OgRe_type)
       
       if(data[7,index_OgRe_type]==0){
         sub_data[]<-0
      }
       else{
       sub_data <- data[1:6,index_OgRe_type]/data[7,index_OgRe_type]*100
      }
       names(sub_data)<- c("Bitumendach", "Ziegeldach", "Dach weitere", "Strasse", "Hof", "Putzfassade")
       barplot(sub_data, ylim = c(0, 100), main = OgRe_type , sub = substance)
     }
   }
 }