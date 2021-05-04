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
 catchments <- c('Wuhle', 'Flughafensee')
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
 
 
 
 
 # grouped barplots: substance shares for each SST per source in relation to total load from SST
 library(datasets)
 catchments <- c('Wuhle', 'Flughafensee')
 substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
 OgRe_types <- c("ALT", "NEU", "EFH", "GEW", "AND")
 sources <- c("Dach", "Strasse", "Hof", "Putzfassade")
 
 # build output matrix
 catchment_substance_shares<- matrix(nrow = 4, ncol = 5)
 colnames(catchment_substance_shares)<- c('ALT','NEU', 'EFH', 'GEW', 'AND')
 rownames(catchment_substance_shares)<- c("Dach", "Strasse", "Hof", "Putzfassade")
 
 for(catchment in catchments){
   for(substance in substances){
     
     #read in substance load tables for every SST
     current_data <- read.csv(paste0('data_output/catchment_area_analysis/',catchment,'_',substance,'.csv'), sep = ',')
     # assign current output matrix (specific for catchment and substance)
     current_shares<- catchment_substance_shares
     
     for (OgRe_type in OgRe_types) {
       
      for (my_source in sources) {
        #crating indices to assign results to the right cell
        index_OgRe_type <- which(OgRe_types==OgRe_type)
        index_source <- which(sources==my_source)
        
        # Assign 0% to the cell if the total load is 0 to avoid dividing by 0.
        # If not equal to 0, the source-specific load from the current OgRe area is
        # divided by the total load from the OgRe area and is multiplied by 100.
        if(current_data[5,index_OgRe_type]==0){
          current_shares[index_source,index_OgRe_type] <- 0
        } else {
        current_shares[index_source,index_OgRe_type] <- current_data[index_source,index_OgRe_type]/current_data[5, index_OgRe_type]*100
        }
        assign(paste0(catchment,'_',substance,'_shares'),current_shares)
        #write.csv(current_shares, paste0('data_output/',catchment,'_',substance,'_shares'))
        }
      }
      # assign the colour vector
      # design the barplot
      # shape the legend
      colours <- c('slategray1','steelblue1','royalblue2', 'steelblue3', 'royalblue4')
      barplot(t(current_shares), main = substance, sub = catchment, beside = T, ylim = c(0,100), ylab = 'Share of load [%]', col = colours, axis.lty="solid")
      legend(100, rownames(t(current_shares)), cex = 0.8, fill = colours, title = "SST")
     
     }  
 }
 

 
 # grouped barplots: substance shares for each SST per source in relation to total load from catchment
 library(datasets)
 catchments <- c('Wuhle', 'Flughafensee')
 substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
 OgRe_types <- c("ALT", "NEU", "EFH", "GEW","AND")
 sources <- c("Dach", "Strasse", "Hof", "Putzfassade")
 
 # build output matrix
 catchment_substance_shares<- matrix(nrow = 4, ncol = 5)
 colnames(catchment_substance_shares)<- c('ALT','NEU', 'EFH', 'GEW', 'AND')
 rownames(catchment_substance_shares)<- c("Dach weitere", "Strasse", "Hof", "Putzfassade")
 
 for(catchment in catchments){
   for(substance in substances){
     
     #read in substance load tables for every SST
     current_data <- read.csv(paste0('data_output/catchment_area_analysis/',catchment,'_',substance,'.csv'), sep = ',')
     # assign current output matrix (specific for catchment and substance)
     current_shares<- catchment_substance_shares
     
     for (OgRe_type in OgRe_types) {
       
       for (my_source in sources) {
         #crating indices to assign results to the right cell
         index_OgRe_type <- which(OgRe_types==OgRe_type)
         index_source <- which(sources==my_source)
         
         # Assign 0% to the cell if the total load is 0 to avoid dividing by 0.
         # If not equal to 0, the source-specific load from the current OgRe area is
         # divided by the total load from the OgRe area and is multiplied by 100.
         if(current_data[5,index_OgRe_type]==0){
           current_shares[index_source,index_OgRe_type] <- 0
         } else {
           current_shares[index_source,index_OgRe_type] <- current_data[index_source,index_OgRe_type]/sum(current_data[5,])*100
         }
         assign(paste0(catchment,'_',substance,'_shares'),current_shares)
         #write.csv(current_shares, paste0('data_output/',catchment,'_',substance,'_shares'))
       }
     }
     # assign the colour vector
     # design the barplot
     # shape the legend
     colours <- c('slategray1','steelblue1','royalblue2', 'steelblue3', 'royalblue4')
     barplot(t(current_shares), main = substance, sub = catchment, beside = T, ylim = c(0,100), ylab = 'Fracht [%]', col = colours, axis.lty="solid", cex.axis = 1.5, cex.names =1.5, cex.lab=1.5, cex.sub=1.5, cex.main= 2)
     legend(20,100, rownames(t(current_shares)), cex = 1, fill = colours, title = "SST")
     
   }  
 }
 
 #### Plots für Andreas und Daniel 29.4.2021
 
 
 # Plots Wuhle
 # Daten einlesen
 Wuhle_0<-read.csv('data_output/calculation_load_reduction/loads_Wuhle_measure_extend_0%.csv')
 Wuhle_10<- read.csv('data_output/calculation_load_reduction/loads_Wuhle_measure_extend_10%.csv')
 Wuhle_20 <- read.csv('data_output/calculation_load_reduction/loads_Wuhle_measure_extend_20%.csv')
 Wuhle_50 <- read.csv('data_output/calculation_load_reduction/loads_Wuhle_measure_extend_50%.csv')
 Flughafensee_0 <- read.csv('data_output/calculation_load_reduction/loads_Flughafensee_measure_extend_0%.csv')
 Flughafensee_10 <- read.csv('data_output/calculation_load_reduction/loads_Flughafensee_measure_extend_10%.csv')
 Flughafensee_20 <- read.csv('data_output/calculation_load_reduction/loads_Flughafensee_measure_extend_20%.csv')
 Flughafensee_50 <- read.csv('data_output/calculation_load_reduction/loads_Flughafensee_measure_extend_50%.csv')
 
Wuhle_Diuron <- cbind(Wuhle_0[1],Wuhle_10[1],Wuhle_20[1],Wuhle_50[1])
Wuhle_Mecoprop <- cbind(Wuhle_0[2],Wuhle_10[2],Wuhle_20[2],Wuhle_50[2])
Wuhle_Zn <- cbind(Wuhle_0[3],Wuhle_10[3],Wuhle_20[3],Wuhle_50[3])


boxplot(Wuhle_Diuron, log = 'y',  ylab= 'load [kg/a]', outline=FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Wuhle_Mecoprop, log = 'y',  ylab= 'load [kg/a]', outline=FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
boxplot(Wuhle_Zn, log = 'y',  ylab= 'load [kg/a]', outline=FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')


 # Box-plots erstellen
 boxplot(Wuhle_0, log = 'y',  ylab= 'load [kg/a]', outline=FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
 boxplot(Wuhle_Diuron, ylab = 'load [kg/a)', las=1, main = 'Diuron', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
 boxplot(Wuhle_Mecoprop, ylab = 'load [kg/a)', las=1, main = 'Mecoprop', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
 boxplot(Wuhle_Terbutryn, ylab = 'load [kg/a)', las=1, main = 'Terbutryn', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
 boxplot(Wuhle_Benzothiazol, ylab = 'load [kg/a)', las=1, main = 'Benzothiazol', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
 boxplot(Wuhle_Zn, ylab = 'load [kg/a)', main = 'Zn', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
 boxplot(Wuhle_Cu, ylab = 'load [kg/a)', main = 'Cu', names =  c('Dach', 'Strasse', 'Hof', 'Putzfassade', 'gesamt'), outline = FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 'lightblue')
 
 
 
 # Einzelwerte
 Wuhle_0_solo<-read.csv('data_output/Frachtreduktion/loads_Wuhle_measure_extend_0%.csv')
 Wuhle_10_solo<- read.csv('data_output/Frachtreduktion/loads_Wuhle_measure_extend_10%.csv')
 Wuhle_20_solo <- read.csv('data_output/Frachtreduktion/loads_Wuhle_measure_extend_20%.csv')
 Wuhle_50_solo <- read.csv('data_output/Frachtreduktion/loads_Wuhle_measure_extend_50%.csv')
 Flughafensee_0_solo <- read.csv('data_output/Frachtreduktion/loads_Flughafensee_measure_extend_0%.csv')
 Flughafensee_10_solo <- read.csv('data_output/Frachtreduktion/loads_Flughafensee_measure_extend_10%.csv')
 Flughafensee_20_solo <- read.csv('data_output/Frachtreduktion/loads_Flughafensee_measure_extend_20%.csv')
 Flughafensee_50_solo <- read.csv('data_output/Frachtreduktion/loads_Flughafensee_measure_extend_50%.csv')
 
 Wuhle_Diuron_solo <- cbind(Wuhle_0_solo[1],Wuhle_10_solo[1],Wuhle_20_solo[1],Wuhle_50_solo[1])
 Wuhle_Mecoprop_solo <- cbind(Wuhle_0_solo[2],Wuhle_10_solo[2],Wuhle_20_solo[2],Wuhle_50_solo[2])
 Wuhle_Zn_solo <- cbind(Wuhle_0_solo[3],Wuhle_10_solo[3],Wuhle_20_solo[3],Wuhle_50_solo[3])
 
 
 WD <- Wuhle_Diuron_solo/Wuhle_Diuron_solo[1,1]
 WM <- Wuhle_Mecopropsolo/Wuhle_Mecoprop_solo[1,1]
 WZ <- Wuhle_Zn_solo/Wuhle_Zn_solo[1,1]
 as.matrix(WD)
 class(WD)
 
 barplot(WD)
 barplo