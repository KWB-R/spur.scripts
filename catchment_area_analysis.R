library(data.table)

###Daten einlesen
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))


catchments <- c('Wuhle', 'Flughafensee')
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
OgRe_types <- c("ALT", "NEU", "EFH", "GEW", "AND")
sources <- c("Dach", "Strasse", "Hof", "Putzfassade")

catchment_substance <- matrix(nrow = 5, ncol = 5)
colnames(catchment_substance) <- c('ALT','NEU', 'EFH', 'GEW', 'AND')
rownames(catchment_substance) <- c("Dach", "Strasse", "Hof", "Putzfassade", 'total')


for(catchment in catchments){

  # Auswahl des catchments
  BTFs_catchemnt <- subset(berlin_runoff, AGEB1==catchment)

for(substance in substances){
    
  # create a output matrix to store loads
  current_output <- catchment_substance
  
for(OgRe_type in OgRe_types){  

  # Auswahl des OgRe_type
  BTF_OgRe_catchment <-subset(BTFs_catchemnt, OgRe_Type==OgRe_type)
  # Einlesen von Konzentrationstabellen
  assign(paste0('c_',OgRe_type), read.csv(paste0('data/Konz_',OgRe_type,'.csv'), sep = ';')) 
  
for(my_source in sources){  
  
  # Auswahl der richtigen Konzentration
  OgRe_type_current <- eval(parse(text = paste0("c_", OgRe_type)))
  # Bestimmung von Zeile und Spalte um die richtige Konzentration abzugreifen
  col_Konz <- which(names(OgRe_type_current) == paste0("Konz_", substance))
  row_Konz <- which(OgRe_type_current$Source == my_source)
  # Auswahl der aktuellen Konzentration
  concentration <- OgRe_type_current[row_Konz, col_Konz]
  
  #Erstellen von Indizes als Koordinaten zum Speichern der Ergebnisse
  index_source<- which(sources==my_source)
  index_OgRe_type <- which(OgRe_types==OgRe_type)
  
  # Auswahl der aktuellen runoff-Spalte (Quellspezifisch)
  col_runoff <- which(names(BTF_OgRe_catchment) == paste0("runoff_", sources[index_source]))


  # Frachtberechnung mit dem entsprechenden runoff und der korrespondierenden Konzentration
  current_output[index_source, index_OgRe_type] <- sum(BTF_OgRe_catchment[col_runoff]*concentration/1000) #from g to kg
  
      }  
  current_output[5, index_OgRe_type]<- sum(current_output[,index_OgRe_type], na.rm = TRUE)
    }
  assign(paste0(catchment,'_',substance), current_output)
  write.csv(current_output, paste0('data_output/catchment_area_analysis/',catchment,'_',substance,'.csv'), row.names = FALSE)
   }
}


