library(data.table)

###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade'))

#catchment <- 'Wuhle'
#substance <- 'Diuron'
#OgRe_type <- 'NEU'
#my_source <- 'Putzfassade'


catchments <- c('Wuhle', 'Flughafensee')
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
OgRe_types <- c("ALT", "NEU", "EFH", "GEW", "AND")
sources <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade")

catchment_substance <- matrix(nrow = 7, ncol = 5)
colnames(catchment_substance) <- c('ALT','NEU', 'EFH', 'GEW', 'AND')
rownames(catchment_substance) <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade", 'total')


for(catchment in catchments){

  # selection of the catchment area
  BTFs_catchemnt <- subset(berlin_runoff, AGEB1==catchment)

for(substance in substances){
    
  # create a output matrix to store loads
  current_output <- catchment_substance
  
for(OgRe_type in OgRe_types){  

  # selection of the OgRe_type
  BTF_OgRe_catchment <-subset(BTFs_catchemnt, OgRe_Type==OgRe_type)
  # read in concentration tables and store them into objects
  assign(paste0('c_',OgRe_type), read.csv(paste0('data/Konz_',OgRe_type,'.csv'), sep = ';')) 
  
for(my_source in sources){  
  
  # Which concentration file is to be used
  OgRe_type_current <- eval(parse(text = paste0("c_", OgRe_type)))
  # which row and which column must be selected for the correct cell
  col_Konz <- which(names(OgRe_type_current) == paste0("Konz_", substance))
  row_Konz <- which(OgRe_type_current$Source == my_source)
  # find and store concentration
  concentration <- OgRe_type_current[row_Konz, col_Konz]
  
  #Create coordinates to save the loads correctly and find the right runoff column
  index_source<- which(sources==my_source)
  index_OgRe_type <- which(OgRe_types==OgRe_type)
  
  # choosing the runoff column for current source
  col_runoff <- which(names(BTF_OgRe_catchment) == paste0("runoff_", sources[index_source]))

 

  # Calculation of the load with current runoff and corresponding concentration
  current_output[index_source, index_OgRe_type] <- sum(BTF_OgRe_catchment[col_runoff]*concentration/1000) #from g to kg
  
  
      }  
  current_output[7, index_OgRe_type]<- sum(current_output[,index_OgRe_type], na.rm = TRUE)
    }
  assign(paste0(catchment,'_',substance), current_output)
  write.csv(current_output, paste0('data_output/catchment_area_analysis/',catchment,'_',substance,'.csv'), row.names = FALSE)
   }
}


