library(data.table)

catchments <- c('Wuhle', 'Flughafensee')

###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade','runoff_total'))



for(catchment in catchments){
  
  BTF_catchment <- subset(berlin_runoff, AGEB1 == catchment)
  catchment_runoff <- sum(BTF_catchment$runoff_total)
  ALT <- subset(BTF_catchment, OgRe_Type=='ALT')
  NEU <- subset(BTF_catchment, OgRe_Type=='NEU')
  EFH <- subset(BTF_catchment, OgRe_Type=='EFH')
  GEW <- subset(BTF_catchment, OgRe_Type=='GEW')
  AND <- subset(BTF_catchment, OgRe_Type=='AND')
  
  
  x_ALT <- sum(ALT$runoff_total)/catchment_runoff
  x_NEU <- sum(NEU$runoff_total)/catchment_runoff
  x_EFH <- sum(EFH$runoff_total)/catchment_runoff
  x_GEW <- sum(GEW$runoff_total)/catchment_runoff
  x_AND <- sum(AND$runoff_total)/catchment_runoff
  
  composition <- 100* c( x_ALT, x_NEU, x_EFH, x_GEW, x_AND) 
  names(composition)<- c('ALT','NEU','EFH','GEW','AND')
  barplot(composition, ylim = c(0,100), ylab='runoff [%]' , main = catchment)
  
 }