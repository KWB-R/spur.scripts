###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))

catchments <- c('Wuhle', 'Flughafensee')

for(catchment in catchments){
  
  #selecting the surveyed catchemnt
  current_catchment <- assign(paste0('BTF_', catchment), subset(berlin_runoff, AGEB1 == catchment))
  #determine total runoff of the catchment
  assign(paste0('total_runoff_', catchment), sum(current_catchment$runoff_total))
  #determine total area of the catchment
  assign(paste0('total_area_', catchment), sum(current_catchment$FLAECHE))
  
}