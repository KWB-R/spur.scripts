###load data
# ABIMO runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))

catchments <- c('Wuhle', 'Flughafensee')
OgRe_types <- c("ALT", "EFH", "GEW", "NEU", "AND")
sources <- c("Dach", "Strasse", "Hof", "Putzfassade")


for (catchment in catchments) {
  # Auswahl des catchments
  BTFs_catchment <- subset(berlin_runoff, AGEB1== catchment)
  
  assign(paste0(catchment,'_volume'),sum(BTFs_catchment$runoff_total))
  assign(paste0(catchment,'_areas'),colSums(BTFs_catchment[,c('AU_roof','AU_street','AU_yard', 'AU_putz')]))
  
  for (OgRe_type in OgRe_types) {
    BTFs_OgRe_type <- subset(BTFs_catchment, OgRe_Type==OgRe_type )
    
    BTFs_OgRe_type <- BTFs_OgRe_type[,c('runoff_Dach','runoff_Strasse','runoff_Hof','runoff_Putzfassade','AU_roof','AU_street','AU_yard', 'AU_putz')]
    current_volumes <- colSums(BTFs_OgRe_type[1:4])
    current_areas <- colSums(BTFs_OgRe_type[5:8])
    assign(paste0(catchment,'_',OgRe_type,'_volumes'),current_volumes)
    assign(paste0(catchment,'_',OgRe_type,'_areas'),current_areas)
    
  }
}

EZG <- 'Wuhle'
SST <- 'EFH'
#Quelle <- 'Putzfassade'
mr <- c(0, 0.1, 0.2, 0.5)
x <- eval(parse(text = paste0(EZG,'_',SST,'_volumes')))
y <- eval(parse(text = paste0(EZG,'_',SST,'_areas')))

names_runoff <- list(paste0(mr*100,'%'),paste0('runoff_',sources))
absolute_volumes<- matrix(nrow = length(mr), ncol = length(x), dimnames = names_runoff)

names_areas <- list(paste0(mr*100,'%'),paste0('AU_',sources))
absolute_areas<- matrix(nrow = length(mr), ncol = length(y), dimnames = names_areas)



for(n in 1:length(mr)){
  
  absolute_volumes[n,]<-x*mr[n]
  absolute_areas[n,]<-y*mr[n]
  
  
}
specific_volumes <- absolute_volumes/eval(parse(text = paste0(EZG,'_volume')))
specific_areas <- t(t(absolute_areas)/eval(parse(text = paste0(EZG,'_areas'))))


print(absolute_volumes)
print(specific_volumes)

print(absolute_areas)
print(specific_areas)


