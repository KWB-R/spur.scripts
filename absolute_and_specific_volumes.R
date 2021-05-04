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
  
  for (OgRe_type in OgRe_types) {
    BTFs_OgRe_type <- subset(BTFs_catchment, OgRe_Type==OgRe_type )
    
    BTFs_OgRe_type <- BTFs_OgRe_type[,c('runoff_Dach','runoff_Strasse','runoff_Hof','runoff_Putzfassade')]
    current_volumes <- colSums(BTFs_OgRe_type)
    assign(paste0(catchment,'_',OgRe_type,'_volumes'),current_volumes)
  }
}

EZG <- 'Flughafensee'
SST <- 'ALT'
#Quelle <- 'Putzfassade'
mr <- c(0, 0.1, 0.2, 0.5)
x <- eval(parse(text = paste0(EZG,'_',SST,'_volumes')))
names <- list(paste0(mr*100,'%'),paste0('runoff_',sources))
absolute_volumes<- matrix(nrow = length(mr), ncol = length(x), dimnames = names)



for(n in 1:length(mr)){
  
  absolute_volumes[n,]<-x*mr[n]
  
}
specific_volumes <- absolute_volumes/eval(parse(text = paste0(EZG,'_volume')))

print(absolute_volumes)
print(specific_volumes)
