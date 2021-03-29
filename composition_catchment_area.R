
catchments <- c('Wuhle', 'Kleingewässer (Teiche, Tümpel, Gräben)', 'Großer Wannsee', 'Kleine Wannseekette', 'Groß-Glienicker See', 'Grunewaldseenkette', 'Tegeler See', 'Flughafensee', 'Schäfersee')

###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade'))



for(catchment in catchments){
  
  BTF_catchment <- subset(berlin_runoff, AGEB1 == catchment)
  ROW <- sum(BTF_catchment$ROW)
  ALT <- subset(BTF_catchment, OgRe_Type=='ALT')
  NEU <- subset(BTF_catchment, OgRe_Type=='NEU')
  EFH <- subset(BTF_catchment, OgRe_Type=='EFH')
  GEW <- subset(BTF_catchment, OgRe_Type=='GEW')
  STR <- subset(BTF_catchment, OgRe_Type=='STR')
  AND <- subset(BTF_catchment, OgRe_Type=='AND')
  
  x_ALT <- sum(ALT$ROW)/ROW
  x_NEU <- sum(NEU$ROW)/ROW
  x_EFH <- sum(EFH$ROW)/ROW
  x_GEW <- sum(GEW$ROW)/ROW
  x_STR <- sum(STR$ROW)/ROW
  x_AND <- sum(AND$ROW)/ROW
  
  composition <- 100* c( x_ALT, x_NEU, x_EFH, x_GEW, x_STR, x_AND) 
  names(composition)<- c('ALT','NEU','EFH','GEW','STR','AND')
  barplot(composition, ylim = c(0,100), main = catchment)
  
 }