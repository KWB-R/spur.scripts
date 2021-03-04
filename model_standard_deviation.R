library(dplyr)
OgRe_types <- c("ALT", "EFH", "GEW", "NEU", "STR")
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
sources <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade")

OgRe_composite_SPUR <- read.csv(file = 'data/OgRe_composite_SPUR.csv', sep = ';')

#create output matrix design
rel_sd <- data.frame(matrix( data= NA, nrow=6, ncol = 7))
colnames(rel_sd) <- c('source', substances)
rel_sd[,1] <- sources
         
for(OgRe_typ in OgRe_types){

#Zeilen filtern und davon sd bestimmen
for (substance in substances) {

index_substance <- which(substances==substance)
temp <- (filter(OgRe_composite_SPUR,OgRe_composite_SPUR$SiteCode == OgRe_typ, OgRe_composite_SPUR$VariableCode == substance ))
mean_temp<- mean(temp$DataValue, na.rm = TRUE)  
sd_temp <- sd(temp$DataValue)
if(mean_temp==0){
  rel_sd[,index_substance+1]<- 0
  } else {
  rel_sd[,index_substance+1]<- sd_temp/mean_temp
  }
}
#create the output matrices  
x<- assign(paste0(OgRe_typ,'_rel_sd'), rel_sd)
write.csv(x, paste0( 'data/rel_sd_', OgRe_typ, '.csv'), row.names = FALSE)
}


# create rel_sd for OgRe_typ AND
#because no such data for AND is present, the mean of the other OgRe_types will be used
y<-(ALT_rel_sd[,2:7]+NEU_rel_sd[,2:7]+GEW_rel_sd[,2:7]+EFH_rel_sd[,2:7]+STR_rel_sd[,2:7])/length(OgRe_types)
AND_rel_sd<-data.frame(sources,y)
write.csv(AND_rel_sd,'data/rel_sd_AND.csv',row.names = FALSE)
