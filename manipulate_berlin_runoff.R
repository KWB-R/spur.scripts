library(foreign)
###load data
# ABIMO runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')

#berechne total_runoff
runoff_tot<-rowSums(berlin_runoff[47:50])
# hinzufügen von der Spalte total_runoff zu berlin_runoff
berlin_runoff$runoff_tot=runoff_tot
# die Spalten von berlin runoff in die richtige Reihenfolge bringen
berlin_runoff<-berlin_runoff[c(1:50,55,51:54)]

write.dbf(berlin_runoff,'data/berlin_runoff.dbf')
