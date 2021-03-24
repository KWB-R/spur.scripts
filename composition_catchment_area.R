###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade'))



#Wuhle -> 884 BTF
BTF_Wuhle <- subset(berlin_runoff, AGEB1=='Wuhle')
#Fläche in m2
AW <- sum(BTF_Wuhle$FLGES)
W_ALT <- subset(BTF_Wuhle, OgRe_Type=='ALT')
W_NEU <- subset(BTF_Wuhle, OgRe_Type=='NEU')
W_EFH <- subset(BTF_Wuhle, OgRe_Type=='EFH')
W_GEW <- subset(BTF_Wuhle, OgRe_Type=='GEW')
W_STR <- subset(BTF_Wuhle, OgRe_Type=='STR')
W_AND <- subset(BTF_Wuhle, OgRe_Type=='AND')

xW_ALT<- sum(W_ALT$FLGES)/AW
xW_NEu<- sum(W_NEU$FLGES)/AW
xW_EFH<- sum(W_EFH$FLGES)/AW
xW_GEW<- sum(W_GEW$FLGES)/AW
xW_STR<- sum(W_STR$FLGES)/AW
xW_AND<- sum(W_AND$FLGES)/AW

Wuhle<- c(xW_ALT,xW_NEu,xW_EFH,xW_GEW,xW_STR,xW_AND)*100
names(Wuhle)<- c('ALT','NEU','EFH','GEW','STR','AND')

barplot(Wuhle, ylim = c(0,100), main = 'Wuhle')

#kleine Seen -> 718 BTF
BTF_kleine_Seen <- subset(berlin_runoff, AGEB1=='Kleingewässer (Teiche, Tümpel, Gräben)')
#Fläche in m2
AkS <- sum(BTF_kleine_Seen$FLGES)

kS_ALT <- subset(BTF_kleine_Seen, OgRe_Type=='ALT')
kS_NEU <- subset(BTF_kleine_Seen, OgRe_Type=='NEU')
kS_EFH <- subset(BTF_kleine_Seen, OgRe_Type=='EFH')
kS_GEW <- subset(BTF_kleine_Seen, OgRe_Type=='GEW')
kS_STR <- subset(BTF_kleine_Seen, OgRe_Type=='STR')
kS_AND <- subset(BTF_kleine_Seen, OgRe_Type=='AND')

xkS_ALT<- sum(kS_ALT$FLGES)/AkS
xkS_NEu<- sum(kS_NEU$FLGES)/AkS
xkS_EFH<- sum(kS_EFH$FLGES)/AkS
xkS_GEW<- sum(kS_GEW$FLGES)/AkS
xkS_STR<- sum(kS_STR$FLGES)/AkS
xkS_AND<- sum(kS_AND$FLGES)/AkS

kleine_Seen<- c(xkS_ALT,xkS_NEu,xkS_EFH,xkS_GEW,xkS_STR,xkS_AND)*100
names(kleine_Seen)<- c('ALT','NEU','EFH','GEW','STR','AND')

barplot(kleine_Seen, ylim = c(0,100), main = 'kleine Seen')
