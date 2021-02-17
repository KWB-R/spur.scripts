###city structure types and sources
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
OgRe_types <- c("ALT", "EFH", "GEW", "NEU", "AND")
sources <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade")

###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
BTF_input <- foreign::read.dbf('data/berlin_runoff.dbf')
BTF_input <- setnames(BTF_input, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade'))



# backcalculated concentrations from OgRe
c_NEU <- read.csv(file = 'data/Konz_NEU.csv', sep = ';')
c_ALT <- read.csv(file = 'data/Konz_ALT.csv', sep = ';')
c_GEW <- read.csv(file = 'data/Konz_GEW.csv', sep = ';')
c_EFH <- read.csv(file = 'data/Konz_EFH.csv', sep = ';')
c_AND <- read.csv(file = 'data/Konz_AND.csv', sep = ';')



substance_output <- data.frame("ID" = BTF_input$CODE,
                               "Gewässser" = BTF_input$AGEB1,
                               "OgRe_Typ" = BTF_input$OgRe_Type,
                               "load_Bitumendach" = NA,
                               "load_Ziegeldach" = NA,
                               "load_Dach_weitere" = NA,
                               "load_Strasse" = NA,
                               "load_Hof" = NA,
                               "load_Putzfassade" = NA)



#creating data frame for loads
set.seed(5)
nMC <- 1000
loads <- matrix(nrow = nMC,ncol = length(substances))



for (n in 1:nMC){
###calculate loads  
for (substance in substances) {
  
  #substanz auswählen
  
  
  for (OgRe_typ in OgRe_types) {
    
    #Zeilen auswählen die OgRe_typ entsprechen
    
    
    for (my_source in sources) {

OgRe_typ_current <- eval(parse(text = paste0("c_", OgRe_typ)))

col_Konz <- which(names(OgRe_typ_current) == paste0("Konz_", substance))
row_Konz <- which(OgRe_typ_current$Source == my_source)

c_anchor <- OgRe_typ_current[row_Konz, col_Konz]

#with lognormal distribiuted concentrations
concentration <- rlnorm(n= 1, meanlog = log(c_anchor), sdlog = 1 )

row_runoff <- which(BTF_input$OgRe_Type == OgRe_typ)

index_source <- which(sources== my_source)
col_runoff <- which(names(BTF_input) == paste0("runoff_", sources[index_source]))

col_output <- which(names(substance_output) == paste0("load_", sources[index_source]))

substance_output[row_runoff, col_output] <- concentration * BTF_input[row_runoff, col_runoff]

#Quelle auswählen (im entsprechenden c_ File (1 Zelle) und Abflussfile (1 Spalte))
#multiplizieren

      }
   }
  
  #Ergebnis einm neuen data.frame zuweisen
assign(paste0(substance, '_output'), substance_output)
current_output <- assign(paste0(substance, '_output'), substance_output)


#loads <- cbind(loads,assign(paste0(substance, '_load'), sum(colSums(Filter(is.numeric, current_output)))))
index_substance <- which(substances==substance)
loads[[n, index_substance]]<- sum(colSums(Filter(is.numeric, current_output)))
  

  }
}
colnames(loads)<-substances
