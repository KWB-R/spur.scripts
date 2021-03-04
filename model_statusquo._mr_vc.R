library(data.table)

###city structure types and sources
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
OgRe_types <- c("ALT", "EFH", "GEW", "NEU", "AND")
sources <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade")

###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
BTF_input <- foreign::read.dbf('data/berlin_runoff.dbf')
BTF_input <- setnames(BTF_input, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade'))

# read in backcalculated concentrations from OgRe
# read in relativ standard deviations
sd_list <- list()
 for( OgRe_typ in OgRe_types){
  assign(paste0('c_',OgRe_typ), read.csv(paste0('data/Konz_',OgRe_typ,'.csv'), sep = ';')) 
  index_OgRe_typ <- which(OgRe_types==OgRe_typ)
  sd_list[[index_OgRe_typ]] <-assign(paste0('rel_sd_',OgRe_typ), read.csv(paste0('data/rel_sd_', OgRe_typ,'.csv')))
 }

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
loads <- matrix(nrow = nMC, ncol = length(substances))


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
if(c_anchor == 0){
  concentration <- 0
} else {
index_substance <- which(substances==substance)
index_OgRe_typ <- which(OgRe_types==OgRe_typ)

rel_sd_temp<- as.data.frame(sd_list[[index_OgRe_typ]])
sd_temp<- c_anchor*rel_sd_temp[1,1+index_substance]

location<- log(c_anchor^2/sqrt(sd_temp^2+c_anchor^2))
shape<- sqrt(log(1+(sd_temp^2/c_anchor^2)))
concentration <- rlnorm(n=1, meanlog = location, sdlog = shape)
}

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
loads[[n, index_substance]]<- sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE))
  

  }
}
colnames(loads)<-substances
write.csv(loads,'data/simulated_loads.csv',row.names = FALSE)
