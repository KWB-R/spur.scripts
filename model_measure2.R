library(data.table)

# 1. Abschnitt Reduktionszenario
substances<- c('Diuron', 'Mecoprop', 'Zn')
OgRe_types<- c('ALT', 'EFH', 'GEW', 'NEU', 'AND')
sources <-  c("Dach", "Strasse", "Hof", "Putzfassade")
measures <- c('OSM', 'Filter')
catchments <- c('Wuhle', 'Flughafensee')

# Maßnahmen Umfang für die verscheidenen OgRe-types
mr<-0.5


# Maßnahmenumfang Wuhle; vielleicht noch in Liste verstauen
names_U<-list(measures, sources)
U_Wuhle_ALT<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Wuhle_EFH<-matrix(c(0, 0, 0, 0, 0, 0, mr, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Wuhle_GEW<-matrix(c(0, mr, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Wuhle_NEU<-matrix(c(0, mr, 0, 0, 0, mr, 0, mr/2), nrow = 2, ncol = 4, dimnames = names_U)
U_Wuhle_AND<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)


# MAßnahmenumfang Flughafensee; vielleicht noch in Liste verstauen
U_Flughafensee_ALT<-matrix(c(0, mr, 0, 0, 0, mr, mr, mr/2), nrow = 2, ncol = 4, dimnames = names_U)
U_Flughafensee_EFH<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Flughafensee_GEW<-matrix(c(0, mr, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Flughafensee_NEU<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Flughafensee_AND<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)


# Maßnahmen Effizienz 
# n1= diuronfreie Farbe
n1<- c(1, 0, 0)
names(n1)<- substances
#n2= Funkefilter
n2<- c(0.97, 0.85, 0.93)
names(n2)<- substances

# Frachtreduktion durch Maßnahemn
names_F<- list(substances, sources)

for (catchment in catchments) {
  index_catchment <- which(catchments==catchment)
  for(OgRe_type in OgRe_types){
    
  #Erstellen einer Matrix für die verbeleibende Fracht  
  x<- matrix(nrow = 3, ncol = length(sources), dimnames = names_F)
  
    for(substance in substances){
      index_substance<- which(substances==substance)
      for(my_source in sources){
        index_source<- which(sources==my_source)
        
        # Auswahl des richtigen Maßnahmenumfangs für den ausgewählten OgRe-type
        U<- eval(parse(text = paste0('U_', catchment, '_', OgRe_type)))
        
        # Frachtberechnung: Bestimmt, wieviel Fracht nach Anwendung der Maßnahemn ins Gewässer 
        # gelangt (spezifisch für verscheiden OgRe-types in verschiedenen catchments)
        x[index_substance, index_source]<- 1-U[1, index_source]*n1[index_substance]-U[2,index_source]*n2[index_substance]+U[1, index_source]*n1[index_substance]*U[2,index_source]*n2[index_substance]
      }
    }
  # Frachtreduktionspotenzial OgRe-type und catchment zuweisen
  assign(paste0('F_',catchment,'_',OgRe_type),x)
   }
}



####  2. Abschnitt Frachtberechnung unter Anwedung gewählter Szenarien

##    2.1 Festlegung von Parametern

# set a fixed start for the random algorithm
set.seed(5)
# Number of laps for the MonteCarlo simulation
nMC <- 1000

##    2.2 Laden von Inputdaten und erstellen von Ausgangstabellen
# ABIMO runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))

# read in paremeters for anchor_concentration
sd_list <- list()
for( OgRe_type in OgRe_types){
  index_OgRe_type <- which(OgRe_types==OgRe_type)
  # read in back calculated concentrations from OgRe
  assign(paste0('c_',OgRe_type), read.csv(paste0('data/Konz_',OgRe_type,'.csv'), sep = ';'))
  # read in relative standard deviations
  sd_list[[index_OgRe_type]] <-assign(paste0('rel_sd_',OgRe_type), read.csv(paste0('data/rel_sd_', OgRe_type,'.csv'), sep = ';'))
}

#creating data frame for final results (substance loads for every run)
MC_loads <- matrix(nrow = nMC, ncol = length(substances))
colnames(MC_loads)<- substances

substance_load <- matrix(nrow = nMC, ncol = 5)
colnames(substance_load)<- c('load_Dach','load_Strasse', 'load_Hof', 'load_Putzfassade', 'load_total')

#creating data frame for loads
MC_loads <- matrix(nrow = nMC, ncol = length(substances))
colnames(MC_loads)<- substances

substance_load <- matrix(nrow = nMC, ncol = 5)
colnames(substance_load)<- c('load_Dach','load_Strasse', 'load_Hof', 'load_Putzfassade', 'load_total')

##    2.3 Start der verknüpften Schleifen erstellen von Variablen 

for (catchment in catchments) {
# Auswahl des catchments
BTFs_current<- subset(berlin_runoff, AGEB1== catchment)
MC_loads_current <- MC_loads
# finding the areas for different sources and the total catachment are (with and without runoff formation)
areas <-c(sum(BTFs_current$AU_roof),sum(BTFs_current$AU_street),sum(BTFs_current$AU_yard),sum(BTFs_current$AU_putz), sum(sum(BTFs_current$FLGES),sum(BTFs_current$STR_FLGES)))
names(areas) <- c('roof', 'street', 'yard', 'putz', 'total catchment area')


# AUfgrund von nicht bekanntem Anschlussgrad von Fassaden an die Kanalisation wird jeder BTF ein zufälliger Anschlussgrad zwischen 10 und 90% zugewiesen
  for( n in 1:nrow(BTFs_current)){
    BTFs_current[n, 'runoff_Putzfassade'] <- BTFs_current[n,'runoff_Putzfassade']/0.5*runif(n=1, min = 0.1, max=0.9)
  }


  ##   2.4 Start der Einzugsgebietsspezifischen Berechnungen

  for (n in 1:nMC){
    ###calculate loads  
    for (OgRe_type in OgRe_types) {
    
      #Tabelle um berechneten Frachten zu speichern

    
      #Erstellen einer Matrix für die verbeleibende Fracht  
      names_source_loads<- list(substances, sources)
      source_loads<- matrix(nrow = 3, ncol = length(sources), dimnames = names_source_loads)
      names_Ogre_type_loads <- list ('load', substances)
      OgRe_type_loads <-matrix(nrow = 1, ncol = length(substances), dimnames = names_Ogre_type_loads)
    
      for (substance in substances) {
      
            #substanz auswählen
      
      
        for (my_source in sources) {
        
          ## Auswahl der richtigen Ankerkonzentration
          # Which concentration file is to be used
          OgRe_type_current <- eval(parse(text = paste0("c_", OgRe_type)))
          # which row and which column must be selected for the correct cell
          col_Konz <- which(names(OgRe_type_current) == paste0("Konz_", substance))
          row_Konz <- which(OgRe_type_current$Source == my_source)
          # find and store anchor concentration
          c_anchor <- OgRe_type_current[row_Konz, col_Konz]   
        
          ## Bestimmung von Schleifenindexen
          index_substance <- which(substances==substance)
          index_OgRe_type <- which(OgRe_types==OgRe_type)
          index_source <- which(sources== my_source) 
        
            ## Festlegung von Parametern für die log-Normalverteilung von Konzentrationswerten und Umgehung von etwaigen 0 Werten
          if(c_anchor == 0){
            concentration <- 0
          } else {
            rel_sd_temp<- as.data.frame(sd_list[[index_OgRe_type]])
            sd_temp<- c_anchor*rel_sd_temp[index_source,1+index_substance]
          
            ## Berechnung der log-Normalverteilten Konzentration  
            location<- log(c_anchor^2/sqrt(sd_temp^2+c_anchor^2))
            shape<- sqrt(log(1+(sd_temp^2/c_anchor^2)))
            concentration <- rlnorm(n=1, meanlog = location, sdlog = shape)
          }
        
          #pick all BTF belonging to OgRe_type
          row_runoff <- which(BTFs_current$OgRe_Type == OgRe_type)
          #pick column from BTF_current with runoffs from current source
          col_runoff <- which(names(BTFs_current) == paste0("runoff_", sources[index_source]))
        
          #Berechnung der Frachten ohne Maßnahmen (zufällig durch log Normalverteilung generiert)
          source_loads[index_substance, index_source] <- concentration*sum(BTFs_current[row_runoff, col_runoff])
        
        }
        OgRe_type_loads[,index_substance]<- sum(source_loads[index_substance,])
      }
    
      assign(paste0(OgRe_type,'_source_loads'), source_loads)
      assign(paste0(OgRe_type,'_loads'), OgRe_type_loads)
      }
  
    for (substance in substances) {
      index_substance <-  which(substances==substance)
      
      MC_loads_current[[n, index_substance]] <- sum(ALT_loads[index_substance],EFH_loads[index_substance],GEW_loads[index_substance],NEU_loads[index_substance],AND_loads[index_substance])/1000 #from g/kg

    }
  }
assign(paste0('MC_loads_',catchment),MC_loads_current)
}    

for(catchment in catchments){
  
  MC_loads<- eval(parse(text = paste0('MC_loads_',catchment)))
  
  write.csv(MC_loads, paste0('data_output/calculation_load_reduction/loads_',catchment,'_measure_extend_',mr*100,'%.csv'),row.names = FALSE)

}
