library(data.table)
library(plyr)


# Allgemeine Parameter
substances<- c('Diuron', 'Mecoprop', 'Zn')
OgRe_types<- c('ALT', 'EFH', 'GEW', 'NEU', 'AND')
sources <-  c("Dach", "Strasse", "Hof", "Putzfassade")
measures <- c('OSM', 'Filter')
catchments <- c('Wuhle', 'Flughafensee')

### 1. Abschnitt: Definition der Maßnahmen; Eingabe der Eingangsparameter

# Eingabe des Maßnahmenumfang (MU)
# Welcher Anteil der Abflüsse/Fassadenflächen aus den verschiedenen SST soll behandelt werden
MU<-0.2

# Eingabe eines Szenarionamens zum Abspeichern
Szenario<-'alternative_Fassadenfarbe'
#Szenario <- 'behandelter_Dachabfluss'
#Szenario <- 'behandelter_Quartiersabfluss'

# Erstellung der Verzeichnisse zum Abspeichern der Ergebnisse
dir.create('data_output', showWarnings = FALSE)
dir.create('data_output/Frachtmodell', showWarnings = FALSE)
dir.create('data_output/Frachtmodell/Frachten', showWarnings = FALSE)
dir.create('data_output/Frachtmodell/Aufwand', showWarnings = FALSE)

# Namensliste für die Bezeichnung der Maßnahmenmatrize
names_U<-list(measures, sources)

# Maßnahmenumfang für unterschiedliche Abflüsse:
# c(Dach_OSM, Dach_filter, Strasse_OSM, Strasse_Filter, Hof_OSM, Hof_fitler, putzfassade_OSM, Putzfassade_filter)
U_Wuhle_ALT<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Wuhle_EFH<-matrix(c(0, 0, 0, 0, 0, 0, MU, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Wuhle_GEW<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Wuhle_NEU<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Wuhle_AND<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)


# Maßnahmenumfang für unterschiedliche Abflüsse:
# c(Dach_OSM, Dach_filter, Strasse_OSM, Strasse_Filter, Hof_OSM, Hof_fitler, putzfassade_OSM, Putzfassade_filter)
U_Flughafensee_ALT<-matrix(c(0, 0, 0, 0, 0, 0, MU, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Flughafensee_EFH<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Flughafensee_GEW<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Flughafensee_NEU<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)
U_Flughafensee_AND<-matrix(c(0, 0, 0, 0, 0, 0, 0, 0), nrow = 2, ncol = 4, dimnames = names_U)

## 1.2 Reduktionspotenzial der Szenarien (verschiedenen Kombinationen der einzelnen Maßnahmen) im SST des jeweiligen Einzugsgebiets

# Maßnahmen Effizienz 
# m1= diuronfreie Farbe (als Dezimalzahl nicht in Prozent)
m1<- c(1, 0, 0)
names(m1)<- substances
#m2= Funkefilter (als Dezimalzahl nicht in Prozent)
m2<- c(0.9674866893,0.85004301,0.9264175444)
names(m2)<- substances

# Berechnung des Reduktionspotenzials der Szenarien

for (catchment in catchments) {
  index_catchment <- which(catchments==catchment)
  for(OgRe_type in OgRe_types){
    
    #Erstellen einer Ergebnis-Matrix für das Reduktionspotenzial in den einzelnen SST
    names_FA<- list(substances, sources)  
    x<- matrix(nrow = 3, ncol = length(sources), dimnames = names_FA)
    
    for(substance in substances){
      index_substance<- which(substances==substance)
      for(my_source in sources){
        index_source<- which(sources==my_source)
        
        # Auswahl des Maßnahmenumfangs für den ausgewählten OgRe-type und das ausgewählte Gewässer
        U<- eval(parse(text = paste0('U_', catchment, '_', OgRe_type)))
        
        # Berechnung des Frachtanteils, der nach Anwedung des Szenarios aus dem SST ins Gewässer gelangt 
        # (spezifisch für verscheiden OgRe-types in den verschiedenen catchments)
        # Verbeleibender Frachtanteil= 1-U1m1-U2m2+U1m1*U2m2 (bei zwei Maßnahemn)
        x[index_substance, index_source]<- 1-U[1, index_source]*m1[index_substance]-U[2,index_source]*m2[index_substance]+U[1, index_source]*m1[index_substance]*U[2,index_source]*m2[index_substance]
      }
    }
    # Zuweisen des Verbeleibenden Anteils der Fracht nach Anwendung des Szenarios
    # FA = Frachtanteil
    assign(paste0('FA_',catchment,'_',OgRe_type),x)
  }
}


###  2. Abschnitt Frachtberechnung unter Anwedung gewählter Szenarien

##   2.1 Festlegung von Parametern

# Fester Startwert für den random algorithm
set.seed(5)
# Anzahl der Wiederholung für die MonteCarlo simulation
nMC <- 5 #100

##    2.2 Laden von Inputdaten und erstellen von Ausgangstabellen
# ABIMO runoff und OgRe Daten (roof, yard, street (Fassaden runoff ist im Ablauf aus Hof und Straße enthalten))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))

# Einlesen der Parameter für die Berechnung der anchor_concentration

#Liste zum Speichern der Standardabweichung
sd_list <- list()

for( OgRe_type in OgRe_types){
  index_OgRe_type <- which(OgRe_types==OgRe_type)
  # Einlesen der zurückgerechneten Konzentrationen aus OgRe
  assign(paste0('c_',OgRe_type), read.csv(paste0('data/Konz_',OgRe_type,'.csv'), sep = ';'))
  # Einlesen der relativen Standardabweichung
  sd_list[[index_OgRe_type]] <- assign(paste0('rel_sd_',OgRe_type), read.csv(paste0('data/rel_sd_', OgRe_type,'.csv'), sep = ';'))
}

#Erstellen eines data frame für die Endergebnisse (Frachten der Substanzen für jede Wiederholung der MCS)
MC_loads <- matrix(nrow = nMC, ncol = length(substances))
colnames(MC_loads)<- substances

#Erstellen eines data frame Für die Substanzfrachten nach Quelle # Ich glaube es handelt sich hier um ein Relikt und kann gelöscht werden
substance_load <- matrix(nrow = nMC, ncol = 5)
colnames(substance_load)<- c('load_Dach','load_Strasse', 'load_Hof', 'load_Putzfassade', 'load_total')

##    2.3 Start der verknüpften Schleifen,  Erstellen der benötigten Variablen
#     Auswahl der BTF im Einzugsgebiet, Bestimmung der Quellflächen und Anpassung des Fassadenabflussen

for (catchment in catchments) {
  # BTF auf das aktuelle Catchment reduzieren
  BTFs_current<- subset(berlin_runoff, AGEB1== catchment)
  #temporärer data frame dessen Ergebnisse später als Frachten des aktuellen Einzugsgebiet gespeichert werden
  MC_loads_current <- MC_loads
  # Berechnung der Gesamtflächen der verschiedenn Quellen im Einzugsgebiet
  areas <-c(sum(BTFs_current$AU_roof),sum(BTFs_current$AU_street),sum(BTFs_current$AU_yard),sum(BTFs_current$AU_putz), sum(sum(BTFs_current$FLGES),sum(BTFs_current$STR_FLGES)))
  names(areas) <- c('roof', 'street', 'yard', 'putz', 'total catchment area')
  
  
  ##   2.4 Start der Frachteberechnung für das aktuelle Einzugsgebiet ohne Maßnahmen
  
  for (n in 1:nMC){
    ###calculate loads
    print(n)
    for (OgRe_type in OgRe_types) {
      
      #Tabelle um berechneten Frachten zu speichern
      
      #Erstellen einer Matrix für die Ergebnisse der Frachtberechnungen aufgeschlüsselt auf die Quellen 
      names_source_loads<- list(substances, sources)
      source_loads<- matrix(nrow = 3, ncol = length(sources), dimnames = names_source_loads)
      
      #Erstellen einer Matrix für die Ergebnisse der Frachtberechnungen für jeden SST
      names_Ogre_type_loads <- list ('load', substances)
      OgRe_type_loads <-matrix(nrow = 1, ncol = length(substances), dimnames = names_Ogre_type_loads)
      
      #substanz auswählen
      for (substance in substances) {
        
        
        
        for (my_source in sources) {
          
          ## Auswahl der richtigen Ankerkonzentration
          
          # Auswahl der Konzentrationstabelle des aktuellen OgRe_types
          OgRe_type_current <- eval(parse(text = paste0("c_", OgRe_type)))
          # Auswahl der aktuellen Zeile und Spalte um die Ankerkonzentration auszuwählen
          col_Konz <- which(names(OgRe_type_current) == paste0("Konz_", substance))
          row_Konz <- which(OgRe_type_current$Source == my_source)
          # Speichern der aktuellen Ankerkonzentration
          c_anchor <- OgRe_type_current[row_Konz, col_Konz]   
          
          ## Bestimmung von Schleifenindexen
          index_substance <- which(substances==substance)
          index_OgRe_type <- which(OgRe_types==OgRe_type)
          index_source <- which(sources== my_source) 
          ###################################################################
          BTFs_current_OgRe <- subset(BTFs_current,OgRe_Type== OgRe_type)
          current_BTFs<-rownames(BTFs_current_OgRe)
                                 
          current_loads_BTFs_source<- matrix(nrow = length(current_BTFs), ncol = 1)
          col_runoff <- which(names(BTFs_current_OgRe) == paste0("runoff_", sources[index_source]))

          
          for(BTF in current_BTFs){
          
          ## Festlegung von Parametern für die log-Normalverteilung von Konzentrationswerten und Umgehung von etwaigen 0 Werten
            if(c_anchor == 0){
              concentration <- 0
            } else {
              #Auswahl der relativen Standardabweichung des aktuellen OgRe_types
              rel_sd_temp<- as.data.frame(sd_list[[index_OgRe_type]])
              # Berechnung der aktuellen absoluten Standardabweichung
              sd_temp<- c_anchor*rel_sd_temp[index_source,1+index_substance]
              
              ## Berechnung der log-Normalverteilten Konzentration  (Wie kommme ich auf die Berechnung von location und shape?)
              # Bestimmung der benötigten Parameter für die logarithmische Normalverteilung
              location<- log(c_anchor^2/sqrt(sd_temp^2+c_anchor^2))
              shape<- sqrt(log(1+(sd_temp^2/c_anchor^2)))
              
              # Berechnung der logarithmisch normalverteilten Konzentration
              concentration <- rlnorm(n=1, meanlog = location, sdlog = shape)
            }  
            
          index_BTF <- which(BTF== current_BTFs) 
          BTF_current<-subset(BTFs_current_OgRe[BTF,])
          current_loads_BTFs_source[index_BTF]<-BTF_current[,col_runoff]*concentration
          }

          #Summe aller Frachten aus einem bestimmten Quellgebiet, für einen OgRe-Typen
          source_loads[index_substance, index_source]<-sum(current_loads_BTFs_source)
        }
      }
      
      
      ## 2.5 Start der Frachtberechnung mit Maßnahmen
      
      #Auswahl des Frachtanteils nach Umsetzung des aktuellen Szeanrios im aktuellen SST
      current_FA <- eval(parse(text = paste0('FA_',catchment,'_',OgRe_type)))
      #Berechnung der reduzierten Fracht nach Quelle in einem SSt
      reduced_source_loads<-source_loads*current_FA
      
      for (substance in substances) {
        index_substance <-  which(substances==substance)
        # Berechnung der Gesamtfracht einer Substanz aus allen Quellen in einem SST
        OgRe_type_loads[,index_substance]<- sum(reduced_source_loads[index_substance,])
        # Speichern der Ergebnisse
        assign(paste0(OgRe_type,'_loads'), OgRe_type_loads)  
      }
    }
    
    for (substance in substances) {
      index_substance <-  which(substances==substance)
      # Berechnung der Gesamtfracht einer Substanz aus allen SST und speichern des Ergebnis in der Zeile des aktuellen runs 
      MC_loads_current[[n, index_substance]] <- sum(ALT_loads[index_substance],EFH_loads[index_substance],GEW_loads[index_substance],NEU_loads[index_substance],AND_loads[index_substance])/1000 #from g/kg
      
    }
  }
  #Speichern des Ergebnisses der reduzierten Frachten im Einzugsgebiet für alle Runs der MC
  assign(paste0('MC_loads_',catchment),MC_loads_current)
}    

for(catchment in catchments){
  
  # Auswahl der Ergebnisse des aktuellen Einzugsgebiets
  MC_loads<- eval(parse(text = paste0('MC_loads_',catchment)))
  
  # Schreiben eines Ergebnisfiles aller Runs der MC für das gewählte Szenario 
  write.csv(MC_loads, paste0('data_output/Frachtmodell/Frachten/', catchment,'_', Szenario ,'.csv'),row.names = FALSE)
  
}

### 3. Abschnitt: Berechnung des Maßnahmenaufwands (wie viel Volumen muss gefiltert werden,
#               wie viel, Fläche an der Quelle müssen behandelt werden )

for (catchment in catchments) {
  
  Putzfassadenfläche_behandelt <- 0
  Volumen_behandelt <-0 
  
  # Auswahl aller BTF die zum Einzugsgebiet gehören
  BTFs_catchment<- subset(berlin_runoff, AGEB1== catchment)
  # benötigte Spaltennamen anpassen
  colnames(BTFs_catchment) <- c(names(BTFs_catchment[0:51]),'AU_Strasse','AU_Hof','AU_Dach','AU_Putzfassade')
  
  #Berechnung des gesamten Abflussvolumens und der Gesamtfläche an Putzfassaden im Einzugsgebiet
  Putzfassadenfläche_gesamt <- sum(BTFs_catchment$AU_Putzfassade)
  Volumen_gesamt <- sum(BTFs_catchment$runoff_total)
  
  for (OgRe_type in OgRe_types) {
    
    #Auswahl der BTF des Einzugsgebiets die dem aktuellen SST entsprechen
    BTFs_OgRe_type<- as.data.frame(subset( BTFs_catchment, OgRe_Type== OgRe_type))
    
    # Auswahl des Maßnahmenumfangs im aktuellen SST im aktuellen Einzugsgebiet
    U<- eval(parse(text = paste0('U_', catchment, '_', OgRe_type)))
    
    # Berechnung der gesamten behandelten Putzfassadenfläche (die behandelte Fläche des jeweiligen SST wird durch die Schleife zu den vorher bestimmten Flächen addiert)
    Putzfassadenfläche_behandelt <- Putzfassadenfläche_behandelt+ U[1,4]*sum(BTFs_OgRe_type$AU_Putzfassade)
    
    for (my_source in sources) {
      index_source <- which(sources== my_source) 
      # Berechnung des gesamten behandelten Abflussvolumens (durch die Schleife werden die Volumina der einzelnen behandeten Quellen in den einzelnen SST aufsummiert)
      Volumen_behandelt <- Volumen_behandelt+ U[2,index_source]*sum(BTFs_OgRe_type[paste0('runoff_',my_source)])
    }
  }
  
  # Speichern des Anteils der behandelten Fläche und des behandelten Volumen an der Gesamtfläche und des Gesamtvolumens  
  Aufwand_current<- cbind(Volumen_behandelt/Volumen_gesamt*100,Putzfassadenfläche_behandelt/Putzfassadenfläche_gesamt*100)
  # Umwandlung in einen data frame
  as.data.frame(Aufwand_current)
  # Spalten Namen des data frames
  colnames(Aufwand_current) <- c('Volumen [%]', 'Fassade [%]')
  # Schreiben eines files mit dem Aufwand des betrachteten Szenarios
  write.csv(Aufwand_current,paste0('data_output/Frachtmodell/Aufwand/', catchment,'_', Szenario ,'_Aufwand.csv'),row.names = FALSE)
}


