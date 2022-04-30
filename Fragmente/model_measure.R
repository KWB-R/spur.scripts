library(data.table)

# set a fixed start for the random algorithm
set.seed(5)

### loop parameters for substances, city structure types and sources
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
OgRe_types <- c("ALT", "EFH", "GEW", "NEU", "AND")
sources <- c("Dach", "Strasse", "Hof", "Putzfassade")

###load data
# ABIMO runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))

#choosing catchment area (5th for loop as wrapper, catchment vector)
BTF_input <- subset(berlin_runoff, AGEB1=='Wuhle')

# finding the areas for different sources and the total catachment are (with and without runoff formation)
areas <-c(sum(BTF_input$AU_roof),sum(BTF_input$AU_street),sum(BTF_input$AU_yard),sum(BTF_input$AU_putz), sum(sum(BTF_input$FLGES),sum(BTF_input$STR_FLGES)))
names(areas) <- c('roof', 'street', 'yard', 'putz', 'total catchment area')

#due to the uncertain connection of facades to the sewerage system, all BTF are assigned a connection grade between 10 and 90%.
for( n in 1:nrow(BTF_input)){
  BTF_input[n, 'runoff_Putzfassade'] <- BTF_input[n,'runoff_Putzfassade']/0.5*runif(n=1, min = 0.1, max=0.9)
}
  
# read in back calculated concentrations from OgRe
# read in relative standard deviations
sd_list <- list()
for( OgRe_type in OgRe_types){
  index_OgRe_type <- which(OgRe_types==OgRe_type)
  assign(paste0('c_',OgRe_type), read.csv(paste0('data/Konz_',OgRe_type,'.csv'), sep = ';')) 
  sd_list[[index_OgRe_type]] <-assign(paste0('rel_sd_',OgRe_type), read.csv(paste0('data/rel_sd_', OgRe_type,'.csv'), sep = ';'))
}

# creating data fram to store loads for every source in every BTF
substance_output <- data.frame("ID" = BTF_input$CODE,
                               "Gewässser" = BTF_input$AGEB1,
                               "OgRe_Type" = BTF_input$OgRe_Type,
                               "load_Dach" = NA,
                               "load_Strasse" = NA,
                               "load_Hof" = NA,
                               "load_Putzfassade" = NA)


# Number of laps for the MonteCarlo simulation
nMC <- 1000

#creating data frame for loads
MC_loads <- matrix(nrow = nMC, ncol = length(substances))
colnames(MC_loads)<- substances

substance_load <- matrix(nrow = nMC, ncol = 5)
colnames(substance_load)<- c('load_Dach','load_Strasse', 'load_Hof', 'load_Putzfassade', 'load_total')

for (substance in substances){
  assign(paste0(substance,'_osm'), substance_load)
  assign(paste0(substance,'_osm_filtered'), substance_load)
}

#measure on the source:
measure_extent <- c(0.2, 0, 0, 0, 0, 0)
reduction_efficiencies <- c(1,0,0,0,0,0)

#filter
filter_capacity <- 0.2
filter_efficiencies <- c(0.97, 0.85, 0.93 ,0.94, 0.93, 0.89)


for (n in 1:nMC){
  ###calculate loads  
  for (substance in substances) {
    
    #substanz auswählen
    
    
    for (OgRe_type in OgRe_types) {
      
      #Zeilen auswählen die OgRe_type entsprechen
      
      
      for (my_source in sources) {
        
        # Which concentration file is to be used
        OgRe_type_current <- eval(parse(text = paste0("c_", OgRe_type)))
        # which row and which column must be selected for the correct cell
        col_Konz <- which(names(OgRe_type_current) == paste0("Konz_", substance))
        row_Konz <- which(OgRe_type_current$Source == my_source)
        # find and store anchor concentration
        c_anchor <- OgRe_type_current[row_Konz, col_Konz]
        
        index_substance <- which(substances==substance)
        index_OgRe_type <- which(OgRe_types==OgRe_type)
        
        #Set parameters for lognormal distribution; bypass 0
        if(c_anchor == 0){
          concentration <- 0
        } else {
          rel_sd_temp<- as.data.frame(sd_list[[index_OgRe_type]])
          sd_temp<- c_anchor*rel_sd_temp[1,1+index_substance]
          
          location<- log(c_anchor^2/sqrt(sd_temp^2+c_anchor^2))
          shape<- sqrt(log(1+(sd_temp^2/c_anchor^2)))
          concentration <- rlnorm(n=1, meanlog = location, sdlog = shape)
        }
        #pick all BTF belonging to OgRe_type
        row_runoff <- which(BTF_input$OgRe_Type == OgRe_type)
        index_source <- which(sources== my_source)
        
        #pick column from BTF_input with runoffs from current source
        col_runoff <- which(names(BTF_input) == paste0("runoff_", sources[index_source]))
        #selects the column in which the calculated load is to be written
        col_output <- which(names(substance_output) == paste0("load_", sources[index_source]))
        
        #calculates the load and writes it into the intended cell (calculation for fixed facade runoff)
        
        treated_volume <- measure_extent[index_substance]*BTF_input[row_runoff, col_runoff]
        treated_concentration <- concentration * (1-reduction_efficiencies[index_substance])
        untreated_volume <- (1-measure_extent[index_substance])*BTF_input[row_runoff, col_runoff]
        untreated_concentration <- concentration
        
        substance_output[row_runoff, col_output] <- treated_volume * treated_concentration + untreated_volume * untreated_concentration
        
      }
    }
    
    #Ergebnis einem neuen data.frame zuweisen
    assign(paste0(substance, '_output'), substance_output)
    current_output <- assign(paste0(substance, '_output'), substance_output)
    
    MC_loads[[n, index_substance]]<- sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE))/1000 #from g to kg
    
    
    
    #Stoffspezifische Ergebnisse mit Aufschlüsselung nach Quelle zuweisen (temporär, weil hardgecoded)  
    current_load<-c(colSums(Filter(is.numeric, current_output),na.rm = TRUE)[1:4], sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE)))/1000 #from g to kg
    
    if(substance== 'Diuron'){
      Diuron_osm[n,]<-current_load 
      Diuron_osm_filtered[n,]<- Diuron_osm[n,] - filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Mecoprop'){
      Mecoprop_osm[n,]<-current_load
      Mecoprop_osm_filtered[n,]<- Mecoprop_osm[n,]- filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Terbutryn'){
      Terbutryn_osm[n,]<-current_load
      Terbutryn_osm_filtered[n,]<- Terbutryn_osm[n,]- filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Benzothiazol'){
      Benzothiazol_osm[n,]<-current_load
      Benzothiazol_osm_filtered[n,]<- Benzothiazol_osm[n,]- filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Zn'){
      Zn_osm[n,]<-current_load
      Zn_osm_filtered[n,]<- Zn_osm[n,]- filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Cu'){
      Cu_osm[n,]<-current_load
      Cu_osm_filtered[n,]<- Cu_osm[n,]- filter_capacity*current_load*filter_efficiencies[index_substance]
    }
    
  }
 #load reduction through filtration.dependent from filter efficiencies per substance and treated volume (filter capacity) 
    MC_loads[n,] <-MC_loads[n,] - filter_capacity*sweep(MC_loads, MARGIN = 2, filter_efficiencies, '*')[n,] 
}


write.csv(MC_loads,'data_output/calculation_load_reduction/measure_loads.csv',row.names = FALSE)

write.csv(Diuron_osm_filtered, 'data_output/calculation_load_reduction/measure_Diuron.csv', row.names = FALSE)
write.csv(Mecoprop_osm_filtered, 'data_output/calculation_load_reduction/measure_Mecoprop.csv', row.names = FALSE)
write.csv(Terbutryn_osm_filtered, 'data_output/calculation_load_reduction/measure_Terbutryn.csv', row.names = FALSE)
write.csv(Benzothiazol_osm_filtered, 'data_output/calculation_load_reduction/measure_Benzothiazol.csv', row.names = FALSE)
write.csv(Zn_osm_filtered, 'data_output/calculation_load_reduction/measure_Zn.csv', row.names = FALSE)
write.csv(Cu_osm_filtered, 'data_output/calculation_load_reduction/measure_Cu.csv', row.names = FALSE)
