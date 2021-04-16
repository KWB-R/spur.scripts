library(data.table)

# set a fixed start for the random algorithm
set.seed(5)

###city structure types and sources
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
OgRe_types <- c("ALT", "EFH", "GEW", "NEU", "AND")
sources <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade")

###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
BTF_input <- foreign::read.dbf('data/berlin_runoff.dbf')
BTF_input <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade','runoff_total'))

#due to the uncertain connection of facades to the sewerage system, all BTF are assigned a connection grade between 10 and 90%.
for( n in 1:nrow(BTF_input)){
  BTF_input[n,52] <- BTF_input[n,52]/0.5*runif(n=1, min = 0.1, max=0.9)
}
  
# read in backcalculated concentrations from OgRe
# read in relativ standard deviations
sd_list <- list()
for( OgRe_type in OgRe_types){
  assign(paste0('c_',OgRe_type), read.csv(paste0('data/Konz_',OgRe_type,'.csv'), sep = ';')) 
  index_OgRe_type <- which(OgRe_types==OgRe_type)
  sd_list[[index_OgRe_type]] <-assign(paste0('rel_sd_',OgRe_type), read.csv(paste0('data/rel_sd_', OgRe_type,'.csv')))
}

substance_output <- data.frame("ID" = BTF_input$CODE,
                               "Gewässser" = BTF_input$AGEB1,
                               "OgRe_Type" = BTF_input$OgRe_Type,
                               "load_Bitumendach" = NA,
                               "load_Ziegeldach" = NA,
                               "load_Dach_weitere" = NA,
                               "load_Strasse" = NA,
                               "load_Hof" = NA,
                               "load_Putzfassade" = NA)



#creating data frame for loads
nMC <- 1000
total_reduced <- matrix(nrow = nMC, ncol = length(substances))

substance_load <- matrix(nrow = nMC, ncol = 5)
colnames(substance_load)<- c('load_Dach','load_Strasse', 'load_Hof', 'load_Putzfassade', 'load_total')
Diuron_reduced <- substance_load
Mecoprop_reduced <- substance_load
Terbutryn_reduced <- substance_load
Benzothiazol_reduced <- substance_load
Zn_reduced <- substance_load
Cu_reduced <- substance_load

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
        
        OgRe_type_current <- eval(parse(text = paste0("c_", OgRe_type)))
        
        col_Konz <- which(names(OgRe_type_current) == paste0("Konz_", substance))
        row_Konz <- which(OgRe_type_current$Source == my_source)
        
        c_anchor <- OgRe_type_current[row_Konz, col_Konz]
        
        index_substance <- which(substances==substance)
        index_OgRe_type <- which(OgRe_types==OgRe_type)
        #with lognormal distribiuted concentrations
        if(c_anchor == 0){
          concentration <- 0
        } else {
          rel_sd_temp<- as.data.frame(sd_list[[index_OgRe_type]])
          sd_temp<- c_anchor*rel_sd_temp[1,1+index_substance]
          
          location<- log(c_anchor^2/sqrt(sd_temp^2+c_anchor^2))
          shape<- sqrt(log(1+(sd_temp^2/c_anchor^2)))
          concentration <- rlnorm(n=1, meanlog = location, sdlog = shape)
        }
        #pick all BTF which are OgRe_type
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
        
        
        # create normal distributed runoff volumes for facades
        #if (my_source == "Putzfassade" ){
        #  facade_proportion<- runif(n=1, min = 0.1, max = 0.9)
        #substance_output[row_runoff, col_output] <- concentration * BTF_input[row_runoff, col_runoff]/0.5*facade_proportion
        
        #}else{
        #substance_output[row_runoff, col_output] <- concentration * BTF_input[row_runoff, col_runoff]
        #}
        
        #Quelle auswählen (im entsprechenden c_ File (1 Zelle) und Abflussfile (1 Spalte))
        #multiplizieren
        
      }
    }
    
    #Ergebnis einem neuen data.frame zuweisen
    assign(paste0(substance, '_output'), substance_output)
    current_output <- assign(paste0(substance, '_output'), substance_output)
    
    total_reduced[[n, index_substance]]<- sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE))/1000 #from g to kg
    
    #load reduction through filtration.dependent from filter efficiencies per substance and treated volume (filter capacity) 
    total_reduced <-total_reduced - filter_capacity*sweep(total_reduced, MARGIN = 2, filter_efficiencies, '*')
    
    #Stoffspezifische Ergebnisse mit Aufschlüsselung nach Quelle zuweisen (temporär, weil hardgecoded)  
    current_load<-c(sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE)[1:3]),colSums(Filter(is.numeric, current_output),na.rm = TRUE)[4:6], sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE)))/1000 #from g to kg
    
    if(substance== 'Diuron'){
      Diuron_reduced[n,]<-current_load - filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Mecoprop'){
      Mecoprop_reduced[n,]<-current_load - filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Terbutryn'){
      Terbutryn_reduced[n,]<-current_load - filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Benzothiazol'){
      Benzothiazol_reduced[n,]<-current_load - filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Zn'){
      Zn_reduced[n,]<-current_load - filter_capacity*current_load*filter_efficiencies[index_substance]
    }else if(substance== 'Cu'){
      Cu_reduced[n,]<-current_load - filter_capacity*current_load*filter_efficiencies[index_substance]
    }
    
  }
  
}  

colnames(total_reduced)<-substances
write.csv(total_reduced,'data_output/calculation_load_reduction/reduced_simulated_loads.csv',row.names = FALSE)

write.csv(Diuron_reduced, 'data_output/calculation_load_reduction/reduced_load_Diuron.csv', row.names = FALSE)
write.csv(Mecoprop_reduced, 'data_output/calculation_load_reduction/reduced_load_Mecoprop.csv', row.names = FALSE)
write.csv(Terbutryn_reduced, 'data_output/calculation_load_reduction/reduced_load_Terbutryn.csv', row.names = FALSE)
write.csv(Benzothiazol_reduced, 'data_output/calculation_load_reduction/reduced_load_Benzothiazol.csv', row.names = FALSE)
write.csv(Zn_reduced, 'data_output/calculation_load_reduction/reduced_load_Zn.csv', row.names = FALSE)
write.csv(Cu_reduced, 'data_output/calculation_load_reduction/reduced_load_Cu.csv', row.names = FALSE)
