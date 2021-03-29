library(data.table)

###city structure types and sources
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
OgRe_types <- c("ALT", "EFH", "GEW", "NEU", "AND")
sources <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade")

###load data
# ABIMO runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade'))
#choosing catchment area (5th for loop as wrapper, catchment vector)
BTF_input <- subset(berlin_runoff, AGEB1=='Wuhle')

  which(names(BTF_input) == paste0("runoff_", sources[index_source]))
  
# read in back calculated concentrations from OgRe
# read in relative standard deviations
sd_list <- list()
for( OgRe_type in OgRe_types){
  index_OgRe_type <- which(OgRe_types==OgRe_type)
  assign(paste0('c_',OgRe_type), read.csv(paste0('data/Konz_',OgRe_type,'.csv'), sep = ';')) 
  sd_list[[index_OgRe_type]] <-assign(paste0('rel_sd_',OgRe_type), read.csv(paste0('data/rel_sd_', OgRe_type,'.csv')))
}

# creating data fram to store loads for every source in every BTF
substance_output <- data.frame("ID" = BTF_input$CODE,
                               "Gewässser" = BTF_input$AGEB1,
                               "OgRe_Type" = BTF_input$OgRe_Type,
                               "load_Bitumendach" = NA,
                               "load_Ziegeldach" = NA,
                               "load_Dach_weitere" = NA,
                               "load_Strasse" = NA,
                               "load_Hof" = NA,
                               "load_Putzfassade" = NA)



# set a fixed start for the random algorithm
set.seed(5)

# Number of laps for the MonteCarlo simulation
nMC <- 1000

#creating data frame for loads
total_loads <- matrix(nrow = nMC, ncol = length(substances))

substance_load <- matrix(nrow = nMC, ncol = 5)
colnames(substance_load)<- c('load_Dach','load_Strasse', 'load_Hof', 'load_Putzfassade', 'load_total')
Diuron_load <- substance_load
Mecoprop_load <- substance_load
Terbutryn_load <- substance_load
Benzothiazol_load <- substance_load
Zn_load <- substance_load
Cu_load <- substance_load


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
        
        #Set parameters for lognormal distribution; bypass 0
        if(c_anchor == 0){
          concentration <- 0
        } else {
          index_substance <- which(substances==substance)
          index_OgRe_type <- which(OgRe_types==OgRe_type)
          
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
       
        substance_output[row_runoff, col_output] <- concentration * BTF_input[row_runoff, col_runoff]
        
      }
    }
    
    #Ergebnis einem neuen data.frame zuweisen
    assign(paste0(substance, '_output'), substance_output)
    current_output <- assign(paste0(substance, '_output'), substance_output)
    
    
    total_loads[[n, index_substance]]<- sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE))/1000 #from g to kg
    
    
    #Stoffspezifische Ergebnisse mit Aufschlüsselung nach Quelle zuweisen (temporär, weil hardgecoded)  
    current_load<-c(sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE)[1:3]),colSums(Filter(is.numeric, current_output),na.rm = TRUE)[4:6], sum(colSums(Filter(is.numeric, current_output),na.rm = TRUE)))/1000 #from g to kg

    if(substance== 'Diuron'){
    Diuron_load[n,]<-current_load
   }else if(substance== 'Mecoprop'){
    Mecoprop_load[n,]<-current_load
   }else if(substance== 'Terbutryn'){
     Terbutryn_load[n,]<-current_load
   }else if(substance== 'Benzothiazol'){
     Benzothiazol_load[n,]<-current_load
   }else if(substance== 'Zn'){
     Zn_load[n,]<-current_load
   }else if(substance== 'Cu'){
     Cu_load[n,]<-current_load
   }

  }
  
}  

colnames(total_loads)<-substances
write.csv(total_loads,'data_output/calculation_status_quo/Wuhle_simulated_loads.csv',row.names = FALSE)

write.csv(Diuron_load, 'data_output/calculation_status_quo/Wuhle_load_Diuron.csv', row.names = FALSE)
write.csv(Mecoprop_load, 'data_output/calculation_status_quo/Wuhle_load_Mecoprop.csv', row.names = FALSE)
write.csv(Terbutryn_load, 'data_output/calculation_status_quo/Wuhle_load_Terbutryn.csv', row.names = FALSE)
write.csv(Benzothiazol_load, 'data_output/calculation_status_quo/Wuhle_load_Benzothiazol.csv', row.names = FALSE)
write.csv(Zn_load, 'data_output/calculation_status_quo/Wuhle_load_Zn.csv', row.names = FALSE)
write.csv(Cu_load, 'data_output/calculation_status_quo/Wuhle_load_Cu.csv', row.names = FALSE)
