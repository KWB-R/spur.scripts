library(data.table)

#directory for output data
write.dir <- "data_output/"




# 1. read input tables --------------------------------------------------------------------

# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
BTF_input <- foreign::read.dbf('data/berlin_runoff.dbf')
BTF_input <- setnames(BTF_input, old=c('runoff_str', 'runoff_yar', 'runoff_bit', 'runoff_zie', 'runoff_res', 'runoff_put'), new= c('runoff_Strasse','runoff_Hof','runoff_Bitumendach','runoff_Ziegeldach','runoff_Dach_weitere','runoff_Putzfassade'))



# backcalculated concentrations from OgRe
c_NEU <- read.csv(file = 'data/Konz_NEU.csv', sep = ';')
c_ALT <- read.csv(file = 'data/Konz_ALT.csv', sep = ';')
c_GEW <- read.csv(file = 'data/Konz_GEW.csv', sep = ';')
c_EFH <- read.csv(file = 'data/Konz_EFH.csv', sep = ';')
conc_OgRe <- list(c_NEU, c_ALT, c_GEW, c_EFH)


# measurements from BaSaR
BBRf <- read.csv(file = 'data_prelim_sources/BBRf_20200518_conc.txt', sep = ';')
BBRr <- read.csv(file = 'data_prelim_sources/BBRr_20200518_conc.txt', sep = ';')
BBRs <- read.csv(file = 'data_prelim_sources/BBRs_20200518_conc.txt', sep = ';')
BBWf <- read.csv(file = 'data_prelim_sources/BBWf_20200518_conc.txt', sep = ';')
BBWr <- read.csv(file = 'data_prelim_sources/BBWr_20200518_conc.txt', sep = ';')
BBWs <- read.csv(file = 'data_prelim_sources/BBWs_20200518_conc.txt', sep = ';')
conc_BaSaR <- list(BBRf, BBRr, BBRs, BBWf, BBWr, BBWs)

# literature data mining?


# field data OST (M. Burkhardt?)


# 1.1 define anchor for distributions
# 2. build distributions for concentrations ----------------------------------------------

# get facade diuron, backcalculated from OgRe


#Müsste nach unseren neuen Erkenntnissen Terbutryn nicht aus Dach und Fassade stammen?
# get facade terbutryn, backcalculated from OgRe


# get roof mecoprop, backcalculated from OgRe

#benzothiazol kommt vor allem aus dem Straßenablauf wird das beachtet?
# get roof benzothiazol, backcalculated from OgRe


# get roof zinc, backcalculated from OgRe

        
#kupfer kommt vor allem aus dem Straßenablauf wird das beachtet?
# get roof copper, backcalculated from OgRe

# BaSaR...


substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
locations <- c('BBRf', 'BBRr', 'BBRs', 'BBWf', 'BBWr', 'BBWs')

for (i in 1:length(substances)){
  
  for (j in 1:length(locations)){
    print(paste(paste(substances[i], locations[j],sep = "_"), '<-', paste('conc_BaSaR$', locations[j], '_20200518_conc.csv$', substances[i], sep = "")))   
  }  
}

Diuron_BBRf <- conc_BaSaR$BBRf_20200518_conc.csv$Diuron
Diuron_BBRr <- conc_BaSaR$BBRr_20200518_conc.csv$Diuron
DiurOn_BBRs <- conc_BaSaR$BBRs_20200518_conc.csv$Diuron
Diuron_BBWf <- conc_BaSaR$BBWf_20200518_conc.csv$Diuron
Diuron_BBWr <- conc_BaSaR$BBWr_20200518_conc.csv$Diuron
Diuron_BBWs <- conc_BaSaR$BBWs_20200518_conc.csv$Diuron

# other sources of concentration data

# put all together and make random draw from distribution

# random draw from [lognormal?] distribution with observed mean and sd
# mecoprop_normal <- rlnorm(n = 1, 
#                           meanlog = mean(log(mecoprop)), 
#                           sdlog = sd(log(mecoprop)))
# mecoprop_reduced <- coeff_reduction_mecoprop * mecoprop_normal
# diuron_normal <- ...
# diuron_reduced <- ...
# ...(other substances)

# conc_yard_rest (?)
# conc_street_rest (OgRe STR)



# 3. emissions status quo ---------------------------------------------------------------


substance_output <- data.frame("ID" = BTF_input$CODE,
                                            "Gewässser" = BTF_input$AGEB1,
                                            "OgRe_Typ" = BTF_input$OgRe_Type,
                                            "load_Bitumendach" = NA,
                                            "load_Ziegeldach" = NA,
                                            "load_Dach_weitere" = NA,
                                            "load_Strasse" = NA,
                                            "load_Hof" = NA,
                                            "load_Putzfassade" = NA)

#for(substance in substances){
#a <-paste(substance, 'output', sep = '_') 
#a <- data.frame("ID" = BTF_input$CODE,
#                                         "Gewässser" = BTF_input$AGEB1,
#                                          "OgRe_Typ" = BTF_input$OgRe_Type,
#                               "load_Bitumendach" = NA,
#                               "load_Ziegeldach" = NA,
#                               "load_Dach_weitere" = NA,
#                               "load_Strasse" = NA,
#                               "load_Hof" = NA,
#                               "load_Putzfassade" = NA)
#}

for (substance in substances){
  paste(substance,'output', sep= '_')
  print(paste(substance,'output', sep= '_'))
}








#prepare output tables status quo
for (substance in substances){
paste(substance,'output', sep= '_') <- data.frame("ID" = BTF_input$CODE,
                                                  "Gewässser" = BTF_input$AGEB1,
                                                  "OgRe_Typ" = BTF_input$OgRe_Type,
                                                  "load_Bitumendach" = NA,
                                                  "load_Ziegeldach" = NA,
                                                  "load_Dach_weitere" = NA,
                                                  "load_Strasse" = NA,
                                                  "load_Hof" = NA,
                                                  "load_Putzfassade" = NA)
  
  
  print(paste(substance,'output', sep= '_')) 

} 

print(paste(substance,'output', sep= '_'))



<- data.frame("ID" = BTF_input$CODE,
                        "Gewässser" = BTF_input$AGEB1,
                        "OgRe_Typ" = BTF_input$OgRe_Type,
                        "load_Bitumendach" = NA,
                        "load_Ziegeldach" = NA,
                        "load_Dach_weitere" = NA,
                        "load_Strasse" = NA,
                        "load_Hof" = NA,
                        "load_Putzfassade" = NA)
print(paste(substance,'output', sep= '_')

#      load_blockteilfläche = load_roof + load_street + load_yard

#      load_roof = vol_roof * mecoprop_normal

#      load_street = load_facade + load_street_rest
#      load_yard = load_facade + load_yard_rest

#      load_facade = vol_facade * diuron_normal
#      load_street_rest = vol_street_rest * conc_street_rest
#      load_yard_rest = vol_yard_rest * conc_yard_rest

#      load_tileroof
#      load_bitumenroof
#      load_greenroof
#      ...(other sources)



# 4. reduced emissions (measures) -----------------------------------------------------

# 4.1 vary reduction parameters
# coeff_reduction_mecoprop <- random draw from distribution
# same for other parameters and measures

# for each substance:

# load_facade = load_facade_reduced + load_facade_normal 
# load_roof = load_roof_reduced + load_roof_normal

# effect of low-emission paint/other materials (roof tiles, bitumen membrane, etc.):

# load_facade_reduced = 
#    vol_facade * percent_facade_painted * conc_facade_reduced + 
#    vol_facade * (1 - percent_facade_painted) * conc_facade
# same for roof

# effect of filter:
# load_blockteilfläche = load_filtered + load_unfiltered

# load_unfiltered_mecoprop_bitumenroof = (1 - %filtered_vol_bitumenroof)*load_bitumenroof
# load_filtered_mecoprop_bitumenroof = %filtered_vol_bitumenroof*coeff_filter_mecoprop*load_bitumenroof

# reduction factors for filter from SpuR data

# substances / sources

# geographic distribution of filters
