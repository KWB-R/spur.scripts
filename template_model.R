# 1. read input tables --------------------------------------------------------------------

# abimo runoff (roof, yard, street (last two include facade runoff))
vol <- foreign::read.dbf('data/vs_2019_SPUR.dbf')

# backcalculated concentrations from OgRe
# Create name vector for OgRe files
# read in Ogre Files
files_OgRe <- c('Konz_NEU.csv', 'Konz_GEW.csv', 'Konz_ALT.csv', 'Konz_EFH.csv')
conc_OgRe <- lapply(X = file.path('data', files_OgRe),
               FUN = read.csv,
               sep = ';',
               header = TRUE)
names(conc_OgRe) <- files_OgRe

# measurements from BaSaR
# Create name vector for OgRe files
# read in Ogre Files
files_BaSaR <- c('BBRf_20200518_conc.csv', 'BBRr_20200518_conc.csv', 'BBRs_20200518_conc.csv','BBWf_20200518_conc.csv','BBWr_20200518_conc.csv', 'BBWs_20200518_conc.csv')

conc_BaSaR <- lapply(X = file.path('data_prelim_sources', files_BaSaR),
                    FUN = read.csv,
                    sep = ';',
                    header = TRUE)
names(conc_BaSaR) <- files_BaSaR


# literature data mining?


# field data OST (M. Burkhardt?)


# 1.1 define anchor for distributions
# 2. build distributions for concentrations ----------------------------------------------

# get facade diuron, backcalculated from OgRe
diuron_ALT <- conc_OgRe$Konz_ALT.csv[
  conc_OgRe$Konz_ALT.csv$Source == 'Putzfassade' , 'Konz_Diuron']
diuron_GEW <- conc_OgRe$Konz_GEW.csv[
  conc_OgRe$Konz_GEW.csv$Source == 'Putzfassade' , 'Konz_Diuron']
diuron_EFH <- conc_OgRe$Konz_EFH.csv[
  conc_OgRe$Konz_EFH.csv$Source == 'Putzfassade' , 'Konz_Diuron']
diuron_OgRe <- c(ALT=diuron_ALT, 
            GEW=diuron_GEW, 
            EFH=diuron_EFH)

#Müsste nach unseren neuen Erkenntnissen Terbutryn nicht aus Dach und Fassade stammen?
# get facade terbutryn, backcalculated from OgRe
terbutryn_GEW <- conc_OgRe$Konz_GEW.csv[
  conc_OgRe$Konz_GEW.csv$Source == 'Putzfassade' , 'Konz_Terbutryn']
terbutryn_ALT <- conc_OgRe$Konz_ALT.csv[
  conc_OgRe$Konz_ALT.csv$Source == 'Putzfassade' , 'Konz_Terbutryn']
terbutryn_NEU <- conc_OgRe$Konz_NEU.csv[
  conc_OgRe$Konz_NEU.csv$Source == 'Putzfassade' , 'Konz_Terbutryn']
terbutryn_EFH <- conc_OgRe$Konz_EFH.csv[
  conc_OgRe$Konz_EFH.csv$Source == 'Putzfassade' , 'Konz_Terbutryn']
terbutryn_OgRe <- c(ALT=terbutryn_ALT, 
               GEW=terbutryn_GEW, 
               EFH=terbutryn_EFH, 
               NEU=terbutryn_NEU)

# get roof mecoprop, backcalculated from OgRe
mecoprop_GEW <- conc_OgRe$Konz_GEW.csv[
  conc_OgRe$Konz_GEW.csv$Source == 'Bitumendach' , 'Konz_Mecoprop']
mecoprop_ALT <- conc_OgRe$Konz_ALT.csv[
  conc_OgRe$Konz_ALT.csv$Source == 'Bitumendach' , 'Konz_Mecoprop']
mecoprop_NEU <- conc_OgRe$Konz_NEU.csv[
  conc_OgRe$Konz_NEU.csv$Source == 'Bitumendach' , 'Konz_Mecoprop']
mecoprop_EFH <- conc_OgRe$Konz_EFH.csv[
  conc_OgRe$Konz_EFH.csv$Source == 'Bitumendach' , 'Konz_Mecoprop']
mecoprop_OgRe <- c(ALT=mecoprop_ALT, 
               GEW=mecoprop_GEW, 
               EFH=mecoprop_EFH, 
               NEU=mecoprop_NEU)

#benzothiazol kommt vor allem aus dem Straßenablauf wird das beachtet?
# get roof benzothiazol, backcalculated from OgRe
benzothiazol_GEW <- conc_OgRe$Konz_GEW.csv[
  conc_OgRe$Konz_GEW.csv$Source == 'Bitumendach' , 'Konz_Benzothiazol']
benzothiazol_ALT <- conc_OgRe$Konz_ALT.csv[
  conc_OgRe$Konz_ALT.csv$Source == 'Bitumendach' , 'Konz_Benzothiazol']
benzothiazol_NEU <- conc_OgRe$Konz_NEU.csv[
  conc_OgRe$Konz_NEU.csv$Source == 'Bitumendach' , 'Konz_Benzothiazol']
benzothiazol_EFH <- conc_OgRe$Konz_EFH.csv[
  conc_OgRe$Konz_EFH.csv$Source == 'Bitumendach' , 'Konz_Benzothiazol']
benzothiazol_OgRe <- c(ALT=benzothiazol_ALT, 
              GEW=benzothiazol_GEW, 
              EFH=benzothiazol_EFH, 
              NEU=terbutryn_NEU)

# get roof zinc, backcalculated from OgRe
zinc_GEW <- conc_OgRe$Konz_GEW.csv[
  conc_OgRe$Konz_GEW.csv$Source == 'Bitumendach' , 'Konz_Zn']
zinc_ALT <- conc_OgRe$Konz_ALT.csv[
  conc_OgRe$Konz_ALT.csv$Source == 'Bitumendach' , 'Konz_Zn']
zinc_NEU <- conc_OgRe$Konz_NEU.csv[
  conc_OgRe$Konz_NEU.csv$Source == 'Bitumendach' , 'Konz_Zn']
zinc_EFH <- conc_OgRe$Konz_EFH.csv[
  conc_OgRe$Konz_EFH.csv$Source == 'Bitumendach' , 'Konz_Zn']
zinc_Ogre <- c(ALT=zinc_ALT, 
               GEW=zinc_GEW, 
               EFH=zinc_EFH, 
               NEU=zinc_NEU)

#kupfer kommt vor allem aus dem Straßenablauf wird das beachtet?
# get roof copper, backcalculated from OgRe
copper_GEW <- conc_OgRe$Konz_GEW.csv[
  conc_OgRe$Konz_GEW.csv$Source == 'Bitumendach' , 'Konz_Cu']
copper_ALT <- conc_OgRe$Konz_ALT.csv[
  conc_OgRe$Konz_ALT.csv$Source == 'Bitumendach' , 'Konz_Cu']
copper_NEU <- conc_OgRe$Konz_NEU.csv[
  conc_OgRe$Konz_NEU.csv$Source == 'Bitumendach' , 'Konz_Cu']
copper_EFH <- conc_OgRe$Konz_EFH.csv[
  conc_OgRe$Konz_EFH.csv$Source == 'Bitumendach' , 'Konz_Cu']
copper_OgRe <- c(ALT=copper_ALT, 
          GEW=copper_GEW, 
          EFH=copper_EFH, 
          NEU=copper_NEU)
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

# for each substance:
for(substanc in substances)

#    for each source:
sources <- c('Bitumendach', 'Ziegeldach', 'Dach_weitere', 'Strasse', 'Hof', 'Putzfassade')  
  
for(i in BTF){
  load_roof_BTF[i] <- vol_roof_BTF[i]
  load_BTF[i] <- load_roof_BTF[i]+lood_street_BTF[i]+load_yard_BTF[i]
}
  
  
  load_Bitumendach <- vol_bitumendach*conc_anchor[substanc]

  load_Ziegeldach
  load_Dach_weitere
  load_Strasse
  load_Hof
  load_Putzfassade
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
