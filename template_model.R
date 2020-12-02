# 1. read input tables --------------------------------------------------------------------

# abimo runoff (roof, yard, street (last two include facade runoff))
vol <- foreign::read.dbf('data/vs_2019_SPUR.dbf')

# backcalculated concentrations from OgRe
files <- c('Konz_NEU.csv', 'Konz_GEW.csv', 'Konz_ALT.csv', 'Konz_EFH.csv')
conc <- lapply(X = file.path('data', files),
               FUN = read.csv,
               sep = ';',
               header = TRUE)
names(conc) <- files



# measurements from BaSaR


# literature data mining?


# field data OST (M. Burkhardt?)


# 1.1 define anchor for distributions
# 2. build distributions for concentrations ----------------------------------------------

# get facade diuron, backcalculated from OgRe
diuron_ALT <- conc$Konz_ALT.csv[
  conc$Konz_ALT.csv$Source == 'Putzfassade' , 'Konz_Diuron']
diuron_GEW <- conc$Konz_GEW.csv[
  conc$Konz_GEW.csv$Source == 'Putzfassade' , 'Konz_Diuron']
diuron_EFH <- conc$Konz_EFH.csv[
  conc$Konz_EFH.csv$Source == 'Putzfassade' , 'Konz_Diuron']
diuron <- c(ALT=diuron_ALT, 
            GEW=diuron_GEW, 
            EFH=diuron_EFH)

# get facade terbutryn, backcalculated from OgRe
terbutryn_GEW <- conc$Konz_GEW.csv[
  conc$Konz_GEW.csv$Source == 'Putzfassade' , 'Konz_Terbutryn']
terbutryn_ALT <- conc$Konz_ALT.csv[
  conc$Konz_ALT.csv$Source == 'Putzfassade' , 'Konz_Terbutryn']
terbutryn_NEU <- conc$Konz_NEU.csv[
  conc$Konz_NEU.csv$Source == 'Putzfassade' , 'Konz_Terbutryn']
terbutryn_EFH <- conc$Konz_EFH.csv[
  conc$Konz_EFH.csv$Source == 'Putzfassade' , 'Konz_Terbutryn']
terbutryn <- c(ALT=terbutryn_ALT, 
               GEW=terbutryn_GEW, 
               EFH=terbutryn_EFH, 
               NEU=terbutryn_NEU)

# BaSaR...

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

#    for each source:
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
