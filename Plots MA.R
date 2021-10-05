library(tidyverse)
library(hrbrthemes)
library(viridis)
library(data.table)
library(datasets)
library(ggplot2)

#Funktionen--------------------------------------------------------------------------------------------------------
#functions
getMonitoringTable <- function(subfolder,
                               dbName, dbTable, 
                               format, project, site, source,
                               detLimOperator, dateTimeFormat, 
                               tz){
  
  library(dplyr)
  
  if(format == 'txt'){
    
    db <- read.table(file.path(subfolder, dbName), 
                     header = TRUE, 
                     sep = ";", 
                     colClasses = 'character')
    
  } else if (format == 'xls'){
    db <- as.data.frame(readxl::read_excel(file.path(subfolder, dbName), 
                                           sheet = dbTable, 
                                           col_types = "text", 
                                           skip = 3, 
                                           na = c("na", "nb","NA")),
                        stringsAsFactors = FALSE)
  } else {
    stop('wrong file format, either "xls" or "txt"')
  }
  
  db$site <- site
  db$project <- project
  db$source <- source
  
  # grab targeted substances and accompanying event data
  index <- grepl(pattern = paste('tBegRain', 'tEndRain','Regenhöhe_mm',
                                 'Anzahl_Ereignisse',
                                 'Lufttemperatur', 'Wind',
                                 'Zn', 'Cu', 'Diuron', 'Isoproturon', 
                                 'Mecoprop', 'Terbutryn',  'Terbumeton', 
                                 'Benzothiazol', 'OIT', 'site', 'project',
                                 'source',
                                 sep = '|',
                                 collapse = ''),
                 x = colnames(db))
  db <- db[, index]
  
  # lengthen data
  db <- db %>% 
    tidyr::pivot_longer(cols = dplyr::contains(c('Zn', 'Cu', 'Diuron', 'Isoproturon', 
                                                 'Mecoprop', 'Terbutryn',  'Terbumeton', 
                                                 'Benzothiazol', 'OIT')),
                        names_to = 'substance',
                        values_to = 'concentration') %>%
    dplyr::mutate(concentration = as.character(concentration),
                  Regenhöhe_mm = as.numeric(Regenhöhe_mm))
  
  # remove '<' and apply detLimOperator
  detLimManage <- function(x, detLimOperator){
    rowsDetLim <- which(sapply(X = strsplit(x = x, 
                                            split = '<'), 
                               FUN = length) > 1)
    x <- as.numeric(gsub(pattern = '<', 
                         replacement = '', 
                         x = gsub(pattern = ',', 
                                  replacement = '.', 
                                  x = x)))
    x[rowsDetLim] <- x[rowsDetLim]*detLimOperator
    return (x)
  }
  
  db$concentration <- detLimManage(x = db$concentration, 
                                   detLimOperator = detLimOperator)
  
  # in spur data, get rid of filter outlflow concentrations, and remove '_zu' in substance names
  if(project == 'spur'){
    index <- !grepl(pattern = '_ab$',
                    x = db$substance)
    db <- db[index, ]
    
    db$substance <- gsub(pattern = '_zu$', replacement = '', x = db$substance)
  }
  
  # format dateTime columns
  tCols <- grep(pattern='tBeg|tEnd', x = colnames(db))
  for(col in tCols){
    db[[col]] <- as.POSIXct(db[[col]], format = dateTimeFormat, tz = tz)
  }
  
  return(db)
}




makeSubstanceTable <- function(dbNames, substance){
  
  output <- data.frame()
  
  colnam <- c("tBegRain", "tEndRain", "Regenhöhe_mm", "Anzahl_Ereignisse",
              "site", "project", "source", "substance", "concentration")
  
  for(dbi in dbNames){
    dat <- get(dbi)
    dat <- dat[dat$substance == substance,
               grepl(pattern = paste('tBegRain', 'tEndRain', 'Regenhöhe',
                                     'Anzahl_Ereig', 'site', 'project', 'source',
                                     'substance', 'concentration',
                                     sep = '|', collapse = ''), 
                     x = colnames(dat))]
    colnames(dat) <- colnam
    dat$stormDuration <- (as.numeric(dat$tEndRain) - as.numeric(dat$tBegRain))/
      as.numeric(dat$Anzahl_Ereignisse)/3600
    dat$rainMeanIntensity <- dat$Regenhöhe_mm/dat$stormDuration
    
    output <- rbind(output, dat)
  }
  
  return(output)
}


plotSubstance <- function(substance){
  
  library(colorspace)
  
  nsites <- length(unique(substance$site))
  name <- substitute(substance)
  
  par(mfcol=c(1, 3))
  boxplot(substance$concentration, las = 2); title(main = name)
  plot(concentration ~ Regenhöhe_mm, data = substance,
       col = rainbow_hcl(nsites),
       pch = 20, cex = 2.5, main = name)
  plot(concentration ~ rainMeanIntensity, data = substance,
       col = rainbow_hcl(nsites),
       pch = 20, cex = 2.5, main = name)
  legend('topright', legend = unique(substance$site), pch = 20, cex=1.5,
         col = rainbow_hcl(nsites))
}
#----------------------------------------------------------------------------------------------------------
#Plots

# 1. Filter Frachtreduktion

# Wirkungsgrad pro Event: Für durchschnittlichen Wirkungsgras müssen
# die einzelnen Ergebnisse gewichtet werden

# Box plot
filter_data <- read.csv('data/filter_performance.csv', header=TRUE, sep = ';')

Wirkungsgrad_Regenereignisse <- as.data.frame(cbind(filter_data$e_Zink, filter_data$e_Diuron, filter_data$e_Mecoprop))

colnames(Wirkungsgrad_Regenereignisse) <- c('ƞ Zink', 'ƞ Diuron', 'ƞ Mecoprop')
boxplot(Wirkungsgrad_Regenereignisse)

# box plot ggplot

name <- c(rep('ƞ Zink',length(filter_data$e_Zink)), rep('ƞ Diuron',length(filter_data$e_Diuron)), rep('ƞ Mecoprop',length(filter_data$e_Zink)))
value <-100*c(filter_data$e_Zink, filter_data$e_Diuron, filter_data$e_Mecoprop)
data <- data.frame(name, value) 

data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  stat_boxplot(geom = 'errorbar', width= 0.5, alpha=1)+
  geom_boxplot( fill= c("#894A94", "#5BA56D", "#F3AE0D"), alpha= 1 ) + ##6699CC #DDCC77
  coord_cartesian(ylim = c(0, 100))+
  theme_minimal() +
  ggtitle("") +
  xlab("")+
  ylab("Frachtreduktion [%]")+
  theme(legend.position="none",
        plot.title = element_text(size=14),
        axis.title.y= element_text(hjust = 0.5, size = 12), 
        axis.text = element_text(size = 14)) #funktioniert aus irgendeinem Grund nicht



#2. Ablfussanteil der verschiedenen SST im Gewässereinzugsgebiet
catchments <- c('Wuhle', 'Flughafensee')

###load data
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))

for(catchment in catchments){
  
  #Auswahl des catchments
  BTF_catchment <- subset(berlin_runoff, AGEB1 == catchment)
  # Bestimmung des gesammten runoff aus dem catchment
  catchment_runoff <- sum(BTF_catchment$runoff_total)
  #Auswahl der BTFs nach OgRe_type
  BTF_ALT <- subset(BTF_catchment, OgRe_Type=='ALT')[47:51]
  BTF_NEU <- subset(BTF_catchment, OgRe_Type=='NEU')[47:51]
  BTF_EFH <- subset(BTF_catchment, OgRe_Type=='EFH')[47:51]
  BTF_GEW <- subset(BTF_catchment, OgRe_Type=='GEW')[47:51]
  BTF_AND <- subset(BTF_catchment, OgRe_Type=='AND')[47:51]
  
  #Berechnung des runoff-Anteils der Ogre-Typen am gesamt runoff
  r_ALT <- sum(BTF_ALT$runoff_total)/catchment_runoff
  r_NEU <- sum(BTF_NEU$runoff_total)/catchment_runoff
  r_EFH <- sum(BTF_EFH$runoff_total)/catchment_runoff
  r_GEW <- sum(BTF_GEW$runoff_total)/catchment_runoff
  r_AND <- sum(BTF_AND$runoff_total)/catchment_runoff
  
  #Umrechnung in %
  Typ <- c('ALT','NEU','EFH','GEW','AND')
  Anteil <- 100* c( r_ALT, r_NEU, r_EFH, r_GEW, r_AND) 
  composition <- as.data.frame(cbind(Typ, Anteil))
  composition$Anteil = as.numeric(composition$Anteil)
  
  print(ggplot(composition, aes(x=Typ, y=Anteil))+
    geom_bar(fill= '#6c84e6', stat='identity', colour="black")+
    scale_y_continuous(limits=c(0,47))+
    labs(y= 'Anteil [%]', x= '')+
    ggtitle(paste0('Zusammensetzung ',catchment))+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))+  
    geom_text(aes(label=round(Anteil,0), vjust=-1),colour= 'black')+
    theme(axis.title.y= element_text(hjust = 0.5, size = 12), 
          axis.title.x= element_text(hjust = 0.5, size = 12),
          axis.text = element_text(size = 12)))
  
  ALT <- colSums(BTF_ALT[1:4])/catchment_runoff
  NEU <- colSums(BTF_NEU[1:4])/catchment_runoff
  EFH <- colSums(BTF_EFH[1:4])/catchment_runoff
  GEW <- colSums(BTF_GEW[1:4])/catchment_runoff
  AND <- colSums(BTF_AND[1:4])/catchment_runoff
  
  runoff_sources <- data.frame(
    values <- 100*c(ALT, NEU, EFH, GEW, AND),
    SST <- c(rep('ALT',4), rep('NEU',4), rep('EFH',4), rep('GEW',4), rep('AND',4)) ,
    sources <- rep(c('Strasse','Hof', 'Dach', 'Putzfassade'),5))

# 3. Abflüsse aufgeschlüsselt nach Quelle    
    print(ggplot(data=runoff_sources, aes(x=sources, y=values, group=SST, fill=SST))+
      geom_bar(position = 'dodge', stat = 'identity', colour="black")+
      scale_y_continuous(limits=c(0,25))+
      scale_fill_manual(values = c('#7d96ff','#5b6fc2', '#485899','#364170', '#232a45'))+
      labs(y= 'Anteil [%]', x= '')+
      ggtitle(paste0(catchment,': Abfluss nach Quelle'))+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label=round(values,0), vjust=-1))+
      theme(axis.title.y= element_text(hjust = 0.5, size = 12), 
            axis.title.x= element_text(hjust = 0.5, size = 12),
            axis.text = element_text(size = 12),
            legend.position = c(0.9,0.75),
            legend.title = element_blank()))
}


#4. Frachten aufgeschlüsselt nach Quellen Status Quo

###Daten einlesen
# abimo runoff and OgRe information (roof, yard, street (last two include facade runoff))
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))


catchments <- c('Wuhle', 'Flughafensee')
substances <- c('Diuron', 'Mecoprop', 'Zn')
OgRe_types <- c("ALT", "NEU", "EFH", "GEW", "AND")
sources <- c("Dach", "Strasse", "Hof", "Putzfassade")

load_sources<- data.frame()

for(catchment in catchments){
  
  # Auswahl des catchments
  BTFs_catchemnt <- subset(berlin_runoff, AGEB1==catchment)
  
  for(substance in substances){
    
    for(OgRe_type in OgRe_types){  
      
      # Auswahl des OgRe_type
      BTF_OgRe_catchment <-subset(BTFs_catchemnt, OgRe_Type==OgRe_type)
      # Einlesen von Konzentrationstabellen
      assign(paste0('c_',OgRe_type), ead.csv(paste0('data/Konz_',OgRe_type,'.csv'), sep = ';')) 
      
      for(my_source in sources){  
        
        # Auswahl der richtigen Konzentration
        OgRe_type_current <- eval(parse(text = paste0("c_", OgRe_type)))
        # Bestimmung von Zeile und Spalte um die richtige Konzentration abzugreifen
        col_Konz <- which(names(OgRe_type_current) == paste0("Konz_", substance))
        row_Konz <- which(OgRe_type_current$Source == my_source)
        # Auswahl der aktuellen Konzentration
        concentration <- OgRe_type_current[row_Konz, col_Konz]
        
        # Auswahl der aktuellen runoff-Spalte (Quellspezifisch)
        col_runoff <- which(names(BTF_OgRe_catchment) == paste0("runoff_", my_source))
        
        # Frachtberechnung mit dem entsprechenden runoff und der korrespondierenden Konzentration
        load_sources<- rbind(load_sources,c(sum(BTF_OgRe_catchment[col_runoff]*concentration/1000),OgRe_type,my_source)) #from g to kg
        colnames(load_sources)<- c('Fracht','SST', 'Quelle')
        load_sources$Fracht<- as.numeric(load_sources$Fracht)
        
      }  
    }
    colors_Diuron<- c('#e295f0', '#ab63b8', '#894A94', '#6e3478','#4a1c52')
    colors_Mecoprop<- c('#b1f2c1','#7dc990','#5BA56D', '#367a47', '#194d26')
    colors_Zn<-  c('#c9bea1','#ccb783','#d6b35e','#e0ac2f','#ffb300')
    
    print(load_sources)
    load_sources$Fracht<- load_sources$Fracht/sum(load_sources$Fracht)*100
    
    print(ggplot(data=load_sources, aes(x=Quelle, y=Fracht, group=SST, fill=SST))+
            geom_bar(position = 'dodge', stat = 'identity', colour="black")+
            scale_y_continuous(limits= c(0, max(load_sources$Fracht)))+
            scale_fill_manual(values = eval(parse(text =paste0('colors_',substance))))+
            labs(y= 'Anteil [%]', x= '')+
            ggtitle(substance, subtitle = paste0('Frachtanteil nach Quelle\n',catchment))+
            theme_minimal()+
            theme(plot.title = element_text(hjust = 0.5))+
            theme(axis.title.y= element_text(hjust = 0.5, size = 12), 
                  axis.title.x= element_text(hjust = 0.5, size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_blank(),
                  plot.subtitle= element_text(hjust = 0.5)))
    
    print(load_sources)
    load_sources <- data.frame()
    
  }
}



#5. Vergleich der rückgerechneten Werte (OgRe) mit gemessenen Werten aus BaSaR
# explore mean emitted concentrations from BaSaR and SpuR facades

# grab data from both projects BaSaR and SpuR

sites <- c('bbr', 'bbw')
sources<- c('facade', 'roof', 'sewer')
substances<- c('Diuron', 'Mecoprop', 'Zn')

for(place in sites){
  for (my_source in sources) {
    
    assign( paste0('basar_', place, '_', my_source),getMonitoringTable(
      subfolder = 'data_prelim_sources',
      dbName = paste0(place,'_',my_source,'_20200518_conc.txt'),
      dbTable = NA,
      format = 'txt',
      project = 'basar',
      detLimOperator = 0.5,
      site = place,
      source = my_source,
      dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
      tz = 'Etc/GMT-1'))
  }  
}

for(substance in substances){
  # make tables for target substances
  
  assign(paste0(substance, '_measurments_basar'),rbind(makeSubstanceTable(
    dbNames = c('basar_bbr_facade',
                'basar_bbw_facade', 
                'basar_bbr_roof',
                'basar_bbw_roof',
                'basar_bbr_sewer',
                'basar_bbw_sewer'),
    substance = substance)))
}

# remove BBR
Diuron_measurments_basar <- Diuron_measurments_basar[Diuron_measurments_basar$site != 'bbr', ]
# remove BBR (green roof)
Mecoprop_measurments_basar <- Mecoprop_measurments_basar[Mecoprop_measurments_basar$site != 'bbr', ]

typen <- c('ALT', 'NEU', 'GEW', 'EFH')

for(typ in typen){
  assign(paste0('c_', typ), read.table(paste0('data/Konz_',typ,'.csv'), header = TRUE, sep = ';'))
 } 


substances <- c('Diuron', 'Mecoprop', 'Zn')
color<- c("#894A94", "#5BA56D", "#F3AE0D")
scale_max<- c(1800,60,3500)
c <- c(5,6,3)
r <- c(4,1,1)

for(substance in substances){
index_substance<- which(substances== substance)

source <-eval(parse(text = paste0(substance,'_measurments_basar$source')))
value <- as.numeric(eval(parse(text = paste0(substance,'_measurments_basar$concentration'))))
data <- data.frame(source, value)


print(ggplot(data= data, aes(x= source, y= value))+
  stat_boxplot(geom = 'errorbar', width= 0.5, alpha=1)+
  geom_boxplot(outlier.shape = NA, fill= color[index_substance], alpha= 1 )+
  scale_y_continuous(limits= c(0,scale_max[index_substance]))+
  geom_hline(aes(yintercept=1000*c_ALT[r[index_substance],c[index_substance]], linetype= 'ALT'), color= '#d1a619')+
  geom_hline(aes(yintercept=1000*c_NEU[r[index_substance],c[index_substance]], linetype= 'NEU'), color= '#ab1619')+
  geom_hline(aes(yintercept=1000*c_GEW[r[index_substance],c[index_substance]], linetype= 'GEW'), color= '#299642')+
  geom_hline(aes(yintercept=1000*c_EFH[r[index_substance],c[index_substance]], linetype= 'EFH'), color= '#091094')+
  scale_linetype_manual(name = "SST", values = c(2, 2, 2, 2), guide = guide_legend(override.aes = list(color = c("#d1a619", "#ab1619",'#299642','#091094'))))+
  theme_minimal()+
  xlab("")+
  ggtitle(substance)+
  ylab("Konzentration [μg/L]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(size=14),
        axis.title.y= element_text(hjust = 0.5, size = 12), 
        axis.text = element_text(size = 12)))
} 


# 6. Frachtreduktion durch Maßnahmen 

library('reshape2')

catchments<- c('Wuhle', 'Flughafensee')
Szenarien<- c('Szenario_1','Szenario_2', 'Szenario_3', 'Szenario_4')
extends <- c(0, 10, 20, 50)

#Einlesen aller modellierten Frachten
for (Szenario in Szenarien) {
  for (catchment in catchments) {
    for (extend in extends) {
    assign(paste0(catchment,'_' , Szenario,'_', extend), read.csv(paste0('data_output/calculation_load_reduction/', Szenario,'/' ,catchment,'/', Szenario,'_', extend,'%.csv')))
    }
  }
}

#Berechnung des Reduktionspotenzials
#plotten der Potenzials
potencials<- c(10,20,50)

for (catchment in catchments) {
  for (Szenario in Szenarien) {
    for (potencial in potencials) {

      current_Potenzial<- eval(parse(text = paste0(catchment,'_', Szenario, '_', potencial)))
      current_Ausgangslage <- eval(parse(text = paste0(catchment,'_', Szenario, '_0')))
      current_Frachtreduktion<- 1-current_Potenzial/current_Ausgangslage
    
      assign(paste0('Frachtreduktion_', catchment,'_', Szenario), current_Frachtreduktion*100)
      assign(paste0('Frachtreduktion_', catchment,'_', Szenario), melt(eval(parse(text = paste0('Frachtreduktion_', catchment,'_', Szenario)))))
    
      #veraltet
      #assign(paste0('rp',potencial,'_', catchment), (1-eval(parse(text = paste0(catchment,'_', Szenario, '_', potencial)))/eval(parse(text = paste0(catchment,'_', Szenario,'_0'))))*100)
      #assign(paste0('rp',extend,'_', catchment), melt(eval(parse(text=paste0('rp',extend,'_', catchment)))))
    
      print(ggplot(data=eval(parse(text=paste0('Frachtreduktion_', catchment,'_', Szenario))), aes(x= variable, y=value))+
              stat_boxplot(geom = 'errorbar', width= 0.5, alpha=1)+
              geom_boxplot( fill= c("#894A94", "#5BA56D", "#F3AE0D"), alpha= 1)+
              theme_minimal()+
              xlab("")+
              ylab("Frachtreduktion [%]")+
              ggtitle(paste0(Szenario), subtitle = paste0(catchment,' ',potencial,'%'))+
              theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5))+
              theme(plot.title = element_text(size=14),
                    axis.title.y= element_text(hjust = 0.5, size = 12), 
                    axis.text = element_text(size = 12)))
    }
  }
}


# Vergleich der Konzentrationen in Wuhle und Panke
#### Wuhle
simulated_loads<-read.csv('data_output/calculation_status_quo/Wuhle_simulated_loads.csv')
simulated_loads<- simulated_loads[,c('Diuron','Mecoprop','Zn')]

library(data.table)
berlin_runoff <- foreign::read.dbf('data/berlin_runoff.dbf')
berlin_runoff <- setnames(berlin_runoff, old=c('runoff_str', 'runoff_yar', 'runoff_roo', 'runoff_put','runoff_tot'), new= c('runoff_Strasse','runoff_Hof','runoff_Dach','runoff_Putzfassade','runoff_total'))

BTF_catchment <- subset(berlin_runoff, AGEB1=='Wuhle')
total_runoff <- colSums(BTF_catchment['runoff_total'])

concentrations <- simulated_loads*1000000/total_runoff

boxplot(concentrations, ylim= c(min(concentrations), max(concentrations)) , yaxp=c(1e-2, 1e+3, 1) , log = 'y',  ylab= '[μg/L]', outline=FALSE, las=0, cex.axis= 1.2, cex.lab= 1.2, col = '#485899', main= 'Wuhle', names= c('Diuron', 'Mecoprop', 'Zink'), yaxt="n")
at.y <- outer(1:9, 10^(-2:3))
lab.y <- ifelse(log10(at.y) %% 1 == 0, at.y, NA)
axis(2, at=at.y, labels=lab.y, las=1)


#Panke
#data directory
data.dir <- "data/"

#get Panke wet weather data
x_Panke <- read.table(file = file.path(data.dir, "Panke_wet_weather.csv"), sep = ";", dec = ".", header = TRUE, as.is = TRUE)

#substances of interest
substances <- c('diuron', 'mecoprop', 'zinc') 

#reduce Panke data to substances of interest
index <- which(x_Panke$VariableName %in% substances)
x_Panke <- x_Panke[index,]

#simple boxplot
boxplot(DataValue~VariableName, data = x_Panke, log = "y", las=0, col = '#485899', xlab='', ylab = '[µg/L]', cex.lab=1.2, cex.axis=1.2, names= c('Diuron', 'Mecoprop', 'Zink'), ylim= c(min(concentrations), max(concentrations)), main='Panke', yaxt="n" )
at.y <- outer(1:9, 10^(-2:3))
lab.y <- ifelse(log10(at.y) %% 1 == 0, at.y, NA)
axis(2, at=at.y, labels=lab.y, las=1)

