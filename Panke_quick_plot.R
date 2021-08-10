
#data directory
data.dir <- "data/"

#get Panke wet weather data
x_Panke <- read.table(file = file.path(data.dir, "Panke_wet_weather.csv"), sep = ";", dec = ".", header = TRUE, as.is = TRUE)

#substances of interest
substances <- c('diuron', 'mecoprop', 'zinc') #c("diuron", "mecoprop", "terbutryn", "benzothiazole", "copper", "zinc" )

#reduce Panke data to substances of interest
index <- which(x_Panke$VariableName %in% substances)
x_Panke <- x_Panke[index,]

#simple boxplot
#par(mgp=c(2.5, 1, 0), fin=c(5,4))
boxplot(DataValue~VariableName, data = x_Panke, log = "y", las=1, col = 'lightblue', xlab='', ylab = 'µg/L', cex.lab=1.2, cex.axis=1.2 )
