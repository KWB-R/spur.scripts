
#data directory
data.dir <- "data/"

#get Panke wet weather data
x_Panke <- read.table(file = file.path(data.dir, "Panke_wet_weather.csv"), sep = ";", dec = ".", header = TRUE, as.is = TRUE)

#substances of interest
substances <- c("zinc", "copper", "benzothiazole", "mecoprop", "diuron", "terbutryn")

#reduce Panke data to substances of interest
index <- which(x_Panke$VariableName %in% substances)
x_Panke <- x_Panke[index,]

#simple boxplot
boxplot(DataValue~VariableName, data = x_Panke, log = "y")
