#install.packages("geosphere")
library(geosphere)
library(openxlsx)
# http://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/05-staedte.html?__blob=publicationFile
cities=read.xlsx("~/Downloads/05-staedte.xlsx",sheet = 2,startRow = 5)
cities$city=cities$X7
cities$plz=cities$X8
cities$population=cities$insgesamt
cities$area=cities$insgesamt/cities$je.km2
# https://public.opendatasoft.com/explore/dataset/postleitzahlen-deutschland/export/
plz=as.data.frame(data.table::fread(file = "~/Downloads/postleitzahlen-deutschland.csv",sep = ";")[,c("Geo Point","Ortsname","Postleitzahl")])
plz$geo=plz$`Geo Point`
plz[grep("Hamburg", plz$Ortsname),c("Ortsname","geo")]
plz$plz=as.character(plz$Postleitzahl)

m=merge(plz[,c("Ortsname","geo","plz")], cities[,c("city","plz","area","population")], by="plz",all.x=T)
m$lat=as.numeric(gsub("^([^,]+).*","\\1",m$geo))
m$lng=as.numeric(gsub("^[^,]+,([0-9.]+)","\\1",m$geo))

# calculate distance to 20146
target_plz="20146"
m$distance=as.numeric(distm(m[m$plz==target_plz,c("lng","lat")], m[,c("lng","lat")], fun = distHaversine))
# take cities in 50km radius, median distance for areas with multiple PLZs
a=aggregate(distance ~ city + plz + population, m[!is.na(m$population) & m$distance<50000,], median)
write.csv(x = m, file="deCities-merged.csv", row.names = F)
write.csv(x = a, file="deCities-50km.csv", row.names = F)
