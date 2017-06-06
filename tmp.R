
# set wd, load libs, read initial csv into dataframe
setwd("/home/bigdata09/projs/mob/")
#install.packages(“ggplot2″)
#install.packages(“jsonlite”)
#install.packages(“plyr”)
library(RJSONIO)
library(ggmap)
library(geosphere)
library(data.table)
library(gdata)
library(varhandle)

df00 <- read.csv("Bevolkingsontwikkeli_040617192441.csv",header = TRUE)
#dim(df00)
# [1] 388  17

# get only the location-name and population variables
df01 <- df00[1:388,c(1,17)]

# get lon & lat values from google maps api from each municipality based on location names
#geocodes <- geocode(as.character(df01$Regio.s))
#write.csv(geocodes,"geocodes.csv")
geo <- read.csv("geocodes.csv",header=TRUE)
df01 <- data.frame(df01[,1:2],geo)
df01 <- df01[,-c(3)]

# rename variable-names for clarity
colnames(df01)[1] <- "municipality"
colnames(df01)[2] <- "population_01042017"
colnames(df01)[3] <- "longitude"
colnames(df01)[4] <- "latitude"

#head(df01)
#     municipality population_01042017 longitude latitude
# 1   Aa en Hunze               25294  6.749528 53.01048
# 2       Aalburg               13061  5.057085 51.75129
# 3      Aalsmeer               31393  4.750244 52.26064
# 4        Aalten               27134  6.580678 51.92667

#dim(df01)
# [1] 388   4

# create a distance matrix from two lists (-1 on both first and last city-row)
l1 <- data.frame(longitude = df01[1:387,3],
                 latitude = df01[1:387,4])
l2 <- data.frame(longitude = df01[2:388,3],
                 latitude = df01[2:388,4])
mtx <- distm(l1[,c('longitude','latitude')], l2[,c('longitude','latitude')], fun=distVincentyEllipsoid)

#head(mtx)
#           [,1]      [,2]     [,3]      [,4]      [,5]      [,6]
# [1,] 181410.97 158998.75 121147.8  45689.08 189958.68 204965.68
# [2,]      0.00  60463.11 106807.8 178628.27  29788.73  46693.60
# [3,]  60463.11      0.00 130832.1 141961.74  43773.50  51019.39
# [4,] 106807.76 130832.08      0.0 145751.95 131649.72 150232.45

#write.csv(mtx,"matrix_0.csv")

#dim(mtx)
# [1] 387 387
# elements in mtx = 149769
# checked: 387*387 = 149769

# remove duplicates and 'eigen'-distances
mtx[lower.tri(mtx)] <- NA
#head(mtx)
#         [,1]      [,2]     [,3]      [,4]      [,5]      [,6]
# [1,] 181411 158998.75 121147.8  45689.08 189958.68 204965.68
# [2,]     NA  60463.11 106807.8 178628.27  29788.73  46693.60
# [3,]     NA        NA 130832.1 141961.74  43773.50  51019.39
# [4,]     NA        NA       NA 145751.95 131649.72 150232.45

#dim(mtx)
# [1] 387 387

# transpose as quick-fix for correct iteration direction in as.list
mx <- t(mtx)
dists <- as.list(mx)
dists2 <- dists[!is.na(dists)]
dists3 <- data.frame(dists2)
dists4 <- t(dists3)
distances <- data.frame(dists4)

#length(distances)
# [1] 75078
# checked: 149769 + 387 / 2 = 75078

# to get all 'pairwise' combinations first combine all variables, then hen split for further comparison
# dataframe should (row-wise) increase from 388 to 75078 (see above)
# get X-combinations per Y w/ 'combn' 
# split into seperate variables

# municipalities
compared_municipalities <- data.frame(combn(as.character(df01$municipality), 2, FUN = paste, collapse="_"))

# populations
compared_populations <- data.frame(combn(as.character(df01$population_01042017), 2, FUN = paste, collapse="_"))

# locations 
combined_lonlats <- paste(df01$longitude, df01$latitude, sep = ",")
compared_locations <- data.frame(combn(as.character(combined_lonlats), 2, FUN = paste, collapse="_"))

# ------------ new dataframe ------------

# into a new dataframe, should be 75078x4
df011 <- data.frame(compared_municipalities, distances)
df012 <- data.frame(df011, compared_populations)
df02 <- data.frame(df012, compared_locations)

# rename column names
colnames(df02)[1] <- "compared_municipalities"
colnames(df02)[2] <- "distances"
colnames(df02)[3] <- "compared_populations"
colnames(df02)[4] <- "compared_locations"

# make seperate lists with split-up's of compared variables
 
lcm <- data.frame(strsplit(as.character(df02$compared_municipalities), '_'))
lcm2 <- t(lcm)
lcm3<- data.frame(lcm2)

lcp <- data.frame(strsplit(as.character(df02$compared_populations), '_'))
lcp2 <- t(lcp)
lcp3<- data.frame(lcp2)

lcl <- data.frame(strsplit(as.character(df02$compared_locations), '_'))
lcl2 <- t(lcp)
lcl3<- data.frame(lcl2)

df021 <- data.frame(lcm3, lcp3)
df022 <- data.frame(df021, lcl3)
colnames(df022)[1] <- "lcm"
colnames(df022)[2] <- "lcp"
colnames(df022)[3] <- "lcl"


lcp <- data.frame(strsplit(as.character(df02$compared_populations), '_'))
lcl1 <- data.frame(strsplit(as.character(df02$compared_locations), '_'))


df02$combined_locations <- paste(df02$longitude, df01$latitude, sep = ",")
compared_locations <- combn(as.character(df02$combined_locations), 2, FUN = paste, collapse="_")
df03 <- cbind.data.frame(df03,compared_locations)
list_compared_locations <- strsplit(as.charactcompared_populationser(compared_populations), '_')


lcp <- strsplit(as.character(compared_populations), '_')
lcl1 <- strsplit(as.character(compared_locations), '_')


  
lcl2 <- strsplit(as.character(df022$), '_')

h

df04 <- data.frame(df03, do.call((rbind, listcomparedpopulations), stringsAsFactors = FALSE))

  
locs <- do.call(rbind, lapply(strsplit(df02$compared_locations,",",T)))

l_cl <- do.call(rbind, lapply)
                
lcl2 <- (strsplit(as.character(compared_municipalities), "_"))
lcl3 <- t(lcl2)
lcl4 <- data.frame(lcl3)

strsplit(as.character(length), ",") 

#write.csv(geocodes,"geocodes.csv")

x1 <- rbind(list_compared_municipalities)
x3 <- data.frame(do.call(cbind, list_compared_municipalities))


df03 <- data.frame(df02, do.call(rbind, list_compared_municipalities))
df04 <- data.frame(df03, do.call(rbind, list_compared_populations))
df05 <- data.frame(df04, do.call(rbind, list_compared_locations))
                   

colnames(df02)[5] <- "list_compared_municipalities"
colnames(df02)[6] <- "list_compared_populations"
colnames(df02)[7] <- "list_compared_locations"
  
# ? df04 <- data.frame(df03, do.call(rbind, listcomparedpopulations), stringsAsFactors = FALSE)

length(compared_municipalities)
# [1] 75078

colnames(df05)[x] <- "muni_i"
colnames(df05)[x] <- "muni_j"
colnames(df05)[x] <- "pop_i"
colnames(df05)[x] <- "pop_j"
colnames(df05)[x] <- "lonlat_i"
colnames(df05)[x] <- "lonlat_j"
 
# this works. something about the r inferno.
df05$muni_i <- as.character(df05$muni_i)
df05$muni_j <- as.character(df05$muni_j)
df05$muni_i <- as.numeric(df05$muni_i)
df05$muni_j <- as.numeric(df05$muni_j)
df05$muni_i <- as.integer(df05$muni_i)
df05$muni_j <- as.integer(df05$muni_j)

df05$pop_i <- as.character(df05$pop_i)
df05$pop_j <- as.character(df05$pop_j)
df05$pop_i <- as.numeric(df05$pop_i)
df05$pop_j <- as.numeric(df05$pop_j)
df05$pop_i <- as.integer(df05$pop_i)
df05$pop_j <- as.integer(df05$pop_j)

df05$lonlat_i <- as.character(df05$lonlat_i)
df05$lonlat_j <- as.character(df05$lonlat_j)
df05$lonlat_i <- as.numeric(df05$lonlat_i)
df05$lonlat_j <- as.numeric(df05$lonlat_j)
df05$lonlat_i <- as.integer(df05$lonlat_i)
df05$lonlat_j <- as.integer(df05$lonlat_j)


# when drawing edges later on
# decide direction based on larger population out of any 2-combination
df05$direction <- 0

ifelse(
  (df05$pop_i[i] > df05$pop_j[i])
  , df05$direction[i] <- df05$lonlat_i[i]
  , ifelse(df05$pop_i[i] < df05$pop_j[i]
           , df05$direction[i] <- df05$lonlat_j[i])
  , df05$direction[i] <- 0)

# the gravity is based on ((pop_i * pop_j) / distances)
df05$gravity <- 0
for (i in 1:nrowdf05){
    df05$gravity[i] <- ((pop_i[i] * pop_j[i]) / distances[i])
}



ifelse(
  (df05$pop_i[i] > df05$pop_j[i])
  , df05$gravity[i] <- df05$lonlat_i[i]
  , ifelse(df05$pop_i[i] < df05$pop_j[i]
           , df05$gravity[i] <- df05$lonlat_j[i])
  , df05$gravity[i] <- 0)


for (i in 1:nrow(df05)){
  if(df05$pop_i[i] > df05$pop_j[i]){
    df05$direction[i] <- df05$lonlat_i[i]
  }else if (df05$pop_i[i] < df05$pop_j[i]) {
    df05$direction[i] <- df05$lonlat_j[i]
  }else if (df05$pop_i[i] == df05$pop_j[i]) {
    df05$direction[i] <- 0
  }else {
    stop()    
  }
}




# script adapted from https://github.com/asheshwor/mapping-location-history/blob/master/mapping_google_history.r

#Load packages
library(maps)
library(ggmap)
library(mapdata)
library(mapproj)
library(maptools)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(scales)
#Setting up directory and file list
dirName <- "//ASH-desktop/Users/asheshwor/Documents/R_git/history_KML/"
fileList <- c(dir(dirName))
numFiles <- length(fileList)
#Reading coordinates from KML file downloaded
#  from google location history
lat <- numeric(0)
lon <- numeric(0)
tStart <- Sys.time() #track time
# WARNING!! takes a while to loop through
#took ~10 min for 191,355 sets in 12 months of history (4 gb ram win 7 64 bit)
#took 1.4 hrs for the same on my old toshiba (3 gb ram win 7 64 bit)
for (i in 1:numFiles) {
  dirTemp <- paste(dirName, fileList[i], sep="")
  hist <- getKMLcoordinates(dirTemp) #read KML file
  maxl <- length(hist)
  for (j in 1:maxl) {
    hist.0 <- hist[[j]]
    lat <- c(lat, hist.0[2])
    lon <- c(lon, hist.0[1])
  }
}
tFinish <- Sys.time()
difftime(tFinish, tStart) #calculating time it took to read
#convert to a dataframe
hist.df <- data.frame(lon, lat)
#Write extracted coordinates to a csv file
write.csv(hist.df, file="//ASH-desktop/Users/asheshwor/Documents/R_git/LocationHistory.csv")
#Reading coordinates
hist.df2 <- read.csv(file="//ASH-desktop/Users/asheshwor/Documents/R_git/LocationHistory.csv", header=TRUE)
hist.df2 <- hist.df2[,-1] #dropping first column

#rounding off data for density plots
hist.df3 <- hist.df2
hist.df3$lon <- round(hist.df3$lon, 4)
hist.df3$lat <- round(hist.df3$lat, 4)

#plotting the points
nl01 <- "nl_01" #create empty map
nl01_map <- qmap(nl01, zoom=14, maptype="roadmap", legend="topleft") #google roadmap
png("nl01_map.png",720,720)
nl01_map + geom_point(aes(x = lon, y = lat), 
                      data = hist.df2, 
                      color = couleur[8], alpha = 0.5)
dev.off()
 




 
# > head(df03)
#                   dist_names distances compared_populations pop_i pop_j
# 1       Aa en Hunze_Aalburg    181411          25294,13061 25294 13061
# 2      Aa en Hunze_Aalsmeer  158998.7          25294,31393 25294 31393
# 3        Aa en Hunze_Aalten  121147.8          25294,27134 25294 27134

x <- diff(df03$pop_i, df03$pop_j)
df04 <- data.frame(df03,data.frame)
df03 <- data.frame(pop1=unlist(s), AB=rep(v$AB, sapply(s, FUN=length)))
df02 <- cbind(dist_names,distances,compared_populations)
compared_populations <- combn(as.character(df01$population_01042017), 2, FUN = paste, collapse=",")
max_pop <- as.numeric(max(df01$population_01042017))
df02 <- cbind(dist_names,distances,compared_populations)



# na_municipalities <- df01[is.na(df01$longitude),]
#           municipality population_01042017 longitude latitude
# 113 Goeree-Overflakkee               48788        NA       NA
# 156     Hof van Twente               34972        NA       NA
# 199              Lopik               14290        NA       NA
# 369          De Wolden               23789        NA       NA

# dfx <- df01[is.na(df01$longitude),]



