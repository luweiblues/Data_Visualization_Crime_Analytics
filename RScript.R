
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
install.packages(x) # warning: this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages
 

# Set your own working directory
#setwd("Coursera/Data_Visualization")

seattle.map = readOGR(dsn = './Neighborhoods/WGS84', layer = 'Neighborhoods')

head(seattle.map@data)
plot(seattle.map, col = 'turquoise')
plot(seattle.map)

### Read in crime incidents data  #####
Seattle_crime = read.csv('./data/seattle_incidents_summer_2014.csv', header = T, na.strings=c("", "NA"))

df_coords = Seattle_crime[Seattle_crime$Longitude!=0 & Seattle_crime$Latitude!=0, ]
df_coords_thefts = Seattle_crime[Seattle_crime$Longitude!=0 & Seattle_crime$Latitude!=0 & (Seattle_crime$Summarized.Offense.Description %in% c('BIKE THEFT', 'MAIL THEFT', 'VEHICLE THEFT', 'THEFT OF SERVICES')), ]
mat = as.matrix(df_coords[, 15:16])
mat_thefts = as.matrix(df_coords_thefts[, 15:16])
sp1 = SpatialPoints(coords = mat)
sp1_theft = SpatialPoints(coords = mat_thefts)

sodo = seattle.map[!is.na(seattle.map$L_HOOD), ]
sodo_s = seattle.map[!is.na(seattle.map$S_HOOD) & seattle.map$S_HOOD != '000', ]
sodo_thefts = seattle.map[!is.na(seattle.map$S_HOOD), ]


proj4string(sp1) = proj4string(sodo) 
proj4string(sp1) = proj4string(sodo_s) 
proj4string(sp1_theft) = proj4string(sodo_thefts) 

overlap = over(sp1, sodo)
overlap_s = over(sp1, sodo_s)
overlap_theft = over(sp1_theft, sodo_thefts)

# Add a column for crime count
overlap$CrimeCount = 1
overlap_s$CrimeCount = 1
overlap_theft$CrimeCount = 1

# Aggregate
crime_ag = aggregate(CrimeCount ~ L_HOOD, FUN = sum, data = overlap)
crime_ag_s = aggregate(CrimeCount ~ S_HOOD, FUN = sum, data = overlap_s)
crime_ag_thefts = aggregate(CrimeCount ~ S_HOOD, FUN = sum, data = overlap_theft)


sodo@data = left_join(sodo@data, crime_ag)
sodo_s@data = left_join(sodo_s@data, crime_ag_s)
sodo_thefts@data = left_join(sodo_thefts@data, crime_ag_thefts)

area_crime = sodo_s@data[!is.na(sodo_s@data$L_HOOD), c("S_HOOD", "L_HOOD", "CrimeCount")]
area_crime = area_crime[order(area_crime$CrimeCount, decreasing = 'T'), ]
area_crime[1:10, ]

area_thefts = sodo_thefts@data[!is.na(sodo_thefts@data$L_HOOD), c("S_HOOD", "L_HOOD", "CrimeCount")]
area_thefts = area_thefts[order(area_thefts$CrimeCount, decreasing = 'T'), ]
area_thefts[1:10, ]

# 1. How do incidents vary by neighborhood? 
#    Which incidents are most common in the city center? 
#    In what areas or neighborhoods are robberies or thefts most common?

# 1. Thermal map for crime incidents in Seattle Neighborhoods
qtm(sodo, fill = 'CrimeCount', fill.title = 'Crime Count', style = 'gray', legend.position = c("left", "bottom"))
qtm(sodo_s, fill = 'CrimeCount', fill.title = 'Crime Count', style = 'gray', legend.position = c("left", "bottom"))
qtm(sodo_thefts, fill = 'CrimeCount', fill.title = 'Theft Count', style = 'gray', legend.position = c("left", "bottom"))


# 2. How do incidents vary by time of day? 
#    Which incidents are most common in the evening? 
#    During what periods of the day are robberies most common?

time_vec = strptime(Seattle_crime$Occurred.Date.or.Date.Range.Start, format = '%m/%d/%Y %I:%M:%S %p')

Seattle_crime$time = strftime(time_vec, '%H')

#ggplot(data = Seattle_crime, aes(x = Occurred.Date.or.Date.Range.Start)) + geom_point(aes(color = Offense.Type))

m = matrix(0, nrow = 24, ncol = 2)

newdf = Seattle_crime[, c(7, ncol(Seattle_crime))]

for (i in 1:nrow(newdf)) {
  if (newdf[i, 1] == 'ROBBERY') {
    row = as.integer(newdf[i, 2]) + 1
    m[row, 2] = m[row, 2] + 1
  }
}

Seattle_crime$Hour = as.integer(Seattle_crime$time)

# Aggregate by hour
Seattle_crime$CrimeCount = 1
crime_ag_hour = aggregate(CrimeCount ~ Hour, FUN = sum, data = Seattle_crime)
ggplot(crime_ag_hour, aes(Hour, CrimeCount, group = 1)) + geom_line() + xlab("Hour") + ylab("Hourly Incidents") + ggtitle("Hourly Crime Incidents in Seattle 2014 Summer")

m_df = data.frame(m)
colnames(m_df) = c('Hour', 'Incidents')
m_df$Hour = sort(unique(newdf$time))

# 2. Hourly Robbery Incidents in Seattle 2014
ggplot(m_df, aes(Hour, Incidents, group = 1)) + geom_line() + xlab("Hour") + ylab("Hourly Robbery Incidents") + ggtitle("Hourly Robbery Incidents in Seattle 2014 Summer") + scale_y_continuous(breaks = seq(0, 65, by = 10))

# 3. How do incidents vary month to month in the Summer 2014 dataset?
crime_df = df_coords
crime_df$CrimeCount = 1

# Aggregate by month
crime_ag_month = aggregate(CrimeCount ~ Month, FUN = sum, data = crime_df)

# 3. Month to month crime incidents
ggplot(crime_ag_month, aes(Month, CrimeCount, group = 1)) + geom_bar(stat = 'identity') + xlab("Month") + ylab("Monthly Incidents") + ggtitle("2014 Summer Monthly Crime Incidents in Seattle") + scale_y_continuous(breaks = seq(0, 12000, by = 2000))
ggplot(crime_ag_month, aes(x = Month) + geom_bar(stat = "bin"))
ggplot(crime_ag_month, aes(Month, CrimeCount, group = 1)) + geom_line() + xlab("Month") + ylab("Monthly Incidents") + ggtitle("2014 Summer Monthly Crime Incidents in Seattle")