library(rgdal)
library(leaflet)
library(ggplot2)
library(dplyr)
#library(shiny)

champaign <- read.csv('data/all_cityworks_records.csv', stringsAsFactors = F)
#shapefile <- readOGR('shapefiles/', 'Address_Points')

## data preprocessing

champaign$DESCRIPTION <- as.factor(champaign$DESCRIPTION)
champaign$DATETIMECLOSED <- as.Date(champaign$DATETIMECLOSED, "%m/%d/%Y")
champaign$DATETIMEINIT <- as.Date(champaign$DATETIMEINIT, "%m/%d/%Y")
champaign$STATUS <- as.factor(champaign$STATUS)
champaign$PROBADDRESS <- as.factor(toupper(champaign$PROBADDRESS))


## data cleansing

# removing rows with empty values
champaign <- champaign[!(champaign$SRX=="" | champaign$SRY==""), ]

# removing commas from lat/long
champaign$SRX <- gsub(',',"",champaign$SRX)
champaign$SRY <- gsub(',',"",champaign$SRY)

# subsetting data into OPEN & CLOSED STATUSES
champaign_open = subset(champaign, STATUS=='OPEN' | STATUS=='Open')
champaign_closed = subset(champaign, STATUS=='CLOSED')

champaign_closed$TIMESPAN <- as.numeric(champaign_closed$DATETIMECLOSED - 
  champaign_closed$DATETIMEINIT)

# removing rows where DATETIMECLOSED < DATETIMEINIT
champaign_closed <- champaign_closed[!((champaign_closed$TIMESPAN)<0 | 
                                         is.na(champaign_closed$DATETIMECLOSED)),]

champaign_closed_sorted <- champaign_closed[order(-champaign_closed$TIMESPAN),]


champaign_closed_nad83 <- data.frame(long=as.double(champaign_closed$SRX), 
                                     lat=as.double(champaign_closed$SRY))
champaign_open_nad83 <- data.frame(long=as.double(champaign_open$SRX), 
                                     lat=as.double(champaign_open$SRY))
champaign_closed_sorted_nad83 <- data.frame(long=as.double(champaign_closed_sorted$SRX), 
                                     lat=as.double(champaign_closed_sorted$SRY))

# setting properties of the Spatial df
# EPSG:3435 for given .prj file

coordinates(champaign_closed_nad83) <- c('long','lat')
proj4string(champaign_closed_nad83)=CRS("+init=EPSG:3435")

coordinates(champaign_open_nad83) <- c('long','lat')
proj4string(champaign_open_nad83)=CRS("+init=EPSG:3435")

coordinates(champaign_closed_sorted_nad83) <- c('long','lat')
proj4string(champaign_closed_sorted_nad83)=CRS("+init=EPSG:3435")


#transforming from NAD83 Coordinate Ref. System to WGS84 (standard)
champaign_closed_wgs84 <- spTransform(champaign_closed_nad83, CRS("+init=epsg:4326"))
champaign_open_wgs84 <- spTransform(champaign_open_nad83, CRS("+init=epsg:4326"))
champaign_closed_sorted_wgs84 <- spTransform(champaign_closed_sorted_nad83, 
                                             CRS("+init=epsg:4326"))

icons_closed <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "green"
)

icons_open <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "red"
)

m_closed <- leaflet(champaign_closed_wgs84) %>% addTiles() %>% 
  addAwesomeMarkers(lng=champaign_closed_wgs84$long, lat=champaign_closed_wgs84$lat, 
                    icon=icons_closed, 
                    label = champaign_closed$DESCRIPTION, 
                    popup = paste("Address:", champaign_closed$PROBADDRESS,
                                  "<br><br>", 
                                  "Problem: ", champaign_closed$DESCRIPTION, 
                                  "<br><br>", 
                                  "Init date: ", champaign_closed$DATETIMEINIT, 
                                  "<br><br>", 
                                  "Days taken to resolve: ", 
                                  champaign_closed$TIMESPAN), 
                    clusterOptions = markerClusterOptions()) 
  

m_open <- leaflet(champaign_open_wgs84) %>% addTiles() %>%
  addAwesomeMarkers(lng=champaign_open_wgs84$long, lat=champaign_open_wgs84$lat, 
                    icon=icons_open, label = champaign_open$DESCRIPTION, 
                    popup = paste("Address:", champaign_open$PROBADDRESS,
                                  "<br><br>", 
                                  "Problem: ", champaign_open$DESCRIPTION, 
                                  "<br><br>", 
                                  "Init date: ", champaign_open$DATETIMEINIT), 
                    clusterOptions = markerClusterOptions())


# 20 requsts that took the most time to solve  
m_most_time <- leaflet(champaign_closed_sorted_wgs84[1:20,]) %>% addTiles() %>% 
  addAwesomeMarkers(lng=champaign_closed_sorted_wgs84$long[1:20], 
                    lat=champaign_closed_sorted_wgs84$lat[1:20], 
                    icon=icons_closed, 
                    label = champaign_closed_sorted$DESCRIPTION[1:20], 
                    popup = paste("Address: ", champaign_closed_sorted$PROBADDRESS[1:20], 
                                  "<br><br>",
                                  "Problem: ", champaign_closed_sorted$DESCRIPTION[1:20],
                                  "<br><br>",
                                  "Init date: ", champaign_closed_sorted$DATETIMEINIT[1:20], 
                                  "<br><br>", 
                                  "Days taken: ", 
                                  champaign_closed_sorted$TIMESPAN[1:20]))


# plot for Top 10 service requests 
avg_days_per_issue <- setNames(aggregate(champaign_closed$TIMESPAN, 
                                list(champaign_closed$DESCRIPTION), mean), 
                               c('DESCRIPTION', 'TIMESPAN'))
avg_days_per_issue <- avg_days_per_issue[order(-avg_days_per_issue$TIMESPAN),]

top_service_requests <- ggplot(avg_days_per_issue[1:10,]) + 
  aes(DESCRIPTION, TIMESPAN) + 
  geom_col(fill='#FF6666') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab(c('Service request')) + ylab(c('Avg days to resolve')) +
  ggtitle('Top 10 requests that took the most time to solve')


# plot for Top 10 service addresses 
avg_days_per_place <- setNames(aggregate(champaign_closed$TIMESPAN, 
                                         list(champaign_closed$PROBADDRESS), mean), 
                               c('PROBADDRESS', 'TIMESPAN'))
avg_days_per_place <- avg_days_per_place[order(-avg_days_per_place$TIMESPAN),]

top_service_places <- ggplot(avg_days_per_place[1:10,]) + 
  aes(PROBADDRESS, TIMESPAN) + 
  geom_col(fill='#FF6666') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab(c('Address')) + ylab(c('Avg days to resolve'))


binpal <- colorBin("Reds", champaign_closed_sorted$TIMESPAN, 4, pretty = FALSE, 
                   reverse = TRUE)
w <- leaflet(champaign_closed_sorted_wgs84[1:20,]) %>% addTiles() %>%
  addCircles(lng = champaign_closed_sorted_wgs84$long[1:20], 
             lat = champaign_closed_sorted_wgs84$lat[1:20], weight = 0.5,
             radius = sqrt(champaign_closed_sorted$TIMESPAN)*20 , 
             color = binpal(champaign_closed_sorted$TIMESPAN),
             popup = paste("Days to solve problem:", 
                           champaign_closed_sorted$TIMESPAN[1:20], sep = ' '))




#address_count <- count(champaign_closed, "PROBADDRESS")
#address_count_join <- unique(merge(address_count, champaign_closed, by="PROBADDRESS", 
#                                   all.x=T))
#address_count_sorted <- address_count_join[order(-address_count_join$freq),]
  


  
