## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message=FALSE, warning=FALSE
)

## ----datadir-------------------------------------------------------------
datadir <- "/home/lino/Téléchargements"

# One must install the dev version of maptools not the common one
#devtools::install_github("environmentalinformatics-marburg/mapview", ref = "develop")

library(leaflet)
library(dplyr)
library(rgdal)
library(ggplot2)
library(mapview)

library(UrbanEco)

## ------------------------------------------------------------------------
# Download, unzip and load Vélib station shapefile
shpbike <- UrbanEco::download.unzip(url = "https://github.com/linogaliana/UrbanEco/blob/master/inst/stations-velib-disponibilites-en-temps-reel.zip?raw=true")

## ----plot shapefile, fig.width=12, fig.height=8--------------------------
leaflet(shpbike) %>% addTiles() %>% addCircles() 

## ----load data-----------------------------------------------------------
out <- UrbanEco::download.unzip(url = "http://vlsstats.ifsttar.fr/rawdata/RawData/RawData_OLD/data_all_Paris.jjson_2017-06-01-1496291195.gz", extension = '.json')

df <- do.call(rbind,out)

# Keep only data we are intersted in
df <- dplyr::tbl_df(df) %>% filter(status == "OPEN") %>%
  dplyr::mutate(timestamp = lubridate::as_datetime(download_date)) %>%
  select(bike_stands,number,available_bike_stands,available_bikes,timestamp)


## ----show data-----------------------------------------------------------
knitr::kable(head(df,10), caption = "Velib Data")

## ----merge data----------------------------------------------------------
# Merge data: tibble dataframe
dfbike.xy <- tbl_df(merge(df,shpbike))

# Transform back in spatial object
sp::coordinates(dfbike.xy) <- ~coords.x1+coords.x2
proj4string(dfbike.xy) <- proj4string(shpbike)

## ----bike station change-------------------------------------------------
# Keep only morning and after working hours periods
dfbike <- df %>% filter(between(lubridate::hour(timestamp),7,10) | between(lubridate::hour(timestamp),17,20))

## ------------------------------------------------------------------------
# Create a period identifier
dfbike <- dfbike %>% dplyr::mutate(period1 = between(lubridate::hour(timestamp),7,10)) %>%
  dplyr::mutate(period2 = between(lubridate::hour(timestamp),17,20)) %>%
  dplyr::mutate(period = period1 + 2*period2) %>%
  select(-period1,-period2)

# Create vacancy rate variable by period
dfbike <- dfbike %>% dplyr::mutate(vacancy_rate = available_bike_stands/bike_stands) %>%
  group_by(number,period) %>%
  summarise(vacancy_rate = mean(vacancy_rate,na.rm=T))

## ------------------------------------------------------------------------
# Create a stands use variable change from morning to afternoon
dfbike <- dfbike %>% group_by(number) %>% arrange(period) %>%
  dplyr::mutate(stands_change = vacancy_rate-lag(vacancy_rate)) %>%
  dplyr::mutate(station.group = stands_change>0) %>%
  filter(period == 2) %>% ungroup()

## ------------------------------------------------------------------------
# Merge data: tibble dataframe
dfbike2.xy <- tbl_df(merge(dfbike,shpbike))

# Transform back in spatial object
sp::coordinates(dfbike2.xy) <- ~coords.x1+coords.x2
proj4string(dfbike2.xy) <- proj4string(shpbike)

## ---- fig.width=12, fig.height=8-----------------------------------------
# Make a map
leaflet(dfbike2.xy) %>% addTiles() %>%
  addCircles(color = ~ ifelse(station.group == TRUE, 'red', 'blue'),
                                                  radius = ~abs(stands_change)*600,
             popup = ~htmltools::htmlEscape(name))

## ------------------------------------------------------------------------
address <- "27 Rue Saint-Guillaume, 75007 Paris"

getGEOAPI <- function(query = '8 bd du port',
                      geo.place = F){

  url <- if (!geo.place) paste0("https://api-adresse.data.gouv.fr/search/?q=",
                                URLencode(query)) else paste0("http://sirene.addok.xyz/search/?q=",
                                                              URLencode(query))

  query.result <- jsonlite::fromJSON(httr::content(httr::GET(url = url),"text"))

  return(query.result)
}

scpo.xy <- getGEOAPI(address)
coordinates <- as.numeric(unlist(scpo.xy$features$geometry$coordinates))

## ---- fig.width=12, fig.height=8-----------------------------------------
leaflet(dfbike2.xy) %>% addTiles() %>% setView(coordinates[1],  coordinates[2], zoom = 15) %>%
  addMarkers(coordinates[1],  coordinates[2], popup = htmltools::htmlEscape("Sciences Po")) %>%
  addCircles(color = ~ ifelse(station.group == TRUE, 'red', 'blue'),
                                                  radius = ~abs(stands_change)*100,
             popup = ~htmltools::htmlEscape(name))

## ------------------------------------------------------------------------
df <- df %>% dplyr::mutate(vacancy_rate = available_bike_stands/bike_stands)

df2 <- df %>% dplyr::mutate(day = lubridate::wday(timestamp),
                                                 hour = lubridate::hour(timestamp)) %>%
  group_by(number,day,hour) %>% summarise(vacancy_rate = mean(vacancy_rate,na.rm=T)) %>%
  ungroup() %>% select(-day,-hour) %>%
  group_by(number) %>% dplyr::mutate(n = row_number()) %>% ungroup()

knitr::kable(head(df), caption = "Data after dimensionality reduction")

## ------------------------------------------------------------------------
data_wide <- reshape2::dcast(df2, number ~ n, value.var="vacancy_rate")

knitr::kable(head(data_wide[,1:10]), caption = "Vélib data wide-formatted")

## ------------------------------------------------------------------------
# Drop observations with NA
data_wide <- na.omit(data_wide)

# Cluster observations
clusters <- kmeans(data_wide[,-1], 5, nstart = 20)
data_wide$cluster <- clusters$cluster

# Keep only cluster in data_wide2 dataframe
data_wide2 <- data_wide %>% select(number,cluster)

knitr::kable(head(data_wide2))

## ------------------------------------------------------------------------
data.mean <- data_wide[,-which(colnames(data_wide) == "number")] %>% group_by(cluster) %>%
  summarise_all(funs(mean(.,na.rm = T))) %>% ungroup() %>% select(-cluster)

p <- lapply(1:nrow(data.mean), function(i)({
  
  df   <- data.frame(y = as.numeric(data.mean[i,]))
  df$x <- as.numeric(1:nrow(df))
  df$d <- seq(
     from= as.POSIXct("2017-12-04 00:00", tz="Europe/Paris"),
     to= as.POSIXct("2017-12-10 23:00", tz="Europe/Paris"),
     by="hour"
   )
  df$d <- paste0(lubridate::wday(df$d,label = TRUE), " ",
                 lubridate::hour(df$d), ":00")
  brk <- df$x[seq(1,nrow(df),by = 10)]
  lbl <- as.character(df$d[seq(1,nrow(df),by = 10)])
  
  ggplot(data = df, aes(x = x, y=y)) + geom_line() +
    scale_x_continuous(breaks = brk, labels = lbl) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(x = "", y = "Vacancy rate", title = paste0("Cluster ",i))
  })
)

## ---- fig.width=10, fig.height=8-----------------------------------------
gridExtra::grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]], nrow = 3)

## ------------------------------------------------------------------------
distCenter <- function(cluster.group){
  
  data <- data_wide %>% filter(cluster == as.numeric(cluster.group)) %>%
    select(-cluster)
  cluster.center = clusters$centers[cluster.group,,drop=F]
  
  p <- sapply(1:nrow(data), function(i) sqrt(sum((as.numeric(data[i,-1])-as.numeric(cluster.center))^2)))
  p <- data.frame(id = data$number,dist = p)

  return(p)
} 

dist <- lapply(1:5, function(gg) distCenter(gg))
dist <- do.call(rbind,dist) %>% rename(number = id)

knitr::kable(head(dist), caption = "Distance to the center of the cluster")

## ------------------------------------------------------------------------
data_wide <- data_wide2
data_wide <- left_join(data_wide,dist)

## ------------------------------------------------------------------------
# Join spatial data and non spatial data
dfbike2.xy@data <- left_join(dfbike2.xy@data,data_wide)

# Drop NAs
dfbike2.xy@data <- dfbike2.xy@data[!is.na(dfbike2.xy@data$cluster),]

## ------------------------------------------------------------------------
p2 <- lapply(1:nrow(dfbike2.xy@data), function(i)({
  p[[dfbike2.xy@data$cluster[i]]]
})
  )

## ------------------------------------------------------------------------
# Size of p
pryr::object_size(p)

# Size of p & p2 in memory
pryr::object_size(p,p2)
# THE SAME !

## ---- fig.width=12, fig.height=8, eval = T-------------------------------
# Create colors
pal <- colorFactor(
  palette = "Dark2",
  domain = dfbike2.xy$cluster)

# Map
leaflet(dfbike2.xy) %>% addTiles() %>%
  addCircles(color = ~ pal(cluster),
             radius = ~50*dist,
             popup = ~mapview::popupGraph(p2, type = "svg")) %>%
    addLegend("bottomright", pal = pal, values = ~cluster,
    title = "Cluster",
    opacity = 1
  )

## ---- eval = T-----------------------------------------------------------
# If needed, install.packages('dismo')
voronoi.map = dismo::voronoi(dfbike2.xy)

## ---- eval = T, fig.width=12, fig.height=8-------------------------------
pal <- colorFactor(
  palette = "Dark2",
  domain = voronoi.map$cluster)

leaflet(voronoi.map) %>% addTiles() %>%
  addPolygons(color = ~ pal(cluster), fillColor = ~ pal(cluster),
               opacity = 0.8, weight = 1) %>%
  setView(mean(dfbike2.xy@bbox[1,]), mean(dfbike2.xy@bbox[2,]),
          zoom = 14) %>%
      addLegend("bottomright", pal = pal, values = ~cluster,
    title = "Cluster",
    opacity = 1
  )

