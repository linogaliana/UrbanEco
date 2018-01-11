## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(rgdal)
library(UrbanEco)

datadir <- "/home/lino/Téléchargements"

## ---- message=FALSE, warning=FALSE, results='hide'-----------------------
IRISdir <- paste0(datadir, "/CONTOURS-IRIS_2-0__SHP_LAMB93_FXX_2014-01-01/CONTOURS-IRIS",
                  "/1_DONNEES_LIVRAISON_2014/CONTOURS-IRIS_2-0_SHP_LAMB93_FE-2014")

df.IRIS <- rgdal::readOGR(paste0(IRISdir,"/CONTOURS-IRIS_FE.shp"))

## ---- message=FALSE, warning=FALSE---------------------------------------
class(df.IRIS)

## ---- message=FALSE, warning=FALSE---------------------------------------
proj4string(df.IRIS)

## ---- message=FALSE, warning=FALSE---------------------------------------
proj4string(df.IRIS) <- CRS("+init=epsg:2154")

## ---- message=FALSE, warning=FALSE, eval=FALSE---------------------------
#  df.WSG84 <- sp::spTransform(df.IRIS,CRS("+init=epsg:4326"))

## ---- message=FALSE, warning=FALSE---------------------------------------
df.IRIS$DEP <- substr(df.IRIS$DEPCOM, start = 1, stop = 2)  

## ---- message=FALSE, warning=FALSE---------------------------------------
# METHOD 1: []
df75.m1 <- df.IRIS[df.IRIS$DEP %in% c('75','77','78','91','92','93','94','95'),]

# METHOD 2: SUBSET
df75.m2 <- subset(df.IRIS,df.IRIS$DEP == '75')

## ---- message=FALSE, warning=FALSE, fig.width=8, fig.height=5------------
plot(df75.m1)

## ---- message=FALSE, warning=FALSE, fig.width=8, fig.height=5------------
plot(df75.m2)

## ---- message=FALSE, warning=FALSE---------------------------------------
df75.m1@data <- df75.m1@data %>% group_by(NOM_COM) %>% dplyr::mutate(IRIS.tot = n())

knitr::kable(df75.m1@data[sample(1:nrow(df75.m1@data),10,F),])

## ---- results='hide'-----------------------------------------------------
shapeVelib <- paste0(datadir,"/stations-velib-disponibilites-en-temps-reel/jcdecaux_bike_data.shp")

shpbike <- rgdal::readOGR(shapeVelib)

## ------------------------------------------------------------------------
class(shpbike)

## ------------------------------------------------------------------------
df <- tbl_df(data.frame(shpbike))

## ------------------------------------------------------------------------
dfpolygon <- broom::tidy(df75.m2)
knitr::kable(head(dfpolygon,10), caption = "SpatialPolygon converted as dataframe")

## ------------------------------------------------------------------------
knitr::kable(head(df), caption = "Dataframe we will transform in SpatialDataFrame")

## ------------------------------------------------------------------------
coordinates(df) <- ~coords.x1+coords.x2

## ------------------------------------------------------------------------
class(df)
proj4string(df)

## ---- message=FALSE, warning=FALSE---------------------------------------
proj4string(df) <- CRS("+init=epsg:2154")

## ---- fig.width=12, fig.height=8-----------------------------------------
leaflet::leaflet(df) %>% leaflet::addTiles() %>%
  leaflet::addCircles(popup = ~name)


## ---- eval = FALSE-------------------------------------------------------
#  rgdal::writeOGR(df,getwd(), driver="ESRI Shapefile", overwrite_layer=TRUE)

## ------------------------------------------------------------------------
df.filosofi <- readxl::read_xls(paste0(datadir,"/BASE_TD_FILO_DISP_IRIS_2013.xls"), skip = 5)
knitr::kable(head(df.filosofi[,1:10]), caption = "Infracommunal income data")

## ------------------------------------------------------------------------
knitr::kable(head(df.IRIS@data), caption = "Shapefile")

## ------------------------------------------------------------------------
df.filosofi <- df.filosofi %>% rename(DCOMIRIS = IRIS)

## ------------------------------------------------------------------------
df.merge <- merge(df.IRIS,df.filosofi)
class(df.merge)

## ------------------------------------------------------------------------
knitr::kable(head(df.merge@data[sample(1:nrow(df.merge),10,F),c(1:3,12:16)]))

## ------------------------------------------------------------------------
df.merge2 <- subset(df.merge, DEP == '75')
df.merge2 <- sp::spTransform(df.merge2,CRS("+init=epsg:4326"))

## ---- fig.height=10, fig.width=12----------------------------------------

library(leaflet)

pal <- colorNumeric(
  palette = "Reds",
  domain = df.merge2$DISP_MED13)


leaflet(df.merge2) %>% addTiles() %>%
  addPolygons(color = ~pal(DISP_MED13), fillColor = ~pal(DISP_MED13),
               opacity = 2, weight = 1)


## ---- fig.height=10, fig.width=12----------------------------------------
df.merge3 <- df.IRIS
df.merge3@data <- left_join(df.IRIS@data,df.filosofi)

df.merge3 <- subset(df.merge3,df.merge3$DEP == '75')

