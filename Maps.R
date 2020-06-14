require(raster)
require(tmap)
require(sf)
require(readxl)
require(sp)
data("World")
dir <- "E:/Dropbox/Dropbox/Libro_Springer"
wdpa <- raster("E:/CIAT/wdpa_reclass.tif")
#PRELIMINAR
ecor <- shapefile("E:/CIAT/Ecoregions_examples/tnc_terr_ecoregions.shp")
#acc <- raster("E:/travel_time_to_cities_12.tif")
world2 <- World
World <- sf::st_transform(world2,st_crs(ecor))
coords <- as.data.frame(readxl::read_xlsx(paste0(dir,"/","Informacion Muestreo. CORREGIDO.xlsx"),sheet = "Puntos de Muestreo"))
coords <- coords[which(!is.na(coords$Longitud)),]
coords$Longitud <- as.numeric(as.character(coords$Longitud))
coords$Latitud <- as.numeric(as.character(coords$Latitud))
coordinates(coords)  <- ~Longitud+Latitud
xmin<-round(extent(coords)[1]) ;if(xmin< -180){xmin=-180}
xmax<-round(extent(coords)[2])
ymin<-round(extent(coords)[3])
ymax<-round(extent(coords)[4]) ;if(ymax>90){ymax=90}
sp_NA <-  extent(c(xmin,xmax,ymin,ymax))
coords <- st_as_sf(coords)

sp_NA <- st_bbox(c(xmin=xmin-0.5, xmax=xmax+0.5, ymax=ymax+0.5, ymin=ymin-0.5), crs = st_crs(World))

tm_map_raster <-  tm_shape(World,bbox = sp_NA) +  tm_fill(col="gray91",legend.show = F) +
  tm_shape(wdpa,bbox = sp_NA,) + tm_raster(legend.show = F,pal = "#3182bd")+ # #chartreuse4
  tm_shape(coords,bbox = sp_NA) + tm_dots(legend.show = F,col = "red",size = 2) + #purple
  tm_shape(World,bbox = sp_NA) +  tm_borders("black") +

  tm_facets(nrow = 1, sync = TRUE)+
  tm_layout(inner.margins=0,
            legend.text.size=10,
            legend.title.size=10,
            legend.position = c("left","bottom"),
            legend.bg.color = "white", legend.bg.alpha=.2);gc()

tmap_save(filename=paste0(dir,"/","MAP_WDPA_v2",".pdf"),tm=tm_map_raster,dpi=300,width =100,height=100,units = "cm");gc()
##############################

# 
# tm_map_raster2 <-  tm_shape(World,bbox = sp_NA) +  tm_fill(col="gray91",legend.show = F) +
#   tm_shape(acc,bbox = sp_NA,) + tm_raster(title = "Travel time")+ # #chartreuse4
#   tm_shape(coords,bbox = sp_NA) + tm_dots(legend.show = F,col = "red",size = 2) + #purple
#   tm_shape(World,bbox = sp_NA) +  tm_borders("black") +
# 
#   tm_facets(nrow = 1, sync = TRUE)+
#   tm_layout(inner.margins=0,
#             legend.text.size=10,
#             legend.title.size=10,
#             legend.outside = T,
#             legend.bg.color = "white", legend.bg.alpha=.2);gc()
# 
# tmap_save(filename=paste0(dir,"/","MAP_ACC_v1",".pdf"),tm=tm_map_raster2,dpi=300,width =100,height=100,units = "cm");gc()
