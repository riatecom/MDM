library(rnaturalearth)
library(sf)
library(cartography)

# 
# # Data download
# 
# ## Natural Earth
# ne_download(scale = 50, type = "ocean", category = "physical", destdir = "data", 
#             load = FALSE, returnclass = 'sf')
# ne_download(scale = 50, type = "countries", category = "cultural", 
#             destdir = "data", load = FALSE, returnclass = 'sf')
# # Dataset OIM
# download.file(url = "http://missingmigrants.iom.int/global-figures/all/csv?download=1&eid=12836", 
#               destfile = "data/mdm.csv")




## Data import
mdm <- read.csv("data/mdm.csv", stringsAsFactors = F)
latlon <- matrix(as.numeric(unlist(strsplit(mdm$Location.Coordinates, 
                                            split = ", "))), 
                 ncol = 2, byrow = T)
colnames(latlon) <- c("lat", 'lon')
mdm <- cbind(mdm, latlon)

# Transform to sf object
mdm <- st_as_sf(mdm, coords = c("lon", "lat"), crs = 4326)
mdm <- mdm[mdm$Region.of.Incident%in%"Mediterranean",]
mdm <- st_transform(mdm, 3395)


ocean <- st_read(dsn = "data/ne_50m_ocean.shp")
ocean <- st_transform(ocean, 3395 )
bbocean <- st_bbox(c(xmin = -982800, xmax = 4070000, 
                     ymin = 3450000, ymax = 5710000), crs = st_crs(ocean))
ocean <- st_crop(ocean, bbocean)
ocean <- st_cast(ocean, to = "POLYGON")
ocean <- ocean[10, ]
p2 <- rbind(c(3030089,4992571), 
            c(3030089,5927043), 
            c(4155959,5927043),
            c(4155959,4992571), 
            c(3030089,4992571))
pol <-st_polygon(list(p2))
pol <- st_sfc(pol, crs = st_crs(ocean))
ocean <- st_difference(ocean, pol)

plot(ocean$geometry)
sizes <- getFigDim(x = ocean, width = 1200,mar = c(0.4,0,1.2,0.4), res = 100)
# export the map
png(file = "toto.png", width = sizes[1], height = sizes[2], res = 100)
par(mar = c(0.4,0,1.2,0.4))
b <- st_bbox(ocean)
f <- 26
xlim <- c(b[1] + (b[3] - b[1])/f , b[3] - (b[3] - b[1])/f)
ylim <- c(b[2] + (b[4] - b[2])/f , b[4] - (b[4] - b[2])/f)
x <- st_as_sfc(st_bbox(ocean))
# plot(x, xlim = xlim, ylim = ylim,  border = "red", lwd = 1)
plot(ocean$geometry,col = "lightblue", border = NA, add=F,
     xlim = xlim, ylim = ylim)
propSymbolsLayer(mdm, var  = "Number.Dead", col = "red", inches = 0.5,
                 border = "white", lwd = .6, legend.pos = "n")
layoutLayer(frame = FALSE, tabtitle = TRUE, scale = 200, north = F, 
            sources="", author = "")
# points(b[1] - (b[3] - b[1])/25, b[2] - (b[4] - b[2])/25, pch = 3, cex = 2) 
dev.off()




bbocean[c(1,3)] + c(1000, 2000)
getFigDim(x = ocean, width = 800, res = 100, mar= c(0,0,1.2,0))

png("toto.png", width = 800, height = 380, res = 100)
par(mar = c(0,0,1.2,0))
plot(ocean$geometry,col = "lightblue", border = NA, 
     xlim = c(-570000, 3900000), ylim = c(3500000, 5600000))
propSymbolsLayer(mdm, var  = "Number.Dead", col = "red", inches = 0.5,
                 border = "white", lwd = .6, legend.pos = "n")
layoutLayer(frame = FALSE, tabtitle = TRUE, scale = 200, north = F, author = "")
dev.off()


bbocean[c(2,4)] + c((bbocean[3]-bbocean[1])*2/25,0 )

# MAp position
dev.off()
bbmed <- st_bbox(mdm)
par(mar=c(0,0,0,0))
plot(st_geometry(ocean), col = "lightblue", border = NA, 
     xlim = bbmed[c(1,3)], ylim = bbmed[c(2,4)])
plot(mdm$geometry, add=T, pch = 21, bg = "red", col = "white", 
     lwd = .8, cex = 0.75)


# MAp Quantities
plot(st_geometry(ocean), col = "lightblue", border = NA, 
     xlim = bbmed[c(1,3)], ylim = bbmed[c(2,4)])
propSymbolsLayer(mdm, var  = "Number.Dead", col = "red", inches = 0.5,
                 border = "white", lwd = .6)




# Map zone
library(cartogram)
par(mar=c(0,0,0,0), mfrow = c(1,2))
bbzone <- st_bbox(c(xmin = 1049943, ymin = 3462796, 
                    xmax =2187520,  ymax = 4759943), crs = st_crs(mdm))
mdmzone <- st_crop(mdm, bbzone)
mdmzone <- mdmzone[!is.na(mdmzone$Number.Dead),]

plot(st_geometry(ocean), col = "lightblue", border = NA, 
     xlim = bbzone[c(1,3)], ylim = bbzone[c(2,4)])
propSymbolsLayer(mdmzone, var  = "Number.Dead", col = "red", 
                 border = "white", lwd = .6, inches = 0.25)

w <- 1 - (mdmzone$Number.Dead / max(mdmzone$Number.Dead))
mdmdor <- cartogram_dorling(x = st_jitter(mdmzone), 
                            weight = "Number.Dead", k = 2)
plot(st_geometry(ocean), col = "lightblue", border = NA, 
     xlim = bbzone[c(1,3)], ylim = bbzone[c(2,4)])
plot(mdmdor$geometry, add=T,  border = "white", col = "red", lwd = .8)


# 
# library(SpatialPosition)
# x <- as(mdm,'Spatial')
# xx <- quickStewart(spdf = x, df = x@data, var = "Number.Dead", typefct = "exponential", span = 75000, beta = 3)
# par(mar=c(0,0,0,0))
# plot(st_geometry(ocean), col = "lightblue", border = NA, 
#      xlim = bbmed[c(1,3)], ylim = bbmed[c(2,4)])
# choroLayer(spdf = xx, var = "center", nclass = 8, add=T)


## Across time
dev.off()
par(mar=c(0,0,0,0), mfrow = c(2,3))
for (i in 2014:2018){
  plot(st_geometry(ocean), col = "lightblue", border = NA, 
       xlim = bbzone[c(1,3)], ylim = bbzone[c(2,4)])
  propSymbolsLayer(st_jitter(mdmzone[mdmzone$Reported.Year==i, ]), var  = "Number.Dead", 
                   col = "red", fixmax = 100,legend.pos = "n",
                   border = "white", lwd = .6, inches = 0.2)
  mtext(text = i, side = 3, line = -2)
}









