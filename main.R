library(rnaturalearth)
library(sf)
library(cartography)

dev.off()

# Data download

## Natural Earth
ne_download(scale = 50, type = "ocean", category = "physical", destdir = "data", 
            load = FALSE, returnclass = 'sf')
ne_download(scale = 50, type = "countries", category = "cultural", 
            destdir = "data", load = FALSE, returnclass = 'sf')

# Dataset OIM 

## Download

for (i in 2014:2018){
  download.file(url = paste0("http://missingmigrants.iom.int/global-figures/",i,"/csv"), destfile = paste0("data/mdm",i,".csv"))
}

## Data import

mdm <- read.csv("data/mdm2014.csv", stringsAsFactors = F)
mdm <- rbind(mdm,read.csv("data/mdm2015.csv", stringsAsFactors = F))
mdm <- rbind(mdm,read.csv("data/mdm2016.csv", stringsAsFactors = F))
mdm <- rbind(mdm,read.csv("data/mdm2017.csv", stringsAsFactors = F))
mdm <- rbind(mdm,read.csv("data/mdm2018.csv", stringsAsFactors = F))

# data.frame -> sf

latlon <- matrix(as.numeric(unlist(strsplit(mdm$Location.Coordinates, 
                                            split = ", "))), ncol = 2, byrow = T)
colnames(latlon) <- c("lat", 'lon')
mdm <- cbind(mdm, latlon)

mdm <- st_as_sf(mdm, coords = c("lon", "lat"), crs = 4326)
mdm <- mdm[mdm$Region.of.Incident%in%"Mediterranean",]
mdm <- st_transform(mdm, 3395)

# Ocean

ocean <- st_read(dsn = "data/ne_50m_ocean.shp")
ocean <- st_transform(ocean, 3395 )
oceanun <- ocean

# plot(ocean$geometry)

bbocean <- st_bbox(c(xmin = -982842.6, xmax = 4047417.1, 
                     ymin = 3500000, ymax = 5710000), crs = st_crs(ocean))
ocean <- st_crop(ocean, bbocean)
ocean <- st_cast(ocean, to = "POLYGON")
plot(ocean$geometry, col = "red")
plot(ocean$geometry[10], col = "blue", add=T)
ocean <- ocean[10, ]


p2 <- rbind(c(3030089,4992571), 
            c(3030089,5927043), 
            c(4155959,5927043),
            c(4155959,4992571), 
            c(3030089,4992571))
pol <-st_polygon(list(p2))
pol <- st_sfc(pol, crs = st_crs(ocean))
ocean <- st_difference(ocean, pol)

bbocean[c(1,3)] + c(1000, 2000)
getFigDim(x = ocean, width = 800, res = 100, mar= c(0,0,1.2,0))

# CARTOGRAPHIC LAYOUT

png("outputs/layout.png", width = 800, height = 380, res = 100)

par(mar = c(0,0,1.2,0))
plot(ocean$geometry,col = "lightblue", border = NA, 
     xlim = c(-570000, 3900000), ylim = c(3500000, 5600000))
propSymbolsLayer(mdm, var  = "Number.Dead", col = "red", inches = 0.5,
                 border = "white", lwd = .6, legend.pos = "n")
layoutLayer(frame = FALSE, tabtitle = FALSE, scale = 200, north = F, author = "T. Giraud & N. Lambert, 2018", source = "Sources: IOM, 2018")

p <- c(-648841, 4467573)
text(p[1], 4507864, "Gibraltar", adj = c(0.5,0), cex=0.7)
arrows(p[1], p[2], p[1], p[2]-100000, code = 2, length = 0.1)
text(1667817, 5026113, "ITALY", adj = c(0.5,0), col="#70747a",cex=0.8)
text(-467143.1, 4800787, "SPAIN", adj = c(0.5,0), col="#70747a",cex=0.8)
text(3419723, 4654326, "TURKEY", adj = c(0.5,0), col="#70747a",cex=0.8)
text(3391557, 3460100, "EGYPT", adj = c(0.5,0), col="#70747a",cex=0.8)
text(1065071, 4305071, "TUNISIA", adj = c(0.5,0), col="#70747a",cex=0.8)
text(-427711.1, 3910751, "MOROCCO", adj = c(0.5,0), col="#70747a",cex=0.8)
text(434159.2, 4091012, "ALGERIA", adj = c(0.5,0), col="#70747a",cex=0.8)
text( 1481923, 3505165, "LIBYA", adj = c(0.5,0), col="#70747a",cex=0.8)
text( 2400125, 4806420, "GREECE", adj = c(0.5,0), col="#70747a",cex=0.8)


dev.off()
locator(1)
# MAp position

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









