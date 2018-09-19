

# Data download
download.file(url = "http://missingmigrants.iom.int/global-figures/all/csv?download=1&eid=12836", destfile = "data/mdm.csv")


# Import table
mdm <- read.csv("data/mdm.csv", stringsAsFactors = F)
latlon <- matrix(as.numeric(unlist(strsplit(mdm$Location.Coordinates, split = ", "))), ncol = 2, byrow = T)
colnames(latlon) <- c("lat", 'lon')
mdm <- cbind(mdm, latlon)


# Transform to sf object
library(sf)
mdm <- st_as_sf(mdm, coords = c("lon", "lat"), crs = 4326)
mdm <- mdm[mdm$Region.of.Incident%in%"Mediterranean",]

# Natural Earth DL
library(rnaturalearth)
ne_download(scale = 50, type = "ocean", category = "physical", destdir = "data", 
            load = FALSE, returnclass = 'sf')
ocean <- st_read(dsn = "data/ne_50m_ocean.shp")
ocean <- st_transform(ocean, 3395 )
mdm <- st_transform(mdm, 3395)


ne_download(scale = 50, type = "countries", category = "cultural", 
            destdir = "data", load = FALSE, returnclass = 'sf')



bbmed <- st_bbox(mdm)

par(mar=c(0,0,0,0))
plot(st_geometry(ocean), col = "lightblue", border = NA, xlim = bbmed[c(1,3)], ylim = bbmed[c(2,4)])
plot(mdm$geometry, add=T, pch = 21, bg = "red", col = "white", lwd = .5, cex = 0.75)



mdm$Number.Dead

library(cartography)
propSymbolsLayer(mdm, var  = "Number.Dead")








