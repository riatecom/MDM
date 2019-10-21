library(rnaturalearth)
library(sf)
library(cartography)

mdm <- read.csv("data/MissingMigrants-Global-2019-06-26T11-57-26.csv", stringsAsFactors = F)
mdm$Total.Dead.and.Missing <- as.numeric(mdm$Total.Dead.and.Missing)
mdm <- mdm[mdm$Region == "Mediterranean",]
mdm$Total.Dead.and.Missing <- as.numeric(mdm$Total.Dead.and.Missing)
mdm$Total.Dead.and.Missing[mdm$Web.ID == 40345] <- 750
med <- aggregate(mdm$Total.Dead.and.Missing,list(mdm$Reported.Year), sum, simplify = TRUE )
colnames(med) <- c("year","nb")
total <- round(sum(med$nb),-2)
med[med$year==2019,"year"] <- "2019*"
barplot(med$nb, xlab=paste0("Total over the perdiod: ",total,"\n(*) from 1 January to 27 June 2019"), ylab="Number of persons", names.arg=med$year,
        border="#991313",col=c("red","red","red","red","red","#ffbaba"))


mdm$ym <-paste(mdm$Reported.Month,mdm$Reported.Year)
med2 <- aggregate(mdm$Total.Dead.and.Missing,list(mdm$ym), sum, simplify = TRUE )
med2 <- med2[order(-med2$x),] 
colnames(med2) <- c("Nb")
head(med2)

