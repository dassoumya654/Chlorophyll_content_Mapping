library(ncdf4)
library(raster)
library(rgdal)


f <- list.files(path = getwd(), pattern = "jp2$",full.names = FALSE)

# Image for Finding chlorophyll content
Sent_stack <- stack(f, quick= TRUE)
Sent_stack <- brick(file.choose())
band5 <- raster(file.choose())
band4 <- raster(file.choose())

### Calculate Leaf Area Index ##############################
LAI <- 0.57 * exp(2.33 * ((Sent_stack$sent_stack_2.2- Sent_stack$sent_stack_2.1)/(Sent_stack$sent_stack_2.2 + Sent_stack$sent_stack_2.1)))
writeRaster(LAI, "lai_2.tif", format= "GTiff", overwrite = TRUE)

### Input the rice cultivated area shapefile ###############
rice_shp <- readOGR(file.choose())
clip1 <- crop(Sent_stack, rice_shp, snap= "in")
ref_values <- extract(Sent_stack, rice_shp, df=TRUE )
names(ref_values) <- paste0("B",c(4:7))
names(ref_values)[1] <- "ID"

sub_wave <- ref_values[2:5]
names(sub_wave) <- paste0('B',4:7)

d <- as.data.frame(t(sub_wave))
l <- c(665,703.9,740.2,782.5)

plot(l,d[,1])
lines(l~d[,1],type="o", lwd=2)
for (i in 1:1000) {
  points(l,d[,i])
}

### fit a Linear Model ######################################
c <- lm(l~d[,j])
b <- predict.lm(c)

out <- as.data.frame(NULL)
for (j in 1:2000) {
  m <- summary(lm(l~d[,j]))
  out[j, 1] <- m$coefficients[1,1]
  out[j, 2] <- m$coefficients[2,1]
  out[j, 3] <- m$r.squared
}

min <- out[which.min(out$V3),]
##############################################################
v <- function(f){val_a *f + val_b}
a <- integrate(v , lamda_1 , lamda_2)

h <- max(sub_wave$B7)

### Find Normalized Area Under Curve
naoc <- 1 - ((a$value)/(h *(783 - 665)))
o <- ((0.0219) * (exp(10.02 * naoc)))

### Input the LAI #############################################
r <- raster(file.choose())

### Chlorophyll content map ###################################
chloro <- o/r
writeRaster(chloro, "cholorphyll.tif", format= "GTiff", overwrite = TRUE)
