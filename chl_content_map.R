library(ncdf4)
library(raster)
library(rgdal)


f <- list.files(path = getwd(), pattern = "jp2$",full.names = FALSE)

Sent_stack <- stack(f, quick= TRUE)
Sent_stack <- brick(file.choose())

lai <- raster(file.choose())

band5 <- raster(file.choose())
band4 <- raster(file.choose())

LAI <- 8.452 * ((Sent_stack$sent_stack_2.2- Sent_stack$sent_stack_2.1)/(Sent_stack$sent_stack_2.2 + Sent_stack$sent_stack_2.1))

writeRaster(LAI, "lai_2.tif", format= "GTiff", overwrite = TRUE)

clip1 <- crop(Sent_stack, rice_shp, snap= "in")

rice_shp <- readOGR(file.choose())

ref_values <- extract(Sent_stack, rice_shp, df=TRUE )

names(ref_values) <- paste0("B",c(4:7))
names(ref_values)[1] <- "ID"

sub_wave <- ref_values[2:5]
names(sub_wave) <- paste0('B',4:7)

d <- as.data.frame(t(sub_wave))

plot(l,d[,1])
lines(l~d[,1],type="o", lwd=2)

l <- c(665,703.9,740.2,782.5)

for (i in 1:1000) {
  points(l,d[,i])
}

c <- lm(l~d[,j])
b <- predict.lm(c)


out <- as.data.frame(NULL)
for (j in 1:2000) {
  m <- summary(lm(l~d[,j]))
  out[j, 1] <- m$coefficients[1,1]
  out[j, 2] <- m$coefficients[2,1]
  out[j, 3] <- m$r.squared
}

out[which.min(out$V3),]

v <- function(f){0.05942066*f + 525.1035}
a <- integrate(v ,665 ,783)

h <- max(sub_wave$B7)

naoc <- 1 - ((a$value)/(h *(783 - 665)))

o <- ((0.0219) * (exp(10.02 * naoc)))

r <- raster(file.choose())

chloro <- o/r

writeRaster(chloro, "cholorphyll.tif", format= "GTiff", overwrite = TRUE)
