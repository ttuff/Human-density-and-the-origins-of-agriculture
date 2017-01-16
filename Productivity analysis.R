# Library
library(maptools)
library(raster)
setwd("~/Desktop/Human density and the origins of agriculture")
# Load patricks productivity PCA data
load('Productivity_ALL.RDATA')

# Load origin shapefiles
origins <- readShapePoly('Origins_updated.shp')

origin.time.region <- c(2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 
                        2, 2, 1, 2, 2, 2, 2, 2, 2, 2) # 1 = early; 2 = middle


# Extract the data
prod.origin <- extract(Productivity.ALL, origins)
# Mean and SD per region
means <- lapply(prod.origin, colMeans, na.rm = TRUE)
sds <- lapply(prod.origin, sd, na.rm = TRUE)
names(means) <- origins@data$CONTINENT
ymax <- max(unlist(means))
ymin <- min(unlist(means))
time <- 4:21

# Plot
pdf("productivity.pdf", 20, 30) 
par(mfrow = c(7, 4), mar = c(5, 7, 5, 1))
for (i in 1:length(means)) {
  plot(y = means[[i]], x = time, xlim = c(21, 4), ylim = c(ymin, ymax),
       main = names(means)[i], cex.main = 4, cex.lab = 3, cex.axis = 2,
       ylab = "Productivity (PCA axis)", xlab = "Thousand of years ago (k)",
       pch = 20, lwd = 1, type = "l", 
       col = c("purple", "green")[origin.time.region[i]])
  up <- sds[[i]] + means[[i]]
  down <-  means[[i]] - sds[[i]]
  lines(up ~ time, lty = 2)
  lines(down ~ time, lty = 2)
  
}
dev.off()