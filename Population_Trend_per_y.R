# Library
library(raster)
library(maptools)
library(gam)
# Load data
load("PopD_all_December.rdata")

# Extract data to a matrix
Pop <- values(PopD.ALL)
r <- raster(PopD.ALL, 1)

# Read the polygons
origins <- readShapePoly('Origins_updated.shp')

# Extract data
per.origin <- extract(r, origins, cellnumber = TRUE, buffer = 100000)
names(per.origin) <- origins@data[, 1]

# Function standardization
std <- function(x) {
  b <- (x - min(x)) / (max(x) - min(x))
  return(rev(b))
}

# Calculating mean and 
global.means <- global.gams <- list()

for (j in 1:length(per.origin)) {
  print(j)
  originI <- Pop[per.origin[[j]][, 1], ]
  time <- 21:4
  originI <- na.exclude(originI)
  b <- apply(originI, 1, std)
  nJ <- nrow(originI)
  predictions <- matrix(nrow = nJ, ncol = length(time))
  for(i in 1:nJ) {
    model <- gam(b[, i] ~ s(time, df = 15))
    col <- sample(rainbow(100), 1)
    predictions[i, ] <- predict(model)
  }
  global.means[[j]] <- apply(predictions, 2, mean) 
  global.gams[[j]] <- apply(predictions, 2, sd)
}



origin.time.region <- c(2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 
                        2, 2, 1, 2, 2, 2, 2, 2, 2, 2) # 1 = early; 2 = middle


# Plot per region
tiff("all_origins_pop_trend.tif", width = 50, height = 40, res = 300,
     units = 'cm')
par(mfrow = c(4, 5))
for (j in 1:length(global.means)) {
plot(seq(0, 1, length.out = length(time)) ~ time, col = "white", main = names(per.origin)[j],
     xlim = c(21, 4), ylab = "Pop Density", 
     xlab = "Thousand of years ago")
  x <- global.means[[j]]
  y <- global.gams[[j]]
  down <- x - y
  up <- x + y
  lines(y = down, x = time, lty = 3, col = "gray40", lwd = 1)
  lines(y = up, x = time, lty = 3, col = "gray40", lwd = 1)
  lines(y = x, x = time, lwd = 2)
  if (origin.time.region[j] == 1) {
    polygon(cbind(c(12, 8.2, 8.2, 12, 12), c(-1, -1, 2, 2, -1)),
            col = rgb(0, 1, 0, alpha = .2), border = F)
  }
  if (origin.time.region[j] == 2) {
    polygon(cbind(c(8.2, 4.2, 4.2, 8.2, 8.2), c(-1, -1, 2, 2, -1)),
            col = rgb(.28, 0, .28, alpha = .2), border = F)
  }
}
dev.off()


# Save it in pdf
pdf("all_origins_pop_trend.pdf", width = 40, height = 50)
par(mfrow = c(5, 4), mar =  c(5, 7, 7, 5))
for (j in 1:length(global.means)) {
  plot(seq(0, 1, length.out = length(time)) ~ time, col = "white", main = names(per.origin)[j],
       xlim = c(21, 4), ylab = "Pop Density", 
       xlab = "Thousand of years ago", cex.lab = 3, cex.main = 4, cex.axis = 2)
  x <- global.means[[j]]
  y <- global.gams[[j]]
  down <- x - y
  up <- x + y
  lines(y = down, x = time, lty = 3, col = "gray40", lwd = 3)
  lines(y = up, x = time, lty = 3, col = "gray40", lwd = 3)
  lines(y = x, x = time, lwd = 4)
  if (origin.time.region[j] == 1) {
    polygon(cbind(c(12, 8.2, 8.2, 12, 12), c(-1, -1, 2, 2, -1)),
            col = rgb(0, 1, 0, alpha = .2), border = F)
  }
  if (origin.time.region[j] == 2) {
    polygon(cbind(c(8.2, 4.2, 4.2, 8.2, 8.2), c(-1, -1, 2, 2, -1)),
            col = rgb(.28, 0, .28, alpha = .2), border = F)
  }
}
dev.off()

