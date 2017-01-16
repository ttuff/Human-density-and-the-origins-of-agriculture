setwd("~/Desktop/Human density and the origins of agriculture")

# Get the predctions from Population_trend script
load("prediction.RData")
# Read the polygons
origins <- readShapePoly('Origins_updated.shp')

# Extract data

cells <- do.call(rbind, sapply(per.origin, subset, select = 1))
cells
g.means <- apply(prediction[-cells, ], 2, mean, na.rm = TRUE) 
g.gams <- apply(prediction[-cells, ], 2, sd, na.rm = TRUE)
g.means2 <- apply(prediction[cells, ], 2, mean, na.rm = TRUE) 
g.gams2 <- apply(prediction[cells, ], 2, sd, na.rm = TRUE)

pdf("Global_pop_trend_comparisson.pdf", width = 25, height = 20)
par(mar = c(5, 7, 7, 5))
plot(seq(0, 1, length.out = length(time)) ~ time, col = "white", main = "GLOBAL",
     xlim = c(21, 4), ylab = "Population Density (standardized)", 
     xlab = "Thousand of years ago", cex.lab = 3, cex.main = 4, cex.axis = 2)
down <- g.means - g.gams
up <- g.means + g.gams
lines(y = down, x = time, lty = 3, col = "gray40", lwd = 3)
lines(y = up, x = time, lty = 3, col = "gray40", lwd = 3)
lines(y = g.means, x = time, lwd = 4)

lines(y = g.means2, x = time, lwd = 3, col = "red")
down2 <- g.means2 - g.gams2
up2 <- g.means2 + g.gams2
lines(y = down2, x = time, lty = 3, col = "red", lwd = 3)
lines(y = up2, x = time, lty = 3, col = "red", lwd = 3)


polygon(cbind(c(12, 8.2, 8.2, 12, 12), c(-1, -1, 2, 2, -1)),
        col = rgb(0, 1, 0, alpha = .2), border = F)
polygon(cbind(c(8.2, 4.2, 4.2, 8.2, 8.2), c(-1, -1, 2, 2, -1)),
        col = rgb(.28, 0, .28, alpha = .2), border = F)
dev.off()



# Betas
# beta1[beta1 == 0] <- NA
# beta2[beta2 == 0] <- NA
# beta3[beta3 == 0] <- NA
betas <- list(beta1, beta2, beta3)
beta1 <- -beta1
beta2 <- -beta2
beta3 <- -beta3

origin.time.region <- c(2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 
                        2, 2, 1, 2, 2, 2, 2, 2, 2, 2) # 1 = early; 2 = middle
cells1 <- do.call(rbind, sapply(per.origin[origin.time.region == 1]
, subset, select = 1))
cells2 <- do.call(rbind, sapply(per.origin[origin.time.region == 2]
                                , subset, select = 1))

pdf("Global_pop_beta_comparisson.pdf", width = 20, height = 30)
par(mfrow = c(3, 1), mar = c(7, 7, 5, 5))
hist(na.omit(beta1[-cells]), 15, freq = F, col = rgb(0, 1, 0, alpha = 1), border = F, 
     main = "Early Holocene (12-8k)", xlim = c(-.5, .5), cex.lab = 3, 
     cex.main = 3, cex.axis = 1.5, xlab = "Slope")
hist(na.omit(beta1[cells1]), 15, col = rgb(0, 0, 1, alpha = .4), add = TRUE, freq = F,
     border = F)
abline(v = 0, lty = 2)
legend(x = -0.4, y = 5, c("Non-origins", "Origins"), 
       fill = c(rgb(0, 1, 0, alpha = 1), rgb(0, 0, 1, alpha = .4)),
       cex = 4)

hist(na.omit(beta2[-cells]), 15, freq = F, col = rgb(0, 1, 0, alpha = 1), border = F, 
     main = "Middle Holocene (8-4k)", xlim = c(-.5, .5), cex.lab = 3, 
     cex.main = 3, cex.axis = 1.5, xlab = "Slope")
hist(na.omit(beta2[cells2]), 15, col = rgb(0, 0, 1, alpha = .4), add = TRUE, freq = F,
     border = F)
abline(v = 0, lty = 2)
hist(na.omit(beta3[-cells]), 15, freq = F, col = rgb(0, 1, 0, alpha = 1), border = F, 
     main = "Early-Middle Holocene (12-4k)", xlim = c(-.5, .5),cex.lab = 3, 
     cex.main = 3, cex.axis = 1.5, xlab = "Slope")
hist(na.omit(beta3[cells]), 15, col = rgb(0, 0, 1, alpha = .4), add = TRUE, freq = F,
     border = F)
abline(v = 0, lty = 2)
dev.off()
