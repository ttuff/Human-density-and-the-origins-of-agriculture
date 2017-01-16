library(png)
library(maptools)
library(raster)

## Figure 1 for a publication analyzing human density in relation to the origins of agriculture. 
setwd("~/Desktop/Human density and the origins of agriculture")

domestication_times <- read.csv("~/Desktop/Human density and the origins of agriculture/Domestication timing larson 2014.csv")

dim(domestication_times)
h <- which(is.na(domestication_times[,3]))
domestication_times <- cbind(domestication_times, rep(NA, length(domestication_times[,1])))
domestication_times[,9] <- domestication_times[,3]
domestication_times[h,9] <- domestication_times[h,7]
colnames(domestication_times)[9] <- "adopt exploitation date"
domestication_times[,10] <- domestication_times[,7]
domestication_times[which(is.na(domestication_times[,10])),10] <- 0
colnames(domestication_times)[10] <- "start of ag"
#save(domestication_times, file="~/Desktop/Human density and the origins of agriculture/Domestication timing larson 2014.Rdata")

plot(0,0, xlim=c(15,0), ylim=c(-1,1))

type_number <- 8
	match <- domestication_times[ which(domestication_times$Region == levels(domestication_times$Region)[ type_number]), 9]
	maxer <- max(match, na.rm=TRUE)
	j <- ecdf(maxer-match)
	print(j)
	

x_seq <- rev(c(0,seq(0, maxer, length.out=100)))
y_seq <- -c(0, j(seq(0, maxer, length.out=100)))

lines(x_seq, y_seq, type="l", ylim=c(-1,1))
polygon(c(0, x_seq), c(0, y_seq), border="black", col=adjustcolor("cornflowerblue", alpha=0.5))
abline(v= maxer - quantile(j)[2])

	
	break_one <- maxer
			break_two <- maxer - quantile(j)[2]
				
	polygon(x=c(break_two, break_two, break_one, break_one), y=c(0, 1, 1, 0), col=adjustcolor("cornflowerblue", alpha=0.5), border=NA)
			lines(x=c(break_two, break_two), y=c(0,-1), col="white")
dev.off()



quartz(width=8, height=8)
#pdf("~/Desktop/Human density and the origins of agriculture/Figure_1_Kavanagh_et_al_2017_GAM.pdf", width=8, height=8)

layout(matrix(c(
	1, 1, 1, 1, 1, 1, 1, 1,
	3,	6, 7, 8, 9, 10, 11,	4, 
	3,	5, 5, 5, 5, 5, 5, 	4, 
	3, 	12, 13, 14, 15, 16, 17,	4,
	2, 2, 2, 2, 2, 2, 2, 2
	), 5, 8, byrow=TRUE), width=c(1, 1, 1, 1, 1, 1, 1, 1), height=c(0.5, 1, 1.5, 1, 0.5))
#layout.show(a)


par(mar=c(0,0,0,0))

# 1-4 label margins
blankplot <- function(){
	
	plot(0,0, xlim=c(4,21), ylim=c(1, 1.25), bty="n", type="n", xaxt="n", yaxt="n", xlab="", ylab="")
}

blankplot()
blankplot()
blankplot()
blankplot()


frameplot <- function(){
	plot(21:0,rep(0, 22), xlim=c(17,4), ylim=c(0, 2.25), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
}

frameplot_bottom <- function(){
	plot(21:0,rep(0, 22), xlim=c(17,4), ylim=c(-0.25, 2), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
}


origins <- readShapePoly('Origins_updated.shp')

origin.time.region <- c(2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 
                        2, 2, 1, 2, 2, 2, 2, 2, 2, 2) # 1 = early; 2 = middle
as.character(origins$CONTINENT)

#subset_order <- c(1, 2, 3, 5, 6, 8, 9, 10, 11, 12, 17, 18)
subset_order <- c(8, 10, 9, 5, 18, 7, 6, 20, 1, 2, 13, 16)
origins_subset <- origins[subset_order,]
origins_subset$CONTINENT



d <- readPNG("~/Desktop/Human density and the origins of agriculture/earth.png")
png(file=paste("40962.png",sep=""),width=2000,height=1000, bg="transparent")
par(mar=c(0,0,0,0))
plot(seq(-180, 180, length.out = 19), seq(-90, 90, length.out = 19), type="n",xlim=c(-180, 180),ylim=c(-90, 90), xaxt="n")

rasterImage(d, -180, -90, 180, 90, interpolate=TRUE, col=d)

polygon(x=c(-180,-180, 180,180), y=c(-90, 90, 90, -90), col=adjustcolor("white", alpha=0.1))
#rasterImage(d, -13.5, -13.5, 375, 375, interpolate=TRUE, col=d)
plot(origins_subset, add=TRUE, col=adjustcolor("white", alpha=.8), xaxt="n", border="white", lwd=4) #still need to reproject!!!
dev.off()

d <- readPNG("~/Desktop/Human density and the origins of agriculture/40962.png")
dim(d)
par(mar=c(0,0,0,0))
plot(0:360,0:360,type="n",xlim=c(20,360),ylim=c(65,295), yaxt="n", xaxt="n")
rasterImage(d, -28.5, -13.5, 388, 375, interpolate=TRUE, col=d)
axis(2, label=seq(-90, 90, length.out = 19), at=seq(1, 360, length.out = 19), las=1)
mtext("latitude", 2, line=4, at=180)
abline(h=seq(1, 360, length.out = 19), col=adjustcolor("grey10", alpha= 0.4), lwd=1)
abline(h=180, col=adjustcolor("white", alpha= .5), lwd=1)


load('~/Desktop/Human density and the origins of agriculture/PopD_all_December.rdata')

# Extract the data
prod.origin <- extract(PopD.ALL, origins_subset)

library(matrixStats)
# Mean and SD per region
means <- lapply(prod.origin, colMeans, na.rm = TRUE)
sds <- lapply(prod.origin, colSds, na.rm = TRUE)

## new values from Bruno's GAM model (produced in script called Population_Trend_per_y.R)
means <- global.means
sds <- global.gams

names(means) <- origins_subset@data$CONTINENT
ymax <- max(unlist(means))
ymin <- min(unlist(means))
time <- 4:21
#plot(origins)
#means[[1]] +
#sds[[1]]
#scale(as.numeric(means[[1]]), center=FALSE)

name_vector <- as.character(origins_subset@data$CONTINENT)

###################

type_number <- 8

complex_figure <- function(type_number, i, means, sds){
						
if(i < 6)	polygon(x=c(12,12,8.2,8.2), y=c(-1,3,3,-1), col=adjustcolor("cornflowerblue", alpha=0.4), border=NA)					
if(i > 5)	polygon(x=c(8.2,8.2,4.2,4.2), y=c(-1,3,3,-1), col=adjustcolor("limegreen", alpha=0.4), border=NA)
									
	match <- domestication_times[ which(domestication_times$Region == levels(domestication_times$Region)[ type_number]), 9]
	maxer <- max(match, na.rm=TRUE)
	j <- ecdf(maxer-match)
	print(j)
	

x_seq <- rev(c(0,seq(0, maxer, length.out=100)))
y_seq <- -c(0, j(seq(0, maxer, length.out=100)))

#lines(x_seq, y_seq, type="l", ylim=c(-1,1))
#polygon(c(0, x_seq), c(0, y_seq), border="black", col=adjustcolor("cornflowerblue", alpha=0.5))
#abline(v= maxer - quantile(j)[2])

	
	break_one_1 <- maxer
			break_two_1 <- maxer - quantile(j)[2]
				
#	polygon(x=c(break_two_1, break_two_1, break_one_1, break_one_1), y=c(0, 1, 1, 0), col=adjustcolor("cornflowerblue", alpha=0.5), border=NA)
			

	match <- domestication_times[ which(domestication_times$Region == levels(domestication_times$Region)[ type_number]), 10]
	maxer <- max(match, na.rm=TRUE)
	j <- ecdf(maxer-match)
	print(j)
	

x_seq <- rev(c(0,seq(0, maxer, length.out=100)))
y_seq <- 2+c(0, j(seq(0, maxer, length.out=100)))

#lines(x_seq, y_seq)
#polygon(c(0, x_seq), c(2, y_seq), border="black", col=adjustcolor("limegreen", alpha=0.5))

	
	break_one_2 <- maxer
			break_two_2 <- maxer - quantile(j)[2]
				
#	polygon(x=c(break_two_2, break_two_2, break_one_2, break_one_2), y=c(1, 2, 2, 1), col=adjustcolor("limegreen", alpha=0.5), border=NA)
			
	
		#abline(v=11)
	type <- 1
		
		if(type == 1){
	x <- c(means[[i]] , means[[i]]  + abs(sds[[i]]), means[[i]]  - abs(sds[[i]]))
	scaled <- scale(x , center=FALSE)
	meanss <- scaled[1:18]
	sdss_plus <- scaled[19:36]
	sdss_minus <- scaled[37:54]
	#abline(v=10, col="red")
	length(scaled)
	#lines(4:21, means[[i]] + sds[[i]])
	#polygon(x=c(4:21, 21:4), y=c(sdss_plus, rev(sdss_minus)), col=adjustcolor("firebrick", alpha=1), border="white")
	polygon(x=c(21:4,4:21), y=c(sdss_plus, rev(sdss_minus)), col=adjustcolor("firebrick", alpha=1), border="white")	
	}
	
	if(type == 2){
	x <- c(means[[i]] , means[[i]]  + abs(sds[[i]]), means[[i]]  - abs(sds[[i]]))
	scaled <- x + 1 #scale(x , center=FALSE)
	meanss <- scaled[1:18]
	sdss_plus <- scaled[19:36]
	sdss_minus <- scaled[37:54]
	#abline(v=10, col="red")
	length(scaled)
	#lines(4:21, means[[i]] + sds[[i]])
	polygon(x=c(4:21, 21:4), y=c(sdss_plus, rev(sdss_minus)), col=adjustcolor("firebrick", alpha=1), border="white")	
	}

if(type == 3){
	x <- c(means[[i]] , means[[i]]  + abs(sds[[i]]), means[[i]]  - abs(sds[[i]]))
	scaled <- x #scale(x , center=FALSE)
	meanss <- scaled[1:18]
	sdss_plus <- scaled[19:36]
	sdss_minus <- scaled[37:54]
	#abline(v=10, col="red")
	length(scaled)
	#lines(4:21, means[[i]] + sds[[i]])
	polygon(x=c(4:21, 21:4), y=c(sdss_plus, rev(sdss_minus)), col=adjustcolor("firebrick", alpha=1), border="white")	
	}

	
	
means_long_y <- c(1,1,1,1,1, meanss)
means_long_x <- c(0:4, 4:21)
 
			break_one <- break_one_2
			break_two <- break_two_2
		#		polygon(x=c(break_one, break_one, 22, 22), y=c(1, 2, 2, 1), col=adjustcolor("white", alpha=0.8), border=NA)
		#		polygon(x=c(break_two, break_two, break_one, break_one), y=c(1, 2, 2, 1), col=adjustcolor("white", alpha=0), border=NA)
			#	polygon(x=c(-1,-1, break_two , break_two), y=c(1.9, 3.1, 3.1, 1.9), col=adjustcolor("white", alpha=0.8), border=NA)	
				#abline(v= break_one, col="white")
				#abline(v= break_two, col="white")
				
				break_one <- break_one_1
			break_two <- break_two_1
		#		polygon(x=c(break_one, break_one, 22, 22), y=c(0, 1, 1, 0), col=adjustcolor("white", alpha=0.8), border=NA)
		#		polygon(x=c(break_two, break_two, break_one, break_one), y=c(0, 1, 1, 0), col=adjustcolor("white", alpha=0), border=NA)
			#	polygon(x=c(-1,-1, break_two , break_two), y=c(-1.1, .1, .1, -1.1), col=adjustcolor("white", alpha=0.8), border=NA)	
				#abline(v= break_one, col="white")
				#abline(v= break_two, col="white")
				
#lines(x=c(break_one_2, break_one_2), y=c(1,3), col="white")
#lines(x=c(break_one_1, break_one_1), y=c(1,-1), col="white")
#lines(x=c(break_two_2, break_two_2), y=c(1,3), col="white")
#lines(x=c(break_two_1, break_two_1), y=c(1,-1), col="white") 
#lines(4:21, meanss)
	lines(21:4, meanss)
	
}



for(i in 1:12){

	

	if(i > 6){frameplot()}else{frameplot_bottom()}

	
		## customize polygons for each graph
	if(i == 1){ #mesoamerica  #values from Larson
		
			complex_figure(3, i, means, sds)
				
	
		}
	
	
	#########
	if(i == 2 ){ #NW lowlands SA  #values from Larson
		
		complex_figure(6, i, means, sds)
	

		}
		
		#########
	if( i == 3){ #NW lowlands SA  #values from Larson
		
		complex_figure(6, i, means, sds)
		
		}


		#########
	if(i == 4){ #Fertile crescent aka Southwest asia  #values from Larson
		
		
	complex_figure(8, i, means, sds)
				
		}
		
		#########
	if(i == 5){ #loess plateau  #values from Larson
		
		complex_figure(2, i, means, sds)
			
		}
		
		
		#########
	if(i == 6){ #new guinea  #values from Larson
		
		complex_figure(4, i, means, sds)
		
		}


#########
	if(i == 7){ #Eastern N.A.  #values from Larson
		
		complex_figure(5, i, means, sds)
		
			}


		#########
	if(i == 8){ #Andes  #values from Larson
		
		complex_figure(6, i, means, sds)
		
				}


#########
	if(i == 9){ #W. African Sav  #values from Larson
		
		complex_figure(1, i, means, sds)
		
			}


#########
	if(i == 10){ #Sudanic sav  #values from Larson
		
		complex_figure(1, i, means, sds)
		
				}


#########
	if(i == 11){ #Ganges  #values from Larson
		
		
		complex_figure(7, i, means, sds) 
		
		}


#########
	if(i == 12){ #loess  #values from Larson
		
		complex_figure(2, i, means, sds)
		 
		 		}

		
		
		#lines(4:21, means[[i]])
		
		abline(h = 1, col=adjustcolor("forestgreen", alpha=.5), lty=2)
		
	# add axes to some locations
	if(i == 1 | i == 7){axis(2, at=seq(0,2, by=0.25), label=seq(0,2, by=0.25), las=1)}
	if(i == 6 | i == 12){axis(4, at=seq(0,2, by=0.25), label=seq(0,2, by=0.25), las=1)}
	#if(i == 6 | i == 12){axis(4, at=seq(2,3, by=0.25), label=seq(0,1, by=0.25), las=1)
	#	axis(4, at=seq(-1,0, by=0.25), label=rev(seq(0,1, by=0.25)), las=1)
	#	}
	if(i > 6){axis(1)} else{axis(3)}

	
	# add text 
	if(i < 7){polygon(x=c(-30, -30, 30, 30), y=c(-0.1, -0.5, -0.5, -0.1), col="black")
	mtext(name_vector[i], 1, line=-1.2, col="white", cex=0.5)}
	
	if(i > 6){polygon(x=c(-30, -30, 30, 30), y=c(2.1, 2.5, 2.5, 2.1), col="black")
	mtext(name_vector[i], 3, line=-1.2, col="white", cex=0.5)}
	
	# add axis labels
	if(i == 1 | i ==  7){mtext("scaled density potential", 2, line=4, at=1)}
	if(i ==  3){mtext("Thousand years before present", 3, line=3.5, at =5)}
	if(i ==  9){mtext("Thousand years before present", 1, line=3.5, at =5)
		
		}
	
}






saveToPDF <- function(...) {
    d = dev.copy(pdf,...)
    dev.off(d)
}

saveToPNG <- function(...) {
    d = dev.copy(png,...)
    dev.off(d)
}

## Try them out

saveToPDF("my.pdf", height=8,width=8)
saveToPNG("my.png", height=8, width=8, units="in", res=300)
dev.off()


