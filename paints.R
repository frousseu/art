
library(jpeg)
library(scales)
library(raster)
library(deldir)
library(abind)

show_col2<-function (colours, labels = TRUE, borders = NULL, nrow=NULL,ncol=NULL){
	n <- length(colours)
	if(is.null(nrow) || is.null(ncol)){
	  ncol <- ceiling(sqrt(n))
	  nrow <- ceiling(n/ncol)
	}
	colours <- c(colours, rep(NA, nrow * ncol - length(colours)))
	colours <- matrix(colours, ncol = ncol, byrow = TRUE)
	#old <- par(pty = "s", mar = c(0, 0, 0, 0))
	#on.exit(par(old))
	sizey <- nrow(colours)
	sizex <- ncol(colours)
	plot(c(0, sizex), c(0, -sizey), type = "n", xlab = "", ylab = "", 
						axes = FALSE,xaxs="i",yaxs="i")
	rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours), 
						col = colours, border = borders)
}

#download.file("https://i2.wp.com/www.milanor.net/blog/wp-content/uploads/2017/06/paletteR-1.jpg", "image.jpg")

path<-"C:/Users/User/Documents/paints"
x<-sample(list.files(path))
#x<-x[rep(2,48)]
x2<-x[1:4]


di<-lapply(seq_along(x),function(i){
	p<-readJPEG(paste(path,x[i],sep="/"))
	dim(p)
})
di<-sapply(di,"[",c(1:2))
hist(di[1,]/di[2,])
ratio<-mean(di[1,]/di[2,])
ratio<-0.8
w<-18

set.seed(333)

png(file.path("C:/Users/User/Documents",paste0("paints",gsub("-| |:","",Sys.time()),".png")),width=w,height=(w/(3*2))*23*ratio,units="in",res=300)
par(mfrow=c(ceiling(length(x)/3),6),mar=c(0.5,0.5,0.5,0.5),oma=c(0.5,0.5,0.5,0.5),bg=gray(0))

cols<-lapply(seq_along(x),function(i){
	p<-readJPEG(paste(path,x[i],sep="/"))
	p<-p[,,1:3]
	p2<-abind(p,p[,,1])
	p2[,,4]<-1
 dimension    <- dim(p)
 h<-dimension[1]
 l<-dimension[2]
 robs<-h/l
 if(robs>ratio){
   a<-h-ratio*l
   h1<-floor(a/2)
   h2<-ceiling(a/2)
   p2[c(1:h1,(nrow(p2)-h2):nrow(p2)),,4]<-0.5
   p3<-p2[setdiff(1:nrow(p2),c(1:h1,(nrow(p2)-h2):nrow(p2))),,]
 }else{
 	 a<-(ratio*l-h)/ratio
 	 l1<-floor(a/2)
 	 l2<-ceiling(a/2)
 	 p2[,c(1:l1,(ncol(p2)-l2):ncol(p2)),4]<-0.5
 	 p3<-p2[,setdiff(1:ncol(p2),c(1:l1,(ncol(p2)-l2):ncol(p2))),]
 	 
 }
 dimension3<-dim(p3)
 
 p_rgb <- data.frame(
	  x = rep(1:dimension3[2], each = dimension3[1]),
	  y = rep(dimension3[1]:1, dimension3[2]),
	  R = as.vector(p3[,,1]), #slicing our array into three
	  G = as.vector(p3[,,2]),
	  B = as.vector(p3[,,3])
 )
 k_means<-kmeans(p_rgb[,c("R","G","B")],centers=35,iter.max=100)
 col<-rgb(k_means$centers)
 #col<-gray(rev(seq(0,1,length.out=48)))
 show_col2(col,labels=FALSE,border=NA,nrow=5,ncol=7)
 text(par("usr")[2]-0.05,par("usr")[3]+0.05,gsub(".jpg","",x[i]),col=gray(ifelse(mean(k_means$centers[length(col),])<0.6,1,0),0.15),adj=c(1,0))
 plot(0:1,0:1,type="n",axes=FALSE,xaxs="i",yaxs="i")
 rasterImage(p3,0,0,1,1)
 col
})
dev.off()

cols<-unlist(cols)


###


### tess

x<-runif(16*86,0,2)
y<-runif(16*86,0,1)
tess<-deldir(x,y)
plot(x, y, type="n", asp=1)
#points(x, y, pch=20, col="red", cex=0.5)
plot(tess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)
par(mar=c(0,0,0,0))
plot.tile.list(tile.list(tess),fillcol=cols,border=NA,showpoints=FALSE)







