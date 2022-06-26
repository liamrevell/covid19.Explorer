make.transparent<-function(color,alpha){
	if(length(color)>1 && length(alpha)>1){
		if(length(color) != length(alpha)){
			cat("Lengths of color and alpha should match.\n")
			cat("Using only first alpha value....\n")
			cols<-make.transparent(color,alpha[1])
		} else {
			cols<-mapply(make.transparent,color=color,alpha=alpha)
		}
	} else if(length(color)>1 && length(alpha)==1){
		cols<-sapply(color,make.transparent,alpha=alpha)
	} else if(length(color)==1 && length(alpha)>1){
		cols<-sapply(alpha,make.transparent,color=color)
	} else {
		RGB<-col2rgb(color)[,1]/255
		cols<-rgb(RGB[1],RGB[2],RGB[3],alpha)
	}
	cols
}