just.cases<-function(states="Massachusetts",
	cumulative=FALSE,
	data=list(),
	window=7,
	bg="transparent",
	xlim=c(60,366+365),
	per.capita=FALSE,
	cols=NULL,
	...){
	if(length(states)>0){
		ss<-states
		if(cumulative) window<-1
		if("New York"%in%states){
			ii<-which(states=="New York")
			ny<-c("New York City","New York (excluding NYC)")
			nyCases<-rowSums(sapply(ny,infection.estimator,data=data,getCases=TRUE,
				window=7))
			nyDeaths<-rowSums(sapply(ny,infection.estimator,data=data,getDeaths=TRUE,
				window=7))
			states<-states[-ii]
		} else ii<-NULL
		Cases<-setNames(
			lapply(states,infection.estimator,data=data,getCases=TRUE,window=window),
			states)
		Deaths<-setNames(
			lapply(states,infection.estimator,data=data,getDeaths=TRUE,window=window),
			states)
		if(!is.null(ii)){
			Cases$"New York"<-nyCases
			Cases<-Cases[ss]
			Deaths$"New York"<-nyDeaths
			Deaths<-Deaths[ss]
		}
		if(cumulative){
			Cases<-lapply(Cases,cumsum)
			Deaths<-lapply(Deaths,cumsum)
		}
		states<-ss
		if(per.capita){
			SS<-age.deaths(data=data,plot=FALSE,return="States")
			for(i in 1:length(states)){
				if(states[i]=="New York"){
					pp<-c(rep(sum(SS[c("New York","New York City"),"2020"]),366),
						rep(sum(SS[c("New York","New York City"),"2021"]),
						length(Cases[[i]])-366))
				} else {
					pp<-c(rep(SS[states[i],"2020"],366),rep(SS[states[i],"2021"],
						length(Cases[[i]])-366))
				}
				Cases[[i]]<-Cases[[i]]/(pp/1e6)
				Deaths[[i]]<-Deaths[[i]]/(pp/1e6)
			}
		}
		if(is.null(cols)){
			cols<-if(length(states)==1) "black" else 
				c("black",distinctColorPalette(length(states)-1))
		}
		maxCases<-max(sapply(Cases,max))
		maxDeaths<-max(sapply(Deaths,max))
		denom<-if(per.capita) "/1M" else ""
		ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31,
			31,28,31,30,31,30,31,31,30,31,30,31))
		mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",
			"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",
			"Jan")
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,3.1),bg=bg)
		plot(NA,xlim=xlim,ylim=1.25*c(0,maxCases),bty="n",ylab="",xlab="",axes=FALSE)
		if(cumulative){ 
			title(ylab=paste("cumulative cases",denom,sep=""),line=4) 
		} else {
			title(ylab=paste("daily cases",denom,sep=""),line=4)
		}
		Args<-list(...)
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		do.call(axis,Args)
		Args$side<-2
		Args$labels<-FALSE
		Args$at<-NULL
		h<-do.call(axis,Args)
		Args$at<-h
		Args$labels<-relabel.axis(h)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		do.call(axis,Args)
		nulo<-mapply(lines,Cases,col=cols,MoreArgs=list(lwd=2))
		legend(x="topleft",states,lwd=2,col=cols,
			bty="n",cex=0.9,xpd=TRUE,xjust=0.5,yjust=1)
		if(cumulative){ 
			mtext(paste("a) cumulative confirmed SARS-CoV-2 infections",denom,sep=""),
				adj=0,line=1,cex=1.2)
		} else {
			mtext(paste("a) daily confirmed SARS-CoV-2 infections",denom,sep=""),
				adj=0,line=1,cex=1.2)
		}
		plot(NA,xlim=xlim,ylim=1.25*c(0,maxDeaths),bty="n",ylab="",xlab="",axes=FALSE)
		if(cumulative){
			title(ylab=paste("cumulative deaths",denom,
				sep=""),line=4) 
		} else { 
			title(ylab=paste("daily deaths",denom,sep=""),line=4)
		}
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		do.call(axis,Args)
		Args$side<-2
		Args$labels<-FALSE
		Args$at<-NULL
		h<-do.call(axis,Args)
		Args$at<-h
		Args$labels<-relabel.axis(h)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		do.call(axis,Args)
		nulo<-mapply(lines,Deaths,col=cols,MoreArgs=list(lwd=2))
		legend(x="topleft",states,lwd=2,col=cols,
			bty="n",cex=0.9,xpd=TRUE,xjust=0.5,yjust=1)
		if(cumulative){
			mtext(paste("b) cumulative COVID-19 deaths",denom,sep=""),adj=0,
				line=1,cex=1.2)
		} else {
			mtext(paste("b) daily confirmed COVID-19 deaths",denom,sep=""),adj=0,
				line=1,cex=1.2)
		}
		invisible(list(Cases=Cases,Deaths=Deaths))
	}
}
