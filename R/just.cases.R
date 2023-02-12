just.cases<-function(states="Massachusetts",
	cumulative=FALSE,
	data=list(),
	window=7,
	bg="transparent",
	xlim=c(60,366+365+365),
	per.capita=FALSE,
	cols=NULL,
	show=c("both","cases","deaths","neither"),
	plot.bars=FALSE,
	...){
	show<-show[1]
	if(length(states)>1) plot.bars=FALSE
	if(plot.bars){
		obj<-just.cases(states,cumulative,data,window=1,per.capita=per.capita,show="neither",
			plot.bars=FALSE)
		CASES<-obj$Cases[[1]]
		DEATHS<-obj$Deaths[[1]]
	}
	POST<-if(window>1&&!plot.bars) paste(" (",window,"-day moving average)",sep="") else ""
	leg.POST<-if(plot.bars&&!cumulative) paste(" (",window,"-day moving average)",sep="") else ""
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
		maxCases<-if(plot.bars) max(CASES) else max(sapply(Cases,max))
		maxDeaths<-if(plot.bars) max(DEATHS) else max(sapply(Deaths,max))
		denom<-if(per.capita) "/1M" else ""
		ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31,
			31,28,31,30,31,30,31,31,30,31,30,31,
			31,28,31,30,31,30,31,31,30,31,30,31))
		mm<-c("Jan '20","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",
			"Jan '21","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",
			"Jan '22","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",
			"Jan '23")
		mfrow<-if(show=="both") c(2,1) else c(1,1)
		if(show!="neither"){
			par(mfrow=mfrow,mar=c(5.1,5.1,3.1,3.1),bg=bg)
			Args<-list(...)
			if(show%in%c("both","cases")){
				plot(NA,xlim=xlim,ylim=1.25*c(0,maxCases),bty="n",ylab="",
					xlab="",axes=FALSE)
				if(plot.bars){
					col<-col2rgb(palette()[4])/256
					col<-rgb(col[1],col[2],col[3],alpha=0.75)
					for(i in 1:length(DEATHS)){
						polygon(i+c(-0.5,0.5,0.5,-0.5),
							c(0,0,CASES[i],CASES[i]),
							border=FALSE,col=col)
					}
				}
				if(cumulative){ 
					title(ylab=paste("cumulative cases",denom,sep=""),line=4) 
				} else {
					title(ylab=paste("daily cases",denom,sep=""),line=4)
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
				if(is.null(Args$las)) Args$las<-2
				do.call(axis,Args)
				nulo<-mapply(lines,Cases,col=cols,MoreArgs=list(lwd=2))
				legend(x="topleft",paste(states,leg.POST),
					lwd=2,col=cols,bty="n",cex=0.9,xpd=TRUE,xjust=0.5,yjust=1)
				if(cumulative){
					PRE<-if(show=="both") "a) c" else "C"
					mtext(paste(PRE,"umulative confirmed SARS-CoV-2 infections",denom,sep=""),
						adj=0,line=1,cex=1.2)
				} else {
					PRE<-if(show=="both") "a) d" else "D"
					mtext(paste(PRE,"aily confirmed SARS-CoV-2 infections",denom,POST,sep=""),
						adj=0,line=1,cex=1.2)
				}
				if(xlim[2]>(366+365+365-120))
					legend("topright",
						legend="from Oct. 20, '22\nthe U.S. CDC\nno longer reports\ndaily COVID-19\ncases or deaths.",
						bty="n",cex=0.7)
			}
			if(show%in%c("both","deaths")){
				plot(NA,xlim=xlim,ylim=1.25*c(0,maxDeaths),bty="n",ylab="",xlab="",
					axes=FALSE)
				if(plot.bars){
					col<-col2rgb(palette()[2])/256
					col<-rgb(col[1],col[2],col[3],alpha=0.75)
					for(i in 1:length(DEATHS)){
						polygon(i+c(-0.5,0.5,0.5,-0.5),
							c(0,0,DEATHS[i],DEATHS[i]),
							border=FALSE,col=col)
					}
				}
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
				if(is.null(Args$las)) Args$las<-2
				do.call(axis,Args)
				nulo<-mapply(lines,Deaths,col=cols,MoreArgs=list(lwd=2))
				legend(x="topleft",paste(states,leg.POST),
					lwd=2,col=cols,bty="n",cex=0.9,xpd=TRUE,xjust=0.5,yjust=1)
				if(cumulative){
					PRE<-if(show=="both") "b) c" else "C"
					mtext(paste(PRE,"umulative COVID-19 deaths",denom,sep=""),adj=0,
						line=1,cex=1.2)
				} else {
					PRE<-if(show=="both") "b) d" else "D"
					mtext(paste(PRE,"aily confirmed COVID-19 deaths",denom,POST,sep=""),adj=0,
						line=1,cex=1.2)
				}
				if(xlim[2]>(366+365+365-120))
					legend("topright",
						legend="from Oct. 20, '22\nthe U.S. CDC\nno longer reports\ndaily COVID-19\ncases or deaths.",
						bty="n",cex=0.7)
			}
		}
		invisible(list(Cases=Cases,Deaths=Deaths))
	}
}
