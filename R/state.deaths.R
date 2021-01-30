state.deaths<-function(
	state="Massachusetts",
	plot=c("raw","per capita","excess","excess per capita",
	"percent above normal"),
	corrected=FALSE,
	data=list(),
	date.range=list(),
	...){
	plot<-plot[1]
	ss<-state
	if(state=="New York (excluding NYC)") state<-"New York"
	if(!is.null(data$Counts)) Counts<-data$Counts
	else Counts<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv")
	if(!is.null(data$Provis)) Provis<-data$Provis
	else Provis<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv")
	if(!is.null(data$States)) States<-data$States
	else States<-read.csv("https://liamrevell.github.io/data/nst-est2019-01.csv",row.names=1)
	if(!is.null(date.range$start.date)){ 
		start.date<-date.range$start.date
		if(!isDate(start.date)) start.date<-as.Date(start.date,format="%m/%d/%Y")
	} else start.date<-as.Date("01/04/2020",format="%m/%d/%Y")
	if(!is.null(date.range$end.date)){
		end.date<-date.range$end.date
		if(!isDate(end.date)) end.date<-as.Date(end.date,format="%m/%d/%Y")
	} else end.date<-as.Date("01/02/2021",format="%m/%d/%Y")
	jj<-which(Provis$MMWR.Year==2020)
	start.day<-as.numeric(start.date-as.Date("01/01/2020",format="%m/%d/%Y"))
	end.day<-as.numeric(end.date-as.Date("01/01/2020",format="%m/%d/%Y"))
	US.popn<-setNames(c(colSums(States),331002651+States["Puerto Rico",5]),2015:2020)
	States<-cbind(States,States[,5]/sum(States[,5])*US.popn[6])
	colnames(States)<-2015:2020
	nyc<-8336817
	nyc<-nyc*States["New York",]/States["New York",5]
	States["New York",]<-as.vector(States["New York",])-nyc
	States<-rbind(States,nyc)
	rownames(States)[nrow(States)]<-"New York City"
	States<-rbind(States,colSums(States))
	rownames(States)[nrow(States)]<-"United States"
	Deaths<-matrix(NA,53,6,dimnames=list(1:53,2015:2020))
	ii<-which(Counts[,1]==state)
	for(i in 1:4){
		jj<-which(Counts$MMWR.Year==2014+i)
		jj<-intersect(ii,jj)
		Deaths[1:52,i]<-Counts[jj[1:52],"All..Cause"]
	}
	ii<-which(Provis[,1]==state)
	for(i in 5:6){
		jj<-which(Provis$MMWR.Year==2014+i)
		jj<-intersect(ii,jj)
		Deaths[1:length(jj),i]<-Provis[jj,"All.Cause"]
	}
	Deaths<-Deaths[1:52,]
	PerCapita<-(Deaths/matrix(as.numeric(States[state,]),52,6,
		byrow=TRUE))*1000000
	PerCapitaExcess<-PerCapita-matrix(rowMeans(PerCapita[,1:5]),52,6)
	if(corrected){
		Deaths<-Deaths*matrix(as.numeric(States[state,6])/
			as.numeric(States[state,1:6]),52,6,byrow=TRUE)
	}
	Normal<-rowMeans(Deaths[,1:5])
	Excess<-Deaths-matrix(Normal,52,6)
	PercentAbove<-Excess/matrix(Normal,52,6)*100
	cumPercentAbove<-apply(Excess,2,cumsum)/matrix(cumsum(Normal),52,6)*100
	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
				"Sep","Oct","Nov","Dec","Jan (2021)")
	days<-seq(4,by=7,length.out=52)
	if(plot=="raw"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1),bg="transparent")
		plot(NA,xlim=c(start.day,end.day),
			ylim=c(0,1.05*max(Deaths,na.rm=TRUE)),
			bty="o",xlab="",ylab="",
			axes=FALSE,...)
		title(ylab="Death count",line=4)
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		for(i in 1:5) lines(days,Deaths[,i],...)
		d2020<-Deaths[,"2020"]
		d2020<-d2020[!is.na(d2020)]
		lines(days[1:length(d2020)],d2020,lwd=5,col="white",...)
		lines(days[1:(length(d2020)-5)],d2020[1:(length(d2020)-5)],lwd=3,
					col=palette()[4],...)
		lines(days[(length(d2020)-5):length(d2020)],
					d2020[(length(d2020)-5):length(d2020)],lwd=3,
					col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
					 c("weekly deaths 2015-2019",
						 "weekly deaths 2020"),bty="n",cex=1,
					 lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
					 xjust=0.5,yjust=1)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dotted",
					 lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4)
		box(bty="l")
		mtext(paste("a)",ss,"total death count (or provisional count)"),adj=0,line=1,cex=1.2)
		plot(NA,xlim=c(start.day,end.day),ylim=c(0,1.05*max(colSums(Deaths,na.rm=TRUE))),
				 bty="o",xlab="",ylab="",
				 axes=FALSE,...)
		title(ylab="Death count",line=4)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		for(i in 1:5) lines(days,cumsum(Deaths[,i]),...)
		d2020<-Deaths[,"2020"]
		d2020<-d2020[!is.na(d2020)]
		d2020<-cumsum(d2020)
		lines(days[1:length(d2020)],d2020,lwd=5,col="white",...)
		lines(days[1:(length(d2020)-5)],d2020[1:(length(d2020)-5)],lwd=3,
					col=palette()[4],...)
		lines(days[(length(d2020)-5):length(d2020)],
					d2020[(length(d2020)-5):length(d2020)],lwd=3,
					col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
					 c("cumulative deaths 2015-2019",
						 "cumulative deaths 2020"),bty="n",cex=1,
					 lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
					 xjust=0.5,yjust=1)
		box(bty="l")
		mtext(paste("b)",ss,"cumulative death count (or provisional count)"),adj=0,line=1,cex=1.2)
	} else if(plot=="excess"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1),bg="transparent")
		plot(NA,xlim=c(start.day,end.day),ylim=c(min(Excess[,1:5]),
			1.05*max(Excess,na.rm=TRUE)),bty="o",
			xlab="",ylab="",
			axes=FALSE,...)
		title(ylab="Excess death count",line=4)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		lines(par()$usr[1:2],c(0,0))
		for(i in 1:5) lines(days,Excess[,i],...)
		e2020<-Excess[,"2020"]
		e2020<-e2020[!is.na(e2020)]
		lines(days[1:length(e2020)],e2020,lwd=5,col="white",...)
		lines(days[1:(length(e2020)-5)],e2020[1:(length(e2020)-5)],lwd=3,
			col=palette()[4],...)
		lines(days[(length(e2020)-5):length(e2020)],
			e2020[(length(e2020)-5):length(e2020)],lwd=3,
			col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
			c("excess weekly deaths 2015-2019",
			"excess weekly deaths 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dotted",
			lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4,bg=rgb(0,0,0,alpha=0.5))
		box(bty="l")
		mtext(paste("a)",ss,"excess death count (or provisional count)"),adj=0,line=1,cex=1.2)
		plot(NA,xlim=c(start.day,end.day),ylim=c(min(apply(Excess,2,cumsum),na.rm=TRUE),
			1.05*max(apply(Excess,2,cumsum),na.rm=TRUE)),
			bty="o",xlab="",ylab="",
			axes=FALSE,...)
		title(ylab="Excess death count",line=4)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		lines(par()$usr[1:2],c(0,0))
		for(i in 1:5) lines(days,cumsum(Excess[,i]),...)
		e2020<-cumsum(e2020)
		lines(days[1:length(e2020)],e2020,lwd=5,col="white",...)
		lines(days[1:(length(e2020)-5)],e2020[1:(length(e2020)-5)],lwd=3,
			col=palette()[4],...)
		lines(days[(length(e2020)-5):length(e2020)],
			e2020[(length(e2020)-5):length(e2020)],lwd=3,
			col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
			c("excess deaths 2015-2019",
			"excess deaths 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		box(bty="l")
		mtext(paste("b)",ss,
			"cumulative excess death count (or provisional count)"),adj=0,line=1,cex=1.2)
	} else if(plot=="per capita"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1),bg="transparent")
		plot(NA,xlim=c(start.day,end.day),ylim=c(0,
			1.05*max(PerCapita,na.rm=TRUE)),bty="o",
			xlab="",ylab="",
			axes=FALSE,...)
		title(ylab="Deaths/1M population",line=4)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		for(i in 1:5) lines(days,PerCapita[,i],...)
		pc2020<-PerCapita[,"2020"]
		pc2020<-pc2020[!is.na(pc2020)]
		lines(days[1:length(pc2020)],pc2020,lwd=5,col="white",...)
		lines(days[1:(length(pc2020)-5)],pc2020[1:(length(pc2020)-5)],lwd=3,
			col=palette()[4],...)
		lines(days[(length(pc2020)-5):length(pc2020)],
			pc2020[(length(pc2020)-5):length(pc2020)],lwd=3,
			col=palette()[4],lty="dotted",...)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dotted",
			lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4,bg=rgb(0,0,0,alpha=0.5))
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
			c("weekly deaths/1M population 2015-2019",
			"weekly deaths/1M population 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		box(bty="l")
		mtext(paste("a)",ss,"deaths/1M population (or provisional)"),adj=0,line=1,cex=1.2)
		plot(NA,xlim=c(start.day,end.day),ylim=c(0,1.05*max(apply(PerCapita,2,cumsum),na.rm=TRUE)),
			bty="o",xlab="",ylab="",
			axes=FALSE,...)
		title(ylab="Deaths/1M population",line=4)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		for(i in 1:5) lines(days,cumsum(PerCapita[,i]),...)
		pc2020<-cumsum(pc2020)
		lines(days[1:length(pc2020)],pc2020,lwd=5,col="white",...)
		lines(days[1:(length(pc2020)-5)],pc2020[1:(length(pc2020)-5)],lwd=3,
			col=palette()[4],...)
		lines(days[(length(pc2020)-5):length(pc2020)],
			pc2020[(length(pc2020)-5):length(pc2020)],lwd=3,
			col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
			c("cumulative deaths/1M population 2015-2019",
			"cumulative deaths/1M population 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		box(bty="l")
		mtext(paste(
			"b)",ss,"cumulative deaths/1M population (or provisional)"),adj=0,line=1,cex=1.2)
	} else if(plot=="excess per capita"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1),bg="transparent")
		plot(NA,xlim=c(start.day,end.day),ylim=c(min(PerCapitaExcess[,1:5]),
			1.05*max(PerCapitaExcess,na.rm=TRUE)),bty="o",
			xlab="",ylab="",
			axes=FALSE,...)
		title(ylab="Excess deaths/1M population",line=4)
		Args<-list(...)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		lines(par()$usr[1:2],c(0,0))
		for(i in 1:5) lines(days,PerCapitaExcess[,i],...)
		pce2020<-PerCapitaExcess[,"2020"]
		pce2020<-pce2020[!is.na(pce2020)]
		lines(days[1:length(pce2020)],pce2020,lwd=5,col="white",...)
		lines(days[1:(length(pce2020)-5)],pce2020[1:(length(pce2020)-5)],lwd=3,
			col=palette()[4],...)
		lines(days[(length(pce2020)-5):length(pce2020)],
			pce2020[(length(pce2020)-5):length(pce2020)],lwd=3,
			col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
			c("excess weekly deaths/1M population 2015-2019",
			"excess weekly deaths/1M population 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dotted",
			lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4,bg=rgb(0,0,0,alpha=0.5))
		box(bty="l")
		mtext(paste("a)",ss,"excess deaths/1M population (or provisional)"),adj=0,line=1,cex=1.2)
		plot(NA,xlim=c(start.day,end.day),ylim=c(min(apply(PerCapitaExcess,2,cumsum),na.rm=TRUE),
			1.05*max(apply(PerCapitaExcess,2,cumsum),na.rm=TRUE)),
			bty="o",xlab="",ylab="",
			axes=FALSE,...)
		title(ylab="Excess deaths/1M population",line=4)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		lines(par()$usr[1:2],c(0,0))
		for(i in 1:5) lines(days,cumsum(PerCapitaExcess[,i]),...)
		pce2020<-cumsum(pce2020)
		lines(days[1:length(pce2020)],pce2020,lwd=5,col="white",...)
		lines(days[1:(length(pce2020)-5)],pce2020[1:(length(pce2020)-5)],lwd=3,
			col=palette()[4],...)
		lines(days[(length(pce2020)-5):length(pce2020)],
			pce2020[(length(pce2020)-5):length(pce2020)],lwd=3,
			col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
			c("cumulative excess deaths/1M population 2015-2019",
			"cumulative excess deaths/1M population 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		box(bty="l")
		mtext(paste(
			"b)",ss,"cumulative excess deaths/1M population (or provisional)"),adj=0,line=1,cex=1.2)
	} else if(plot=="percent above normal"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1),bg="transparent")
		plot(NA,xlim=c(start.day,end.day),ylim=c(min(PercentAbove[,1:5]),
				 1.05*max(PercentAbove,na.rm=TRUE)),bty="o",
				 xlab="",ylab="",
				 axes=FALSE,...)
		title(ylab="% above normal",line=4)
		Args<-list(...)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		lines(par()$usr[1:2],c(0,0))
		for(i in 1:5) lines(days,PercentAbove[,i],...)
		pa2020<-PercentAbove[,"2020"]
		pa2020<-pa2020[!is.na(pa2020)]
		lines(days[1:length(pa2020)],pa2020,lwd=5,col="white",...)
		lines(days[1:(length(pa2020)-5)],pa2020[1:(length(pa2020)-5)],lwd=3,
					col=palette()[4],...)
		lines(days[(length(pa2020)-5):length(pa2020)],
					pa2020[(length(pa2020)-5):length(pa2020)],lwd=3,
					col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
					 c("weekly % above normal 2015-2019",
						 "weekly % above normal 2020"),bty="n",cex=1,
					 lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
					 xjust=0.5,yjust=1)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dotted",
					 lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4,bg=rgb(0,0,0,alpha=0.5))
		box(bty="l")
		mtext(paste("a)",ss,"% deaths above normal (or provisional)"),adj=0,line=1,cex=1.2)
		plot(NA,xlim=c(start.day,end.day),
			ylim=c(min(cumPercentAbove,na.rm=TRUE),
			1.05*max(cumPercentAbove,na.rm=TRUE)),
			bty="o",xlab="",ylab="",
			axes=FALSE,...)
		title(ylab="% above normal",line=4)
		#rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],
		#		 border=grey(0.75),col=grey(0.99))
		Args<-list(...)
		Args$type<-NULL
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		lines(par()$usr[1:2],c(0,0))
		for(i in 1:5) lines(days,cumPercentAbove[,i],...)
		pa2020<-cumPercentAbove[,"2020"]
		pa2020<-pa2020[!is.na(PercentAbove[,"2020"])]
		lines(days[1:length(pa2020)],pa2020,lwd=5,col="white",...)
		lines(days[1:(length(pa2020)-5)],pa2020[1:(length(pa2020)-5)],lwd=3,
					col=palette()[4],...)
		lines(days[(length(pa2020)-5):length(pa2020)],
					pa2020[(length(pa2020)-5):length(pa2020)],lwd=3,
					col=palette()[4],lty="dotted",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.11*diff(par()$usr[3:4]),
					 c("cumulative % above normal 2015-2019",
						 "cumulative % above normal 2020"),bty="n",cex=1,
					 lwd=c(1,2),col=c("black",palette()[4]),seg.len=4,xpd=TRUE,
					 xjust=0.5,yjust=1)
		box(bty="l")
		mtext(paste(
			"b)",ss,"cumulative % deaths above normal (or provisional)"),adj=0,line=1,cex=1.2)
	} else if(plot=="States"){
			invisible(States)
	} else if(plot=="Deaths"){
			invisible(Deaths)
	} else if(plot=="Excess"){
			invisible(Excess) 
	}	else if(plot=="PerCapita"){
			invisible(PerCapita)
	} else if(plot=="PerCapitaExcess"){
			invisible(PerCapitaExcess)
	} else if(plot=="PercentAbove"){
			invisible(PercentAbove)
	}
}

isDate<-function(date) inherits(date,"Date")

