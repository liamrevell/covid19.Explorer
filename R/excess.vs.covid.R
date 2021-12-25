excess.vs.covid<-function(
	age.group=c(
	"Under 25 years",
	"25-44 years",
	"45-64 years",
	"65-74 years",
	"75-84 years",
	"85 years and older"),
	cumulative=FALSE,
	data=list(),
	xlim=c(60,366+365),
	bg="transparent",
	lwd=2,
	...){
	if(length(age.group)>0){
		agg<-c("Under 1 year","1-4 years","5-14 years",
			"15-24 years","25-34 years","35-44 years","45-54 years",
			"55-64 years","65-74 years","75-84 years","85 years and over")
		cdg<-c("Under 25 years","25-44 years","45-64 years","65-74 years",
			"75-84 years","85 years and older")
		ii<-which(cdg%in%age.group)
		jj<-c()
		if(1%in%ii) jj<-c(jj,1,2,3,4)
		if(2%in%ii) jj<-c(jj,5,6)
		if(3%in%ii) jj<-c(jj,7,8)
		if(4%in%ii) jj<-c(jj,9)
		if(5%in%ii) jj<-c(jj,10)
		if(6%in%ii) jj<-c(jj,11)
		obj<-covid.deaths(data=data,plot=FALSE,age.group=agg[jj])
		all.covid<-sapply(obj$CD,rowSums)
		all.weekly<-rowSums(all.covid)
		all.excess<-age.deaths(data=data,state="United States",return="Excess",
			regression=TRUE,plot=FALSE,age.group=age.group)
		excess<-c(all.excess[6:52,"2020"],all.excess[1:52,"2021"])
		nn<-min(sum(!is.na(excess)),length(all.weekly))
		excess<-excess[1:nn]
		all.weekly<-all.weekly[1:nn]
		all.cumsum<-cumsum(all.weekly)
		excess.cumsum<-cumsum(excess)
		if(cumulative){
			all.weekly<-all.cumsum
			excess<-excess.cumsum
		}
		par(mar=c(5.1,5.1,3.1,2.1),bg=bg)
		ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31,
			31,28,31,30,31,30,31,31,30,31,30,31))
		mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",
			"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",
			"Jan")
		xx<-seq(from=35.5,by=7,length.out=length(excess.cumsum))
		plot(xx,excess,type="l",bty="n",axes=FALSE,xlab="",ylab="",
			lwd=lwd,col=palette()[2],ylim=c(0,1.2*max(c(excess,all.weekly))),
			xlim=xlim)
		lines(xx,all.weekly,col=palette()[7],lwd=lwd)
		Args<-list(...)
		Args$side<-2
		Args$labels<-FALSE
		h<-do.call(axis,Args)
		Args$at<-h
		Args$labels<-relabel.axis(h)
		do.call(axis,Args)
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		Args$las<-2
		v<-do.call(axis,Args)
		CDG<-c("<25","25-44","45-64","65-74",
			"75-84",">85")
		pp<-if(sum(ii)==sum(1:length(CDG))) "all ages" else paste(paste(CDG[ii],collapse=", "),"years",sep=" ")
		if(cumulative)
			legend("topleft",c("cumulative excess deaths 2020/21",
				"cumulative confirmed COVID-19 deaths",pp),
				lwd=c(2,2,NA),col=c(palette()[c(2,7)],NA),bty="n",
				cex=0.8)
		else
			legend("topleft",c("weekly excess deaths 2020/21",
				"confirmed COVID-19 deaths",pp),
				lwd=c(lwd,lwd,NA),col=c(palette()[c(2,7)],NA),bty="n",
				cex=0.8)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		clip(0,max(xx),0,1.2*max(c(excess,all.weekly)))
		obj<-data.frame(Excess.deaths=excess,Confirmed.COVID=all.weekly)
		invisible(obj)
	}
}
