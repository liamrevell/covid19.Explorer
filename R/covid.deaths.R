## https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-W/vsak-wrfu

covid.deaths<-function(
	age.group=c(
	"Under 1 year",
	"1-4 years",
	"5-14 years",
	"15-24 years",
	"25-34 years",
	"35-44 years",
	"45-54 years",
	"55-64 years",
	"65-74 years",
	"75-84 years",
	"85 years and over"),
	sex=c("Female","Male"),
	all.causes=TRUE,
	cumulative=FALSE,
	data=list(),
	xlim=c(31,366-15),
	bg="transparent",
	...){
	if(!is.null(data$CovidDeaths)) CovidDeaths<-data$CovidDeaths
	else CovidDeaths<-read.csv("https://liamrevell.github.io/data/Provisional_COVID-19_Death_Counts_by_Sex__Age__and_Week.csv")
	ii<-which(CovidDeaths$Age.Group%in%age.group)
	DD<-CovidDeaths[ii,]
	ii<-which(DD$Sex%in%sex)
	DD<-DD[ii,]
	mmwr<-sort(unique(DD$MMWR.Week))
	cd<-sapply(mmwr,function(week,DD) 
		sum(DD$COVID.19.Deaths[which(DD$MMWR.Week==week)]),
		DD=DD)
	td<-sapply(mmwr,function(week,DD) 
		sum(DD$Total.Deaths[which(DD$MMWR.Week==week)]),
		DD=DD)-cd

	if(cumulative){ 
		td<-cumsum(td)
		cd<-cumsum(cd)
	}

	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","Jan (2021)")

	xx<-seq(from=32,by=7,length.out=length(td))

	par(mar=c(5.1,5.1,2.1,3.1),bg=bg)
	plot(NA,xlim=xlim,ylim=c(0,1.1*max(td+cd)),bty="n",axes=FALSE,
		xlab="",ylab="")
	polygon(x=c(xx,max(xx),min(xx)),y=c(td,0,0),border=FALSE,
		col=palette()[4])
	polygon(x=c(xx,xx[length(xx):1]),y=c(td+cd,td[length(td):1]),
		border=FALSE,col=palette()[2])

	Args<-list(...)
	Args$side<-2
	Args$labels<-FALSE
	h<-do.call(axis,Args)
	Args$at<-h
	Args$labels<-if(max(td+cd)>1000000) paste(h/1000000,"M",sep="") else
		if(max(td+cd)>1000) paste(h/1000,"k",sep="") else h
	do.call(axis,Args)
	abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
	Args$side<-1
	Args$at<-ms
	Args$labels<-mm
	v<-do.call(axis,Args)

	legend(x="topleft",c("non COVID-19 deaths",
		"confirmed COVID-19 deaths"),pch=15,cex=0.9,
		col=palette()[c(4,2)],
		pt.cex=1.5,bty="n",xpd=TRUE,
		xjust=0.5,yjust=1)

}