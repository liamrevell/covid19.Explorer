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
	plot=c("standard","smooth","bar"),
	show=c("raw","per.capita","percent"),
	...){
	plot<-plot[1]
	show<-show[1]
	
	if(!is.null(data$CovidDeaths)) CovidDeaths<-data$CovidDeaths
	else CovidDeaths<-read.csv("https://liamrevell.github.io/data/Provisional_COVID-19_Death_Counts_by_Sex__Age__and_Week.csv")
	if(!is.null(data$Age.Pop)) Age.Pop<-data$Age.Pop
	else Age.Pop<-read.csv("https://liamrevell.github.io/data/US_Population_by_Age.csv")
	
	if(length(age.group)>0 && length(sex)>0){
		
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
		
		if(show=="per.capita"){
			ii<-which(Age.Pop$Age.Group%in%age.group)
			Age.Pop<-Age.Pop[ii,]
			ii<-which(Age.Pop$Sex%in%sex)
			Age.Pop<-Age.Pop[ii,]
			pop<-sum(Age.Pop$Total.Population)
			cd<-cd/(pop/1e6)
			td<-td/(pop/1e6)
		} else if(show=="percent"){
			Tot<-td+cd
			cd<-cd/Tot*100
			td<-td/Tot*100
		}

		ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31))
		mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
			"Sep","Oct","Nov","Dec","Jan (2021)")

		xx<-seq(from=32,by=7,length.out=length(td))

		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,3.1),bg=bg)
		
		pp<-if(show=="per.capita") "/ 1M population" else 
			if(show=="percent") "as % of all deaths" else ""
			
		qq<-if(show=="per.capita") "/ 1M" else if(show=="percent") "%" else ""
		
		plot(NA,xlim=xlim,ylim=c(0,1.2*max(cd)),bty="n",axes=FALSE,
			xlab="",ylab="")
			
		if(plot=="standard"){
			polygon(x=c(xx,max(xx),min(xx)),y=c(cd,0,0),border=FALSE,
				col=palette()[2])
		} else if(plot=="bar"){
			for(i in 1:length(cd)) polygon(xx[i]+c(-3.25,3.25,3.25,-3.25),
				c(0,0,cd[i],cd[i]),border=FALSE,col=palette()[2])
		} else if(plot=="smooth"){
			cds<-predict(loess(cd~xx,span=0.1))
			lines(xx,cds,lwd=2,lty="dashed",col=palette()[2])
		}
		
		Args<-list(...)
		Args$side<-2
		Args$labels<-FALSE
		h<-do.call(axis,Args)
		Args$at<-h
		Args$labels<-if(max(cd)>1000000) paste(h/1000000,"M",sep="") else
			if(max(cd)>1000) paste(h/1000,"k",sep="") else h
		do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		title(ylab=if(cumulative) paste("cumulative deaths",qq) else paste("weekly deaths",qq))
		
		if(cumulative)
			mtext(paste("a) cumulative COVID-19 deaths",pp),adj=0,line=1,cex=1.2)
		else
			mtext(paste("a) weekly COVID-19 deaths",pp),adj=0,line=1,cex=1.2)

		legend(x="topleft","confirmed COVID-19 deaths",
			pch=15,cex=0.9,
			col=palette()[2],
			pt.cex=1.5,bty="n",xpd=TRUE,
			xjust=0.5,yjust=1)
		
		plot(NA,xlim=xlim,ylim=c(0,1.2*max(td+cd)),bty="n",axes=FALSE,
			xlab="",ylab="")
			
		if(plot=="standard"){
			polygon(x=c(xx,max(xx),min(xx)),y=c(td,0,0),border=FALSE,
				col=palette()[4])
			polygon(x=c(xx,xx[length(xx):1]),y=c(td+cd,td[length(td):1]),
				border=FALSE,col=palette()[2])	
		} else if(plot=="bar"){
			for(i in 1:length(td)) polygon(xx[i]+c(-3.25,3.25,3.25,-3.25),
				c(0,0,td[i],td[i]),border=FALSE,col=palette()[4])
			for(i in 1:length(cd)) polygon(xx[i]+c(-3.25,3.25,3.25,-3.25),
				c(0,0,cd[i],cd[i])+td[i],border=FALSE,col=palette()[2])
		} else if(plot=="smooth"){
			tds<-predict(loess(td~xx,span=0.1))
			lines(xx,cds,lwd=2,lty="dashed",col=palette()[2])
			lines(xx,tds,lwd=2,lty="dashed",col=palette()[4])
		}

		Args<-list(...)
		Args$side<-2
		Args$labels<-FALSE
		if(show=="percent") Args$at<-seq(0,100,by=20)
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
			
		title(ylab=if(cumulative) paste("cumulative deaths",qq) else 
			paste("weekly deaths",qq))
		
		if(cumulative)
			mtext(paste("b) cumulative COVID-19 and non-COVID deaths",pp),adj=0,line=1,cex=1.2)
		else
			mtext(paste("b) weekly COVID-19 and non-COVID deaths",pp),adj=0,line=1,cex=1.2)
	}
}