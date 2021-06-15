## https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-W/vsak-wrfu

IN<-function(x,y){
	x<-toupper(x)
	y<-toupper(y)
	x%in%y
}

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
	sex=c("Male","Female"),
	all.causes=TRUE,
	cumulative=FALSE,
	data=list(),
	xlim=c(60,366+135),
	bg="transparent",
	plot=c("standard","smooth","bar"),
	show=c("raw","per.capita","percent","percent.of.covid.deaths"),
	split.groups=TRUE,
	show.total.deaths=TRUE,
	palette=c("new","original"),
	...){
	plot<-plot[1]
	show<-show[1]
	palette<-palette[1]
	
	if(!is.null(data$CovidDeaths)) CovidDeaths<-data$CovidDeaths
	else CovidDeaths<-read.csv("https://liamrevell.github.io/data/Provisional_COVID-19_Death_Counts_by_Sex__Age__and_Week.csv")
	if(!is.null(data$Age.Pop)) Age.Pop<-data$Age.Pop
	else Age.Pop<-read.csv("https://liamrevell.github.io/data/US_Population_by_Age.csv")
	
	if(length(age.group)>0 && length(sex)>0){
		
		DD<-CovidDeaths

		dates<-as.Date(DD$End.Week,format="%m/%d/%Y")
		ii<-which(dates>="2021-01-09")
		DD$MMWR.Week[ii]<-DD$MMWR.Week[ii]+53

		ii<-which(IN(DD$Age.Group,age.group))
		DD<-DD[ii,]

		ii<-which(DD$Sex%in%sex)
		DD<-DD[ii,]

		mmwr<-sort(unique(DD$MMWR.Week))
		mmwr<-mmwr[6:length(mmwr)]

		cd<-sapply(mmwr,function(week,DD) 
			sum(DD$COVID.19.Deaths[which(DD$MMWR.Week==week)]),
			DD=DD)
		td<-sapply(mmwr,function(week,DD) 
			sum(DD$Total.Deaths[which(DD$MMWR.Week==week)]),
			DD=DD)-cd

		CD<-setNames(vector(mode="list",length=length(age.group)),age.group)
		TD<-setNames(vector(mode="list",length=length(age.group)),age.group)

		for(i in 1:length(age.group)){
			CD[[i]]<-matrix(NA,length(cd),length(sex),dimnames=list(mmwr,sex))
			TD[[i]]<-matrix(NA,length(td),length(sex),dimnames=list(mmwr,sex))
			for(j in 1:length(sex)){
				ii<-which(IN(DD$Age.Group,age.group[i]))
				jj<-which(DD$Sex%in%sex[j])
				CD[[i]][,j]<-if(cumulative) cumsum(DD[intersect(ii,jj),"COVID.19.Deaths"][mmwr]) else
					DD[intersect(ii,jj),"COVID.19.Deaths"][mmwr]
				TD[[i]][,j]<-if(cumulative) cumsum(DD[intersect(ii,jj),"Total.Deaths"][mmwr]) else
					DD[intersect(ii,jj),"Total.Deaths"][mmwr]
			}
		}

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
			ii<-which(Age.Pop$Age.Group%in%age.group)
			jj<-which(Age.Pop$Sex%in%sex)
			Pop<-Age.Pop[intersect(ii,jj),]
			for(i in 1:length(age.group)){
				for(j in 1:length(sex)){
					ii<-intersect(which(Age.Pop$Age.Group==age.group[i]),
						which(Age.Pop$Sex==sex[j]))
					CD[[i]][,j]<-CD[[i]][,j]/(Pop[ii,"Total.Population"]/1e6)
				}
			}
		} else if(show=="percent"){
			Tot<-td+cd
			cd<-cd/Tot*100
			td<-td/Tot*100
			for(i in 1:length(age.group)){
				for(j in 1:length(sex)){
					CD[[i]][,j]<-CD[[i]][,j]/Tot*100
				}
			}
		} else if(show=="percent.of.covid.deaths"){
			for(i in 1:length(age.group)){
				for(j in 1:length(sex)){
					CD[[i]][,j]<-CD[[i]][,j]/cd*100
					CD[[i]][is.nan(CD[[i]][,j]),j]<-0
				}
			}
			Tot<-td+cd
			cd<-cd/Tot*100
			td<-td/Tot*100
		}

		if(split.groups) 
			if(show%in%c("per.capita","percent")&&plot%in%c("bar","standard")) 
				plot<-"smooth"

		ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30))
		mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
			"Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul")

		xx<-seq(from=35.5,by=7,length.out=length(td))

		if(show.total.deaths)
			par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,3.1),bg=bg)
		else par(mar=c(5.1,5.1,3.1,3.1),bg=bg)
		
		pp<-if(show=="per.capita") "/ 1M population" else 
			if(show=="percent") "as % of all deaths" else 
			if(show=="percent.of.covid.deaths") "as % of COVID deaths" else
			""
			
		qq<-if(show=="per.capita") "/ 1M" else 
			if(show=="percent"||show=="percent.of.covid.deaths") "%" else ""

		if(show=="percent.of.covid.deaths"){
			tmp<-cd
			if(cumulative==FALSE){
				cd<-cd/cd*100
				cd[is.nan(cd)]<-0
			} else cd<-cd/max(cd)*100
		} else tmp<-cd
		
		ylim<-if(split.groups&&plot=="smooth")
			c(0,max(sapply(CD,max))) else c(0,1.2*max(cd))

		if(split.groups&&plot!="smooth"&&show=="percent.of.covid.deaths")
			ylim<-c(0,120)

		plot(NA,xlim=xlim,ylim=ylim,bty="n",axes=FALSE,
			xlab="",ylab="")
		if(palette=="new"){
			cols<-hcl.colors(22,palette="Temps")
			## cols<-cols[sort(c(seq(1,32,by=3),seq(2,32,by=3)))]
		} else if(palette=="original"){
			cols<-colorRampPalette(colors=brewer.pal("YlOrRd",n=8))(52)
			cols<-cols[sort(c(seq(1,52,by=5),seq(2,52,by=5)))]
		}
		Cols<-matrix(cols,11,2,byrow=TRUE,
			dimnames=list(c("Under 1 year","1-4 years","5-14 years",
			"15-24 years","25-34 years","35-44 years","45-54 years",
			"55-64 years","65-74 years","75-84 years","85 years and over"),
			c("Female","Male")))
		Cols<-Cols[age.group,sex,drop=FALSE]

		if(plot=="standard"){
			if(!split.groups){
				polygon(x=c(xx,max(xx),min(xx)),y=c(cd,0,0),border=FALSE,
					col=palette()[2])
			} else {
				tots<-rep(0,length(cd))
				for(i in length(age.group):1){
					for(j in length(sex):1){
						polygon(x=c(xx,xx[length(xx):1]),
							y=c(CD[[i]][,j]+tots,tots[length(tots):1]),
							border=FALSE,col=Cols[names(CD)[i],j])
						tots<-tots+CD[[i]][,j]
					}
				}
			}
		} else if(plot=="bar"){
			if(!split.groups){
				for(i in 1:length(cd)) polygon(xx[i]+c(-3.25,3.25,3.25,-3.25),
					c(0,0,cd[i],cd[i]),border=FALSE,col=palette()[2])
			} else {
				tots<-rep(0,length(cd))
				for(i in length(age.group):1){
					for(j in length(sex):1){
						for(k in 1:length(cd)){
							polygon(xx[k]+c(-3.25,3.25,3.25,-3.25),
								c(0,0,CD[[i]][k,j],CD[[i]][k,j])+tots[k],
								border=FALSE,col=Cols[names(CD)[i],j])
							tots[k]<-tots[k]+CD[[i]][k,j]
						}
					}
				}
			}
		} else if(plot=="smooth"){
			cds<-predict(loess(cd~xx,span=0.1))
			if(!split.groups){
				if(show!="percent.of.covid.deaths")
					lines(xx,cds,lwd=2,lty="dashed",col=palette()[2])
				else
					lines(xx,cd,lwd=2,lty="dashed",col=palette()[2])
			} else {
				for(i in length(age.group):1){
					for(j in length(sex):1){
						lines(xx,predict(loess(CD[[i]][,j]~xx,span=0.1)),
							lwd=2,lty="dashed",col=Cols[names(CD)[i],j])
					}
				}
			}
		}
		
		Args<-list(...)
		Args$side<-2
		Args$labels<-FALSE
		if(show=="percent.of.covid.deaths") 
			Args$at<-seq(0,100,by=20)
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
		
		if(show.total.deaths){
			if(cumulative)
				mtext(paste("a) cumulative COVID-19 deaths",pp),adj=0,line=1,cex=1.2)
			else
				mtext(paste("a) weekly COVID-19 deaths",pp),adj=0,line=1,cex=1.2)
		}
		cd<-tmp

		if(!split.groups){
			legend(x="topleft","confirmed COVID-19 deaths",
				pch=15,cex=0.9,
				col=palette()[2],
				pt.cex=1.5,bty="n",xpd=TRUE,
				xjust=0.5,yjust=1)
		} else {
			BG<-if(show=="percent.of.covid.deaths") 
				make.transparent("white",0.75) else "transparent"
			if(ncol(Cols)==2){
				i<-1
				xy<-legend(x="topleft",rep("",nrow(Cols)+1),
					pch=15,cex=0.8,
					col=c("transparent",Cols[,i]),pt.cex=1.5,xpd=TRUE,
					xjust=0.5,yjust=1,bg="transparent",box.col="transparent")
				text(xy$text$x[1]+1,xy$text$y[1],
					if(colnames(Cols)[i]=="Female") "F" else "M",cex=0.8,pos=2)
			} else i<-0
			xy<-legend(x=if(i==1) xy$rect$left+strwidth("W") else "topleft",
				y=if(i==1) xy$rect$top else NULL,
				legend=c("",rownames(Cols)),
				pch=15,cex=0.8,
				col=c("transparent",Cols[,i+1]),
				pt.cex=1.5,bg=BG,box.col="transparent")
			text(xy$text$x[1]+1,xy$text$y[1],if(colnames(Cols)[i+1]=="Female") "F" else "M",
				cex=0.8,pos=2)
			if(ncol(Cols)==2){
				i<-1
				xy<-legend(x="topleft",rep("",nrow(Cols)+1),
					pch=15,cex=0.8,
					col=c("transparent",Cols[,i]),pt.cex=1.5,xpd=TRUE,
					xjust=0.5,yjust=1,bg="transparent",box.col="transparent")
				text(xy$text$x[1]+1,xy$text$y[1],
					if(colnames(Cols)[i]=="Female") "F" else "M",cex=0.8,pos=2)
			}
		}
		if(show.total.deaths){
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
				if(show!="percent.of.covid.deaths")
					lines(xx,cds,lwd=2,lty="dashed",col=palette()[2])
				else
					lines(xx,cd,lwd=2,lty="dashed",col=palette()[2])
				tds<-predict(loess(td~xx,span=0.1))
				lines(xx,tds,lwd=2,lty="dashed",col=palette()[4])
			}

			Args<-list(...)
			Args$side<-2
			Args$labels<-FALSE
			if(show=="percent"||show=="percent.of.covid.deaths") 
				Args$at<-seq(0,100,by=20)
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

			pp<-if(show=="percent.of.covid.deaths") "as % of all deaths" else pp
			
			if(cumulative)
				mtext(paste("b) cumulative COVID-19 and non-COVID deaths",pp),adj=0,line=1,cex=1.2)
			else
				mtext(paste("b) weekly COVID-19 and non-COVID deaths",pp),adj=0,line=1,cex=1.2)
		}
	}
	invisible(list(CD=CD,TD=TD))
}

shade<-function(rgb,deviation=10){
	new<-rgb+sample(c(-1,1),3,replace=TRUE)*deviation
	while(any(new<0)||any(new>255))
		new<-rgb+sample(c(-1,1),3,replace=TRUE)*deviation
	new
}

