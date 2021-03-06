age.deaths<-function(
	state="Massachusetts",
	age.group=c("Under 25 years","25-44 years","45-64 years",
	"65-74 years","75-84 years","85 years and older"),
	plot=c("raw & excess","raw & percent above normal"),
	cumulative=TRUE,
	corrected=FALSE,
	data=list(),
	date.range=list(),
	return=NULL,
	year=2020,
	bg="transparent",
	...){
	plot<-plot[1]
	if(plot==FALSE) PLOT<-FALSE
	else PLOT<-TRUE
	if(length(age.group)>0){
		leg1.bg<-"transparent" ## colorRampPalette(colors=c("white", "#E8E8E8"))(10)[2]
		RGB<-col2rgb(leg1.bg)[,1]/255
       	leg1.bg<-"transparent" ## rgb(RGB[1],RGB[2],RGB[3],0.75)
		leg2.bg<-"transparent" ## colorRampPalette(colors=c("white", "#E8E8E8"))(10)[7]
		RGB<-col2rgb(leg2.bg)[,1]/255
        	leg2.bg<-"transparent" ## rgb(RGB[1],RGB[2],RGB[3],0.75)
		plot<-plot[1]
		ss<-state
		if(state=="New York (excluding NYC)") state<-"New York"
		if(!is.null(data$age.Counts)) Counts<-data$age.Counts
		else Counts<-read.csv("https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD")
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
		jj<-which(Counts$Year==year)
		start.day<-as.numeric(start.date-as.Date("01/01/2020",format="%m/%d/%Y"))
		end.day<-as.numeric(end.date-as.Date("01/01/2020",format="%m/%d/%Y"))
		US.popn<-setNames(c(colSums(States),331002651+States["Puerto Rico",5],
			332722610+States["Puerto Rico",5]),2015:2021)
		States<-cbind(States,States[,5]/sum(States[,5])*US.popn[6])
		States<-cbind(States,States[,5]/sum(States[,5])*US.popn[7])
		colnames(States)<-2015:2021
		nyc<-8336817
		nyc<-nyc*States["New York",]/States["New York",5]
		States["New York",]<-as.vector(States["New York",])-nyc
		States<-rbind(States,nyc)
		rownames(States)[nrow(States)]<-"New York City"
		States<-rbind(States,colSums(States))
		rownames(States)[nrow(States)]<-"United States"
		Deaths<-matrix(0,53,7,dimnames=list(1:53,2015:2021))
		ii<-which(Counts$Type=="Predicted (weighted)")
		Counts<-Counts[ii,]
		nn<-max(Counts[which(Counts$Year==2021),]$Week)
		ii<-which(Counts[,1]==state)
		for(i in 1:7){
			jj<-which(Counts$Year==2014+i)
			for(j in 1:length(age.group)){
				kk<-intersect(intersect(ii,jj),
					which(Counts$Age.Group%in%age.group[j]))
				ww<-Counts[kk,]$Week
				Deaths[ww,i]<-Deaths[ww,i]+
					Counts[kk,"Number.of.Deaths"]
			}
		}
		if(nn<52) Deaths[(nn+1):52,i]<-NA
		## fix problem with 53 end of weeks in 2020
		Deaths[,7]<-c(Deaths[53,6],Deaths[1:52,7])
		Deaths<-Deaths[1:52,]
		if(nn>52){ 
			Deaths<-Deaths[1:52,]
			nn<-52
		}
		if(any(Deaths[!is.na(Deaths)]==0)){
			pp<-sum(Deaths[!is.na(Deaths)]==0)/length(Deaths[!is.na(Deaths)]==0)
			foo<-function(x,pp) (pp-ppois(10,x))^2
			lambda<-optimize(foo,c(0,40),pp=pp)$minimum
			truncatedMean<-sum(dpois(0:10,lambda=lambda)/sum(dpois(0:10,lambda=lambda))*0:10)
			Deaths[Deaths==0]<-truncatedMean
		}
		if(corrected){
			Deaths2020<-Deaths*matrix(as.numeric(States[state,6])/
				as.numeric(States[state,1:7]),52,7,byrow=TRUE)
			Deaths2021<-Deaths*matrix(as.numeric(States[state,7])/
				as.numeric(States[state,1:7]),52,7,byrow=TRUE)
		} else Deaths2020<-Deaths2021<-Deaths
		Normal2020<-rowMeans(Deaths2020[,1:5])
		Normal2021<-rowMeans(Deaths2021[,1:5])
		Excess<-cbind(Deaths2020[,1:6]-matrix(Normal2020,52,6),
			(Deaths2021[,1:7]-matrix(Normal2021,52,7))[,7])
		colnames(Excess)[7]<-"2021"
		PercentAbove<-Excess/matrix(Normal2020,52,7)*100
		cumPercentAbove<-apply(Excess,2,cumsum)/matrix(cumsum(Normal2020),52,7)*100
		if(year==2020){
			ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31))
			mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
				"Sep","Oct","Nov","Dec","Jan '21")
			days<-seq(4,by=7,to=max(ms))
		} else if(year==2021){
			ms<-cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31))
			mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
				"Sep","Oct","Nov","Dec","Jan '22")
			days<-seq(2,by=7,to=max(ms))
		}
		## start plotting
		if(year==2020) nn<-52
		if(PLOT){
			par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1),bg=bg)
			plot(NA,xlim=c(start.day,end.day),bty="n",
				ylim=c(0,1.2*max(Deaths,na.rm=TRUE)),
				xlab="",ylab="",
				axes=FALSE,...)
			title(ylab="Death count",line=4)
			Args<-list(...)
			Args$type<-NULL		
			Args$side<-2
			Args$labels<-FALSE
			h<-do.call(axis,Args)
			Args$at<-h
			Args$labels<-relabel.axis(h)
			do.call(axis,Args)
			abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
			Args$side<-1
			Args$at<-ms
			Args$labels<-mm
			v<-do.call(axis,Args)
			for(i in 1:nn){
				polygon(rep(days[i],4)+c(-2.5,2.5,2.5,-2.5),
					c(rep(0,2),rep(Deaths[i,as.character(year)],2)),
					col="grey")
			}
			for(i in 1:5){
				lines(days,Deaths[,i])
			}
			lines(days,rowMeans(Deaths[,1:5]),lwd=4,
				col=rgb(1,1,1,1))
			lines(days,rowMeans(Deaths[,1:5]),lwd=2,
				col=palette()[2])
			legend(if(year==2020) "topleft" else "topright",
				c("average weekly deaths\n2015-2019",
				"weekly deaths 2015-2019",
				paste("weekly deaths",year)),pch=c(NA,NA,22),pt.bg=c(NA,NA,"grey"),
				cex=0.7,pt.cex=c(NA,NA,1.2),lwd=c(2,1,NA),col=c(palette()[2],"black",
				"black"),bg=leg1.bg,box.col="transparent")
			mtext(paste("a)",ss,"weekly deaths by age group (or provisional)"),adj=0,line=1,cex=1.2)
		}
		if(plot=="raw & excess"){
			if(cumulative){
				cumExcess<-apply(Excess,2,cumsum)
				if(PLOT){
					plot(NA,xlim=c(start.day,end.day),
						ylim=c(1.2*min(cumExcess,na.rm=TRUE),
						1.2*max(cumExcess,na.rm=TRUE)),
						bty="l",xlab="",ylab="",
						axes=FALSE,...)
					title(ylab="Cumulative excess death",line=4)
					Args<-list(...)
					Args$type<-NULL
					Args$side<-2
					Args$labels<-FALSE
					h<-do.call(axis,Args)
					Args$at<-h
					Args$labels<-relabel.axis(h)
					do.call(axis,Args)
					abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
					Args$side<-1
					Args$at<-ms
					Args$labels<-mm
					v<-do.call(axis,Args)
					for(i in 1:5){
						lines(days,cumExcess[,i])
					}
					e2020<-cumExcess[,as.character(year)]
					e2020<-e2020[!is.na(e2020)]
					if(year==2020) ww<-0 else ww<-5
					lines(days[1:(length(e2020)-ww)],
						e2020[1:(length(e2020)-ww)],lwd=5,
						col=rgb(1,1,1,1))
					lines(days[1:(length(e2020)-ww)],
						e2020[1:(length(e2020)-ww)],lwd=3,
						col=palette()[4])
					if(ww>0){
						lines(days[(length(e2020)-ww):length(e2020)],
							e2020[(length(e2020)-ww):length(e2020)],lwd=3,
							col=rgb(1,1,1,1))
						lines(days[(length(e2020)-ww):length(e2020)],
							e2020[(length(e2020)-ww):length(e2020)],lwd=3,
							col=palette()[4],lty="dotted")
					}
					legend(if(year==2020) "topleft" else "topright",
						c("cumulative excess deaths\n2015-2019",
						paste("cumulative excess deaths",year),
						"data incomplete"),
						lwd=c(1,2,2),col=c("black",palette()[4],palette()[4]),
						lty=c("solid","solid","dotted"),cex=0.7,
						bg=leg2.bg,box.col="transparent")
					mtext(paste(
						"b)",ss,"cumulative deaths above normal (or provisional)"),adj=0,line=1,cex=1.2)
				}
			} else {
				if(PLOT){
					plot(NA,xlim=c(start.day,end.day),
						ylim=c(1.2*min(Excess,na.rm=TRUE),
						1.2*max(Excess,na.rm=TRUE)),
						bty="l",xlab="",ylab="",
						axes=FALSE,...)
					title(ylab="Excess death count",line=4)
					Args<-list(...)
					Args$type<-NULL
					Args$side<-2
					Args$labels<-FALSE
					h<-do.call(axis,Args)
					Args$at<-h
					Args$labels<-relabel.axis(h)
					do.call(axis,Args)
					abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
					Args$side<-1
					Args$at<-ms
					Args$labels<-mm
					v<-do.call(axis,Args)
					for(i in 1:nn){
						polygon(rep(days[i],4)+c(-2.5,2.5,2.5,-2.5),
							c(rep(0,2),rep(Excess[i,as.character(year)],2)),
							col="grey")
					}
					for(i in 1:5){
						lines(days,Excess[,i])
					}
					legend(if(year==2020) "topleft" else "topright",
						c("weekly excess deaths 2015-2019",
						paste("weekly excess deaths",year)),pch=c(NA,22),pt.bg=c(NA,"grey"),
						cex=0.7,pt.cex=c(NA,1.2),lwd=c(1,NA),col=c("grey",
						"black"),bg=leg2.bg,box.col="transparent")
					mtext(paste(
						"b)",ss,"weekly deaths above normal (or provisional)"),adj=0,line=1,cex=1.2)
				}
			}
		} else if(plot=="raw & percent above normal"){
			if(cumulative){
				if(PLOT){
					plot(NA,xlim=c(start.day,end.day),
						ylim=c(1.2*min(cumPercentAbove,na.rm=TRUE),
						1.2*max(cumPercentAbove,na.rm=TRUE)),
						bty="l",xlab="",ylab="",
						axes=FALSE,...)
					title(ylab="Cumulative excess death",line=4)
					Args<-list(...)
					Args$type<-NULL
					Args$side<-2
					h<-do.call(axis,Args)
					abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
					Args$side<-1
					Args$at<-ms
					Args$labels<-mm
					v<-do.call(axis,Args)
					for(i in 1:5){
						lines(days,cumPercentAbove[,i])
					}
					e2020<-cumPercentAbove[,as.character(year)]
					e2020<-e2020[!is.na(e2020)]
					if(year==2020) ww<-0 else ww<-5
					lines(days[1:(length(e2020)-ww)],
						e2020[1:(length(e2020)-ww)],lwd=5,
						col=rgb(1,1,1,1))
					lines(days[1:(length(e2020)-ww)],
						e2020[1:(length(e2020)-ww)],lwd=3,
						col=palette()[4])
					if(ww>0){
						lines(days[(length(e2020)-ww):length(e2020)],
							e2020[(length(e2020)-ww):length(e2020)],lwd=5,
							col=rgb(1,1,1,1))
						lines(days[(length(e2020)-ww):length(e2020)],
							e2020[(length(e2020)-ww):length(e2020)],lwd=3,
							col=palette()[4],lty="dotted")
					}
					legend(if(year==2020) "topleft" else "topright",
						c("cumulative % excess deaths\n2015-2019",
						"cumulative % excess deaths 2020",
						"data incomplete"),
						lwd=c(1,2,2),col=c("black",palette()[4],palette()[4]),
						lty=c("solid","solid","dotted"),cex=0.7,
						bg=leg2.bg,box.col="transparent")
					box(bty="l")
					mtext(paste(
						"b)",ss,"cumulative % deaths above normal (or provisional)"),adj=0,line=1,cex=1.2)
				}
			} else {
				if(PLOT){
					plot(NA,xlim=c(start.day,end.day),
						ylim=c(1.2*min(PercentAbove,na.rm=TRUE),
						1.2*max(PercentAbove,na.rm=TRUE)),
						bty="l",xlab="",ylab="",
						axes=FALSE,...)
					title(ylab="% death above normal",line=4)
					Args<-list(...)
					Args$type<-NULL				
					Args$side<-2
					h<-do.call(axis,Args)
					abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
					Args$side<-1
					Args$at<-ms
					Args$labels<-mm
					v<-do.call(axis,Args)
					for(i in 1:nn){
						polygon(rep(days[i],4)+c(-2.5,2.5,2.5,-2.5),
							c(rep(0,2),rep(PercentAbove[i,as.character(year)],2)),
							col="grey")
					}
					for(i in 1:5){
						lines(days,PercentAbove[,i])
					}
					legend(if(year==2020) "topleft" else "topright",
						c("weekly % excess deaths 2015-2019",
						paste("weekly % excess deaths",year)),pch=c(NA,22),pt.bg=c(NA,"grey"),
						cex=0.7,pt.cex=c(NA,1.2),lwd=c(1,NA),col=c("black",
						"black"),bg=leg2.bg,box.col="transparent")
					box(bty="l")
					mtext(paste(
						"b)",ss,"weekly % deaths above normal (or provisional)"),adj=0,line=1,cex=1.2)
				}
			}
		}
	}
	if(!is.null(return)){
		if(return=="States"){
			invisible(States)
		} else if(return=="Deaths"){
			invisible(Deaths)
		} else if(return=="Excess"){
			invisible(Excess) 
		} else if(return=="PercentAbove"){
			invisible(PercentAbove)
		} else if(return=="cumPercentAbove"){
			invisible(cumPercentAbove)
		}
	}
}