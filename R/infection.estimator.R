## https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36

moving.average<-function(x,window=7){
	xx<-c(rep(0,floor(window/2)),x,rep(x[length(x)],
		ceiling(window/2)))
	ma<-rep(NA,length(x))
	for(i in 1:length(x)) ma[i]<-mean(xx[0:(window-1)+i])
	ma
}

infection.estimator<-function(state="Massachusetts",
	cumulative=FALSE,
	data=list(),
	delay=20,
	ifr=0.005,
	window=7,
	smooth=TRUE,
	span=c(0.2,0.3),
	percent=FALSE,
	plot=TRUE,
	bg="transparent",
	xlim=c(60,366+90),
	show.points=FALSE,
	alpha=c(0.25,0.8),
	...){
	if(length(alpha)==1) alpha<-rep(alpha,2)
	if(hasArg(getCases)) getCases<-list(...)$getCases
	else getCases<-FALSE
	if(getCases) plot<-FALSE
	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31,31,28,31))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr")
	ttime<-max(ms)
	if(plot=="infection.ratio"){ 
		show.cr<-TRUE
		plot<-TRUE
	} else show.cr<-FALSE
	if(smooth) if(length(span)==1) span<-c(span,0.3)
	cols<-make.transparent(c("darkgreen",palette()[c(4,2)]),
		alpha[2])
	ifr<-make.ifr(ifr,ttime,smooth=smooth,span=span)
	if(plot){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,3.1),bg=bg)
		plot(NA,xlim=xlim,ylim=100*c(0,0.02),bty="n",
			ylab="",xlab="",axes=FALSE)
		title(ylab="assumed IFR (%)",line=4)
		Args<-list(...)
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		lines(1:length(ifr),100*ifr,lwd=2,col=cols[2])
		mtext("a) assumed infection fatality ratio (%) and daily deaths",
			adj=0,line=1,cex=1.2)
	}
	state.codes<-setNames(
		c("US","AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
		"ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
		"MO","MT","NE","NV","NH","NJ","NM","NY","NYC","NC","ND","OH","OK",
		"OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
		"WI","WY"),
		c("United States","Alabama","Alaska","Arizona",
		"Arkansas","California","Colorado","Connecticut",
		"Delaware","District of Columbia","Florida",
		"Georgia","Hawaii","Idaho","Illinois",
		"Indiana","Iowa","Kansas","Kentucky","Louisiana",
		"Maine","Maryland","Massachusetts","Michigan","Minnesota",
		"Mississippi","Missouri","Montana","Nebraska","Nevada",
		"New Hampshire","New Jersey","New Mexico","New York (excluding NYC)",
		"New York City","North Carolina","North Dakota",
		"Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
		"Rhode Island","South Carolina","South Dakota","Tennessee",
		"Texas","Utah","Vermont","Virginia",
		"Washington","West Virginia","Wisconsin","Wyoming"))
	if(!is.null(data$Cases)) { 
		Cases<-data$Cases
		dd<-as.Date(Cases$submission_date,format="%m/%d/%Y")
	} else { 
		Cases<-read.csv("https://liamrevell.github.io/data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
		dd<-as.Date(Cases$submission_date,format="%m/%d/%Y")
		Cases<-Cases[order(dd),]
	}
	Cases<-fixCases(Cases)
	if(state!="United States") Cases<-Cases[Cases$state==state.codes[state],]
	else {
		Temp<-data.frame(
			new_death=rowSums(sapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"new_death"],
				Data=Cases)),
			new_case=rowSums(sapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"new_case"],
				Data=Cases)),
			tot_death=rowSums(sapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"tot_death"],
				Data=Cases)),
			tot_cases=rowSums(sapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"tot_cases"],
				Data=Cases)))
		Cases<-Temp	
	}
	if(percent){ 
		SS<-state.deaths(plot="States",data=data)
		rownames(SS)[which(rownames(SS)=="New York")]<-
			"New York (excluding NYC)"
		population<-SS[state,"2020"]
	}
	newDeaths<-c(rep(0,21),Cases$new_death)
	if(plot){
		ylim<-c(-0.04,1.04)*max(newDeaths)
		par(usr=c(par()$usr[1:2],ylim))
		for(i in 1:length(newDeaths)){
			col<-col2rgb(cols[3])/256
			col<-rgb(col[1],col[2],col[2],alpha=alpha[1])
			polygon(i+c(-0.5,0.5,0.5,-0.5),
				c(0,0,newDeaths[i],newDeaths[i]),
				border=FALSE,col=col)
		}
		Args<-list(...)
		Args$side<-4
		h<-do.call(axis,Args)
		legend("topright",c("assumed IFR (%)",
			"daily COVID-19 deaths"),pch=c(NA,15),
			col=c(cols[2],col),
			cex=0.9,pt.cex=c(NA,1.5),lwd=c(2,NA),
			box.col="transparent")
	}
	newDeaths<-moving.average(newDeaths,window)
	obsCases<-moving.average(c(rep(0,21),Cases$new_case),window)
	if(getCases) return(obsCases)
	if(smooth){
		estCases<-moving.average(c(rep(0,21),Cases$new_death),
			window)
		estCases<-estCases[(delay+1):length(estCases)]
		estCases<-estCases/ifr[1:length(estCases)]
		T<-length(estCases)
		tt<-1:T
		cr<-obsCases[1:length(estCases)]/estCases
		cr[is.nan(cr)]<-0
		cr[cr==Inf]<-0
		cr[cr==-Inf]<-0
		if(window<7) cr<-moving.average(cr,7)
		cr[cr>1]<-1
		cr[cr<0]<-0
		fm<-try(nls(cr~a/(1+exp(-b*(tt-c))),
			start=list(a=0.3,b=0.05,c=100),
			control=list(maxiter=1000)))
		ntries<-0
		while(attr(fm,"class")=="try-error"&&ntries<10){
			ii<-sort(sample(1:T,100))
			fm<-try(nls(cr[ii]~a/(1+exp(-b*(tt[ii]-c))),
				start=list(a=0.3,b=0.05,c=100),
				control=list(maxiter=1000)))
			ntries<-ntries+1
		}
		if(attr(fm,"class")=="try-error"){
			tt<-(T-99):T
			cr<-rep(mean(cr[(T-99):T]),100)
			fm<-lm(cr~tt)
		}
		if(show.cr){
			plot(tt,cr,xlim=xlim,bty="n",pch=21,bg="grey",
				ylab="",xlab="",axes=FALSE)
			lines(tt,predict(fm),lwd=2,col=cols[2])
			Args<-list(...)
			Args$side<-2
			h<-do.call(axis,Args)
			Args$side<-1
			Args$at<-ms
			Args$labels<-mm
			v<-do.call(axis,Args)
			title(ylab="ratio",line=4)
			plot<-FALSE
			mtext(paste("b)",state,"daily confirmed cases/estimated infections"),
				adj=0,line=1,cex=1.2)
		}
		if(delay>0){
			tt<-1:(length(obsCases)-length(estCases))+length(estCases)
			CR<-predict(fm,newdata=data.frame(tt=tt))
			if(length(obsCases[tt])!=length(CR)){
				tt<-(T-99):T
				cr<-rep(mean(cr[(T-99):T]),100)
				fm<-lm(cr~tt)
				tt<-1:(length(obsCases)-length(estCases))+length(estCases)
				CR<-predict(fm,newdata=data.frame(tt=tt))
			}
			if(show.cr) lines(tt,CR,lwd=2,col=cols[2],lty="dotted")
			estCases<-c(estCases,obsCases[tt]/CR)
		}
		tt<-1:length(estCases)
		fit<-loess(estCases~tt,span=span[1])
		estCases<-predict(fit)
		estCases[estCases<0]<-0
		if(delay<=21) estCases[1:(21-delay)]<-0	
	} else {
		estCases<-moving.average(c(rep(0,21),Cases$new_death),window)
		estCases<-estCases[(delay+1):length(estCases)]
		estCases<-estCases/ifr[1:length(estCases)]
		T<-length(estCases)
	}
	if(!cumulative){
		if(percent){
			estCases<-estCases/population*100
			newDeaths<-newDeaths/population*100
			obsCases<-obsCases/population*100
		}
		if(plot){
			plot(NA,xlim=xlim,ylim=c(0,1.25*max(estCases)),bty="n",
				xlab="",
				ylab="",axes=FALSE)
			title(ylab=if(percent) "infections (observed or estimated) %" else
				"infections (observed or estimated)",line=4)
			Args<-list(...)
			Args$side<-2
			Args$labels<-FALSE
			h<-do.call(axis,Args)
			Args$at<-h
			if(percent)
				Args$labels<-paste(h,"%",sep="")
			else
				Args$labels<-if(max(estCases)>1000000) paste(h/1000000,"M",sep="") else
					if(max(estCases)>1000) paste(h/1000,"k",sep="") else h
			do.call(axis,Args)
			abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
			Args$side<-1
			Args$at<-ms
			Args$labels<-mm
			v<-do.call(axis,Args)
			polygon(c(1:T,T,1),
				c(estCases[1:T],0,0),
				border=FALSE,col=cols[1])
			lines(T:length(estCases),
				estCases[T:length(estCases)],
				lty="dotted",lwd=2,col=cols[1])
			polygon(c(1:length(obsCases),length(obsCases),1),
				c(obsCases,0,0),
				border=FALSE,col=cols[2])
			polygon(c(1:length(newDeaths),length(newDeaths),1),
				c(newDeaths,0,0),
				border=FALSE,col=cols[3])
			if(show.points){
				ee<-c(rep(0,21),Cases$new_death)
				ee<-ee[(delay+1):length(ee)]
				ee<-ee/ifr[1:length(ee)]
				if(smooth) ee<-c(ee,
					Cases$new_case[1:length(CR)+length(Cases$new_case)-
					length(CR)]/CR)
				if(percent) ee<-ee/population*100
				points(ee,pch=16,col=make.transparent("grey",0.8),cex=0.7)
			}
			mtext(paste("b)",state,"daily observed or estimated infections"),
				adj=0,line=1,cex=1.2)
			legend(x="topright",c("observed",
				"estimated","deaths"),pch=15,cex=0.9,
				col=c(cols[2],cols[1],cols[3]),
				pt.cex=1.5,bty="n",xpd=TRUE,
				xjust=0.5,yjust=1)
		}
	} else {
		estCases<-cumsum(estCases)
		newDeaths<-cumsum(newDeaths)
		obsCases<-cumsum(obsCases)
		if(percent){
			newDeaths<-newDeaths/population*100
			estCases<-estCases/population*100
			obsCases<-obsCases/population*100
		}
		if(plot){
			plot(NA,xlim=xlim,ylim=c(0,1.25*max(estCases)),bty="n",
				ylab="",
				xlab="",axes=FALSE)
			title(ylab=if(percent) "infections (observed or estimated) %" else
				"infections (observed or estimated)",line=4)
			Args<-list(...)
			Args$side<-2
			Args$labels<-FALSE
			h<-do.call(axis,Args)
			Args$at<-h
			if(percent)
				Args$labels<-paste(h,"%",sep="")
			else
				Args$labels<-if(max(estCases)>1000000) paste(h/1000000,"M",sep="") else
					if(max(estCases)>1000) paste(h/1000,"k",sep="") else h
			do.call(axis,Args)
			abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
			Args$side<-1
			Args$at<-ms
			Args$labels<-mm
			v<-do.call(axis,Args)
			polygon(c(1:T,T,1),
				c(estCases[1:T],0,0),
				border=FALSE,col=cols[1])
			lines(T:length(estCases),
				estCases[T:length(estCases)],
				lty="dotted",lwd=2,col=cols[1])
			polygon(c(1:length(obsCases),length(obsCases),1),
				c(obsCases,0,0),
				border=FALSE,col=cols[2])
			polygon(c(1:length(newDeaths),length(newDeaths),1),
				c(newDeaths,0,0),
				border=FALSE,col=cols[3])
			if(show.points){
				ee<-c(rep(0,21),Cases$tot_death)
				ee<-ee[(delay+1):length(ee)]
				ee<-ee/ifr[1:length(ee)]
				if(smooth) ee<-c(ee,
					ee[length(ee)]+
					cumsum(Cases$new_case[1:length(CR)+length(Cases$new_case)-
					length(CR)]/CR))
				if(percent) ee<-ee/population*100
				points(ee,pch=16,col=make.transparent("grey",alpha[2]),cex=0.7)
			}
			mtext(paste("b)",state,"cumulative observed or estimated infections"),
				adj=0,line=1,cex=1.2)
			legend(x="topright",c("observed",
				"estimated","deaths"),pch=15,cex=0.9,
				col=c(cols[2],cols[1],cols[3]),
				pt.cex=1.5,bty="n",xpd=TRUE,
				xjust=0.5,yjust=1)
		}
	}
	invisible(setNames(estCases,
		seq(from=as.Date("1/1/2020",format="%m/%d/%Y"),by=1,
		length.out=length(estCases))))
}

infections.by.state<-function(states=NULL,
	cumulative=FALSE,
	stacked=TRUE,
	data=list(),
	delay=20,
	ifr=0.005,
	window=7,
	smooth=TRUE,
	span=c(0.2,0.3),
	show.ifr=TRUE,
	bg="transparent",
	xlim=c(60,366+90),
	show.as.percent=FALSE,
	...){
	if(length(span)==1) span<-c(span,0.3)
	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31,31,28,31))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr")
	ttime<-max(ms)
	ifr<-make.ifr(ifr,ttime,smooth=smooth,span=span)
	if(is.null(states)) 
		states<-c("Alabama","Alaska","Arizona",
			"Arkansas","California","Colorado","Connecticut",
			"Delaware","District of Columbia","Florida",
			"Georgia","Hawaii","Idaho","Illinois",
			"Indiana","Iowa","Kansas","Kentucky","Louisiana",
			"Maine","Maryland","Massachusetts","Michigan","Minnesota",
			"Mississippi","Missouri","Montana","Nebraska","Nevada",
			"New Hampshire","New Jersey","New Mexico","New York (excluding NYC)",
			"New York City","North Carolina","North Dakota",
			"Ohio","Oklahoma","Oregon","Pennsylvania",
			"Rhode Island","South Carolina","South Dakota","Tennessee",
			"Texas","Utah","Vermont","Virginia",
			"Washington","West Virginia","Wisconsin","Wyoming")
	if(!is.null(data$Centers)) Centers<-data$Centers
	else Centers<-read.csv("Centers.csv")
	args<-list(data=data,
		cumulative=cumulative,
		delay=delay,
		ifr=ifr,
		window=window,
		smooth=TRUE,
		span=span,
		plot=FALSE)
	foo<-function(state,args){
		args$state<-state
		do.call(infection.estimator,args)
	}
	tmp<-lapply(states,foo,args)
	names(tmp)<-states
	nd<-max(sapply(tmp,length))
	Cases<-matrix(NA,nd,length(states)-1,dimnames=list(NULL,
		states[-34]))
	ii<-grep("New York",colnames(Cases))
	colnames(Cases)[ii]<-"New York"
	for(i in 1:ncol(Cases)){
		ss<-colnames(Cases)[i]
		if(ss=="New York"){
			Cases[1:length(tmp[["New York (excluding NYC)"]]),i]<-
				tmp[["New York (excluding NYC)"]]+tmp[["New York City"]]
		} else Cases[1:length(tmp[[ss]]),i]<-tmp[[ss]]
	}
	Cases[is.na(Cases)]<-0
	tots<-if(cumulative) apply(Cases,2,max) else colSums(Cases)
	ii<-order(tots)
	Cases<-Cases[,ii]
	Centers<-Centers[-which(Centers$name=="Alaska"),]
	Centers<-Centers[-which(Centers$name=="Hawaii"),]
	Centers<-Centers[-which(Centers$name=="Puerto Rico"),]
	colors<-setNames(rep(NA,length(Centers$name)),Centers$name)
	for(i in 1:length(colors)){
		fl<-which(Centers$name=="Florida")
		g<-(max(Centers$longitude)-Centers$longitude[i])/
			diff(range(Centers$longitude))
		r<-(max(Centers$latitude)-Centers$latitude[i])/
			diff(range(Centers$latitude))
		dist2fl<-function(ss,fl){
			sqrt((Centers$longitude[fl]-Centers$longitude[ss])^2+
				(Centers$latitude[fl]-Centers$latitude[ss])^2)
		}
		b<-dist2fl(i,fl)/max(sapply(1:length(Centers$name),dist2fl,
			fl=fl))
		colors[i]<-rgb(red=r,green=g,blue=b)
	}
	colors<-c(colors,setNames(rep("black",2),c("Alaska","Hawaii")))
	if(stacked){
		cumCases<-t(apply(Cases,1,cumsum))
		if(show.as.percent){
			rs<-rowSums(Cases)
			for(i in 1:nrow(cumCases)) 
				cumCases[i,]<-if(rs[i]==0) cumCases[i,] else cumCases[i,]/rs[i]*100
		}
		par(mar=c(5.1,5.1,2.1,3.1),bg=bg)
		yex<-if(show.as.percent) 1.2 else 1.05
		if(hasArg(ylim)) ylim<-list(...)$ylim
		else ylim<-c(0,yex*max(cumCases,na.rm=TRUE))
		plot(NA,xlim=xlim,ylim=ylim,
			bty="n",xlab="",
			ylab="",axes=FALSE)
		tt<-1:nrow(cumCases)
		for(i in 1:ncol(cumCases)){
			if(i>1){
				polygon(c(tt,tt[length(tt):1]),
					c(cumCases[,i-1],cumCases[nrow(cumCases):1,i]),
					border=FALSE,col=colors[colnames(cumCases)[i]])
			} else {
				polygon(c(tt,tt[length(tt)],1),
					c(cumCases[,i],0,0),
					border=FALSE,col=colors[colnames(cumCases)[i]])
			}
		}
		Args<-list(...)
		Args$side<-2
		Args$labels<-FALSE
		if(show.as.percent) Args$at<-c(0,25,50,75,100)
		h<-do.call(axis,Args)
		Args$at<-h
		if(show.as.percent){
			Args$labels<-paste(h,"%",sep="")
		} else {
			Args$labels<-if(cumulative) paste(h/1000000,"M",sep="") else
				paste(h/1000,"k",sep="")
		}
		do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		if(show.as.percent) 
			title(ylab=if(cumulative) "estimated cumulative infections (as % of all infections)" else
				"estimated new infections / day (as % of all infections)",line=4,
				cex.lab=if(is.null(Args$cex.lab)) 1 else Args$cex.lab)
		else 
			title(ylab=if(cumulative) "estimated cumulative infections" else
				"estimated new infections / day",line=4,
				cex.lab=if(is.null(Args$cex.lab)) 1 else Args$cex.lab)
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		old.usr<-par()$usr
		if(show.ifr){
			par(usr=c(par()$usr[1:2],-0.08,2.08))
			Args<-list(...)
			Args$side<-4
			h<-do.call(axis,Args)
			lines(1:length(ifr),ifr*100,col=palette()[4],lwd=2)
			legend("topleft","assumed IFR (%)",
				col=palette()[4],
				cex=0.9,lwd=2,
				box.col="transparent")
		}
		aspect<-par()$din[2]/par()$din[1]
		if(show.as.percent) par(usr=c(-125,52,55-110*aspect,55))
		else if(cumulative) par(usr=c(-135,42,55-110*aspect,55)) else
			par(usr=c(-135,42,55-110*aspect,55))
			## par(usr=c(-240,-63,55-110*aspect,55))
		for(i in 1:(length(colors)-2))
			map("state",regions=names(colors)[i],fill=TRUE,add=TRUE,
				col=colors[i],border="white")
	} else {
		plot(NA,xlim=xlim,ylim=c(0,1.05*max(Cases,na.rm=TRUE)),
			bty="n",xlab="",
			ylab="",axes=FALSE)
		nulo<-apply(Cases,2,lines)
	}
	par(usr=old.usr)
	invisible(Cases)
}

fixCases<-function(Cases){
	states<-setNames(c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
		"ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
		"MO","MT","NE","NV","NH","NJ","NM","NY","NYC","NC","ND","OH","OK",
		"OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
		"WI","WY"),c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
		"ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
		"MO","MT","NE","NV","NH","NJ","NM","NY","NYC","NC","ND","OH","OK",
		"OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
		"WI","WY"))
	Temp<-list(
		dates=lapply(states,
			function(x,Data) Data[Data$state==x,"submission_date"],
			Data=Cases),
		new_death=lapply(states,
			function(x,Data) Data[Data$state==x,"new_death"],
			Data=Cases),
		new_case=lapply(states,
			function(x,Data) Data[Data$state==x,"new_case"],
			Data=Cases),
		tot_death=lapply(states,
			function(x,Data) Data[Data$state==x,"tot_death"],
			Data=Cases),
		tot_cases=lapply(states,
			function(x,Data) Data[Data$state==x,"tot_cases"],
			Data=Cases))
	ll<-sapply(Temp$new_death,length)
	max.ll<-max(ll)
	if(any(ll!=max.ll)){
		ww<-which(ll!=max.ll)
		dd<-Temp$dates[setdiff(names(Temp$dates),names(ww))][[1]]
		for(i in 1:length(ww)){
			nd<-setNames(Temp$new_death[[ww[i]]],Temp$dates[[ww[i]]])
			Temp$new_death[[ww[i]]]<-setNames(rep(0,length(dd)),dd)
			Temp$new_death[[ww[i]]][names(nd)]<-nd
			names(Temp$new_death[[ww[i]]])<-NULL
			nc<-setNames(Temp$new_case[[ww[i]]],Temp$dates[[ww[i]]])
			Temp$new_case[[ww[i]]]<-setNames(rep(0,length(dd)),dd)
			Temp$new_case[[ww[i]]][names(nc)]<-nc
			names(Temp$new_case[[ww[i]]])<-NULL
			Temp$tot_death[[ww[i]]]<-cumsum(Temp$new_death[[ww[i]]])
			Temp$tot_cases[[ww[i]]]<-cumsum(Temp$new_case[[ww[i]]])
			Temp$dates[[ww[i]]]<-dd
		}
	} else dd<-Temp$dates[[1]]
	Cases<-data.frame(
		submission_date=rep(dd,length(states)),
		state=as.vector(sapply(states,function(x,n) rep(x,n),n=length(dd))),
		tot_cases=as.vector(sapply(Temp$tot_cases,function(x) x)),
		new_case=as.vector(sapply(Temp$new_case,function(x) x)),
		tot_death=as.vector(sapply(Temp$tot_death,function(x) x)),
		new_death=as.vector(sapply(Temp$new_death,function(x) x)))
	Cases
}

compare.infections<-function(states=
	c("Massachusetts","California",NULL),
	cumulative=FALSE,
	data=list(),
	delay=20,
	ifr=0.005,
	window=7,
	smooth=TRUE,
	span=c(0.2,0.3),
	plot=TRUE,
	bg="transparent",
	xlim=c(60,366+90),
	per.capita=TRUE,
	cols=NULL,
	...){
	states<-states[!is.null(states)]
	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31,31,28,31))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr")
	ttime<-max(ms)
	denom<-if(per.capita) " / 1M" else ""
	if(length(states)>0){
		set.seed(999)
		if(is.null(cols)){
			cols<-if(length(states)==1) "black" else 
				c("black",distinctColorPalette(length(states)-1))
		}
		## plot deaths
		dd<-list()
		for(i in 1:length(states)){
			if(states[i]=="New York"){ 
				dd[[i]]<-rowSums(
					sapply(c("New York City","New York (excluding NYC)"),
					infection.estimator,cumulative=cumulative,
					data=data,delay=0,ifr=1,window=window,smooth=smooth,
					span=span,percent=FALSE,plot=FALSE))
				if(per.capita){
					SS<-age.deaths(data=data,plot=FALSE,return="States")
					NY<-sum(SS[c("New York","New York City"),"2020"])
					dd[[i]]<-dd[[i]]/NY*100
				}
			} else dd[[i]]<-infection.estimator(states[i],cumulative=cumulative,
				data=data,delay=0,ifr=1,window=window,smooth=smooth,
				span=span,percent=per.capita,plot=FALSE)
		}
		if(per.capita) dd<-lapply(dd,function(x) x*10^4)
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,3.1),bg=bg)
		plot(NA,xlim=xlim,ylim=1.2*c(0,max(sapply(dd,max))),bty="n",
			ylab="",xlab="",axes=FALSE)
		if(cumulative) title(ylab=paste("estimated cumulative deaths",
			denom,sep=""),line=4)
		else title(ylab=paste("estimated daily deaths",denom,sep=""),line=4)
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
		Args$labels<-if(max(h)>1000000) paste(h/1000000,"M",sep="") else
			if(max(h)>1000) paste(h/1000,"k",sep="") else h
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		do.call(axis,Args)
		mapply(lines,dd,col=cols,MoreArgs=list(lty="dashed"))
		legend(x="topleft",states,lty="dashed",col=cols,
			bty="n",cex=0.9,xpd=TRUE,xjust=0.5,yjust=1)
		if(cumulative) 
			mtext(paste("a) cumulative deaths",denom,sep=""),adj=0,line=1,cex=1.2)
		else
			mtext(paste("a) daily deaths",denom,sep=""),adj=0,line=1,cex=1.2)
		## plot infections
		ii<-list()
		for(i in 1:length(states)){
			if(states[i]=="New York"){ 
				ii[[i]]<-rowSums(
					sapply(c("New York City","New York (excluding NYC)"),
					infection.estimator,cumulative=cumulative,
					data=data,delay=delay,ifr=ifr,window=window,smooth=smooth,
					span=span,percent=FALSE,plot=FALSE))
				if(per.capita){
					SS<-age.deaths(data=data,plot=FALSE,return="States")
					NY<-sum(SS[c("New York","New York City"),"2020"])
					ii[[i]]<-ii[[i]]/NY*100
				}
			} else ii[[i]]<-infection.estimator(states[i],cumulative=cumulative,
				data=data,delay=delay,ifr=ifr,window=window,smooth=smooth,
				span=span,percent=per.capita,plot=FALSE)
		}
		if(per.capita) ii<-lapply(ii,function(x) x*10^4)
		plot(NA,xlim=xlim,ylim=1.2*c(0,max(sapply(ii,max))),bty="n",
			ylab="",xlab="",axes=FALSE)
		if(cumulative) title(ylab=paste("estimated cumulative infections",
			denom,sep=""),line=4)
		else title(ylab=paste("estimated daily infections",denom,sep=""),line=4)
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
		Args$labels<-if(max(h)>1000000) paste(h/1000000,"M",sep="") else
			if(max(h)>1000) paste(h/1000,"k",sep="") else h
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		do.call(axis,Args)
		mapply(lines,ii,col=cols,MoreArgs=list(lty="dashed"))
		legend(x="topleft",states,lty="dashed",col=cols,
			bty="n",cex=0.9,xpd=TRUE,xjust=0.5,yjust=1)
		if(cumulative) 
			mtext(paste("b) estimated cumulative infections",denom,sep=""),adj=0,line=1,cex=1.2)
		else
			mtext(paste("b) estimated daily infections",denom,sep=""),adj=0,line=1,cex=1.2)
	}
}



