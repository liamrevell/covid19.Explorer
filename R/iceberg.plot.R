iceberg.plot<-function(
	state="United States",
	data=list(),
	delay=20,
	ifr=0.005,
	window=3,
	smooth=TRUE,
	span=c(0.2,0.3),
	bg="transparent",
	xlim=c(60,366+90),
	alpha=0.5,
	...){
	if(state!="New York"){
		Infections<-infection.estimator(
			state=state,
			cumulative=FALSE,
			data=data,
			delay=20,
			ifr=ifr,
			window=window,
			smooth=TRUE,
			span=span,
			plot=FALSE,...)
		Cases<-infection.estimator(
			state=state,
			data=data,
			window=window,
			getCases=TRUE,
			plot=FALSE)
	} else {
		Infections<-infection.estimator(
			state="New York City",
			cumulative=FALSE,
			data=data,
			delay=20,
			ifr=ifr,
			window=window,
			smooth=TRUE,
			span=span,
			plot=FALSE,...) +
			infection.estimator(
			state="New York (excluding NYC)",
			cumulative=FALSE,
			data=data,
			delay=20,
			ifr=ifr,
			window=window,
			smooth=TRUE,
			span=span,
			plot=FALSE,...)
		Cases<-infection.estimator(
			state="New York City",
			data=data,
			window=window,
			getCases=TRUE,
			plot=FALSE) +
			infection.estimator(
			state="New York (excluding NYC)",
			data=data,
			window=window,
			getCases=TRUE,
			plot=FALSE)
	}
	Infections<-Infections-Cases
	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31,31,28,31,30))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May")
	ttime<-max(ms)
	par(bg=bg)
	plot(NA,xlim=xlim,xlab="",ylab="",
		ylim=c(-max(Infections),max(c(0.33*max(Infections),max(Cases)))),
		axes=FALSE,bty="n")
	Args<-list(...)
	Args$side<-2
	Args$labels<-FALSE
	h<-do.call(axis,Args)
	Args$at<-h
	Args$labels<-relabel.axis(h,abs.val=TRUE)
	do.call(axis,Args)
	abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
	Args$side<-1
	Args$at<-ms
	Args$labels<-mm
	v<-do.call(axis,Args)
	T<-length(Infections)
	polygon(c(1:T,T,1),c(Cases,0,0),col=palette()[4],border=FALSE)
	polygon(c(1:T,T,1),c(-Infections,0,0),col=make.transparent(palette()[4],alpha),
		border=FALSE)
	lines(c(1,length(Infections)),rep(0,2),col=palette()[4])
	legend("topleft",c(paste("confirmed COVID-19 infections",state),
		"estimated unobserved infections"),pch=15,cex=0.9,
		col=c(palette()[4],make.transparent(palette()[4],0.5)),
		pt.cex=1.5,bty="n",xpd=TRUE,xjust=0.5,yjust=1)
}

