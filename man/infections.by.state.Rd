\name{infections.by.state}
\alias{infections.by.state}
\title{Show distribution of estimated COVID-19 infections among states}
\usage{
	infections.by.state(states=NULL,
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
		...)
}
\arguments{
	\item{states}{states or jurisdictions to plot.}
	\item{cumulative}{show cumulative infections (logical).}
	\item{stacked}{show infections stacked (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{delay}{average lag from infection to death.}
	\item{ifr}{infection-fatality-ratio/rate or function. If more than one value is specified, IFR is interpolated using LOESS smoothing.}
	\item{window}{window for moving average.}
	\item{smooth}{smooth estimated infections (logical).}
	\item{span}{values of \code{span} to be passed to \code{\link{loess}}. First value is for infections through time. Second value is for IFR if applicable.}
	\item{show.ifr}{show modeled IFR (logical).}
	\item{bg}{background color for plots.}
	\item{xlim}{\emph{x}-limits for the plot.}
	\item{show.as.percent}{show as a percent of all infections (logical).}
	\item{...}{optional arguments.}
}
\description{
	Function to graph the distribution of estimated COVID-19 infections among states through time.
}
\value{
	A plot.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
