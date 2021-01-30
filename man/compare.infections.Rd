\name{compare.infections}
\alias{compare.infections}
\title{Compare estimate COVID-19 infections & observed deaths between states}
\usage{
	compare.infections(
		states=c("Massachusetts","California",NULL),
		cumulative=FALSE,
		data=list(),
		delay=20,
		ifr=0.005,
		window=7,
		smooth=TRUE,
		span=c(0.2,0.3),
		plot=TRUE,
		bg="transparent",
		xlim=c(60,366+31),
		per.capita=TRUE,
		...)
}
\arguments{
	\item{states}{states or jurisdictions to compare.}
	\item{cumulative}{show cumulative infections (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{delay}{average lag from infection to death.}
	\item{ifr}{infection-fatality-ratio/rate or function. If more than one value is specified, IFR is interpolated using LOESS smoothing.}
	\item{window}{window for moving average.}
	\item{smooth}{smooth estimated infections (logical).}
	\item{span}{values of \code{span} to be passed to \code{\link{loess}}. First value is for infections through time. Second value is for IFR if applicable.}
	\item{plot}{create a plot (logical).}
	\item{bg}{background color for plots.}
	\item{xlim}{\emph{x}-limits for the plot.}
	\item{per.capita}{show values as per 1M population (logical).}
	\item{...}{optional arguments.}
}
\description{
	Function to compared estimated COVID-19 infections and observed deaths through time between states.
}
\value{
	A plot.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
