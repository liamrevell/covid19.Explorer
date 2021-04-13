\name{infection.range.estimator}
\alias{infection.range.estimator}
\title{Compare estimated infections under a range of IFR models}
\usage{
	infection.range.estimator(state="Massachusetts",
		cumulative=FALSE,
		data=list(),
		delay=20,
		ifr.low=0.004,
		ifr.high=0.006,
		window=7,
		smooth=TRUE,
		span=c(0.12,0.3),
		percent=FALSE,
		plot=TRUE,
		bg="transparent",
		xlim=c(60,366+105),
		alpha=0.25,
		...)
}
\arguments{
	\item{state}{state or jurisdiction.}
	\item{cumulative}{show cumulative infections (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{delay}{average lag from infection to death.}
	\item{ifr.low}{low value(s) of infection-fatality-ratio/rate or function. If more than one value is specified, IFR is interpolated using LOESS smoothing.}
	\item{ifr.high}{high value(s) of infection-fatality-ratio/rate or function.}
	\item{window}{window for moving average.}
	\item{smooth}{smooth estimated infections (logical).}
	\item{span}{values of \code{span} to be passed to \code{\link{loess}}. First value is for infections through time. Second value is for IFR if applicable.}
	\item{percent}{show values as percent of population (logical).}
	\item{plot}{create a plot (logical).}
	\item{bg}{background color for plots.}
	\item{xlim}{\emph{x}-limits for the plot.}
	\item{alpha}{alpha transparency level for transparent elements of the plot.}
	\item{...}{optional arguments.}
}
\description{
	Function to show estimated COVID-19 infections under a range of different IFR values or models.
}
\value{
	A plot.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
