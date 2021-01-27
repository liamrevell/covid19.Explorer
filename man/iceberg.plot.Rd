\name{iceberg.plot}
\alias{iceberg.plot}
\title{Graph the observed and estimated COVID-19 infections using an iceberg style graph}
\usage{
	iceberg.plot(
		state="United States",
		data=list(),
		delay=20,
		ifr=0.005,
		window=3,
		smooth=TRUE,
		span=c(0.2,0.3),
		bg="transparent",
		xlim=c(45,366+15),
		...)
}
\arguments{
	\item{state}{state or jurisdiction.}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{delay}{average lag from infection to death.}
	\item{ifr}{infection-fatality-ratio/rate or function. If more than one value is specified, IFR is interpolated using LOESS smoothing.}
	\item{span}{values of \code{span} to be passed to \code{\link{loess}}. First value is for infections through time. Second value is for IFR if applicable.}
	\item{window}{window for moving average.}
	\item{smooth}{smooth estimated infections (logical).}
	\item{bg}{background color for plots.}
	\item{xlim}{\emph{x}-limits for the plot.}
	\item{...}{optional arguments.}
}
\description{
	Function to graph observed (above the 'waterline') and unobserved estimated (below it) COVID-19 cases in the United States and its jurisdictions.
}
\value{
	A plot. Also can optionally return various different things to the user if \code{plot=FALSE}.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
