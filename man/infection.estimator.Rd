\name{infection.estimator}
\alias{infection.estimator}
\title{Estimate true number of COVID-19 infections based on deaths & observed cases}
\usage{
	infection.estimator(state="Massachusetts",
		cumulative=FALSE,
		data=list(),
		delay=20,
		ifr=0.005,
		window=7,
		smooth=TRUE,
		span=c(0.12,0.3),
		percent=FALSE,
		plot=TRUE,
		bg="transparent",
		xlim=c(60,366+105),
		show.points=FALSE,
		alpha=c(0.25,0.8),
		cdr=c("sigmoid","average"),
		...)
}
\arguments{
	\item{state}{state or jurisdiction.}
	\item{cumulative}{show cumulative infections (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{delay}{average lag from infection to death.}
	\item{ifr}{infection-fatality-ratio/rate or function. If more than one value is specified, IFR is interpolated using LOESS smoothing.}
	\item{span}{values of \code{span} to be passed to \code{\link{loess}}. First value is for infections through time. Second value is for IFR if applicable.}
	\item{window}{window for moving average.}
	\item{smooth}{smooth estimated infections (logical).}
	\item{percent}{show values as percent of population (logical).}
	\item{plot}{create a plot (logical).}
	\item{bg}{background color for plots.}
	\item{xlim}{\emph{x}-limits for the plot.}
	\item{show.points}{show points used in LOESS smoothing (logical).}
	\item{alpha}{alpha transparency levels for transparent elements of the plot.}
	\item{cdr}{model for case detection rate (CDR) through time.}
	\item{...}{optional arguments.}
}
\description{
	Function to estimate the true number of COVID-19 infections through time using confirmed COVID-19 deaths, an IFR value or function specifid by the user, and observed cases.
}
\value{
	A plot. Also can optionally return various different things to the user if \code{plot=FALSE}.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
