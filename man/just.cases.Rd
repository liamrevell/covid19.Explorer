\name{just.cases}
\alias{just.cases}
\title{Show confirmed SARS-CoV-2 infections and COVID-19 deaths through time}
\usage{
just.cases(states="Massachusetts",
	cumulative=FALSE,
	data=list(),
	window=7,
	bg="transparent",
	xlim=c(60,366+365+31),
	per.capita=FALSE,
	cols=NULL,
	show=c("both","cases","deaths"),
	...)
}
\arguments{
	\item{states}{states or jurisdictions to plot.}
	\item{cumulative}{show cumulative infections (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{window}{window for moving average.}
	\item{bg}{background color for plots.}
	\item{xlim}{\emph{x}-limits for the plot.}
	\item{per.capita}{show per 1M population (logical).}
	\item{cols}{colors for plotting.}
	\item{show}{what to show in plot.}
	\item{...}{optional arguments.}
}
\description{
	Function to graph confirmed SARS-CoV-2 infections and COVID-19 deaths through time.
}
\value{
	A plot.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
