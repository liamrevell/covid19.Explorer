\name{excess.vs.covid}
\alias{excess.vs.covid}
\title{Graph COVID-19 and non COVID-19 deaths by age group}
\usage{
	excess.vs.covid(
	age.group=c(
	"Under 25 years",
	"25-44 years",
	"45-64 years",
	"65-74 years",
	"75-84 years",
	"85 years and older"),
	cumulative=FALSE,
	data=list(),
	xlim=c(60,366+365+120),
	bg="transparent",
	lwd=2,
	...)
}
\arguments{
	\item{age.group}{age group or groups.}
	\item{cumulative}{show cumulative deaths (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{xlim}{\emph{x} limits for the plot.}
	\item{bg}{background color of the plot.}
	\item{lwd}{line width.}
	\item{...}{optional arguments.}
}
\description{
	Function to show COVID and excess deaths for 2020/21.
}
\value{
	A plot.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
