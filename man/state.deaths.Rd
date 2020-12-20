\name{state.deaths}
\alias{state.deaths}
\title{Graph 2020 excess deaths by state or jurisdiction}
\usage{
	state.deaths(state="Massachusetts",
		plot=c("raw","per capita","excess","excess per capita",
		"percent above normal"),
		corrected=FALSE,
		data=list(),
		date.range=list(),
		...)
}
\arguments{
	\item{state}{state or jurisdiction.}
	\item{plot}{what to plot.}
	\item{corrected}{correct past year deaths to 2020 population (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{date.range}{date range for the plot.}
	\item{...}{optional arguments.}
}
\description{
	Function to compute excess deaths by age group and jurisdiction during the 2020 COVID-19 pandemic.
}
\value{
	A plot.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
