\name{age.deaths}
\alias{age.deaths}
\title{Graph 2020 excess deaths by age group and jurisdiction}
\usage{
	age.deaths(state="Massachusetts",
		age.group=c("Under 25 years","25-44 years","45-64 years",
		"65-74 years","75-84 years","85 years and older"),
		plot=c("raw & excess","raw & percent above normal"),
		cumulative=TRUE,
		corrected=FALSE,
		data=list(),
		date.range=list(),
		return=NULL,
		year=2020,
		bg="transparent",
		...)
}
\arguments{
	\item{state}{state or jurisdiction.}
	\item{age.group}{age group or groups.}
	\item{plot}{what to plot.}
	\item{cumulative}{show cumulative deaths or excess deaths (logical).}
	\item{corrected}{correct past year deaths to 2020 population (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{date.range}{date range for the plot.}
	\item{return}{return specific values to the user.}
	\item{year}{year to plot.}
	\item{bg}{background color.}
	\item{...}{optional arguments.}
}
\description{
	Function to compute excess deaths by age group and jurisdiction during the 2020 COVID-19 pandemic.
}
\value{
	A plot. Also can optionally return various different things to the user if \code{return} is specified.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
