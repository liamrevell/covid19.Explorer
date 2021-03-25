\name{covid.deaths}
\alias{covid.deaths}
\title{Graph 2020 excess deaths by age group and jurisdiction}
\usage{
	covid.deaths(
		age.group=c(
		"Under 1 year",
		"1-4 years",
		"5-14 years",
		"15-24 years",
		"25-34 years",
		"35-44 years",
		"45-54 years",
		"55-64 years",
		"65-74 years",
		"75-84 years",
		"85 years and over"),
		sex=c("Female","Male"),
		all.causes=TRUE,
		cumulative=FALSE,
		data=list(),
		xlim=c(32,366+75),
		bg="transparent",
		plot=c("standard","smooth","bar"),
		show=c("raw","per.capita","percent","percent.of.covid.deaths"),
		split.groups=TRUE,
		show.total.deaths=TRUE,
		...)
}
\arguments{
	\item{age.group}{age group or groups.}
	\item{sex}{sex or sexes.}
	\item{all.causes}{ignored.}
	\item{cumulative}{show cumulative deaths (logical).}
	\item{data}{list containing CDC data. If not supplied, function will create this object.}
	\item{xlim}{\emph{x} limits for the plot.}
	\item{bg}{background color of the plot.}
	\item{plot}{plotting options.}
	\item{show}{what to plot.}
	\item{split.groups}{split groups (logical).}
	\item{show.total.deaths}{show total deaths (logica).}
	\item{...}{optional arguments.}
}
\description{
	Function to show COVID and non-COVID deaths in 2020.
}
\value{
	A plot.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
