updateData<-function(Data,what="all"){

	if(length(intersect(what,c("all","Provis")))>0){
		cat("\nUpdating 2019 Weekly Counts of Deaths by State and Select Causes from:")
		cat("\n  https://liamrevell.github.io/data/\n")
		flush.console()
		Provis<-try(read.csv(
			"https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv"))
		cat("\nUpdating 2020-21 Weekly Counts of Deaths by State and Select Causes from:")
		cat("\n  https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6\n")
		flush.console()
		Provis2020<-try(read.csv(
			"https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD"))
		if(!inherits(Provis,"try-error")){
			if(!inherits(Provis2020,"try-error")){
				ii<-which(Provis$MMWR.Year==2020)
				Provis<-Provis[-ii,]
				ii<-which(Provis2020$MMWR.Year==2021)
				Provis<-rbind(Provis,Provis2020[-ii,colnames(Provis)])
				Data$Provis<-Provis
			}
		}
	}
	if(length(intersect(what,c("all","age.Counts")))>0){
		cat("\nUpdating Weekly counts of deaths by jurisdiction and age group from:")
		cat("\n  https://data.cdc.gov/NCHS/Weekly-counts-of-deaths-by-jurisdiction-and-age-gr/y5bj-9g5w\n")
		age.Counts<-try(read.csv("https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD"))
		flush.console()
		if(!inherits(age.Counts,"try-error")) Data$age.Counts<-age.Counts
	}
	if(length(intersect(what,c("all","Cases")))>0){
		cat("\nUpdating United States COVID-19 Cases and Deaths by State over Time from:")
		cat("\n  https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36\n")
		flush.console()
		Cases<-try(read.csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD"))
		if(!inherits(Cases,"try-error")){
			dd<-as.Date(Cases$submission_date,format="%m/%d/%Y")
			Cases<-Cases[order(dd),]
			Data$Cases<-Cases
		}
	}
	if(length(intersect(what,c("all","CovidDeaths")))>0){
		cat("\nUpdating Provisional COVID-19 Death Counts by Sex, Age, and Week from:")
		cat("\n  https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-W/vsak-wrfu\n")
		flush.console()
		CovidDeaths<-try(read.csv("https://data.cdc.gov/api/views/vsak-wrfu/rows.csv?accessType=DOWNLOAD"))
		if(!inherits(CovidDeaths,"try-error")) Data$CovidDeaths<-CovidDeaths
	}
	cat("\n")
	
	Data$Cases[is.na(Data$Cases[,"new_death"]),"new_death"]<-0
	Data$Cases[is.na(Data$Cases[,"new_case"]),"new_case"]<-0
	Data$Cases[is.na(Data$Cases[,"tot_death"]),"tot_death"]<-0
	Data$Cases[is.na(Data$Cases[,"tot_cases"]),"tot_cases"]<-0
	
	Data$date<-max(as.Date(Data$Cases$submission_date,format="%m/%d/%Y"))

	Data
}
