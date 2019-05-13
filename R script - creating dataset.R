library("tidyr")
library("dplyr")

#IMPORT DATA

#Import co2 data
CO2 <- read.csv("~/Msc Data Sience/SM3/Thesis/Thesis R/input/CO2.csv", sep = ";", stringsAsFactors=FALSE)
CO2_new <- reshape(CO2, direction="long", sep='', varying=paste0('X', 1990:2014))
CO2_new$id <- NULL
names(CO2_new) <- c("country", "year", "CO2")

#Import ch4 data
CH4 <- read.csv("~/Msc Data Sience/SM3/Thesis/Thesis R/input/CH4.csv", sep = ";", stringsAsFactors=FALSE)
CH4_new <- reshape(CH4, direction="long", sep='', varying=paste0('X', 1990:2012))
CH4_new$id <- NULL
names(CH4_new) <- c("country", "year", "CH4")

#import n2o data
N2O <- read.csv("~/Msc Data Sience/SM3/Thesis/Thesis R/input/N2O.csv", sep = ";", stringsAsFactors=FALSE)
N2O_new <- reshape(N2O, direction="long", sep='', varying=paste0('X', 1990:2012))
N2O_new$id <- NULL
names(N2O_new) <- c("country", "year", "N2O")

#import ghg data
GHG <- read.csv("~/Msc Data Sience/SM3/Thesis/Thesis R/input/GHG.csv", sep = ";", stringsAsFactors=FALSE)
GHG_new <- reshape(GHG, direction="long", sep='', varying=paste0('X', 1990:2012))
GHG_new$id <- NULL
names(GHG_new) <- c("country", "year", "GHG")

#Merging data into 1 dataset
total_data <- CO2_new %>% 
  left_join(CH4_new, by=c("country","year"))%>%
  left_join(N2O_new, by=c("country", "year"))%>%
  left_join(GHG_new, by=c("country", "year"))

#KYOTO DATASET

#Importing data
Kyoto <- read.csv("~/Msc Data Sience/SM3/Thesis/Thesis R/input/Kyoto Entry.csv", sep = ";", stringsAsFactors=FALSE)

#Renmaing columns
library(dplyr)
Kyoto <- Kyoto %>% 
  rename(
    entry_date = Ratification..Acceptance.A...Accession.a...Approval.AA.,
    country = Participant
  )

#Clean data, rename s.t. compatible with other dataset
Kyoto$entry_date[31] = "17 Dec 2002 a"
Kyoto$country[184] = "United Kingdom"
Kyoto$country[11] = "Bahamas, The"
Kyoto$country[41] = "Cote d'Ivoire"
Kyoto$country[156] = "Slovak Republic"
Kyoto$country[53] = "Egypt, Arab Rep."
Kyoto$country[65] = "Gambia, The"
Kyoto$country[129] = "Macedonia, FYR"
Kyoto$country[185] = "Tanzania"
Kyoto$country[190] = "Venezuela, RB"
Kyoto$country[81] = "Iran, Islamic Rep."
Kyoto$country[46] = "Korea, Dem. People’s Rep."
Kyoto$country[142] = "Korea, Rep."
Kyoto$country[191] = "Vietnam"
Kyoto$country[93] = "Kyrgyz Republic"
Kyoto$country[192] = "Yemen, Rep."
Kyoto$country[143] = "Moldova"
Kyoto$country[113] = "Micronesia, Fed. Sts."
Kyoto$country[20] = "Bolivia"
Kyoto$country[38] = "Congo, Rep."
Kyoto$country[47] = "Congo, Dem. Rep."

#remove numbers and white spaces from country names
Kyoto$country <- gsub("[[:digit:]]", "", Kyoto$country)
Kyoto$country <- gsub("(^\\s+)|(\\s+$)", "", Kyoto$country)

#Change date to year
library(stringr)
Kyoto$entry_date <- gsub("\\s*\\w*$", "", Kyoto$entry_date)
Kyoto$year_entered <- strtoi(str_sub(Kyoto$entry_date, -4, -1))

#MERGING DATASETS
final_data <- total_data %>% 
  right_join(Kyoto, by=c("country"))

#add variable that indicates entry
final_data$member <- ifelse(final_data$year >= final_data$year_entered, 1, 0)
final_data$member <- factor(final_data$member)
