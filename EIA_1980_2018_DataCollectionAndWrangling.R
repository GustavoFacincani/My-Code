#Loading packages
library(tidyverse, quietly = TRUE)
library(readr, quietly = TRUE)
library(lubridate)
library(stringr, quietly = TRUE)
library(readxl)
library(readr)
library(plyr)
library(dplyr)
library(base)
library(utils)
library(reshape2)
library(reshape)

#The montlhy data from 1980-2000 was retrieved from https://www.eia.gov/electricity/data/eia923/eia906u.php
#Each year was in a separate XLS file
#The montlhy data from 2001-2018 was retrieved from https://www.eia.gov/electricity/data/eia923/
#Each year was in a separate ZIP file, within which there are 1, 2 or 3 excel files inside, with names that vary
#So I had to go over each one manually, then saved the ones to be used
#The tables were in 4 different formats, so I had to read it 4 times

#Reading data from 1980 to 2000
Hydro80_00 <- list.files("C:/Users/gusta/Desktop/PhD/CERCWET/ElectricityData/1980_2000", ".xls", full.names = TRUE)
Hydro80_00 

my_data <- ldply(Hydro80_00, read_excel) %>% #reading them all at once
  filter(CENSUS == "93", #filtering by the state of CA
         FUELDESC == "WAT", #filtering hydroelectric power plants
         PMDESC == "HY") %>% #filtering only conventional hydropower facilities 
  select(PCODE, PLTNAME, UTILNAME, YEAR, #selecting the columns of interest - Power Plant Code and Name, Utility name and Year
         GEN01, GEN02, GEN03, GEN04, GEN05, GEN06, GEN07, GEN08, GEN09, GEN10, GEN11, GEN12)  #electricity generation per month (in MWh)

#These files had an extra 0 in from the Plant Code, and some Years were 2-digit while others were 4-digit years
#So I fixed that this way:
my_data$PCODE <- gsub("(^|[^0-9])0+", "\\1", my_data$PCODE, perl = TRUE) #removing the leading zero in the Plant ID
my_data$YEAR <- my_data$YEAR <- year(as.Date(format(parse_date_time(my_data$YEAR, 'y'), "%Y"), "%Y")) #making years with 4-digits (most are not)

my_data



#Reading data from 2001 to 2010
#Selecting all the files
Hydro01_10 <- list.files("C:/Users/gusta/Desktop/PhD/CERCWET/ElectricityData/2001_2010", ".xls", full.names = TRUE)
Hydro01_10 

#Reading them all at once
my_data1 <- ldply(Hydro01_10, read_excel, skip = 7) %>% #skipping first 7 empty rows
  filter(State == "CA", #filtering by the state of CA
         `Reported Fuel Type Code` == "WAT", #filtering hydroelectric power plants
         `AER Fuel Type Code` == "HYC") %>% #filtering only conventional hydropower facilities 
  select(`Plant ID`, `Plant Name`,`Operator Name`, Year,#selecting the columns of interest - Power Plant Code and Name, Utility name and Year
         NETGEN_JAN, NETGEN_FEB, NETGEN_MAR, NETGEN_APR, NETGEN_MAY, NETGEN_JUN, NETGEN_JUL, NETGEN_AUG, NETGEN_SEP, NETGEN_OCT, NETGEN_NOV, NETGEN_DEC) #electricity generation per month (in MWh)

colnames(my_data1) <- colnames(my_data) #naming the columns after my_data to bind them later
my_data1



#Reading the data from 2011 to 2018
Hydro11_18 <- list.files("C:/Users/gusta/Desktop/PhD/CERCWET/ElectricityData/2011_2018", ".xls", full.names = TRUE)
Hydro11_18 


#2011 and 2013 had column names in a different format, so I need to deal with them separately
my_data2 <- ldply(Hydro11_18, read_excel, skip = 5) %>% #skipping first 5 empty rows
  filter(State == "CA", #filtering by the state of CA
         `Reported Fuel Type Code` == "WAT", `AER Fuel Type Code` == "HYC") %>% #filtering only conventional hydropower facilities 
  select(`Plant Id`, `Plant Name`,`Operator Name`, YEAR, #selecting the columns of interest - Power Plant Code and Name, Utility name and Year
         Netgen_Jan, Netgen_Feb, Netgen_Mar, Netgen_Apr, Netgen_May, Netgen_Jun, Netgen_Jul, Netgen_Aug, Netgen_Sep, Netgen_Oct, Netgen_Nov, Netgen_Dec) #electricity generation per month (in MWh)

colnames(my_data2) <- colnames(my_data) #naming the columns after my_data to bind them later

my_data2


#2012, 2014-2019 were in another format
my_data3 <- ldply(Hydro11_18, read_excel, skip = 5) %>% #skipping the first 5 empty rows
  filter(`Plant State` == "CA", #filtering by the state of CA
         `Reported\r\nFuel Type Code` == "WAT", #filtering all hydropower
         `AER\r\nFuel Type Code` == "HYC") %>% #filtering only conventional hydropower facilities 
  select(`Plant Id`, `Plant Name`,`Operator Name`, YEAR, #selecting the columns of interest - Power Plant Code and Name, Utility name and Year
         `Netgen\r\nJanuary`, `Netgen\r\nFebruary`, `Netgen\r\nMarch`, `Netgen\r\nApril`, `Netgen\r\nMay`, `Netgen\r\nJune`, `Netgen\r\nJuly`, `Netgen\r\nAugust`, `Netgen\r\nSeptember`, `Netgen\r\nOctober`, `Netgen\r\nNovember`, `Netgen\r\nDecember`) #selecting the columns of interest  #order the data by year

colnames(my_data3) <- colnames(my_data) #naming the columns after my_data to bind them later
my_data3



#Reading the data, we have to check which stations were considered, based on the ones we previously had

previous_data <- read_excel("C:/Users/gusta/Desktop/PhD/CERCWET/ElectricityData/monthly_hydro_1982_2003.xls", skip = 4, sheet = 2) #skipping the first 4 empty rows, in sheet 2
previous_data

all_data <- rbind(my_data, my_data1, my_data2, my_data3) %>% #joining all the data into one dataset
  subset(PCODE %in% previous_data$`EIA PlantID` | `PCODE` == c("50393", "54554")) %>% #filtering the current data based on the stations of previous_data, and the Friant PH (50393) and New Spicer PH (54554) were not considered in the previous_data
  ungroup() %>%
  arrange(YEAR) #order data by year

#rename columns for ease of understanding
names(all_data) <- c("PlantID", "PlantName", "OperatorName","Year", 
                     "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
all_data


#Reorganizing the data, setting months in one column and electricity generation in another column
all_data <- melt(all_data, id = c("PlantID", "PlantName", "OperatorName", "Year")) %>%
  rename(c(variable = "Month", 
           value = "NetGeneration_MWh"))
all_data


#Each facility had 1, 2 or 3 names in different formats (lower case, upper case, complete, abbreviated, etc)
#So I had to go over each one mannualy and remove duplication, to do that, I did this:

#Selecting only ID and Plant names to do a dataframe
DF.1<-data.frame(unique(all_data[,1:2]))

#Producing a separate .csv file from that to exclude manually the duplication of names, occured in all files
write_csv(DF.1, "Plant_IDs.csv", append=FALSE, col_names=TRUE)

#After I removed the duplications/triplications, I read the file I created 
DF.2<-read.csv("C:/Users/gusta/Desktop/PhD/CERCWET/ElectricityData/Plant_IDs.csv", header=TRUE, sep=",") #O index.txt é o arquivo novo com os nomes editados

#Homogenizing the names of the power plants into the one format I chose
all_data$PlantName <-DF.2[match(all_data$PlantID, DF.2[,1]), 2]

all_data <-  all_data[,-c(1,3),drop=FALSE]  #removing PlantID and OperatorName columns
all_data
DF.2

#Forming table with columns for each power house
#NAs mean the power house was not considered in that year, meanwhile 0 mean no generation
all_data2 <- cast(all_data, Year+Month~PlantName) 
all_data2

write_csv(all_data2, "monthly_hydro_1980_2018.csv", append=FALSE, col_names=TRUE) #Creating a csv file with all data