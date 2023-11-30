#lab 5

library(tidyverse)
library(sf)
library(units)

setwd("/Users/verosovero/Dropbox/Teaching/Data analytics/Econ 106 Fall 2023/Labs/California_School_District_Areas_2021-22")

School_Districts<-st_read("DistrictAreas2122.shp")

#read in college outcomes by district
District_College_Outcomes<-read_csv("District College Outcomes.csv")

District_College_Outcomes<-District_College_Outcomes%>%
  mutate(CDCode=as.character(CDCode))

School_Districts_join<-left_join(x=School_Districts, y=District_College_Outcomes)

#use albers projections
School_Districts_Albers<-st_transform(School_Districts_join, crs=3310)

School_Districts_rates<-School_Districts_Albers%>%
  mutate(CSU_rate=`Enrolled CSU (12 Months)`/`High School Completers`)


#load excel file of GPS coordinates of college campuses into R

college_data <- read_csv("college locations.csv")

#select CSU's from this dataset
CSU_data <-college_data %>%
  filter(F1SYSNAM == "California State University")

# Convert CSU_data to a sf object
CSU_sf <- st_as_sf(CSU_data, coords = c("LONGITUD", "LATITUDE"), crs = 4269)


#select UC's from this dataset
UC_data <-college_data %>%
  filter(F1SYSNAM == "University of California")%>%
  filter(IALIAS!="UCOP")

# Convert CSU_data to a sf object
UC_sf <- st_as_sf(UC_data, coords = c("LONGITUD", "LATITUDE"), crs = 4269)

CSUSB<-CSU_sf%>%
  filter(IALIAS=="Cal State San Bernardino")


#project to CA Albers

CSUSB_Albers<-st_transform(CSUSB, crs=3310)

UC_Albers<-st_transform(UC_sf, crs=3310)

CSU_Albers<-st_transform(CSU_sf, crs=3310)


#college going & CSU locations
ggplot()+
  geom_sf(data=School_Districts_Albers, aes(fill=`College Going Rate - Total (12 Months)`))+
  geom_sf(data=CSU_Albers, color='red')


#csu going & CSU locations
ggplot()+
  geom_sf(data=School_Districts_rates, aes(fill=`CSU_rate`))+
  geom_sf(data=CSU_Albers, color='red')


Riverside_Districts<-School_Districts_rates%>%
  filter(CountyName=="Riverside")


#what is the largest school district in Riverside by Area (square miles)?

Riverside_Districts%>%
  mutate(area=set_units(st_area(Riverside_Districts), mi^2))%>%
arrange(desc(area))%>%
  select(DistrictNa, area)%>%
  slice_head(n=5)

#what is the largest school district in Riverside by # Total Enrollment?
Riverside_Districts%>%
  arrange(desc(EnrollTota))%>%
  select(DistrictNa, EnrollTota)%>%
  slice_head(n=5)

#what is the school district with the highest CSU college going rates?
Riverside_Districts%>%
  arrange(desc(CSU_rate))%>%
  select(DistrictNa, CSU_rate)%>%
  slice_head(n=5)

#how far is this school district from CSUSB?

Jurupa_Unified<-Riverside_Districts%>%
  filter(DistrictNa=="Jurupa Unified")

set_units(st_distance(Jurupa_Unified, CSUSB_Albers), mi)





