#DM_Navdeep_Project

#load required Libraries
library(sp)
library(ggplot2)
library(maptools)
library(RColorBrewer)
library(rJava)
library(knitr)
library(shiny)
suppressPackageStartupMessages(library(googleVis))

# load level 1 india data downloaded from http://gadm.org/country
#india <- readRDS("IND_adm1.rds")
#india <- india[-(32:33),]
#summary(india)

#Read dataset
ind_df <- read.csv("dis_work.csv")

#Remove Irrelevant rows
mydata <- ind_df[7:4866,]

#Give Names to colums
mydata <- setNames( mydata,c("Table_n","State_c","Dist_c","States","u_r","Dis",
                      "Age_gp","Total_p","Total_m","Total_f","Main_p",
                      "Main_m","Main_f","Marg3_p","Marg3_m","Marg3_f",
                      "Marg3_6_p","Marg3_6_m","Marg3_6_f","NonW_p","NonW_m","NonW_f"))

#Drop irrelevant irrelevant and redundant Colums
mydata <- mydata [-c(1,3)]
#head(mydata, n=10)

mydata_InSeeing_Total <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Seeing' & mydata$u_r!= 'Rural'
                   & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InSeeing_Total <- mydata_InSeeing_Total[order(mydata_InSeeing_Total$States),]
#Convert charecter data to numeric
mydata_InSeeing_Total$Total_p <- as.numeric(as.character(mydata_InSeeing_Total$Total_p))
mydata_InSeeing_Total$Total_m <- as.numeric(as.character(mydata_InSeeing_Total$Total_m))
mydata_InSeeing_Total$Total_f <- as.numeric(as.character(mydata_InSeeing_Total$Total_f))
#calculate number per 100 of total population

mydata_InSeeing_Total$T_male <- (sprintf("%.f", mydata_InSeeing_Total$Total_m * 100/mydata_InSeeing_Total$Total_p))
mydata_InSeeing_Total$T_female <- sprintf("%.f", mydata_InSeeing_Total$Total_f * 100/mydata_InSeeing_Total$Total_p)

#Draw the Map of india indicating In_seeing disabled Male Workers per 100 Disabled Parsons in each state
map_1 <- gvisGeoChart(data = mydata_InSeeing_Total, locationvar = "States", colorvar = "T_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                   width = 600, height = 400),chartid = "InSeeing_Male")


#Draw the Map of india indicating In-Seeing Disabled Female Workers per 100 Disabled Parsons in each state
map_2 <- gvisGeoChart(data = mydata_InSeeing_Total, locationvar = "States", colorvar = "T_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSeeing_Female")
plot(gvisMerge(map_1,map_2, horizontal = 'TRUE', chartid ="In-Seeing" ))

mydata_InSeeing_Total$T_male <- as.numeric(as.character(mydata_InSeeing_Total$T_male))
mydata_InSeeing_Total$T_female <- as.numeric(as.character(mydata_InSeeing_Total$T_female))
col1 <- gvisColumnChart(mydata_InSeeing_Total, xvar = "States", yvar = c("T_male", "T_female"))
#pie <- gvisPieChart(mydata_InSeeing_Total, labelvar = "States", numvar = "T_male")
#plot(pie)

# Drawing map for In-Hearing Disability

mydata_InHearing_Total <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Hearing' & mydata$u_r!= 'Rural'
                                 & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InHearing_Total <- mydata_InHearing_Total[order(mydata_InHearing_Total$States),]
#Convert charecter data to numeric
mydata_InHearing_Total$Total_p <- as.numeric(as.character(mydata_InHearing_Total$Total_p))
mydata_InHearing_Total$Total_m <- as.numeric(as.character(mydata_InHearing_Total$Total_m))
mydata_InHearing_Total$Total_f <- as.numeric(as.character(mydata_InHearing_Total$Total_f))
#calculate number per 100 of total population

mydata_InHearing_Total$T_male <- (sprintf("%.f", mydata_InHearing_Total$Total_m * 100/mydata_InHearing_Total$Total_p))
mydata_InHearing_Total$T_female <- sprintf("%.f", mydata_InHearing_Total$Total_f * 100/mydata_InHearing_Total$Total_p)

#Draw the Map of india indicating In_Hearing disabled Male Workers per 100 Disabled Parsons in each state
map_3 <- gvisGeoChart(data = mydata_InHearing_Total, locationvar = "States", colorvar = "T_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InHearing_Male")



#Draw the Map of india indicating In-Hearing Disabled Female Workers per 100 Disabled Parsons in each state
map_4 <- gvisGeoChart(data = mydata_InHearing_Total, locationvar = "States", colorvar = "T_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSeeing_Female")
plot(gvisMerge(map_3,map_4, horizontal = 'TRUE',chartid ='In-Hearing'))

#Drawing Column Chart
mydata_InHearing_Total$T_male <- as.numeric(as.character(mydata_InHearing_Total$T_male))
mydata_InHearing_Total$T_female <- as.numeric(as.character(mydata_InHearing_Total$T_female))
col2 <- gvisColumnChart(mydata_InHearing_Total, xvar = "States", yvar = c("T_male", "T_female"))
plot(col2)

# Drawing map for In-Speach Disability
mydata_InSpeech_Total <- mydata[mydata$Age_gp=='Total' & mydata$Dis=='In-Speech' & mydata$u_r!= 'Rural'
                                & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InSpeech_Total <- mydata_InSpeech_Total[order(mydata_InSpeech_Total$States),]
#Convert charecter data to numeric
mydata_InSpeech_Total$Total_p <- as.numeric(as.character(mydata_InSpeech_Total$Total_p))
mydata_InSpeech_Total$Total_m <- as.numeric(as.character(mydata_InSpeech_Total$Total_m))
mydata_InSpeech_Total$Total_f <- as.numeric(as.character(mydata_InSpeech_Total$Total_f))
#calculate number per 100 of total population

mydata_InSpeech_Total$T_male <- (sprintf("%.f", mydata_InSpeech_Total$Total_m * 100/mydata_InSpeech_Total$Total_p))
mydata_InSpeech_Total$T_female <- sprintf("%.f", mydata_InSpeech_Total$Total_f * 100/mydata_InSpeech_Total$Total_p)

#Draw the Map of india indicating In_Speach disabled Male Workers per 100 Disabled Parsons in each state
map_5 <- gvisGeoChart(data = mydata_InSpeech_Total, locationvar = "States", colorvar = "T_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSpeech_Male")


#Draw the Map of india indicating In-Speach Disabled Female Workers per 100 Disabled Parsons in each state
map_6 <- gvisGeoChart(data = mydata_InSpeech_Total, locationvar = "States", colorvar = "T_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSpeech_Female")
plot(gvisMerge(map_5,map_6, horizontal = 'TRUE', chartid = 'In-Speech Male_Female'))

#Drawing Column Chart
mydata_InSpeech_Total$T_male <- as.numeric(as.character(mydata_InSpeech_Total$T_male))
mydata_InSpeech_Total$T_female <- as.numeric(as.character(mydata_InSpeech_Total$T_female))
col3 <- gvisColumnChart(mydata_InSpeech_Total, xvar = "States", yvar = c("T_male", "T_female"))
plot(col3)

# Drawing map for In-Movement Disability
mydata_InMovement_Total <- mydata[mydata$Age_gp=='Total' & mydata$Dis=='In-Movement' & mydata$u_r!= 'Rural'
                                  & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InMovement_Total <- mydata_InMovement_Total[order(mydata_InMovement_Total$States),]
#Convert charecter data to numeric
mydata_InMovement_Total$Total_p <- as.numeric(as.character(mydata_InMovement_Total$Total_p))
mydata_InMovement_Total$Total_m <- as.numeric(as.character(mydata_InMovement_Total$Total_m))
mydata_InMovement_Total$Total_f <- as.numeric(as.character(mydata_InMovement_Total$Total_f))

#calculate number per 100 of total population
mydata_InMovement_Total$T_male <- (sprintf("%.f", mydata_InMovement_Total$Total_m * 100/mydata_InMovement_Total$Total_p))
mydata_InMovement_Total$T_female <- sprintf("%.f", mydata_InMovement_Total$Total_f * 100/mydata_InMovement_Total$Total_p)

#Draw the Map of india indicating In_Movement disabled Male Workers per 100 Disabled Parsons in each state
map_7 <- gvisGeoChart(data = mydata_InMovement_Total, locationvar = "States", colorvar = "T_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InMovement_Male")


#Draw the Map of india indicating In-Movement Disabled Female Workers per 100 Disabled Parsons in each state
map_8 <- gvisGeoChart(data = mydata_InMovement_Total, locationvar = "States", colorvar = "T_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InMovement_Female")
plot(gvisMerge(map_7,map_8, horizontal = 'TRUE', chartid = 'In-Movement'))

#Drawing Column Chart
mydata_InMovement_Total$T_male <- as.numeric(as.character(mydata_InMovement_Total$T_male))
mydata_InMovement_Total$T_female <- as.numeric(as.character(mydata_InMovement_Total$T_female))
col4 <- gvisColumnChart(mydata_InMovement_Total, xvar = "States", yvar = c("T_male", "T_female"))
plot(col4)

# Drawing map for Mental-Retardation Disability
mydata_MentalRetardation_Total <- mydata[mydata$Age_gp=='Total' & mydata$Dis=='Mental-Retardation' & mydata$u_r!= 'Rural'
                                         & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_MentalRetardation_Total <- mydata_MentalRetardation_Total[order(mydata_MentalRetardation_Total$States),]
#Convert charecter data to numeric
mydata_MentalRetardation_Total$Total_p <- as.numeric(as.character(mydata_MentalRetardation_Total$Total_p))
mydata_MentalRetardation_Total$Total_m <- as.numeric(as.character(mydata_MentalRetardation_Total$Total_m))
mydata_MentalRetardation_Total$Total_f <- as.numeric(as.character(mydata_MentalRetardation_Total$Total_f))
#calculate number per 100 of total population

mydata_MentalRetardation_Total$T_male <- (sprintf("%.f", mydata_MentalRetardation_Total$Total_m * 100/mydata_MentalRetardation_Total$Total_p))
mydata_MentalRetardation_Total$T_female <- sprintf("%.f", mydata_MentalRetardation_Total$Total_f * 100/mydata_MentalRetardation_Total$Total_p)

#Draw the Map of india indicating Mental-Retarded disabled Male Workers per 100 Disabled Parsons in each state
map_9 <- gvisGeoChart(data = mydata_MentalRetardation_Total, locationvar = "States", colorvar = "T_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "MentalRetared_Male")

#Draw the Map of india indicating Mental-Retarded Disabled Female Workers per 100 Disabled Parsons in each state
map_10 <- gvisGeoChart(data = mydata_MentalRetardation_Total, locationvar = "States", colorvar = "T_female", 
                       options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                      width = 600, height = 400),chartid = "MentalRetared_Female")
plot(gvisMerge(map_9,map_10, horizontal = 'TRUE',chartid = 'Mental-Retardation' ))

#Drawing Column Chart
mydata_MentalRetardation_Total$T_male <- as.numeric(as.character(mydata_MentalRetardation_Total$T_male))
mydata_MentalRetardation_Total$T_female <- as.numeric(as.character(mydata_MentalRetardation_Total$T_female))
col5 <- gvisColumnChart(mydata_MentalRetardation_Total, xvar = "States", yvar = c("T_male", "T_female"))
plot(col5)
