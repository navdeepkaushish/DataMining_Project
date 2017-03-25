#DM_Navdeep_Project

#load required Libraries
library(sp)
library(ggplot2)
library(maptools)
library(RColorBrewer)
library(rJava)
library(knitr)
suppressPackageStartupMessages(library(googleVis))

# load level 1 india data downloaded from http://gadm.org/country
india <- readRDS("IND_adm1.rds")
india <- india[-(32:33),]
#summary(india)

#Read dataset
ind_df <- read.csv("dis_work.csv")

#Remove Irrelevant rows
mydata <- ind_df[7:4866,]

#Give Names to colums
mydata <- setNames( mydata,c("Table_n","State_c","DisMain_c","States","u_r","Dis",
                             "Age_gp","Total_p","Total_m","Total_f","Main_p",
                             "Main_m","Main_f","Marg3_p","Marg3_m","Marg3_f",
                             "NonW_p","NonW_m","NonW_f","NonW_p","NonW_m","NonW_f"))                            
                                                                          

#Drop irrelevant irrelevant and redundant Colums
mydata <- mydata [-c(1,3)]
#head(mydata, n=10)

mydata_InSeeing_NonW <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Seeing' & mydata$u_r!= 'Rural'
                                  & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InSeeing_NonW <- mydata_InSeeing_NonW[order(mydata_InSeeing_NonW$States),]
#Convert charecter data to numeric
mydata_InSeeing_NonW$NonW_p <- as.numeric(as.character(mydata_InSeeing_NonW$NonW_p))
mydata_InSeeing_NonW$NonW_m <- as.numeric(as.character(mydata_InSeeing_NonW$NonW_m))
mydata_InSeeing_NonW$NonW_f <- as.numeric(as.character(mydata_InSeeing_NonW$NonW_f))
#calculate number per 100 of  population

mydata_InSeeing_NonW$NonW_male <- (sprintf("%.f", mydata_InSeeing_NonW$NonW_m * 100/mydata_InSeeing_NonW$NonW_p))
mydata_InSeeing_NonW$NonW_female <- sprintf("%.f", mydata_InSeeing_NonW$NonW_f * 100/mydata_InSeeing_NonW$NonW_p)

#Draw the Map of india indicating In_seeing  disabled Male Non-Workers per 100 Disabled Parsons in each state
map_1 <- gvisGeoChart(data = mydata_InSeeing_NonW, locationvar = "States", colorvar = "NonW_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSeeing_Male")


#Draw the Map of india indicating In-Seeing  Disabled Female Non-Workers per 100 Disabled Parsons in each state
map_2 <- gvisGeoChart(data = mydata_InSeeing_NonW, locationvar = "States", colorvar = "NonW_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSeeing_Female")
plot(gvisMerge(map_1,map_2, horizontal = 'TRUE', chartid ="InSeeing_NonW" ))

mydata_InSeeing_NonW$NonW_male <- as.numeric(as.character(mydata_InSeeing_NonW$NonW_male))
mydata_InSeeing_NonW$NonW_female <- as.numeric(as.character(mydata_InSeeing_NonW$NonW_female))
col1 <- gvisColumnChart(mydata_InSeeing_NonW, xvar = "States", yvar = c("NonW_male", "NonW_female"))
plot(col1)
#pie <- gvisPieChart(mydata_InSeeing_NonW, labelvar = "States", numvar = "NonW_male")
#plot(pie)

# Drawing map for In-Hearing Disability

mydata_InHearing_NonW <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Hearing' & mydata$u_r!= 'Rural'
                                   & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InHearing_NonW <- mydata_InHearing_NonW[order(mydata_InHearing_NonW$States),]
#Convert charecter data to numeric
mydata_InHearing_NonW$NonW_p <- as.numeric(as.character(mydata_InHearing_NonW$NonW_p))
mydata_InHearing_NonW$NonW_m <- as.numeric(as.character(mydata_InHearing_NonW$NonW_m))
mydata_InHearing_NonW$NonW_f <- as.numeric(as.character(mydata_InHearing_NonW$NonW_f))
#calculate number per 100 of  population

mydata_InHearing_NonW$NonW_male <- (sprintf("%.f", mydata_InHearing_NonW$NonW_m * 100/mydata_InHearing_NonW$NonW_p))
mydata_InHearing_NonW$NonW_female <- sprintf("%.f", mydata_InHearing_NonW$NonW_f * 100/mydata_InHearing_NonW$NonW_p)

#Draw the Map of india indicating In_Hearing  disabled Male Non-Workers per 100 Disabled Parsons in each state
map_3 <- gvisGeoChart(data = mydata_InHearing_NonW, locationvar = "States", colorvar = "NonW_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InHearing_Male")



#Draw the Map of india indicating In-Hearing  Disabled Female Non-Workers per 100 Disabled Parsons in each state
map_4 <- gvisGeoChart(data = mydata_InHearing_NonW, locationvar = "States", colorvar = "NonW_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InHearing_Female")
plot(gvisMerge(map_3,map_4, horizontal = 'TRUE', chartid ="Inearing_NonW" ))

#Drawing Column Chart
mydata_InHearing_NonW$NonW_male <- as.numeric(as.character(mydata_InHearing_NonW$NonW_male))
mydata_InHearing_NonW$NonW_female <- as.numeric(as.character(mydata_InHearing_NonW$NonW_female))
col2 <- gvisColumnChart(mydata_InHearing_NonW, xvar = "States", yvar = c("NonW_male", "NonW_female"))
plot(col2)

# Drawing map for In-Speach Disability
mydata_InSpeech_NonW <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Speech' & mydata$u_r!= 'Rural'
                                  & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InSpeech_NonW <- mydata_InSpeech_NonW[order(mydata_InSpeech_NonW$States),]
#Convert charecter data to numeric
mydata_InSpeech_NonW$NonW_p <- as.numeric(as.character(mydata_InSpeech_NonW$NonW_p))
mydata_InSpeech_NonW$NonW_m <- as.numeric(as.character(mydata_InSpeech_NonW$NonW_m))
mydata_InSpeech_NonW$NonW_f <- as.numeric(as.character(mydata_InSpeech_NonW$NonW_f))
#calculate number per 100 of  population

mydata_InSpeech_NonW$NonW_male <- (sprintf("%.f", mydata_InSpeech_NonW$NonW_m * 100/mydata_InSpeech_NonW$NonW_p))
mydata_InSpeech_NonW$NonW_female <- sprintf("%.f", mydata_InSpeech_NonW$NonW_f * 100/mydata_InSpeech_NonW$NonW_p)

#Draw the Map of india indicating In_Speach  disabled Male Non-Workers per 100 Disabled Parsons in each state
map_5 <- gvisGeoChart(data = mydata_InSpeech_NonW, locationvar = "States", colorvar = "NonW_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSpeech_Male")


#Draw the Map of india indicating In-Speach  Disabled Female Non-Workers per 100 Disabled Parsons in each state
map_6 <- gvisGeoChart(data = mydata_InSpeech_NonW, locationvar = "States", colorvar = "NonW_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSpeech_Female")
plot(gvisMerge(map_5,map_6, horizontal = 'TRUE', chartid ="InSpeech_NonW" ))

#Drawing Column Chart
mydata_InSpeech_NonW$NonW_male <- as.numeric(as.character(mydata_InSpeech_NonW$NonW_male))
mydata_InSpeech_NonW$NonW_female <- as.numeric(as.character(mydata_InSpeech_NonW$NonW_female))
col3 <- gvisColumnChart(mydata_InSpeech_NonW, xvar = "States", yvar = c("NonW_male", "NonW_female"))
plot(col3)

# Drawing map for In-Movement Disability
mydata_InMovement_NonW <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Movement' & mydata$u_r!= 'Rural'
                                    & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InMovement_NonW <- mydata_InMovement_NonW[order(mydata_InMovement_NonW$States),]
#Convert charecter data to numeric
mydata_InMovement_NonW$NonW_p <- as.numeric(as.character(mydata_InMovement_NonW$NonW_p))
mydata_InMovement_NonW$NonW_m <- as.numeric(as.character(mydata_InMovement_NonW$NonW_m))
mydata_InMovement_NonW$NonW_f <- as.numeric(as.character(mydata_InMovement_NonW$NonW_f))

#calculate number per 100 of  population
mydata_InMovement_NonW$NonW_male <- (sprintf("%.f", mydata_InMovement_NonW$NonW_m * 100/mydata_InMovement_NonW$NonW_p))
mydata_InMovement_NonW$NonW_female <- sprintf("%.f", mydata_InMovement_NonW$NonW_f * 100/mydata_InMovement_NonW$NonW_p)

#Draw the Map of india indicating In_Movement  disabled Male Non-Workers per 100 Disabled Parsons in each state
map_7 <- gvisGeoChart(data = mydata_InMovement_NonW, locationvar = "States", colorvar = "NonW_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InMovement_male")


#Draw the Map of india indicating In-Movement  Disabled Female Non-Workers per 100 Disabled Parsons in each state
map_8 <- gvisGeoChart(data = mydata_InMovement_NonW, locationvar = "States", colorvar = "NonW_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InMovement_female")
plot(gvisMerge(map_7,map_8, horizontal = 'TRUE', chartid ="InMovement_NonW" ))

#Drawing Column Chart
mydata_InMovement_NonW$NonW_male <- as.numeric(as.character(mydata_InMovement_NonW$NonW_male))
mydata_InMovement_NonW$NonW_female <- as.numeric(as.character(mydata_InMovement_NonW$NonW_female))
col4 <- gvisColumnChart(mydata_InMovement_NonW, xvar = "States", yvar = c("NonW_male", "NonW_female"))
plot(col4)

# Drawing map for Mental-Retardation Disability
MyData_MentalRetardation_NonW <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='Mental-Retardation' & mydata$u_r!= 'Rural'
                                           & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
MyData_MentalRetardation_NonW <- MyData_MentalRetardation_NonW[order(MyData_MentalRetardation_NonW$States),]
#Convert charecter data to numeric
MyData_MentalRetardation_NonW$NonW_p <- as.numeric(as.character(MyData_MentalRetardation_NonW$NonW_p))
MyData_MentalRetardation_NonW$NonW_m <- as.numeric(as.character(MyData_MentalRetardation_NonW$NonW_m))
MyData_MentalRetardation_NonW$NonW_f <- as.numeric(as.character(MyData_MentalRetardation_NonW$NonW_f))
#calculate number per 100 of  population

MyData_MentalRetardation_NonW$NonW_male <- (sprintf("%.f", MyData_MentalRetardation_NonW$NonW_m * 100/MyData_MentalRetardation_NonW$NonW_p))
MyData_MentalRetardation_NonW$NonW_female <- sprintf("%.f", MyData_MentalRetardation_NonW$NonW_f * 100/MyData_MentalRetardation_NonW$NonW_p)

#Draw the Map of india indicating Mental-Retarded  disabled Male Non-Workers per 100 Disabled Parsons in each state
map_9 <- gvisGeoChart(data = MyData_MentalRetardation_NonW, locationvar = "States", colorvar = "NonW_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "MentalRetared_Male")


#Draw the Map of india indicating Mental-Retarded  Disabled Female Non-Workers per 100 Disabled Parsons in each state
map_10 <- gvisGeoChart(data = MyData_MentalRetardation_NonW, locationvar = "States", colorvar = "NonW_female", 
                       options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                      width = 600, height = 400),chartid = "MentalRetared_Female")
plot(gvisMerge(map_9,map_10, horizontal = 'TRUE', chartid ="Mental_Retardation_NonW" ))

#Drawing Column Chart
MyData_MentalRetardation_NonW$NonW_male <- as.numeric(as.character(MyData_MentalRetardation_NonW$NonW_male))
MyData_MentalRetardation_NonW$NonW_female <- as.numeric(as.character(MyData_MentalRetardation_NonW$NonW_female))
col5 <- gvisColumnChart(MyData_MentalRetardation_NonW, xvar = "States", yvar = c("NonW_male", "NonW_female"))
plot(col5)
