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
                             "Marg3_6_p","Marg3_6_m","Marg3_6_f","NonW_p","NonW_m","NonW_f"))

#Drop irrelevant irrelevant and redundant Colums
mydata <- mydata [-c(1,3)]
#head(mydata, n=10)

mydata_InSeeing_Marg3 <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Seeing' & mydata$u_r!= 'Rural'
                               & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InSeeing_Marg3 <- mydata_InSeeing_Marg3[order(mydata_InSeeing_Marg3$States),]
#Convert charecter data to numeric
mydata_InSeeing_Marg3$Marg3_p <- as.numeric(as.character(mydata_InSeeing_Marg3$Marg3_p))
mydata_InSeeing_Marg3$Marg3_m <- as.numeric(as.character(mydata_InSeeing_Marg3$Marg3_m))
mydata_InSeeing_Marg3$Marg3_f <- as.numeric(as.character(mydata_InSeeing_Marg3$Marg3_f))
#calculate number per 100 of Main population

mydata_InSeeing_Marg3$Marg3_male <- (sprintf("%.f", mydata_InSeeing_Marg3$Marg3_m * 100/mydata_InSeeing_Marg3$Marg3_p))
mydata_InSeeing_Marg3$Marg3_female <- sprintf("%.f", mydata_InSeeing_Marg3$Marg3_f * 100/mydata_InSeeing_Marg3$Marg3_p)

#Draw the Map of india indicating In_seeing Marginal(Less than 3 months) disabled Male Workers per 100 Disabled Parsons in each state
map_1 <- gvisGeoChart(data = mydata_InSeeing_Marg3, locationvar = "States", colorvar = "Marg3_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSeeing_Male")


#Draw the Map of india indicating In-Seeing Marginal(Less than 3 months) Disabled Female Workers per 100 Disabled Parsons in each state
map_2 <- gvisGeoChart(data = mydata_InSeeing_Marg3, locationvar = "States", colorvar = "Marg3_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSeeing_Female")
plot(gvisMerge(map_1,map_2, horizontal = 'TRUE', chartid ="InSeeing_Marginal" ))

mydata_InSeeing_Marg3$Marg3_male <- as.numeric(as.character(mydata_InSeeing_Marg3$Marg3_male))
mydata_InSeeing_Marg3$Marg3_female <- as.numeric(as.character(mydata_InSeeing_Marg3$Marg3_female))
col1 <- gvisColumnChart(mydata_InSeeing_Marg3, xvar = "States", yvar = c("Marg3_male", "Marg3_female"))
plot(col1)
#pie <- gvisPieChart(mydata_InSeeing_Marg3, labelvar = "States", numvar = "Marg3_male")
#plot(pie)

# Drawing map for In-Hearing Disability

mydata_InHearing_Marg3 <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Hearing' & mydata$u_r!= 'Rural'
                                & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InHearing_Marg3 <- mydata_InHearing_Marg3[order(mydata_InHearing_Marg3$States),]
#Convert charecter data to numeric
mydata_InHearing_Marg3$Marg3_p <- as.numeric(as.character(mydata_InHearing_Marg3$Marg3_p))
mydata_InHearing_Marg3$Marg3_m <- as.numeric(as.character(mydata_InHearing_Marg3$Marg3_m))
mydata_InHearing_Marg3$Marg3_f <- as.numeric(as.character(mydata_InHearing_Marg3$Marg3_f))
#calculate number per 100 of Marginal(Less than 3 months) population

mydata_InHearing_Marg3$Marg3_male <- (sprintf("%.f", mydata_InHearing_Marg3$Marg3_m * 100/mydata_InHearing_Marg3$Marg3_p))
mydata_InHearing_Marg3$Marg3_female <- sprintf("%.f", mydata_InHearing_Marg3$Marg3_f * 100/mydata_InHearing_Marg3$Marg3_p)

#Draw the Map of india indicating In_Hearing Marginal(Less than 3 months) disabled Male Workers per 100 Disabled Parsons in each state
map_3 <- gvisGeoChart(data = mydata_InHearing_Marg3, locationvar = "States", colorvar = "Marg3_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InHearing_Male")



#Draw the Map of india indicating In-Hearing Marginal(Less than 3 months) Disabled Female Workers per 100 Disabled Parsons in each state
map_4 <- gvisGeoChart(data = mydata_InHearing_Marg3, locationvar = "States", colorvar = "Marg3_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InHearing_Female")
plot(gvisMerge(map_3,map_4, horizontal = 'TRUE', chartid ="InHearing_Marginal" ))

#Drawing Column Chart
mydata_InHearing_Marg3$Marg3_male <- as.numeric(as.character(mydata_InHearing_Marg3$Marg3_male))
mydata_InHearing_Marg3$Marg3_female <- as.numeric(as.character(mydata_InHearing_Marg3$Marg3_female))
col2 <- gvisColumnChart(mydata_InHearing_Marg3, xvar = "States", yvar = c("Marg3_male", "Marg3_female"))
plot(col2)

# Drawing map for In-Speach Disability
mydata_InSpeech_Marg3 <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Speech' & mydata$u_r!= 'Rural'
                               & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InSpeech_Marg3 <- mydata_InSpeech_Marg3[order(mydata_InSpeech_Marg3$States),]
#Convert charecter data to numeric
mydata_InSpeech_Marg3$Marg3_p <- as.numeric(as.character(mydata_InSpeech_Marg3$Marg3_p))
mydata_InSpeech_Marg3$Marg3_m <- as.numeric(as.character(mydata_InSpeech_Marg3$Marg3_m))
mydata_InSpeech_Marg3$Marg3_f <- as.numeric(as.character(mydata_InSpeech_Marg3$Marg3_f))
#calculate number per 100 of Marginal(Less than 3 months) population

mydata_InSpeech_Marg3$Marg3_male <- (sprintf("%.f", mydata_InSpeech_Marg3$Marg3_m * 100/mydata_InSpeech_Marg3$Marg3_p))
mydata_InSpeech_Marg3$Marg3_female <- sprintf("%.f", mydata_InSpeech_Marg3$Marg3_f * 100/mydata_InSpeech_Marg3$Marg3_p)

#Draw the Map of india indicating In_Speach Marginal(Less than 3 months) disabled Male Workers per 100 Disabled Parsons in each state
map_5 <- gvisGeoChart(data = mydata_InSpeech_Marg3, locationvar = "States", colorvar = "Marg3_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSpeech_Male")


#Draw the Map of india indicating In-Speach Marginal(Less than 3 months) Disabled Female Workers per 100 Disabled Parsons in each state
map_6 <- gvisGeoChart(data = mydata_InSpeech_Marg3, locationvar = "States", colorvar = "Marg3_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InSpeech_Female")
plot(gvisMerge(map_5,map_6, horizontal = 'TRUE', chartid ="InSpeech_Marginal" ))

#Drawing Column Chart
mydata_InSpeech_Marg3$Marg3_male <- as.numeric(as.character(mydata_InSpeech_Marg3$Marg3_male))
mydata_InSpeech_Marg3$Marg3_female <- as.numeric(as.character(mydata_InSpeech_Marg3$Marg3_female))
col3 <- gvisColumnChart(mydata_InSpeech_Marg3, xvar = "States", yvar = c("Marg3_male", "Marg3_female"))
plot(col3)

# Drawing map for In-Movement Disability
mydata_InMovement_Marg3 <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='In-Movement' & mydata$u_r!= 'Rural'
                                    & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
mydata_InMovement_Marg3 <- mydata_InMovement_Marg3[order(mydata_InMovement_Marg3$States),]
#Convert charecter data to numeric
mydata_InMovement_Marg3$Marg3_p <- as.numeric(as.character(mydata_InMovement_Marg3$Marg3_p))
mydata_InMovement_Marg3$Marg3_m <- as.numeric(as.character(mydata_InMovement_Marg3$Marg3_m))
mydata_InMovement_Marg3$Marg3_f <- as.numeric(as.character(mydata_InMovement_Marg3$Marg3_f))

#calculate number per 100 of Marginal(Less than 3 months) population
mydata_InMovement_Marg3$Marg3_male <- (sprintf("%.f", mydata_InMovement_Marg3$Marg3_m * 100/mydata_InMovement_Marg3$Marg3_p))
mydata_InMovement_Marg3$Marg3_female <- sprintf("%.f", mydata_InMovement_Marg3$Marg3_f * 100/mydata_InMovement_Marg3$Marg3_p)

#Draw the Map of india indicating In_Movement Marginal(Less than 3 months) disabled Male Workers per 100 Disabled Parsons in each state
map_7 <- gvisGeoChart(data = mydata_InMovement_Marg3, locationvar = "States", colorvar = "Marg3_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InMovement_male")


#Draw the Map of india indicating In-Movement Marginal(Less than 3 months) Disabled Female Workers per 100 Disabled Parsons in each state
map_8 <- gvisGeoChart(data = mydata_InMovement_Marg3, locationvar = "States", colorvar = "Marg3_female", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "InMovement_female")
plot(gvisMerge(map_7,map_8, horizontal = 'TRUE', chartid ="InMovement_Marginal" ))

#Drawing Column Chart
mydata_InMovement_Marg3$Marg3_male <- as.numeric(as.character(mydata_InMovement_Marg3$Marg3_male))
mydata_InMovement_Marg3$Marg3_female <- as.numeric(as.character(mydata_InMovement_Marg3$Marg3_female))
col4 <- gvisColumnChart(mydata_InMovement_Marg3, xvar = "States", yvar = c("Marg3_male", "Marg3_female"))
plot(col4)

# Drawing map for Mental-Retardation Disability
MyData_MentalRetardation_Marg3 <- mydata[mydata$Age_gp=='15-59' & mydata$Dis=='Mental-Retardation' & mydata$u_r!= 'Rural'
                                        & mydata$u_r!='Urban' & mydata$States!= 'INDIA',]
MyData_MentalRetardation_Marg3 <- MyData_MentalRetardation_Marg3[order(MyData_MentalRetardation_Marg3$States),]
#Convert charecter data to numeric
MyData_MentalRetardation_Marg3$Marg3_p <- as.numeric(as.character(MyData_MentalRetardation_Marg3$Marg3_p))
MyData_MentalRetardation_Marg3$Marg3_m <- as.numeric(as.character(MyData_MentalRetardation_Marg3$Marg3_m))
MyData_MentalRetardation_Marg3$Marg3_f <- as.numeric(as.character(MyData_MentalRetardation_Marg3$Marg3_f))
#calculate number per 100 of Marginal(Less than 3 months) population

MyData_MentalRetardation_Marg3$Marg3_male <- (sprintf("%.f", MyData_MentalRetardation_Marg3$Marg3_m * 100/MyData_MentalRetardation_Marg3$Marg3_p))
MyData_MentalRetardation_Marg3$Marg3_female <- sprintf("%.f", MyData_MentalRetardation_Marg3$Marg3_f * 100/MyData_MentalRetardation_Marg3$Marg3_p)

#Draw the Map of india indicating Mental-Retarded Marginal(Less than 3 months) disabled Male Workers per 100 Disabled Parsons in each state
map_9 <- gvisGeoChart(data = MyData_MentalRetardation_Marg3, locationvar = "States", colorvar = "Marg3_male", 
                      options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                     width = 600, height = 400),chartid = "MentalRetared_Male")


#Draw the Map of india indicating Mental-Retarded Marginal(Less than 3 months) Disabled Female Workers per 100 Disabled Parsons in each state
map_10 <- gvisGeoChart(data = MyData_MentalRetardation_Marg3, locationvar = "States", colorvar = "Marg3_female", 
                       options = list(region = "IN",domain = 'IN', displayMode = "regions",resolution="provinces",
                                      width = 600, height = 400),chartid = "MentalRetared_Female")
plot(gvisMerge(map_9,map_10, horizontal = 'TRUE', chartid ="Mental-Retardation" ))

#Drawing Column Chart
MyData_MentalRetardation_Marg3$Marg3_male <- as.numeric(as.character(MyData_MentalRetardation_Marg3$Marg3_male))
MyData_MentalRetardation_Marg3$Marg3_female <- as.numeric(as.character(MyData_MentalRetardation_Marg3$Marg3_female))
col5 <- gvisColumnChart(MyData_MentalRetardation_Marg3, xvar = "States", yvar = c("Marg3_male", "Marg3_female"))
plot(col5)
