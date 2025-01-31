#install packages
install.packages("ggfortify")
install.packages("cluster")
install.packages("factoextra")

#load required libraries
library(dplyr)
library(ggplot2)
library(stats)
library(ggfortify)
library(cluster)
library(factoextra)

install.packages("readxl")
library(readxl)

Data=read_excel("C:\\Users\\ASUS\\Documents\\2nd Yr 1st Sem\\Data Mining\\Assignment 1\\Crime_Data_from_2020_to_Present.xlsx")
View(Data)

#delete NA raws
Data_1 <- na.omit(Data)
View(Data_1)


# Create a vector of numeric time values for make time correctly
new_times <- c(Data_1$`TIME OCC`)

# Convert to character format with a colon separator
formatted_times <- sprintf("%02d:%02d", new_times %/% 100, new_times %% 100)

# Create a data frame with the formatted times
Time_OCC<- data.frame(`TIME OCC`= formatted_times)
View(Time_OCC)

Data_1[ ,-3]

Data_2=cbind(c(Data_1[ , -3]),Time_OCC)
View(Data_2)

Data_3=Data_2[ ,c(1,2,12,3:11)] # Define the new order of columns
View(Data_3)

# Get rows according to sorted excel file (only 2023)
Data_3$`DATE OCC` <- as.Date(Data_3$`DATE OCC`, format = "%Y-%m-%d")

# Filter rows between specific dates
Data_4<- subset(Data_3, `DATE OCC` >= "2023-12-01" & `DATE OCC` <= "2023-12-31")
View(Data_4)

#Get only teenager
Crime_Data=Data_4[which(Data_4$`Vict Age` >=13 & Data_4$`Vict Age`<=19), ]
View(Crime_Data)

names(Crime_Data) 
head(Crime_Data) 
tail(Crime_Data) 
summary(Crime_Data) 
str(Crime_Data) 

nrow(Crime_Data)
ncol(Crime_Data)
dim(Crime_Data)


#Convert columns

Crime_Data$`AREA NAME`= as.factor(Crime_Data$`AREA NAME`)
Crime_Data$`AREA NAME`= as.numeric(Crime_Data$`AREA NAME`)
table(Crime_Data$`AREA NAME`)

Crime_Data$`DATE OCC`= as.factor(Crime_Data$`DATE OCC`)
Crime_Data$`DATE OCC`= as.numeric(Crime_Data$`DATE OCC`)
table(Crime_Data$`DATE OCC`)

Crime_Data$`Crm Cd Desc`= as.factor(Crime_Data$`Crm Cd Desc`)
Crime_Data$`Crm Cd Desc`= as.numeric(Crime_Data$`Crm Cd Desc`)
table(Crime_Data$`Crm Cd Desc`)

Crime_Data$`Vict Sex`= as.factor(Crime_Data$`Vict Sex`)
Crime_Data$`Vict Sex`= as.numeric(Crime_Data$`Vict Sex`)
table(Crime_Data$`Vict Sex`)

Crime_Data$`Premis Desc`= as.factor(Crime_Data$`Premis Desc`)
Crime_Data$`Premis Desc`= as.numeric(Crime_Data$`Premis Desc`)
table(Crime_Data$`Premis Desc`)


str(Crime_Data)


#checking for any not available in the dataset

anyNA(Crime_Data)

#wss plot to choose maximum number of clusters
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(Crime_Data)



#Spotting the k means in the curve in order to choose the optimum number of cluster=3
KM=kmeans(Crime_Data,2)

#Evaluating cluster Analysis
autoplot(KM,Crime_Data,frame=TRUE)  #cluster plot

#cluster centers
KM$centers

