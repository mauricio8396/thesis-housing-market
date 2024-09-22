
library(magrittr)
library(dplyr)
library(tidyverse)

setwd("~/THESIS")
CA_HOMES <- read.csv("Redfin_Data_CA.csv")
view(CA_HOMES)

#data %>% select("property_type")

glimpse(CA_HOMES)
class(CA_HOMES$property_type)
unique(CA_HOMES$property_type)

CA_HOMES$property_type <- as.factor(CA_HOMES$property_type)
class(CA_HOMES$property_type)

levels(CA_HOMES$property_type)
#5 different categories in property type
CA_HOMES$property_type <- factor((CA_HOMES$property_type),levels=c("All Residential"
                          ,"Condo/Co-op","Multi-Family (2-4 Unit)","Single Family Residential","Townhouse"))

levels(CA_HOMES$property_type)

#we convert property type categories into numerical variables
CA_HOMES$property_type <- as.numeric(CA_HOMES$property_type) 
CA_HOMES$property_type

unique(CA_HOMES$region)
#41 counties represented here, however there are 58 counties in California.
#So, 17 counties of California are missing in our data

CA_HOMES %>% count(region) 
#How many observations per county

#counting NA values per variable
colSums(is.na(CA_HOMES))

#Getting summary of variables such as mean
summary(CA_HOMES)
rowSums(is.na(CA_HOMES))

#creating an empty matrix
NA_matrix_region <- data.frame(matrix(nrow=41,ncol=47))
NA_matrix_region

#grouping number of NA values per county by variable
for (i in 14:58){
m <- CA_HOMES[,i]
count_NA_region <- aggregate(m ~ CA_HOMES$region, CA_HOMES,function(m) {sum(is.na(m))}, na.action = NULL)
count_NA_region
NA_matrix_region[,1] <- count_NA_region[,1]
NA_matrix_region[,i-12] <- count_NA_region[,2]
}
#matrix that counted NA values per county by variable
NA_matrix_region

#adding number of NA values by row
for (i in 1:41){
NA_matrix_region[i,47] <- rowSums(NA_matrix_region[i,2:46])
}
options(max.print=10000)
#same as above matrix, there is an extra column that gives a total number of missing
#data values per county
NA_matrix_region
NA_table <- write.table(NA_matrix_region, file="NA_matrix_region.txt",sep="")

#does each county have a data point for each month period for the time span of the data, if not why?
#if not all counties of all california are represented in this data set why not?
#later on we are going to map all data points onto the country/state map.
#

library(data.table)
CA_HOMES$period_end <- as.Date(CA_HOMES$period_end,"%d/%m/%Y")

CA_HOMES[, .N,by=month(date)]


#CA_HOMES %>% group_by(CA_HOMES$region) %>% summarize(sum_na = sum(is.na(CA_HOMES$median_sale_price_mom))) %>% print(n=41)
#CA_HOMES %>% group_by(CA_HOMES$region) %>% summarize(sum_na = sum(is.na(CA_HOMES[,-8]))) %>% print(n=41)


#average_median_sale <- length("median_sale_price")
#average_median_sale

#median_sale <- data %>% select(median_sale_price)
#median_sale

#median_sale[is.na(median_sale)] <- 0
#median_price_average <- mean(median_sale)
