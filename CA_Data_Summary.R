load("CA_HOMES")

#---------------------------------Data Summary----------------------------------


#data %>% select("property_type")

glimpse(CA_HOMES)
class(CA_HOMES$property_type)
unique(CA_HOMES$property_type)

CA_HOMES$property_type <- as.factor(CA_HOMES$property_type)
class(CA_HOMES$property_type)

levels(CA_HOMES$property_type)
#5 different categories in property type
CA_HOMES$property_type <- factor((CA_HOMES$property_type),
                                 levels=c("All Residential"
                                          ,"Condo/Co-op",
                                          "Multi-Family (2-4 Unit)",
                                          "Single Family Residential",
                                          "Townhouse"))

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
  count_NA_region <- aggregate(m ~ CA_HOMES$region, CA_HOMES, 
                               function(m) {sum(is.na(m))}, na.action = NULL)
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



#converting period_end data into a date class instead of being read as a character
options(max.print=100)
library(data.table)
CA_HOMES$period_end <- as.Date(CA_HOMES$period_end)
CA_HOMES$period_end
#tab <- table(cut(CA_HOMES$period_end,'month'))
#data.frame(Date=format(as.Date(names(tab)),'%m/%Y'),Frequency=as.vector(tab))


#Checking how many entries Alpine County has per month
alp_county <- CA_HOMES[CA_HOMES$region=="Alpine County, CA",]
alp_county
alp_county$period_end <- as.Date(alp_county$period_end,format="%m,%d,%Y")
tab <- table(cut(alp_county$period_end,'month'))
data.frame(Date=format(as.Date(names(tab)),'%m/%Y'),Frequency=as.vector(tab))


#below we are counting the number of times a county appears per month (i.e. does
#it contain all property types per month)
#if not we need to find out why?
#which counties, which counties

#Gives an overall count of how many entries Riverside County over the span of 10 years
table(table(CA_HOMES[CA_HOMES$region=="Riverside County, CA",]$period_begin))

#Gives a summary of how many entries Riverside County has over a month-to-month basis
table(CA_HOMES[CA_HOMES$region=="Riverside County, CA",]$period_begin)
#We do the same for Los Angeles County
table(CA_HOMES[CA_HOMES$region=="Los Angeles County, CA",]$region_type)

#Finding how many CA counties are represented in our data. There are 41 counties
#represeted, while CA has 58 counties. So, 17 counties are missing.
CA_counties <- sort(unique(CA_HOMES$region))

save(CA_counties, file = "CA_counties")

table(table(CA_HOMES[CA_HOMES$region,]$property_type))

#creating an empty matrix
Property_type_obs_matrix <- data.frame(matrix(nrow=41,ncol=5))
Property_type_obs_matrix

for (i in 1:41){
  Property_type_obs <- c(table(CA_HOMES[CA_HOMES$region==CA_counties[i],]$property_type))
  length(Property_type_obs)
  #if length(Property_type_obs) < 5
  #Property_type_obs[5] <- 0
  #if
  
  Property_type_obs_matrix[i,1:length(Property_type_obs)] <- Property_type_obs
}

Property_type_obs_matrix[,6] <- CA_counties
colnames(Property_type_obs_matrix) <- c("All Residential",
                                        "Condo/Co-op",
                                        "Multi-Family (2-4 Unit)",
                                        "Single Family Residential",
                                        "Townhouse",
                                        "Region")

Property_type_obs_matrix


plot(table(CA_HOMES$region))

plot(table(CA_HOMES$period_end))

#plot(table(CA_HOMES[CA_HOMES$region=="Los Angeles County, CA"]$period_end)CA_HOMES$region)

