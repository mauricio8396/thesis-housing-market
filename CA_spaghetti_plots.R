library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
load("CA_HOMES")
load("CA_data_new_orig")
load("CA_data_new_2")

single_family_homes <- CA_HOMES %>% 
                       filter(property_type=="Single Family Residential") %>%
                       select(period_end,median_sale_price,region)

single_family_counties <- unique(single_family_homes$region)
single_family_counties <- sort(single_family_counties)

single_family_homes$newdate <- strptime(single_family_homes$period_end, "%m/%d/%Y")
single_family_homes$txtdate <- format(single_family_homes$newdate,"%b-%y")


g1 <- ggplot(data = single_family_homes, 
             aes(x=as.Date(period_end, "%m/%d/%Y"), y=median_sale_price)) +
             scale_x_date(date_breaks = "3 month", date_labels =  "%b %y") +
      geom_blank() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      labs(title = "Median Sale Prices v Time for Single Family Homes",
           x = "Time (Per 3 months)", y = "Median Sale Prices")

for (i in 1:length(single_family_counties)){
  county_i <- single_family_counties[i]
  county_i_data <- single_family_homes %>% filter(region == county_i)
  g1 <- g1 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           color = i))
}

print(g1)


interaction.plot(single_family_homes$period_end,
                 single_family_homes$region, 
                 single_family_homes$median_sale_price, 
                 xlab="time", ylab="Median Sale Price", legend=F)

#----Spaghetti Plot for Single Family Homes Colored by County----
ggplot(single_family_homes, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
       scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
       theme(axis.text.x = element_text(angle=60, hjust=1)) +
       labs(title = "Median Sale Prices v Time for Single Family Homes",
            x = "Time (Per 3 months)", y = "Median Sale Prices")



condo_homes <- CA_HOMES %>% 
  filter(property_type=="Condo/Co-op") %>%
  select(period_end,median_sale_price,region)

ggplot(condo_homes, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices")



multi_family_homes <- CA_HOMES %>% 
  filter(property_type=="Multi-Family (2-4 Unit)") %>%
  select(period_end,median_sale_price,region)

ggplot(multi_family_homes, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")



townhouses <- CA_HOMES %>% 
  filter(property_type=="Townhouse") %>%
  select(period_end,median_sale_price,region)

ggplot(townhouses, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")

#----Subsets of Data by Area Type (Coastal/Inland)---- 
#Coastal Urban
# Orange County, Los Angeles County, San Francisco County, Ventura,
# San Mateo County, San Diego County, Contra Costa County, Alameda,

coastal_urban <- CA_HOMES %>% filter(region == "Orange County, CA" |
                                     region == "Los Angeles County, CA" |
                                     region == "San Francisco County, CA" |
                                     region == "Ventura County, CA" |
                                     region == "San Mateo County, CA" |
                                     region == "San Diego County, CA" |
                                     region == "Contra Costa County, CA" |
                                     region == "Alameda County, CA" |
                                     region == "Santa Clara County, CA")


#Coastal Not populous
# San Luis Obispo County, Monterey, Mendocino, Humboldt, Del Norte,
# Santa Barbara, Marin, Sonoma, Solano, Santa Cruz

coastal_not_populous <- CA_HOMES %>% 
                        filter(region == "San Luis Obispo County, CA" |
                               region == "Monterey County, CA" |
                               region == "Santa Barbara County, CA" |
                               region == "Marin County, CA"|
                               region == "Napa County, CA" |
                               region == "Sonoma County, CA" |
                               region == "Solano County, CA" |
                               region == "Santa Cruz County, CA")


#Inland Urban
# Santa Clara, Sacramento, Riverside, San Bernardino, San Joaquin, Stanislaus,
# Placer, Fresno, Kern, Merced, Tulare

inland_urban <- CA_HOMES %>%
                filter(region == "Sacramento County, CA" |
                       region == "Riverside County, CA" |
                       region == "San Bernardino County, CA" |
                       region == "San Joaquin County, CA" |
                       region == "Stanislaus County, CA" |
                       region == "Placer County, CA" |
                       region == "Fresno County, CA" |
                       region == "Kern County, CA" |
                       region == "Merced County, CA")

#Inland Not populous
# San Benito, Kings, Madera, Amador, Lake, Shasta, Calaveras,
# Imperial, Tuolumne, Tehama, Glenn, Colusa, Mariposa, Plumas, Lassen, Sisikiyou,
# Mono, Trinity, Sierra, Modoc, Inyo, Alpine, El Dorado, Nevada, Yuba, *Butte,
# Sutter, Napa, *Yolo

inland_not_populous <- CA_HOMES %>%
                       filter(region == "San Benito County, CA" |
                              region == "Madera County, CA" |
                              region == "Amador County, CA" |
                              region == "Lake County, CA" |
                              region == "Calaveras County, CA" |
                              region == "Glenn County, CA" |
                              region == "Colusa County, CA" |
                              region == "Mariposa County, CA" |
                              region == "Alpine County, CA" |
                              region == "El Dorado County, CA" |
                              region == "Nevada County, CA" |
                              region == "Yuba County, CA" |
                              region == "Sutter County, CA" |
                              region == "Butte County, CA" |
                              region == "Yolo County, CA")

#----Plotting Median Sale Prices for Coastal Urban Areas----
single_family_homes2 <- coastal_urban %>% 
  filter(property_type=="Single Family Residential") %>%
  select(period_end,median_sale_price,region)

ggplot(single_family_homes2, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Coastal Urban Counties")


condo_homes2 <- coastal_urban %>% 
  filter(property_type=="Condo/Co-op") %>%
  select(period_end,median_sale_price,region)

ggplot(condo_homes2, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Coastal Urban Counties")




multi_family_homes2 <- coastal_urban %>% 
  filter(property_type=="Multi-Family (2-4 Unit)") %>%
  select(period_end,median_sale_price,region)

ggplot(multi_family_homes2, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Coastal Urban Counties")




townhouses2 <- coastal_urban %>% 
  filter(property_type=="Townhouse") %>%
  select(period_end,median_sale_price,region)

ggplot(townhouses2, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Townhouses",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Coastal Urban Counties")



#----Plotting Median Sale Prices for Coastal Not Populous Areas----
single_family_homes3 <- coastal_not_populous %>% 
  filter(property_type=="Single Family Residential") %>%
  select(period_end,median_sale_price,region)

ggplot(single_family_homes3, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Coastal Not Populous Counties")




condo_homes3 <- coastal_not_populous %>% 
  filter(property_type=="Condo/Co-op") %>%
  select(period_end,median_sale_price,region)

ggplot(condo_homes3, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Coastal Not Populous Counties")




multi_family_homes3 <- coastal_not_populous %>% 
  filter(property_type=="Multi-Family (2-4 Unit)") %>%
  select(period_end,median_sale_price,region)

ggplot(multi_family_homes3, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Coastal Not Populous Counties")




townhouses3 <- coastal_not_populous %>% 
  filter(property_type=="Townhouse") %>%
  select(period_end,median_sale_price,region)

ggplot(townhouses3, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Coastal Not Populous Counties")



#----Plotting Median Sale Prices for Inland Urban Counties----
single_family_homes4 <- inland_urban %>% 
  filter(property_type=="Single Family Residential") %>%
  select(period_end,median_sale_price,region)

ggplot(single_family_homes4, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Inland Urban Counties")



condo_homes4 <- inland_urban %>% 
  filter(property_type=="Condo/Co-op") %>%
  select(period_end,median_sale_price,region)

ggplot(condo_homes4, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Inland Urban Counties")



multi_family_homes4 <- inland_urban %>% 
  filter(property_type=="Multi-Family (2-4 Unit)") %>%
  select(period_end,median_sale_price,region)

ggplot(multi_family_homes4, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Inland Urban Counties")



townhouses4 <- inland_urban %>% 
  filter(property_type=="Townhouse") %>%
  select(period_end,median_sale_price,region)

ggplot(townhouses4, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Inland Urban Counties")


#----Plotting Median Sale Prices for Inland Not Populous Counties----
single_family_homes5 <- inland_not_populous %>% 
  filter(property_type=="Single Family Residential") %>%
  select(period_end,median_sale_price,region)

ggplot(single_family_homes5, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Inland Not Populous Counties")



condo_homes5 <- inland_not_populous %>% 
  filter(property_type=="Condo/Co-op") %>%
  select(period_end,median_sale_price,region)

ggplot(condo_homes5, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Inland Not Populous Counties")



multi_family_homes5 <- inland_not_populous %>% 
  filter(property_type=="Multi-Family (2-4 Unit)") %>%
  select(period_end,median_sale_price,region)

ggplot(multi_family_homes5, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Inland Not Populous Counties")



townhouses5 <- inland_not_populous %>% 
  filter(property_type=="Townhouse") %>%
  select(period_end,median_sale_price,region)

ggplot(townhouses5, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           color = region)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  scale_colour_discrete(name = "Inland Not Populous Counties")



#----Plotting Median Sale Prices for Single Family Homes colored by Area (Urban/Not Populous)----

colors <-c("Coastal Urban"="brown1", 
           "Coastal Not Populous"="dodgerblue", 
           "Inland Urban"="darkorchid1",
           "Inland Not Populous"="springgreen4")

coastal_urban_counties <- unique(coastal_urban$region)

g1 <- ggplot(data = single_family_homes, 
             aes(x=as.Date(period_end, "%m/%d/%Y"), y=median_sale_price)) +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %y") +
  geom_blank() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")

for (i in 1:length(coastal_urban_counties)){
  county_i <- coastal_urban_counties[i]
  county_i_data <- single_family_homes %>% filter(region == county_i)
  g1 <- g1 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Coastal Urban"))
}

coastal_not_populous_counties <- unique(coastal_not_populous$region)

for (i in 1:length(coastal_not_populous_counties)){
  county_i <- coastal_not_populous_counties[i]
  county_i_data <- single_family_homes %>% filter(region == county_i)
  g1 <- g1 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                       colour = "Coastal Not Populous"))
}

inland_urban_counties <- unique(inland_urban$region)

for (i in 1:length(inland_urban_counties)){
  county_i <- inland_urban_counties[i]
  county_i_data <- single_family_homes %>% filter(region == county_i)
  g1 <- g1 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                       colour = "Inland Urban"))
}

inland_not_populous_counties <- unique(inland_not_populous$region)

for (i in 1:length(inland_not_populous_counties)){
  county_i <- inland_not_populous_counties[i]
  county_i_data <- single_family_homes %>% filter(region == county_i)
  g1 <- g1 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                       colour = "Inland Not Populous"))
}


g1 <- g1 + scale_colour_manual(name='Area Type',
                              values=colors)

print(g1)

#----Plotting Median Sale Prices for Condo Homes colored by Area (Urban/Not Populous)----

g2 <- ggplot(data = condo_homes, 
             aes(x=as.Date(period_end, "%m/%d/%Y"), y=median_sale_price)) +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %y") +
  geom_blank() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Condo/Co-op Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")

for (i in 1:length(coastal_urban_counties)){
  county_i <- coastal_urban_counties[i]
  county_i_data <- condo_homes %>% filter(region == county_i)
  g2 <- g2 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Coastal Urban"))
}


for (i in 1:length(coastal_not_populous_counties)){
  county_i <- coastal_not_populous_counties[i]
  county_i_data <- condo_homes %>% filter(region == county_i)
  g2 <- g2 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Coastal Not Populous"))
}


for (i in 1:length(inland_urban_counties)){
  county_i <- inland_urban_counties[i]
  county_i_data <- condo_homes %>% filter(region == county_i)
  g2 <- g2 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Inland Urban"))
}


for (i in 1:length(inland_not_populous_counties)){
  county_i <- inland_not_populous_counties[i]
  county_i_data <- condo_homes %>% filter(region == county_i)
  g2 <- g2 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Inland Not Populous"))
}


g2 <- g2 + scale_colour_manual(name='Area Type',
                               values=colors)

print(g2)


#----Plotting Median Sale Prices for Multi-Family Homes colored by Area (Urban/Not Populous)----
g3 <- ggplot(data = multi_family_homes, 
             aes(x=as.Date(period_end, "%m/%d/%Y"), y=median_sale_price)) +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %y") +
  geom_blank() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")

for (i in 1:length(coastal_urban_counties)){
  county_i <- coastal_urban_counties[i]
  county_i_data <- multi_family_homes %>% filter(region == county_i)
  g3 <- g3 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Coastal Urban"))
}


for (i in 1:length(coastal_not_populous_counties)){
  county_i <- coastal_not_populous_counties[i]
  county_i_data <- multi_family_homes %>% filter(region == county_i)
  g3 <- g3 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Coastal Not Populous"))
}


for (i in 1:length(inland_urban_counties)){
  county_i <- inland_urban_counties[i]
  county_i_data <- multi_family_homes %>% filter(region == county_i)
  g3 <- g3 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Inland Urban"))
}


for (i in 1:length(inland_not_populous_counties)){
  county_i <- inland_not_populous_counties[i]
  county_i_data <- multi_family_homes %>% filter(region == county_i)
  g3 <- g3 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Inland Not Populous"))
}


g3 <- g3 + scale_colour_manual(name='Area Type',
                               values=colors)

print(g3)


#----Plotting Median Sale Prices for Townhouses colored by Area (Urban/Not Populous)----
g4 <- ggplot(data = townhouses, 
             aes(x=as.Date(period_end, "%m/%d/%Y"), y=median_sale_price)) +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %y") +
  geom_blank() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")

for (i in 1:length(coastal_urban_counties)){
  county_i <- coastal_urban_counties[i]
  county_i_data <- townhouses %>% filter(region == county_i)
  g4 <- g4 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Coastal Urban"))
}


for (i in 1:length(coastal_not_populous_counties)){
  county_i <- coastal_not_populous_counties[i]
  county_i_data <- townhouses %>% filter(region == county_i)
  g4 <- g4 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Coastal Not Populous"))
}


for (i in 1:length(inland_urban_counties)){
  county_i <- inland_urban_counties[i]
  county_i_data <- townhouses %>% filter(region == county_i)
  g4 <- g4 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Inland Urban"))
}


for (i in 1:length(inland_not_populous_counties)){
  county_i <- inland_not_populous_counties[i]
  county_i_data <- townhouses %>% filter(region == county_i)
  g4 <- g4 + geom_line(data = county_i_data, 
                       aes(x=as.Date(period_end, "%m/%d/%Y"), 
                           y=median_sale_price,
                           colour = "Inland Not Populous"))
}


g4 <- g4 + scale_colour_manual(name='Area Type',
                               values=colors)

print(g4)


#----Plotting Median Sale Prices per Property Type colored by Area (THE SHORT WAY)----

coastal_urban$area_type <- "coastal_urban"
coastal_not_populous$area_type <- "coastal_not_populous"
inland_urban$area_type <- "inland_urban"
inland_not_populous$area_type <- "inland_not_populous"

combined_data <- rbind(coastal_urban, coastal_not_populous, 
                       inland_urban, inland_not_populous)

combined_data_new <- combined_data[order(combined_data$period_end),]

single_family_new <- combined_data_new %>% 
                     filter(property_type=="Single Family Residential")

ggplot(single_family_new, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


condo_new <- combined_data_new %>% 
             filter(property_type=="Condo/Co-op")

ggplot(condo_new, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


multi_family_new <- combined_data_new %>% 
                    filter(property_type=="Multi-Family (2-4 Unit)")

ggplot(multi_family_new, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


townhomes_new <- combined_data_new %>% 
                 filter(property_type=="Townhouse")

ggplot(townhomes_new, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


# Remove any observations that sold less than 5 homes and replot the spaghetti
# plots 

CA_data_new <- combined_data_new %>% filter(homes_sold >= 3)
CA_data_new <- data_new %>% filter(property_type != "Townhouse" & homes_sold >= 5 |
                                  property_type == "Townhouse")
save(data_new, file = "CA_data_new.Rdata")


single_family_new2 <- CA_data_new %>% 
  filter(property_type=="Single Family Residential")

ggplot(single_family_new2, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")

condo_new2 <- CA_data_new %>% 
  filter(property_type=="Condo/Co-op")

ggplot(condo_new2, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


multi_family_new2 <- CA_data_new %>% 
  filter(property_type=="Multi-Family (2-4 Unit)")

ggplot(multi_family_new2, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


townhomes_new2 <- CA_data_new %>% 
  filter(property_type=="Townhouse")

ggplot(townhomes_new2, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


townhomes_sf <- data_new %>% 
  filter(property_type=="Townhouse" & region == "San Francisco County, CA")

ggplot(townhomes_sf, 
       aes(x = homes_sold, 
           y = median_sale_price)) + geom_point() + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Homes Sold for Townhomes",
       x = "Homes Sold", y = "Median Sale Prices")






#----Plotting Median Sale Prices per Property Type colored by Area (THE SHORT WAY)----

single_family_orig <- CA_data_new_orig %>% 
  filter(property_type=="Single Family Residential")

CA_spaghetti_plot_area_type_sfh_raw <- ggplot(single_family_orig, 
                           aes(x = as.Date(period_end, "%m/%d/%Y"), 
                               y = median_sale_price, 
                               group = region, 
                              colour = area_type)) + geom_line() + 
    scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
            theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(#title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_spaghetti_plot_area_type_sfh_raw.pdf",
       CA_spaghetti_plot_area_type_sfh_raw,
       width=5.85, height=4.39,units="in")

condo_orig <- CA_data_new_orig %>% 
  filter(property_type=="Condo/Co-op")

CA_spaghetti_plot_area_type_condo_raw <- ggplot(condo_orig, 
                   aes(x = as.Date(period_end, "%m/%d/%Y"), 
                       y = median_sale_price, 
                       group = region, 
                       colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
          theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(#title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_spaghetti_plot_area_type_condo_raw.pdf",
       CA_spaghetti_plot_area_type_condo_raw,
       width=5.85, height=4.39,units="in")

multi_family_orig <- CA_data_new_orig %>% 
  filter(property_type=="Multi-Family (2-4 Unit)")

ggplot(multi_family_orig, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


townhomes_orig <- CA_data_new_orig %>% 
  filter(property_type=="Townhouse")

CA_spaghetti_plot_area_type_th_raw <- ggplot(townhomes_orig, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(#title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_spaghetti_plot_area_type_th_raw.pdf",
       CA_spaghetti_plot_area_type_th_raw,
       width=5.85, height=4.39,units="in")

# Remove any observations that sold less than 5 homes and replot the spaghetti
# plots 

single_family_new <- CA_data_new_2 %>% 
  filter(property_type=="Single Family Residential")

CA_spaghetti_plot_area_type_sfh_clean <- ggplot(single_family_new, 
                        aes(x = as.Date(period_end, "%m/%d/%Y"), 
                            y = median_sale_price, 
                           group = region, 
                           colour = area_type)) + geom_line() + 
 scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
         theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(#title = "Median Sale Prices v Time for Single Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_spaghetti_plot_area_type_sfh_clean.pdf",
       CA_spaghetti_plot_area_type_sfh_clean,
       width=5.85, height=4.39,units="in")

condo_new <- CA_data_new_2 %>% 
  filter(property_type=="Condo/Co-op")

CA_spaghetti_plot_area_type_condo_clean <- ggplot(condo_new, 
                  aes(x = as.Date(period_end, "%m/%d/%Y"), 
                      y = median_sale_price, 
                      group = region, 
                      colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(#title = "Median Sale Prices v Time for Condo/Co-op",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_spaghetti_plot_area_type_condo_clean.pdf",
       CA_spaghetti_plot_area_type_condo_clean,
       width=5.85, height=4.39,units="in")


multi_family_new <- CA_data_new_2 %>% 
  filter(property_type=="Multi-Family (2-4 Unit)")

ggplot(multi_family_new, 
       aes(x = as.Date(period_end, "%m/%d/%Y"), 
           y = median_sale_price, 
           group = region, 
           colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Time for Multi-Family Homes",
       x = "Time (Per 3 months)", y = "Median Sale Prices")


townhomes_new <- CA_data_new_2 %>% 
  filter(property_type=="Townhouse")

CA_spaghetti_plot_area_type_th_clean <- ggplot(townhomes_new, 
                   aes(x = as.Date(period_end, "%m/%d/%Y"), 
                       y = median_sale_price, 
                       group = region, 
                       colour = area_type)) + geom_line() + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") + 
     theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(#title = "Median Sale Prices v Time for Townhomes",
       x = "Time (Per 3 months)", y = "Median Sale Prices") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_spaghetti_plot_area_type_th_clean.pdf",
       CA_spaghetti_plot_area_type_th_clean,
       width=5.85, height=4.39,units="in")


townhomes_sf <- data_new %>% 
  filter(property_type=="Townhouse" & region == "San Francisco County, CA")

ggplot(townhomes_sf, 
       aes(x = homes_sold, 
           y = median_sale_price)) + geom_point() + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Median Sale Prices v Homes Sold for Townhomes",
       x = "Homes Sold", y = "Median Sale Prices")