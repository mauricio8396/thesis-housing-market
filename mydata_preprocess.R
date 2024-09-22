library(dplyr)

CA_population <- read.csv("CA_population.csv")
CA_population_2 <- read.csv("CA_population_2.csv")
mydata <- read.csv("Redfin_Data_CA.csv")
View(mydata)

# Preprocess data here


CA_HOMES <- mydata
save(CA_HOMES, file = "CA_HOMES")

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
           region == "Sonoma County, CA" |
           region == "Solano County, CA" |
           region == "Napa County, CA" |
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

coastal_urban$area_type <- "coastal_urban"
coastal_not_populous$area_type <- "coastal_not_populous"
inland_urban$area_type <- "inland_urban"
inland_not_populous$area_type <- "inland_not_populous"

CA_urban_counties <- unique(coastal_urban$region)
CA_urban_npop_counties <- unique(coastal_not_populous$region)
CA_inland_counties <- unique(inland_urban$region)
CA_inland_npop_counties <- unique(inland_not_populous$region)

for (i in 1:length(coastal_urban_counties)) {
  CA_HOMES[CA_HOMES$region == coastal_urban_counties[i],]$area_type <- "coastal_urban"
}


CA_urban_index <- which(CA_HOMES$region == coastal_urban$region)

combined_data <- rbind(coastal_urban, coastal_not_populous, 
                       inland_urban, inland_not_populous)

combined_data_new <- combined_data[order(combined_data$period_end),]

CA_data_new <- combined_data_new %>% filter(homes_sold >= 5)
#CA_data_new <- data_new %>% filter(property_type != "Townhouse" & homes_sold >= 5 |
 #                                 property_type == "Townhouse")

save(CA_data_new, file = "CA_data_new.Rdata")

CA_data_new_2 <- CA_HOMES
CA_data_new_2 <- CA_data_new_2 %>% mutate(area_type =
                     case_when(region == "Orange County, CA" |
                                 region == "Los Angeles County, CA" |
                                 region == "San Francisco County, CA" |
                                 region == "Ventura County, CA" |
                                 region == "San Mateo County, CA" |
                                 region == "San Diego County, CA" |
                                 region == "Contra Costa County, CA" |
                                 region == "Alameda County, CA" |
                                 region == "Santa Clara County, CA" ~ "coastal_urban", 
                               
                               region == "San Luis Obispo County, CA" |
                                 region == "Monterey County, CA" |
                                 region == "Santa Barbara County, CA" |
                                 region == "Marin County, CA"|
                                 region == "Napa County, CA" |
                                 region == "Sonoma County, CA" |
                                 region == "Solano County, CA" |
                                 region == "Santa Cruz County, CA" ~ "coastal_not_populous",
                               
                               region == "Sacramento County, CA" |
                                 region == "Riverside County, CA" |
                                 region == "San Bernardino County, CA" |
                                 region == "San Joaquin County, CA" |
                                 region == "Stanislaus County, CA" |
                                 region == "Placer County, CA" |
                                 region == "Fresno County, CA" |
                                 region == "Kern County, CA" |
                                 region == "Merced County, CA" ~ "inland_urban",
                               
                               region == "San Benito County, CA" |
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
                                 region == "Yolo County, CA" ~ "inland_not_populous"))

CA_data_new_2 <- CA_data_new_2 %>% filter(homes_sold >= 5)
#CA_data_new_2 <- CA_data_new_2 %>% filter(property_type != "Townhouse" & homes_sold >= 5 |
 #                                    property_type == "Townhouse")

save(CA_data_new_2, file = "CA_data_new_2")


CA_data_new_orig <- CA_HOMES
CA_data_new_orig <- CA_data_new_orig %>% mutate(area_type =
                  case_when(region == "Orange County, CA" |
                            region == "Los Angeles County, CA" |
                            region == "San Francisco County, CA" |
                            region == "Ventura County, CA" |
                            region == "San Mateo County, CA" |
                            region == "San Diego County, CA" |
                            region == "Contra Costa County, CA" |
                            region == "Alameda County, CA" |
                            region == "Santa Clara County, CA" ~ "coastal_urban", 
                                                      
                            region == "San Luis Obispo County, CA" |
                            region == "Monterey County, CA" |
                            region == "Santa Barbara County, CA" |
                            region == "Marin County, CA"|
                            region == "Napa County, CA" |
                            region == "Sonoma County, CA" |
                            region == "Solano County, CA" |
                            region == "Santa Cruz County, CA" ~ "coastal_not_populous",
                                                      
                            region == "Sacramento County, CA" |
                            region == "Riverside County, CA" |
                            region == "San Bernardino County, CA" |
                            region == "San Joaquin County, CA" |
                            region == "Stanislaus County, CA" |
                            region == "Placer County, CA" |
                            region == "Fresno County, CA" |
                            region == "Kern County, CA" |
                            region == "Merced County, CA" ~ "inland_urban",
                                                      
                            region == "San Benito County, CA" |
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
                            region == "Yolo County, CA" ~ "inland_not_populous"))

save(CA_data_new_orig, file = "CA_data_new_orig")



CA_data_new_3 <- CA_data_new_2
CA_data_new_3$population <- NA
for (i in 1:length(CA_population$County)){
  for(n in 1:1662){
  if (CA_population$County[i] == CA_data_new_3$region[n]){
    CA_data_new_3$population[n] <- CA_population$X2012[i]
  }
 }
}

for (i in 1:length(CA_population$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2013"))){
    if (CA_population$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population$X2013[i]
    }
  }
}

for (i in 1:length(CA_population$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2014"))){
    if (CA_population$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population$X2014[i]
    }
  }
}

for (i in 1:length(CA_population$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2015"))){
    if (CA_population$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population$X2015[i]
    }
  }
}

for (i in 1:length(CA_population$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2016"))){
    if (CA_population$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population$X2016[i]
    }
  }
}

for (i in 1:length(CA_population$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2017"))){
    if (CA_population$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population$X2017[i]
    }
  }
}

for (i in 1:length(CA_population$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2018"))){
    if (CA_population$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population$X2018[i]
    }
  }
}

for (i in 1:length(CA_population$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2019"))){
    if (CA_population$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population$X2019[i]
    }
  }
}

for (i in 1:length(CA_population_2$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2020"))){
    if (CA_population_2$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population_2$X2020[i]
    }
  }
}

for (i in 1:length(CA_population_2$County)){
  for(n in which(endsWith(CA_data_new_3$period_end,"2021"))){
    if (CA_population_2$County[i] == CA_data_new_3$region[n]){
      CA_data_new_3$population[n] <- CA_population_2$X2021[i]
    }
  }
}

unique_months <- unique(CA_data_new_3$period_end)
CA_data_new_3$time_since <- NA

for (n in 1:length(unique_months)){
for (i in 1:length(CA_data_new_3$period_end)){
if (CA_data_new_3$period_end[i] == unique_months[n]){
  CA_data_new_3$time_since[i] <- n - 1
}
}
}


CA_data_new_3 <- CA_data_new_3 %>% filter(property_type != "Multi-Family (2-4 Unit)" &
                                            property_type != "All Residential")
                  

save(CA_data_new_3, file = "CA_data_new_3")

