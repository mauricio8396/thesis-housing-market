library(broom)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(classInt)
library(RColorBrewer)
library(maps)
load("CA_HOMES")
load("CA_county_boundaries")
load("CA_counties")
load("CA_data_new") 
load("CA_data_new_2")
CA_cty_bound_geom <- st_geometry(CA_county_boundaries)

county_index <- c()
for (i in 1:length(CA_counties)){
  county_index[i] <- which(CA_county_boundaries$V13==CA_counties[i])
}

#----Plotting Single Family Residential Median Sale Prices onto CA Map----

min_median_sale_price_c <- min(CA_HOMES$median_sale_price[CA_HOMES$property_type == "Condo/Co-op"])
max_median_sale_price_c <- max(CA_HOMES$median_sale_price[CA_HOMES$property_type == "Condo/Co-op"])

color_c <- (CA_HOMES$median_sale_price[CA_HOMES$property_type == "Condo/Co-op"] 
            - min_median_sale_price_c)/
  (max_median_sale_price_c - min_median_sale_price_c)*255

min_median_sale_price_m <- min(CA_HOMES$median_sale_price[CA_HOMES$property_type == "Multi-Family (2-4 Unit)"])
max_median_sale_price_m <- max(CA_HOMES$median_sale_price[CA_HOMES$property_type == "Multi-Family (2-4 Unit)"])

color_m <- (CA_HOMES$median_sale_price[CA_HOMES$property_type == "Multi-Family (2-4 Unit)"] 
            - min_median_sale_price_m)/
  (max_median_sale_price_m - min_median_sale_price_m)*255

single_family_index_sfr <- which(CA_HOMES$property_type == "Single Family Residential")

#for (i in 1:length(single_family_index)){
 # CA_HOMES[single_family_index[i],60] <- color[i]
  
#}

feb_2012 <- CA_HOMES %>% filter(period_end == "2/29/2012")

single_family_index <- which(feb_2012$property_type == "Single Family Residential")

min_median_sale_price_s <- min(feb_2012 %>% 
                                 filter(property_type == "Single Family Residential") %>%
                                 select(median_sale_price))
max_median_sale_price_s <- max(feb_2012 %>%
                                 filter(property_type == "Single Family Residential") %>%
                                 select(median_sale_price))

color_s <- ((feb_2012 %>% 
               filter(property_type == "Single Family Residential") %>%
               select(median_sale_price))
            - min_median_sale_price_s)/
  (max_median_sale_price_s - min_median_sale_price_s)*255

labels<-cbind(CA_county_boundaries,
              st_coordinates(st_centroid(CA_county_boundaries$geometry))) 
#get the x and y for the names

p_s = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black") +
  ggtitle("Geographical Plot of Single Family Homes in 2012 filled by Median Sale price")

for (n in 1:length(single_family_index)) {
  for (i in 1:41) {
    if (feb_2012$region[single_family_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      p_s <- p_s + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                           fill = rgb(color_s[n,1],
                                      0,
                                      255-color_s[n,1],
                                      max=255)) 
    }
  }
}

p_s <- p_s + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)

print(p_s)


dec_2021 <- CA_HOMES %>% filter(period_end == "12/31/2021")

single_family_index <- which(dec_2021$property_type == "Single Family Residential")

min_median_sale_price_s2 <- min(dec_2021 %>% 
                                  filter(property_type == "Single Family Residential") %>%
                                  select(median_sale_price))
max_median_sale_price_s2 <- max(dec_2021 %>%
                                  filter(property_type == "Single Family Residential") %>%
                                  select(median_sale_price))

color_s2 <- ((dec_2021 %>% 
                filter(property_type == "Single Family Residential") %>%
                select(median_sale_price))
             - min_median_sale_price_s2)/
  (max_median_sale_price_s2 - min_median_sale_price_s2)*255

p_s2 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(single_family_index)) {
  for (i in 1:41) {
    if (dec_2021$region[single_family_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      p_s2 <- p_s2 + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color_s2[n,1],
                                        0,
                                        255-color_s2[n,1],
                                        max=255)) 
    }
  }
}

p_s2 <- p_s2 + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)
print(p_s2)

feb_2012_s <- feb_2012 %>% 
  filter(property_type == "Single Family Residential")

dec_2021_s <- dec_2021 %>% 
  filter(property_type == "Single Family Residential")

old_median_sale_price_s <- feb_2012 %>% 
  filter(property_type == "Single Family Residential") %>%
  select(median_sale_price)

new_median_sale_price_s <- dec_2021 %>% 
  filter(property_type == "Single Family Residential") %>%
  select(median_sale_price)

values_matched <- feb_2012 %>% 
  filter(property_type == "Single Family Residential") %>% 
  select(region,median_sale_price)

dec_2021_mat <- dec_2021 %>% 
  filter(property_type == "Single Family Residential") %>%
  select(region,median_sale_price)

for (i in 1:nrow(feb_2012_s)){
  for (n in 1:nrow(dec_2021_s)){
    if (values_matched$region[i] == dec_2021_mat$region[n]){
      values_matched[i,3] <- dec_2021_mat$median_sale_price[n]
    }}}

perc_change <- (values_matched$V3 - values_matched$median_sale_price)/
  values_matched$median_sale_price*100

color_s3 <- (perc_change - min(perc_change))/(max(perc_change) - min(perc_change))*255

county_index2 <- c()
for (i in 1:38){
  county_index2[i] <- which(CA_county_boundaries$V13==values_matched[i,1])
}

p_s3 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:length(color_s3)) {
  p_s3 <- p_s3 + geom_sf(data = CA_cty_bound_geom[county_index2[i]], 
                         fill = rgb(color_s3[i],0,255-color_s3[i],max=255)) 
}

p_s3 <- p_s3 + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)
print(p_s3)

#----Plotting Condo/Co-op Median Sale Prices onto CA Map----

condo_index <- which(feb_2012$property_type == "Condo/Co-op")

min_median_sale_price_c <- min(feb_2012 %>% 
                                 filter(property_type == "Condo/Co-op") %>%
                                 select(median_sale_price))
max_median_sale_price_c <- max(feb_2012 %>%
                                 filter(property_type == "Condo/Co-op") %>%
                                 select(median_sale_price))

color_c <- ((feb_2012 %>% 
               filter(property_type == "Condo/Co-op") %>%
               select(median_sale_price))
            - min_median_sale_price_c)/
  (max_median_sale_price_c - min_median_sale_price_c)*255

p_c = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(condo_index)) {
  for (i in 1:41) {
    if (feb_2012$region[condo_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      p_c <- p_c + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                           fill = rgb(color_c[n,1],
                                      0,
                                      255-color_c[n,1],
                                      max=255)) 
    }
  }
}

p_c <- p_c + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)
print(p_c)



condo_index <- which(dec_2021$property_type == "Condo/Co-op")

min_median_sale_price_c2 <- min(dec_2021 %>% 
                                  filter(property_type == "Condo/Co-op") %>%
                                  select(median_sale_price))
max_median_sale_price_c2 <- max(dec_2021 %>%
                                  filter(property_type == "Condo/Co-op") %>%
                                  select(median_sale_price))

color_c2 <- ((dec_2021 %>% 
                filter(property_type == "Condo/Co-op") %>%
                select(median_sale_price))
             - min_median_sale_price_c2)/
  (max_median_sale_price_c2 - min_median_sale_price_c2)*255

p_c2 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(condo_index)) {
  for (i in 1:41) {
    if (dec_2021$region[condo_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      p_c2 <- p_c2 + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color_c2[n,1],
                                        0,
                                        255-color_c2[n,1],
                                        max=255)) 
    }
  }
}

p_c2 <- p_c2 + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)

print(p_c2)

feb_2012_c <- feb_2012 %>% 
  filter(property_type == "Condo/Co-op")

dec_2021_c <- dec_2021 %>% 
  filter(property_type == "Condo/Co-op")

old_median_sale_price_c <- feb_2012 %>% 
  filter(property_type == "Condo/Co-op") %>%
  select(median_sale_price)

new_median_sale_price_c <- dec_2021 %>% 
  filter(property_type == "Condo/Co-op") %>%
  select(median_sale_price)

values_matched <- feb_2012 %>% 
  filter(property_type == "Condo/Co-op") %>% 
  select(region,median_sale_price)

dec_2021_mat <- dec_2021 %>% 
  filter(property_type == "Condo/Co-op") %>%
  select(region,median_sale_price)

for (i in 1:nrow(feb_2012_c)){
  for (n in 1:nrow(dec_2021_c)){
    if (values_matched$region[i] == dec_2021_mat$region[n]){
      values_matched[i,3] <- dec_2021_mat$median_sale_price[n]
    }}}

values_matched <- na.omit(values_matched)

perc_change <- (values_matched$V3 - values_matched$median_sale_price)/
  values_matched$median_sale_price*100

color_c3 <- (perc_change - min(perc_change))/(max(perc_change) - min(perc_change))*255

county_index2 <- c()
for (i in 1:length(values_matched[,1])){
  county_index2[i] <- which(CA_county_boundaries$V13==values_matched[i,1])
}


p_c3 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:length(color_c3)) {
  p_c3 <- p_c3 + geom_sf(data = CA_cty_bound_geom[county_index2[i]], 
                         fill = rgb(color_c3[i],0,255-color_c3[i],max=255)) 
}

p_c3 <- p_c3 + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                         size = 2.75)
print(p_c3)

#----Plotting Multi Family Residential Median Sale Prices onto CA Map----

multi_family_index <- which(feb_2012$property_type == "Multi-Family (2-4 Unit)")

min_median_sale_price_m <- min(feb_2012 %>% 
                                 filter(property_type == "Multi-Family (2-4 Unit)") %>%
                                 select(median_sale_price))
max_median_sale_price_m <- max(feb_2012 %>%
                                 filter(property_type == "Multi-Family (2-4 Unit)") %>%
                                 select(median_sale_price))

color_m <- ((feb_2012 %>% 
               filter(property_type == "Multi-Family (2-4 Unit)") %>%
               select(median_sale_price))
            - min_median_sale_price_m)/
  (max_median_sale_price_m - min_median_sale_price_m)*255

p_m = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(multi_family_index)) {
  for (i in 1:41) {
    if (feb_2012$region[multi_family_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      p_m <- p_m + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                           fill = rgb(color_m[n,1],
                                      0,
                                      255-color_m[n,1],
                                      max=255)) 
    }
  }
}

p_m <- p_m + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)
print(p_m)



multi_family_index <- which(dec_2021$property_type == "Multi-Family (2-4 Unit)")

min_median_sale_price_m2 <- min(dec_2021 %>% 
                                  filter(property_type == "Multi-Family (2-4 Unit)") %>%
                                  select(median_sale_price))
max_median_sale_price_m2 <- max(dec_2021 %>%
                                  filter(property_type == "Multi-Family (2-4 Unit)") %>%
                                  select(median_sale_price))

color_m2 <- ((dec_2021 %>% 
                filter(property_type == "Multi-Family (2-4 Unit)") %>%
                select(median_sale_price))
             - min_median_sale_price_m2)/
  (max_median_sale_price_m2 - min_median_sale_price_m2)*255

p_m2 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(multi_family_index)) {
  for (i in 1:41) {
    if (dec_2021$region[multi_family_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      p_m2 <- p_m2 + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color_m2[n,1],
                                        0,
                                        255-color_m2[n,1],
                                        max=255)) 
    }
  }
}

p_m2 <- p_m2 + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)

print(p_m2)


feb_2012_m <- feb_2012 %>% 
  filter(property_type == "Multi-Family (2-4 Unit)")

dec_2021_m <- dec_2021 %>% 
  filter(property_type == "Multi-Family (2-4 Unit)")

old_median_sale_price_m <- feb_2012 %>% 
  filter(property_type == "Multi-Family (2-4 Unit)") %>%
  select(median_sale_price)

new_median_sale_price_m <- dec_2021 %>% 
  filter(property_type == "Multi-Family (2-4 Unit)") %>%
  select(median_sale_price)

values_matched <- feb_2012 %>% 
  filter(property_type == "Multi-Family (2-4 Unit)") %>% 
  select(region,median_sale_price)

dec_2021_mat <- dec_2021 %>% 
  filter(property_type == "Multi-Family (2-4 Unit)") %>%
  select(region,median_sale_price)

for (i in 1:nrow(feb_2012_m)){
  for (n in 1:nrow(dec_2021_m)){
    if (values_matched$region[i] == dec_2021_mat$region[n]){
      values_matched[i,3] <- dec_2021_mat$median_sale_price[n]
    }}}

values_matched <- na.omit(values_matched)

perc_change <- (values_matched$V3 - values_matched$median_sale_price)/
  values_matched$median_sale_price*100

color_m3 <- (perc_change - min(perc_change))/(max(perc_change) - min(perc_change))*255

county_index2 <- c()
for (i in 1:length(values_matched[,1])){
  county_index2[i] <- which(CA_county_boundaries$V13==values_matched[i,1])
}

p_m3 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:length(color_m3)) {
  p_m3 <- p_m3 + geom_sf(data = CA_cty_bound_geom[county_index2[i]], 
                         fill = rgb(color_m3[i],0,255-color_m3[i],max=255)) 
}

p_m3 <- p_m3 + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)

print(p_m3)

#----Plotting Townhouse Median Sale Prices onto CA Map----

townhouse_index <- which(feb_2012$property_type == "Townhouse")

min_median_sale_price_t <- min(feb_2012 %>% 
                                 filter(property_type == "Townhouse") %>%
                                 select(median_sale_price))
max_median_sale_price_t <- max(feb_2012 %>%
                                 filter(property_type == "Townhouse") %>%
                                 select(median_sale_price))

color_t <- ((feb_2012 %>% 
               filter(property_type == "Townhouse") %>%
               select(median_sale_price))
            - min_median_sale_price_t)/
  (max_median_sale_price_t - min_median_sale_price_t)*255

p_t = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(townhouse_index)) {
  for (i in 1:41) {
    if (feb_2012$region[townhouse_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      p_t <- p_t + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                           fill = rgb(color_t[n,1],
                                      0,
                                      255-color_t[n,1],
                                      max=255)) 
    }
  }
}

p_t <- p_t + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)

print(p_t)



townhouse_index <- which(dec_2021$property_type == "Townhouse")

min_median_sale_price_t2 <- min(dec_2021 %>% 
                                  filter(property_type == "Townhouse") %>%
                                  select(median_sale_price))
max_median_sale_price_t2 <- max(dec_2021 %>%
                                  filter(property_type == "Townhouse") %>%
                                  select(median_sale_price))

color_t2 <- ((dec_2021 %>% 
                filter(property_type == "Townhouse") %>%
                select(median_sale_price))
             - min_median_sale_price_t2)/
  (max_median_sale_price_t2 - min_median_sale_price_t2)*255

p_t2 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(townhouse_index)) {
  for (i in 1:41) {
    if (dec_2021$region[townhouse_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      p_t2 <- p_t2 + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color_t2[n,1],
                                        0,
                                        255-color_t2[n,1],
                                        max=255)) 
    }
  }
}

p_t2 <- p_t2 + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)

print(p_t2)


feb_2012_t <- feb_2012 %>% 
  filter(property_type == "Townhouse")

dec_2021_t <- dec_2021 %>% 
  filter(property_type == "Townhouse")

old_median_sale_price_t <- feb_2012 %>% 
  filter(property_type == "Townhouse") %>%
  select(median_sale_price)

new_median_sale_price_t <- dec_2021 %>% 
  filter(property_type == "Townhouse") %>%
  select(median_sale_price)

values_matched <- feb_2012 %>% 
  filter(property_type == "Townhouse") %>% 
  select(region,median_sale_price)

dec_2021_mat <- dec_2021 %>% 
  filter(property_type == "Townhouse") %>%
  select(region,median_sale_price)

for (i in 1:nrow(feb_2012_t)){
  for (n in 1:nrow(dec_2021_t)){
    if (values_matched$region[i] == dec_2021_mat$region[n]){
      values_matched[i,3] <- dec_2021_mat$median_sale_price[n]
    }}}

values_matched <- na.omit(values_matched)

perc_change <- (values_matched$V3 - values_matched$median_sale_price)/
  values_matched$median_sale_price*100

color_t3 <- (perc_change - min(perc_change))/(max(perc_change) - min(perc_change))*255

county_index2 <- c()
for (i in 1:length(values_matched[,1])){
  county_index2[i] <- which(CA_county_boundaries$V13==values_matched[i,1])
}

p_t3 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:length(color_t3)) {
  p_t3 <- p_t3 + geom_sf(data = CA_cty_bound_geom[county_index2[i]], 
                         fill = rgb(color_t3[i],0,255-color_t3[i],max=255)) 
}

p_t3 <- p_t3 + geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),
                       size = 2.75)

print(p_t3)


# RAW DATA PLOTTING
#----Plotting Single Fam Median Sale Prices (SHORT WAY)----
single_family_residential <- feb_2012 %>% 
  filter(property_type == "Single Family Residential")

CA_counties_s <- sort(unique(single_family_residential$region))
county_index_s <- c()
for (i in 1:length(CA_counties_s)){
  county_index_s[i] <- which(CA_county_boundaries$V13==CA_counties_s[i])
}

for (i in 1:length(single_family_residential$period_end)){
  CA_county_boundaries[county_index_s[i],14] <- single_family_residential %>%
    filter(region == CA_county_boundaries$V13[county_index_s[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
CA_shp_price_single_family_2012_plot <- ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Single Family Homes in 2012 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price)),
                               200000,300000,400000,500000,600000,
                               max(na.omit(CA_county_boundaries$median_sale_price))))

ggsave("CA_shp_price_single_family_2012_plot.pdf",
       CA_shp_price_single_family_2012_plot,
       width=13.5, height=9)

single_family_residential2 <- dec_2021 %>% 
  filter(property_type == "Single Family Residential")

CA_counties_s2 <- sort(unique(single_family_residential2$region))
county_index_s2 <- c()
for (i in 1:length(CA_counties_s2)){
  county_index_s2[i] <- which(CA_county_boundaries$V13==CA_counties_s2[i])
}

for (i in 1:length(single_family_residential2$period_end)){
  CA_county_boundaries[county_index_s2[i],15] <- single_family_residential2 %>%
    filter(region == CA_county_boundaries$V13[county_index_s2[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
CA_shp_price_single_family_2021_plot <- ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.1),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Single Family Homes in 2021 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.1)),
                               600000,900000,1200000,1500000,
                               max(na.omit(CA_county_boundaries$median_sale_price.1))))

ggsave("CA_shp_price_single_family_2021_plot.pdf",
       CA_shp_price_single_family_2021_plot,
       width=13.5, height=9)

CA_county_boundaries$perc_change_s <- (CA_county_boundaries$median_sale_price.1 - 
                    CA_county_boundaries$median_sale_price)/
                  CA_county_boundaries$median_sale_price*100

options(scipen = 999)
CA_shp_percent_change_single_family_plot <- ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=perc_change_s),
          size = 3, 
          color = "black") +
  ggtitle("Percent Change of Single Family Homes (2012,2021)") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Percent Change")

ggsave("CA_shp_percent_change_single_family_plot.pdf",
       CA_shp_percent_change_single_family_plot,
       width=13.5, height=9)

#----Plotting Condo Median Sale Prices (SHORT WAY)----
condos <- feb_2012 %>% 
  filter(property_type == "Condo/Co-op")

CA_counties_c <- sort(unique(condos$region))
county_index_c <- c()
for (i in 1:length(CA_counties_c)){
  county_index_c[i] <- which(CA_county_boundaries$V13==CA_counties_c[i])
}

for (i in 1:length(condos$period_end)){
  CA_county_boundaries[county_index_c[i],17] <- condos %>%
    filter(region == CA_county_boundaries$V13[county_index_c[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.2),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Condo/Co-op in 2012 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.2)),
                               180000,360000,540000,
                               max(na.omit(CA_county_boundaries$median_sale_price.2))))


condos2 <- dec_2021 %>% 
  filter(property_type == "Condo/Co-op")

CA_counties_c2 <- sort(unique(condos2$region))
county_index_c2 <- c()
for (i in 1:length(CA_counties_c2)){
  county_index_c2[i] <- which(CA_county_boundaries$V13==CA_counties_c2[i])
}

for (i in 1:length(condos2$period_end)){
  CA_county_boundaries[county_index_c2[i],18] <- condos2 %>%
    filter(region == CA_county_boundaries$V13[county_index_c2[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.3),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Condo/Co-op in 2021 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                     breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.3)),
                               500000,750000,1000000,
                              max(na.omit(CA_county_boundaries$median_sale_price.3))))

CA_county_boundaries$perc_change_c <- (CA_county_boundaries$median_sale_price.3 - 
                                         CA_county_boundaries$median_sale_price.2)/
                                       CA_county_boundaries$median_sale_price.2*100

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=perc_change_c),
          size = 3, 
          color = "black") +
  ggtitle("Percent Change of Condo/Co-op (2012,2021)") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Percent Change")



#----Plotting Multi Fam Median Sale Prices (SHORT WAY)----
multi_family_residential <- feb_2012 %>% 
  filter(property_type == "Multi-Family (2-4 Unit)")

CA_counties_m <- sort(unique(multi_family_residential$region))
county_index_m <- c()
for (i in 1:length(CA_counties_m)){
  county_index_m[i] <- which(CA_county_boundaries$V13==CA_counties_m[i])
}

for (i in 1:length(multi_family_residential$period_end)){
  CA_county_boundaries[county_index_m[i],20] <- multi_family_residential %>%
    filter(region == CA_county_boundaries$V13[county_index_m[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.4),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Multi Family Homes in 2012 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.4)),
                               250000,500000,750000,
                               max(na.omit(CA_county_boundaries$median_sale_price.4))))


multi_family_residential2 <- dec_2021 %>% 
  filter(property_type == "Multi-Family (2-4 Unit)")

CA_counties_m2 <- sort(unique(multi_family_residential2$region))
county_index_m2 <- c()
for (i in 1:length(CA_counties_m2)){
  county_index_m2[i] <- which(CA_county_boundaries$V13==CA_counties_m2[i])
}

for (i in 1:length(multi_family_residential2$period_end)){
  CA_county_boundaries[county_index_m2[i],21] <- multi_family_residential2 %>%
    filter(region == CA_county_boundaries$V13[county_index_m2[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.5),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Multi Family Homes in 2021 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.5)),
                               700000,1150000,1600000,
                               max(na.omit(CA_county_boundaries$median_sale_price.5))))

CA_county_boundaries$perc_change_m <- (CA_county_boundaries$median_sale_price.5 - 
                                         CA_county_boundaries$median_sale_price.4)/
                                       CA_county_boundaries$median_sale_price.4*100

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=perc_change_m),
          size = 3, 
          color = "black") +
  ggtitle("Percent Change of Multi Family Homes (2012,2021)") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Percent Change")

#----Plotting Townhouse Median Sale Prices (SHORT WAY)----
townhouse <- feb_2012 %>% 
  filter(property_type == "Townhouse")

CA_counties_t <- sort(unique(townhouse$region))
county_index_t <- c()
for (i in 1:length(CA_counties_t)){
  county_index_t[i] <- which(CA_county_boundaries$V13==CA_counties_t[i])
}

for (i in 1:length(townhouse$period_end)){
  CA_county_boundaries[county_index_t[i],23] <- townhouse %>%
    filter(region == CA_county_boundaries$V13[county_index_t[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.6),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Townhouses in 2012 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.6)),
                               225000,450000,675000,
                               max(na.omit(CA_county_boundaries$median_sale_price.6))))


townhouse2 <- dec_2021 %>% 
  filter(property_type == "Townhouse")

CA_counties_t2 <- sort(unique(townhouse2$region))
county_index_t2 <- c()
for (i in 1:length(CA_counties_t2)){
  county_index_t2[i] <- which(CA_county_boundaries$V13==CA_counties_t2[i])
}

for (i in 1:length(townhouse2$period_end)){
  CA_county_boundaries[county_index_t2[i],24] <- townhouse2 %>%
    filter(region == CA_county_boundaries$V13[county_index_t2[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.7),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Townhouses in 2021 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.7)),
                               500000,800000,1100000,
                               max(na.omit(CA_county_boundaries$median_sale_price.7))))

CA_county_boundaries$perc_change_t <- (CA_county_boundaries$median_sale_price.7 - 
                                         CA_county_boundaries$median_sale_price.6)/
                                       CA_county_boundaries$median_sale_price.6*100

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=perc_change_t),
          size = 3, 
          color = "black") +
  ggtitle("Percent Change of Townhomes (2012,2021)") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Percent Change")










# CLEAN DATA PLOTTING
#----Plotting Single Fam Median Sale Prices (SHORT WAY)----
single_family_residential3 <- CA_data_new_2 %>% 
  filter(property_type == "Single Family Residential") %>% 
  filter(period_end == "2/29/2012")

CA_counties_s <- sort(unique(single_family_residential3$region))
county_index_s <- c()
for (i in 1:length(CA_counties_s)){
  county_index_s[i] <- which(CA_county_boundaries$V13==CA_counties_s[i])
}

for (i in 1:length(single_family_residential3$period_end)){
  CA_county_boundaries[county_index_s[i],27] <- single_family_residential3 %>%
    filter(region == CA_county_boundaries$V13[county_index_s[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.8),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Single Family Homes in 2012 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.8)),
                               200000,300000,400000,500000,600000,
                               max(na.omit(CA_county_boundaries$median_sale_price.8))))


single_family_residential4 <- CA_data_new_2 %>% 
  filter(property_type == "Single Family Residential") %>% 
  filter(period_end == "12/31/2012")

CA_counties_s2 <- sort(unique(single_family_residential4$region))
county_index_s2 <- c()
for (i in 1:length(CA_counties_s2)){
  county_index_s2[i] <- which(CA_county_boundaries$V13==CA_counties_s2[i])
}

for (i in 1:length(single_family_residential4$period_end)){
  CA_county_boundaries[county_index_s2[i],28] <- single_family_residential4 %>%
    filter(region == CA_county_boundaries$V13[county_index_s2[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.9),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Single Family Homes in 2021 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.9)),
                               300000,500000,700000,
                               max(na.omit(CA_county_boundaries$median_sale_price.9))))

CA_county_boundaries$perc_change_s2 <- (CA_county_boundaries$median_sale_price.9 - 
                                         CA_county_boundaries$median_sale_price.8)/
  CA_county_boundaries$median_sale_price.8*100

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=perc_change_s2),
          size = 3, 
          color = "black") +
  ggtitle("Percent Change of Single Family Homes (2012,2021)") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Percent Change")

#----Plotting Condo Median Sale Prices (SHORT WAY)----
condos3 <- CA_data_new_2 %>% 
  filter(property_type == "Condo/Co-op") %>% 
  filter(period_end == "2/29/2012")

CA_counties_c <- sort(unique(condos3$region))
county_index_c <- c()
for (i in 1:length(CA_counties_c)){
  county_index_c[i] <- which(CA_county_boundaries$V13==CA_counties_c[i])
}

for (i in 1:length(condos3$period_end)){
  CA_county_boundaries[county_index_c[i],30] <- condos3 %>%
    filter(region == CA_county_boundaries$V13[county_index_c[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.10),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Condo/Co-op in 2012 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.10)),
                               200000,360000,500000,
                               max(na.omit(CA_county_boundaries$median_sale_price.10))))


condos4 <- CA_data_new_2 %>% 
  filter(property_type == "Condo/Co-op") %>% 
  filter(period_end == "12/31/2012")

CA_counties_c2 <- sort(unique(condos4$region))
county_index_c2 <- c()
for (i in 1:length(CA_counties_c2)){
  county_index_c2[i] <- which(CA_county_boundaries$V13==CA_counties_c2[i])
}

for (i in 1:length(condos4$period_end)){
  CA_county_boundaries[county_index_c2[i],31] <- condos4 %>%
    filter(region == CA_county_boundaries$V13[county_index_c2[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.11),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Condo/Co-op in 2021 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.11)),
                               225000,400000,575000,
                               max(na.omit(CA_county_boundaries$median_sale_price.11))))

CA_county_boundaries$perc_change_c2 <- (CA_county_boundaries$median_sale_price.11 - 
                                         CA_county_boundaries$median_sale_price.10)/
  CA_county_boundaries$median_sale_price.10*100

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=perc_change_c2),
          size = 3, 
          color = "black") +
  ggtitle("Percent Change of Condo/Co-op (2012,2021)") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Percent Change")


#----Plotting Townhouse Median Sale Prices (SHORT WAY)----
townhouse3 <- CA_data_new_2 %>% 
  filter(property_type == "Townhouse") %>% 
  filter(period_end == "2/29/2012")

CA_counties_t <- sort(unique(townhouse3$region))
county_index_t <- c()
for (i in 1:length(CA_counties_t)){
  county_index_t[i] <- which(CA_county_boundaries$V13==CA_counties_t[i])
}

for (i in 1:length(townhouse3$period_end)){
  CA_county_boundaries[county_index_t[i],33] <- townhouse3 %>%
    filter(region == CA_county_boundaries$V13[county_index_t[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.12),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Townhouses in 2012 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.12)),
                               250000,450000,650000,
                               max(na.omit(CA_county_boundaries$median_sale_price.12))))


townhouse4 <- CA_data_new_2 %>% 
  filter(property_type == "Townhouse") %>% 
  filter(period_end == "12/31/2012")

CA_counties_t2 <- sort(unique(townhouse4$region))
county_index_t2 <- c()
for (i in 1:length(CA_counties_t2)){
  county_index_t2[i] <- which(CA_county_boundaries$V13==CA_counties_t2[i])
}

for (i in 1:length(townhouse4$period_end)){
  CA_county_boundaries[county_index_t2[i],34] <- townhouse4 %>%
    filter(region == CA_county_boundaries$V13[county_index_t2[i]]) %>%
    select(median_sale_price)
}

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=median_sale_price.13),
          size = 3, 
          color = "black") +
  ggtitle("Geographical Plot of Townhouses in 2021 filled by Median Sale price") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Median Sale Price",
                      breaks=c(min(na.omit(CA_county_boundaries$median_sale_price.13)),
                               250000,450000,650000,
                               max(na.omit(CA_county_boundaries$median_sale_price.13))))

CA_county_boundaries$perc_change_t2 <- (CA_county_boundaries$median_sale_price.13 - 
                                         CA_county_boundaries$median_sale_price.12)/
  CA_county_boundaries$median_sale_price.12*100

options(scipen = 999)
ggplot() + 
  geom_sf(data = CA_county_boundaries,
          aes(fill=perc_change_t2),
          size = 3, 
          color = "black") +
  ggtitle("Percent Change of Townhomes (2012,2021)") +
  geom_text(data = labels, aes(label = labels$NAME_PCASE, x=X, y=Y),colour="white",
            size = 2.75) +
  scale_fill_gradient(low="blue",high="red",
                      name="Percent Change")








