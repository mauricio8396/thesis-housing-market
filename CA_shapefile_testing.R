load("CA_HOMES")

# ------------------------Plotting map of California----------------------------

#library(rgdal)
library(broom)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(classInt)
library(RColorBrewer)
library(maps)
library(stringr)

# plot coloring 

# meidan sale price for single family
# counties shaded by prices, colored by price (light to dark, warm to cool)
# or rainbow spectrum

# percent change in home values, 2010 to 2020, light to cool...which places
#grew the most
#wanna see this change per property type



CA_county_boundaries <- st_read(
  dsn = paste0(
    getwd(), "/data-3/")
)
CA_county_boundaries <- CA_county_boundaries[order(CA_county_boundaries$FMNAME_PC),]
CA_cty_coord <- data.frame(st_coordinates(CA_cty_bound_geom[19]))
CA_cty_coord_2 <- data.frame(st_coordinates(CA_cty_bound_geom[33]))

view(CA_county_boundaries)
CA_county_boundaries[,12] <- c(", CA")
CA_county_boundaries[,13] <- str_c(CA_county_boundaries$FMNAME_PC, 
                                   CA_county_boundaries$V12)

CA_cty_bound_geom <- st_geometry(CA_county_boundaries)


#----Test with regular plot()----
#plot(CA_county_boundaries)
par(mar = c(0,0,0,0))
plot(CA_cty_bound_geom)
plot(CA_cty_bound_geom[33], add=TRUE, col="red")
plot(CA_cty_bound_geom[19],col="green", add=TRUE)
# reset = FALSE: we want to add to a plot with a legend


CA_cty_bound_geom[1,1]

st_geometry_type(CA_county_boundaries)
st_crs(CA_county_boundaries)
st_bbox(CA_county_boundaries)


par(mar=c(0,0,0,0))
plot(CA_county_boundaries, col="#f2f2f2",bg="gray",
     lwd=0.5, border = 1)

county_mat <- as.numeric(county_mat[,2:6,])

#---------Test with GGPlot------




CA_HOMES$z <- as.numeric(factor(CA_HOMES$property_type, 
                                levels = c("All Residential", 
                                           "Condo/Co-op",
                                           "Multi-Family (2-4 Unit)", 
                                           "Single Family Residential", 
                                           "Townhouse")))

property_variety <- table(CA_HOMES$region, CA_HOMES$z)

county_index <- c()
for (i in 1:41){
  county_index[i] <- which(CA_county_boundaries$V13==CA_counties[i])
}

#----Scratch plotting Median Sale Prices onto CA Map----

p_1 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:41) {
  if (property_variety[i,1] >= 108) {
    p_1 <- p_1 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "green") 
  }
  if (property_variety[i,1] > 96 & property_variety[i,1] < 108) {
    p_1 <- p_1 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "light green") 
  }
  if (property_variety[i,1] > 48 & property_variety[i,1] < 96) {
    p_1 <- p_1 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "yellow")
  }
  if (property_variety[i,1] < 48) {
    p_1 <- p_1 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "red")
  }
  
}
print(p_1)

p_2 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:41) {
  if (property_variety[i,2] >= 108) {
    p_2 <- p_2 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "green") 
  }
  if (property_variety[i,2] > 96 & property_variety[i,2] < 108) {
    p_2 <- p_2 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "light green") 
  }
  if (property_variety[i,2] > 48 & property_variety[i,2] < 96) {
    p_2 <- p_2 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "yellow")
  }
  if (property_variety[i,2] < 48) {
    p_2 <- p_2 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "red")
  }
  
}
print(p_2)

p_3 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:41) {
  if (property_variety[i,3] >= 108) {
    p_3 <- p_3 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "green") 
  }
  if (property_variety[i,3] > 96 & property_variety[i,3] < 108) {
    p_3 <- p_3 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "light green") 
  }
  if (property_variety[i,3] > 48 & property_variety[i,3] < 96) {
    p_3 <- p_3 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "yellow")
  }
  if (property_variety[i,3] < 48) {
    p_3 <- p_3 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "red")
  }
  
}
print(p_3)

p_4 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:41) {
  if (property_variety[i,4] == 120) {
    p_4 <- p_4 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "dark green") 
  }
  if (property_variety[i,4] >= 108 & property_variety[i,4] < 120) {
    p_4 <- p_4 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "green") 
  }
  if (property_variety[i,4] > 96 & property_variety[i,4] < 108) {
    p_4 <- p_4 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "light green") 
  }
  if (property_variety[i,4] > 48 & property_variety[i,4] < 96) {
    p_4 <- p_4 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "yellow")
  }
  if (property_variety[i,4] < 48) {
    p_4 <- p_4 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "red")
  }
  
}
print(p_4)

p_5 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:41) {
  if (property_variety[i,5] >= 108) {
    p_5 <- p_5 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "green") 
  }
  if (property_variety[i,5] > 96 & property_variety[i,5] < 108) {
    p_5 <- p_5 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "light green") 
  }
  if (property_variety[i,5] > 48 & property_variety[i,5] <= 96) {
    p_5 <- p_5 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "yellow")
  }
  if (property_variety[i,5] <= 48) {
    p_5 <- p_5 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "red")
  }
  
}
print(p_5)


library(sf)
library(purrr)

parsed %>% 
  list(lat = c(pluck(.,1,1), pluck(.,1,2) ), lon = c(pluck(.,1,3), pluck(.,1,4))) %>% 
  .[-1] %>% 
  as.data.frame() %>% st_as_sf(., coords = c("lon","lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_sf()


all_res_data <- CA_HOMES[CA_HOMES$property_type == "All Residential",]

median_sales <- c()

for (i in 1:41){
  median_sales[i] <- median(all_res_data[all_res_data$region == CA_counties[i],]$median_sale_price)
  
}

p_0 = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (i in 1:41) {
  if (median_sales[i] > 548250) {
    p_0 <- p_0 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = " dark blue") 
  }
  if (median_sales[i] > 395000 & median_sales[i] <= 548250) {
    p_0 <- p_0 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "blue") 
  }
  if (median_sales[i] > 285000 & median_sales[i] <= 395000) {
    p_0 <- p_0 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "light blue") 
  }
  if (median_sales[i] <= 285000) {
    p_0 <- p_0 + geom_sf(data = CA_cty_bound_geom[county_index[i]], fill = "white") 
  }
}
print(p_0)


save(CA_county_boundaries, file = "CA_county_boundaries")
load('CA_county_boundaries')
