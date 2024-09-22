
knitr::opts_chunk$set(echo = TRUE)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
load("CA_HOMES")




#----Scratch Work----

p_s = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(single_family_index)) {
  for (i in 1:41) {
  if (jan_2012$region[single_family_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
    if (color_s[n,1] < 85) {
    p_s <- p_s + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                         fill = rgb(color_s[n,1],0,0,max=85)) 
    }
    if (color_s[n,1] >= 85 & color_s[n,1] < 170) {
    p_s <- p_s + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                         fill = rgb(color_s[n,1],0,color_s[n,1],max=170)) 
    }
    if (color_s[n,1] >= 170) {
    p_s <- p_s + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                         fill = rgb(0,0,color_s[n,1],max=255)) 
    }
    }
  }
}
print(p_s)

p_c = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(condo_index)) {
  for (i in 1:41) {
    if (jan_2012$region[condo_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      if (color[n] < 47.85758) {
        p_c <- p_c + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color[n],0,0,max= 47.85758)) 
      }
      if (color[n] >= 47.85758 & color[n] < 162.60242) {
        p_c <- p_c + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(abs(color[n]),0,abs(color[n]),max=162.60242)) 
      }
      if (color[n] >= 162.60242) {
        p_c <- p_c + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(0,0,abs(color[n]),max=255)) 
      }
    }
  }
}
p_c <- p_c + geom_sf(data = CA_cty_bound_geom[county_index[1]], fill = rgb(150,0,0,max=255)) 
print(p_c)

p_m = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(multi_family_index)) {
  for (i in 1:41) {
    if (jan_2012$region[multi_family_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      if (color[n] < 85) {
        p_m <- p_m + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color[n],0,0,max=85)) 
      }
      if (color[n] >= 85 & color[n] < 170) {
        p_m <- p_m + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color[n],0,color[n],max=170)) 
      }
      if (color[n] >= 170) {
        p_m <- p_m + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(0,0,color[n],max=255)) 
      }
    }
  }
}
print(p_m)

p_t = ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black")

for (n in 1:length(townhouse_index)) {
  for (i in 1:41) {
    if (jan_2012$region[townhouse_index[n]] == CA_county_boundaries$V13[county_index[i]]) {
      if (color[n] < 85) {
        p_t <- p_t + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color[n],0,0,max=85)) 
      }
      if (color[n] >= 85 & color[n] < 170) {
        p_t <- p_t + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(color[n],0,color[n],max=170)) 
      }
      if (color[n] >= 170) {
        p_t <- p_t + geom_sf(data = CA_cty_bound_geom[county_index[i]], 
                             fill = rgb(0,0,color[n],max=255)) 
      }
    }
  }
}
print(p_4)

ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black") + 
  geom_sf(data = CA_cty_bound_geom[county_index[1:41]], fill = ) 

ggplot()

scale_fill_gradient2(low="white",mid="light blue",high="blue",limits=c(-100,100))+
  
  scale_colour_gradient2(
    ...,
    low = muted("red"),
    mid = "white",
    high = muted("blue"),
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

scale_fill_gradient2(
  ...,
  low = muted("red"),
  mid = "white",
  high = muted("blue"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
)

scale_fill_gradientn(
  ...,
  colours,
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill",
  colors
)




#---------All my messy Ideas that don't work-----

county_plot_draft <- ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black") + coord_sf()

for (i in 1:41){
  if (CA_counties[i] == CA_county_boundaries$V12[i]) {
    if (table(CA_HOMES[CA_HOMES$region==CA_counties[i],]$region_type) == 600){
    CA_cty_temp <- CA_county_boundaries[CA_county_boundaries$V13==CA_counties[i],]
    CA_cty_coord <- data.frame(st_coordinates(CA_cty_temp[i]))
  county_plot_final <- county_plot_draft + 
    geom_polygon(data = CA_cty_coord, 
                 aes(CA_cty_coord[,1],CA_cty_coord[,2]), fill = "green")
  }
  }
  county_plot_final
}
print(county_plot)


ggplot() + 
  geom_sf(data = CA_county_boundaries, size = 3, color = "black", fill = "cyan1")

ggplot() + 
  geom_sf(data = CA_county_boundaries, size = 3, color = "black", fill = 1:58) + 
  ggtitle("CA Counties Plot") + 
  coord_sf()



ggplot(data=CA_county_boundaries,aes(fill=))

plot(CA_county_boundaries, col=1)
     
     
county_names <- CA_county_boundaries$FMNAME_PC
spplot(CA_county_boundaries[,3])




CA_county_boundaries = CA_county_boundaries %>% 
  left_join(. , CA_HOMES, by=c("region"="property_type"))




county_bound_fortified <- tidy(CA_county_boundaries2, 
                               region="code")

CA_cnty_bound_df <- broom::tidy(CA_county_boundaries2, 
                                region="_NAME_PCASE")

lapply(CA_cnty_bound_df, class)
  

CA_county_boundaries2$NAME_PCASE

head(CA_county_boundaries2)

labels <- cbind(CA_county_boundaries2, 
                st_coordinates(st_centroid(CA_county_boundaries2$geometry)))

ggplot() +
  geom_sf(data=CA_county_boundaries2, aes(label=NAME_PCASE,x-X, y=Y),
          colour="black")

for (i in 1:41){
  if (property_variety[i,1] >= 110)
  {
    CA_cty_temp <- CA_county_boundaries[CA_county_boundaries$V13==CA_counties[i],]
    CA_coord_temp <- data.frame(st_coordinates(CA_cty_temp))
    p <- p + geom_polygon(aes_all(CA_coord_temp[,1],CA_coord_temp[,2]), 
                          fill = "green")
  }
}
return(p)
print(p)


ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black") + 
  geom_sf(data = CA_cty_bound_geom[19], fill = "green") + 
  geom_sf(data = CA_cty_bound_geom[33], fill = "green")

for (i in 1:41){
  if ((table(CA_HOMES[CA_HOMES$region==CA_counties[i],]$region_type) == 600))
  {
    CA_cty_temp <- CA_county_boundaries[CA_county_boundaries$V13==CA_counties[i],]
    CA_cty_bound_temp <- st_geometry(CA_cty_temp)
    p <- p + geom_sf(data = CA_cty_bound_temp, fill = "green")
  }
  return(p)
}

print(p)

CA_cty_temp <- CA_county_boundaries[CA_county_boundaries$V13==CA_counties[33],]
CA_cty_bound_temp <- st_geometry(CA_cty_temp)

ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black") +
  geom_sf(data = CA_county_boundaries[CA_county_boundaries$V13==CA_counties[33],],
          fill = "green") +
  geom_sf(data = CA_county_boundaries[CA_county_boundaries$V13==CA_counties[34],],
          fill = "green")


(CA_HOMES$property_type[]) 


table(cut(CA_HOMES$property_type, breaks = 'property_type_id'))


CA_HOMES %>%
  mutate(property_type,levels=rep(c("1","2","3","4","5"),times=5)) %>%
  head()


CA_HOMES %>%
  relocate(z, .after = property_type) -> CA_HOMES

ggplot(ukMapFortSub,aes(long, lat, group=group))+
  geom_path(color="black")+coord_map()


ggplot() + 
  geom_sf(data = CA_county_boundaries,size = 3, color = "black") + 
  geom_polygon(data = CA_cty_coord, 
               aes(CA_cty_coord[,1],CA_cty_coord[,2]), fill = "red") +
  geom_polygon(data = CA_cty_coord_2, 
               aes(CA_cty_coord_2[,1],CA_cty_coord_2[,2]), fill = "green") +
  ggtitle("CA Counties Plot") +
  coord_sf()




