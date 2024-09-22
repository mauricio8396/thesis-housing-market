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


#----Scatter Plot percent change vs price colored by area type----
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

CA_counties_coastal <- sort(unique(coastal_urban$region))
county_index_coastal <- c()
for (i in 1:length(CA_counties_coastal)){
  county_index_coastal[i] <- which(CA_county_boundaries$V13==CA_counties_coastal[i])
}

CA_counties_coastal_npop <- sort(unique(coastal_not_populous$region))
county_index_coastal_npop <- c()
for (i in 1:length(CA_counties_coastal_npop)){
  county_index_coastal_npop[i] <- which(CA_county_boundaries$V13==CA_counties_coastal_npop[i])
}

CA_counties_inland <- sort(unique(inland_urban$region))
county_index_inland <- c()
for (i in 1:length(CA_counties_inland)){
  county_index_inland[i] <- which(CA_county_boundaries$V13==CA_counties_inland[i])
}

CA_counties_inland_npop <- sort(unique(inland_not_populous$region))
county_index_inland_npop <- c()
for (i in 1:length(CA_counties_inland_npop)){
  county_index_inland_npop[i] <- which(CA_county_boundaries$V13==CA_counties_inland_npop[i])
}


for (i in 1:length(county_index_coastal)){
  CA_county_boundaries[county_index_coastal[i],26] <- "coastal_urban"
}

for (i in 1:length(county_index_coastal_npop)){
  CA_county_boundaries[county_index_coastal_npop[i],26] <- "coastal_not_populous"
}

for (i in 1:length(county_index_inland)){
  CA_county_boundaries[county_index_inland[i],26] <- "inland_urban"
}

for (i in 1:length(county_index_inland_npop)){
  CA_county_boundaries[county_index_inland_npop[i],26] <- "inland_not_populous"
}

#change variable name
colnames(CA_county_boundaries)[26] = "area_type"


CA_scatter_plot_percent_change_v_sale_price_sfh <- ggplot() + 
                        geom_point(data = CA_county_boundaries,
        aes(x = median_sale_price.8, y = perc_change_s2, colour = area_type)) +
                                      scale_colour_discrete(na.translate = F) +
                                             xlab("Median Sale Price (2012)") +
                                                       ylab("Percent Change") +
                #ggtitle("Percent change vs Median Sale Price for Townhomes") +
                   theme(legend.position = "bottom",
                         legend.title = element_text(size=9),
                         legend.text = element_text(size=7)) +
   scale_colour_discrete(na.translate = F, name="Area Type",
                         labels=c('Coastal Rural','Coastal Urban',
                                  'Inland Rural', 'Inland Urban'))

ggsave("CA_scatter_plot_percent_change_v_sale_price_sfh.pdf",
       CA_scatter_plot_percent_change_v_sale_price_sfh,
       width = 5.85, height = 4.39, units = "in")

CA_scatter_plot_percent_change_v_sale_price_condo <- ggplot() + 
                          geom_point(data = CA_county_boundaries,
      aes(x = median_sale_price.10, y = perc_change_c2, colour = area_type)) +
                                     scale_colour_discrete(na.translate = F) +
                                            xlab("Median Sale Price (2012)") +
                                                      ylab("Percent Change") +
               #ggtitle("Percent change vs Median Sale Price for Townhomes") +
                  theme(legend.position = "bottom",
                        legend.title = element_text(size=9),
                        legend.text = element_text(size=7)) +
  scale_colour_discrete(na.translate = F, name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))


ggsave("CA_scatter_plot_percent_change_v_sale_price_condo.pdf",
       CA_scatter_plot_percent_change_v_sale_price_condo,
       width = 5.85, height = 4.39, units = "in")

CA_scatter_plot_percent_change_v_sale_price_th <- ggplot() + 
                      geom_point(data = CA_county_boundaries,
      aes(x = median_sale_price.12, y = perc_change_t2, colour = area_type)) +
                                            xlab("Median Sale Price (2012)") +
                                                      ylab("Percent Change") +
                #ggtitle("Percent change vs Median Sale Price for Townhomes") +
                  theme(legend.position = "bottom",
                        legend.title = element_text(size=9),
                        legend.text = element_text(size=7)) +
  scale_colour_discrete(na.translate = F, name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_scatter_plot_percent_change_v_sale_price_th.pdf",
       CA_scatter_plot_percent_change_v_sale_price_condo,
       width = 5.85, height = 4.39, units = "in")


