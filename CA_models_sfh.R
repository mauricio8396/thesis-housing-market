library(broom)
library(dplyr)
library(classInt)
library(lme4)
library(nlme)
library(mgcv)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(scales)
library(tidyr)

load("CA_HOMES")
load("CA_county_boundaries")
load("CA_counties")
load("CA_data_new.Rdata")
load("CA_data_new_2")
load("CA_data_new_3")

#variables to include time, area_type, population, property_type(stratified)
# make inference plots
#Generalized additive models
#2 Longitudinal models
#-random intercept
#-random intercept and slope
#Maybe spatial model? 

# Stratify the models by property type
# Include a model with Time
# Include a model with time, region
# log median sale price add the column of fitted values and transform back
# then fit regression line
# time, region and population --- don't use population yet


CA_data_new_3$population <- gsub(",","",CA_data_new_3$population)
CA_data_new_3$population <- as.numeric(CA_data_new_3$population)
hist(CA_data_new_3$population)
d <- density(CA_data_new_3$population)
plot(d, main="Kernel Density of Population")
hist(log(CA_data_new_3$population))
d2 <- density(log(CA_data_new_3$population))
plot(d2, main="Kernel Density of Population")


coastal_urban <- CA_data_new_3 %>% filter(area_type == "coastal_urban")
coastal_not_populous <- CA_data_new_3 %>% filter(area_type == "coastal_not_populous")
inland_urban <- CA_data_new_3 %>% filter(area_type == "inland_urban")
inland_not_populous <- CA_data_new_3 %>% filter(area_type == "inland_not_populous")

single_family_data <- CA_data_new_3 %>% filter(property_type == "Single Family Residential")
coastal_index_sfh <- which(single_family_data$area_type == "coastal_urban" | 
                         single_family_data$area_type == "coastal_not_populous")

inland_index_sfh <- which(single_family_data$area_type == "inland_urban" | 
                         single_family_data$area_type == "inland_not_populous")

single_family_data_2 <- single_family_data
single_family_data_2$area_type[coastal_index_sfh] <- "coastal"
single_family_data_2$area_type[inland_index_sfh] <- "inland"

  

#---------------------------Linear Regression models----------------------------

lin_sfh <- lm(median_sale_price ~ factor(area_type)*poly(time_since,3,raw=TRUE), 
             single_family_data)
summary(lin_sfh)

single_family_data$pred_lin_sfh <- fitted(lin_sfh)

clr <- rep("blue", length(single_family_data$area_type)) # blue for coastal urban
clr[single_family_data$area_type=="coastal_not_populous"] <- "green" # green for coastal rural
clr[single_family_data$area_type=="inland_not_populous"] <- "red" # red for inland rural
clr[single_family_data$area_type=="inland_urban"] <- "purple" # purple for inland urban
table(clr,single_family_data$area_type)

CA_poly_3_plot_sfh <- ggplot(single_family_data,
       aes(x = time_since,
           y = pred_lin_sfh,
           colour = area_type)) +
  geom_point(data = single_family_data, aes(y = median_sale_price), alpha=0.4) +
  geom_line(linewidth=1.2, aes(colour = area_type)) + 
  labs(#title="Median Sale Price vs Elapsed Time colored by Area Type",
       #subtitle="For Single Family Residential",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_poly_3_plot_sfh.pdf",CA_poly_3_plot_sfh,
       width=5.85, height=4.39,units="in")


lin_sfh2 <- lm(median_sale_price ~ factor(area_type)*poly(time_since,5,raw=TRUE), 
              single_family_data)
summary(lin_sfh2)

single_family_data$pred_lin_sfh2 <- fitted(lin_sfh2)

clr <- rep("blue", length(single_family_data$area_type)) # blue for coastal urban
clr[single_family_data$area_type=="coastal_not_populous"] <- "green" # green for coastal rural
clr[single_family_data$area_type=="inland_not_populous"] <- "red" # red for inland rural
clr[single_family_data$area_type=="inland_urban"] <- "purple" # purple for inland urban
table(clr,single_family_data$area_type)

CA_poly_5_plot_sfh <- ggplot(single_family_data,
                             aes(x = time_since,
                                 y = pred_lin_sfh2,
                                 colour = area_type)) +
  geom_point(data = single_family_data, aes(y = median_sale_price), alpha=0.4) +
  geom_line(linewidth=1.2, aes(colour = area_type)) + 
  labs(#title="Median Sale Price vs Elapsed Time colored by Area Type",
       #subtitle="For Single Family Residential",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_poly_5_plot_sfh.pdf",CA_poly_5_plot_sfh,
       width=5.85, height=4.39,units="in")


#----------------Final GAM models for Single Family Residential-----------------
single_family_data$region <- factor(single_family_data$region)
single_family_data$area_type <- factor(single_family_data$area_type)

# fixed effect model
gam_model_sfh1 <- gam(log(median_sale_price) ~ s(time_since),
                   data=single_family_data)
summary(gam_model_sfh1)
gam.check(gam_model_sfh1)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
single_family_data$pred_gam_sfh1 <- exp(gam_model_sfh1$fitted.values)

# This will plot the best line of fit for Single Family observations
CA_reg_line_fixed_gam_sfh <- ggplot(single_family_data, 
                                    aes(time_since, median_sale_price)) +
                                                  geom_point(alpha=0.5) +
      geom_line(aes(time_since,pred_gam_sfh1),color="blue",linewidth=1) +
                labs(#title = "Median Sale price over Time",
                #subtitle = "For Single Family Residential",
                x = "Elapsed Time (by month)",
                y = "Median Sale Price") #+
      #facet_wrap(~ area_type, scales = "free_y")


ggsave("CA_reg_line_fixed_gam_sfh.pdf",CA_reg_line_fixed_gam_sfh,
       width=5.85, height=4.39,units="in")



# random-intercept only
gam_model_sfh2 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re"),
                   data = single_family_data)
summary(gam_model_sfh2)
gam.check(gam_model_sfh2)

single_family_data$pred_gam_sfh2 <- exp(gam_model_sfh2$fitted.values)

CA_reg_line_rand_int_gam_sfh <- ggplot(single_family_data,
                                     aes(x = time_since,
                                         y = pred_gam_sfh2, group = region,
                                         colour = area_type)) +
  geom_point(data = single_family_data, aes(y = median_sale_price)) +
  geom_line(linewidth=0.5, color = "black") + 
  labs(#title="Median Sale Price vs Elapsed Time faceted by Area Type",
       #subtitle="Random Intercept by County for Single Family Residential",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_reg_line_rand_int_gam_sfh.pdf",CA_reg_line_rand_int_gam_sfh,
       width=5.85, height=4.39,units="in")


# random-intercept and random slope
gam_model_sfh3 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re") + 
                     s(region, time_since, bs="re"),
                   data = single_family_data)
summary(gam_model_sfh3)
gam.check(gam_model_sfh3)

single_family_data$pred_gam_sfh3 <- exp(fitted(gam_model_sfh3))

CA_reg_line_rand_int_slope_gam_sfh <- ggplot(single_family_data, 
                                           aes(x = time_since, 
                                               y = pred_gam_sfh3, group = region,
                                               colour = area_type)) +
  geom_point(data = single_family_data, aes(y = median_sale_price)) +
  geom_line(linewidth=0.5, color = "black") +
  labs(#title="Median Sale Price vs Elapsed Time faceted by Area Type", 
       #subtitle="Random Intercept and Slope by County for Single Family Residential",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_reg_line_rand_int_slope_gam_sfh.pdf",
       CA_reg_line_rand_int_slope_gam_sfh,
       width=5.85, height=4.39,units="in")


gam_model_sfh3 <- gam(log(median_sale_price) ~ s(time_since, by=area_type) + s(region, bs="re") + 
                        s(region, time_since, bs="re"),
                      data = single_family_data)

summary(gam_model_sfh3)
gam.check(gam_model_sfh3)

single_family_data$pred_gam_sfh3 <- exp(fitted(gam_model_sfh3))

CA_reg_line_rand_int_slope_gam_sfh <- ggplot(single_family_data, 
                                             aes(x = time_since, 
                                                 y = pred_gam_sfh3, group = region,
                                                 colour = area_type)) +
  geom_point(data = single_family_data, aes(y = median_sale_price)) +
  geom_line(linewidth=0.5, color = "black") +
  labs(#title="Median Sale Price vs Elapsed Time faceted by Area Type", 
    #subtitle="Random Intercept and Slope by County for Single Family Residential",
    x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))


#-------------------GAM including population-------------------
single_family_data_2$region <- factor(single_family_data_2$region)
population_s <- seq(50000,2500000,5000)
time_since <- 119
area_type_s <- unique(single_family_data_2$area_type)[1]
new_data_s <- data.frame(population_s, time_since, area_type_s)
colnames(new_data_s) <- c("population","time_since", "area_type")


population_s2 <- seq(50000,10000000,5000)
area_type_s2 <- unique(single_family_data_2$area_type)[2]
new_data_s2 <- data.frame(population_s2, time_since, area_type_s2)
colnames(new_data_s2) <- c("population","time_since", "area_type")


gam_model_sfh4 <- gam(log(median_sale_price) ~ s(time_since) + 
                                          #s(region, bs="re") +
                                                   area_type +
                                          s(log(population)),
                                  data = single_family_data_2)

summary(gam_model_sfh4)

single_family_data_2$pred_gam_sfh4 <- exp(fitted(gam_model_sfh4))

new_data_s$pred_gam_sfh4 <- exp(predict(gam_model_sfh4, newdata = new_data_s))

new_data_s2$pred_gam_sfh4 <- exp(predict(gam_model_sfh4, newdata = new_data_s2))

combined_new_s <- rbind(new_data_s,new_data_s2)

CA_pop_line_gam_sfh4 <- ggplot(combined_new_s, 
                               aes(population, pred_gam_sfh4, 
                                   colour = area_type)) +
                        geom_line() +
                        #geom_line(linewidth=1, aes(colour=area_type)) +
      labs(#title="Median Sale Price vs Population colored by Area Type", 
                #    subtitle="Population for Single Family Residential",
                                     x="Population",y="Median Sale Price")

ggsave("CA_pop_line_gam_sfh4.pdf",CA_pop_line_gam_sfh4)



#-----------------------Population with no area_type---------------------

gam_model_sfh5 <- gam(log(median_sale_price) ~ s(time_since) + 
                        #s(region, bs="re") +
                        #area_type +
                        s(log(population)),
                      data = single_family_data_2)

summary(gam_model_sfh5)

single_family_data_2$pred_gam_sfh5 <- exp(fitted(gam_model_sfh5))

#new_data$pred_gam_sfh5 <- exp(predict(gam_model_sfh5, newdata = new_data))

#new_data_2$pred_gam_sfh5 <- exp(predict(gam_model_sfh5, newdata = new_data_2))

#combined_new_2 <- rbind(new_data,new_data_2)

combined_new_s$pred_gam_sfh5 <- exp(predict(gam_model_sfh5, newdata = combined_new_s))

dec_2021_s <- single_family_data_2 %>% 
  filter(time_since == 119) %>% 
  select(region, population, median_sale_price, area_type)

CA_pop_line_gam_sfh5 <- ggplot(combined_new_2, 
                               aes(population, pred_gam_sfh5)) +
                        geom_line() +
         #geom_line(linewidth=1, aes(colour=area_type)) +
          labs(#title="Median Sale Price vs Population colored by Area Type", 
          #    subtitle="Population for Single Family Residential",
               x="Population",y="Median Sale Price")

ggsave("CA_pop_line_gam_sfh5.pdf",CA_pop_line_gam_sfh5)

CA_pop_line_gam_sfh5 <- ggplot() +
                        geom_line(aes(population, pred_gam_sfh4, 
                                  colour = area_type), combined_new_s) + 
  geom_line(aes(population, pred_gam_sfh5, colour="both"), combined_new_s) +
  geom_point(aes(population,median_sale_price, color=area_type), dec_2021_s) + 
  geom_text_repel(aes(population,median_sale_price, label=region), dec_2021_s, 
            max.overlaps = Inf, size = 2) +
  #geom_line(linewidth=1, aes(colour=area_type)) +
  labs(#title="Median Sale Price vs Population colored by Area Type", 
    #    subtitle="Population for Single Family Residential",
    x="Population",y="Median Sale Price")

ggsave("CA_pop_line_gam_sfh5.pdf",CA_pop_line_gam_sfh5,
       width=5.85, height=4.39,units="in")


#---------------------GAM with each area type separately----------------------
single_family_coastal <- single_family_data_2 %>% filter(area_type == "coastal")

single_family_inland <- single_family_data_2 %>% filter(area_type == "inland")


gam_model_sfh6 <- gam(log(median_sale_price) ~ s(time_since) + 
                        #s(region, bs="re") +
                        #area_type +
                        s(log(population)),
                      data = single_family_coastal)

gam_model_sfh7 <- gam(log(median_sale_price) ~ s(time_since) + 
                        #s(region, bs="re") +
                        #area_type +
                        s(log(population)),
                      data = single_family_inland)



new_data_s2$pred_gam_sfh_separate <- exp(predict(gam_model_sfh6, newdata = new_data_s2))

new_data_s$pred_gam_sfh_separate <- exp(predict(gam_model_sfh7, newdata = new_data_s))

#combined_new_2 <- rbind(new_data,new_data_2)

combined_new_s <- rbind(new_data_s, new_data_s2) 

dec_2021_s <- single_family_data_2 %>% 
  filter(time_since == 119) %>% 
  select(region, population, median_sale_price, area_type)


CA_pop_line_gam_sfh5 <- ggplot() +
  geom_line(aes(population, pred_gam_sfh_separate, 
                colour = area_type), combined_new_s) + 
  #geom_line(aes(population, pred_gam_sfh5, colour="both"), combined_new_s) +
  geom_point(aes(population,median_sale_price, color=area_type), dec_2021_s) + 
  geom_text_repel(aes(population,median_sale_price, label=region), dec_2021_s, 
                  max.overlaps = Inf, size = 2) +
  #geom_line(linewidth=1, aes(colour=area_type)) +
  labs(#title="Median Sale Price vs Population colored by Area Type", 
    #    subtitle="Population for Single Family Residential",
    x="Population",y="Median Sale Price")

ggsave("CA_pop_line_gam_sfh5.pdf",CA_pop_line_gam_sfh5,
       width=5.85, height=4.39,units="in")

