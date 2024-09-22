library(broom)
library(dplyr)
library(classInt)
library(lme4)
library(nlme)
library(mgcv)
library(ggplot2)
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

townhome_data <- CA_data_new_3 %>% filter(property_type == "Townhouse")
outliers <- which(townhome_data$median_sale_price >= 2000000)
townhome_data <- townhome_data[-outliers,]
outliers <- which(townhome_data$area_type == "coastal_not_populous" &
                     townhome_data$median_sale_price >= 1250000)
townhome_data <- townhome_data[-outliers,]



coastal_index_t <- which(townhome_data$area_type == "coastal_urban" | 
                           townhome_data$area_type == "coastal_not_populous")

inland_index_t <- which(townhome_data$area_type == "inland_urban" | 
                          townhome_data$area_type == "inland_not_populous")

townhome_data_2 <- townhome_data
townhome_data_2$area_type[coastal_index_t] <- "coastal"
townhome_data_2$area_type[inland_index_t] <- "inland"

#---------------------------Linear Regression models----------------------------

lin_townhome <- lm(median_sale_price ~ factor(area_type)*poly(time_since,3,raw=TRUE), 
                townhome_data)
summary(lin_townhome)

townhome_data$pred_lin_th <- fitted(lin_townhome)

clr <- rep("blue", length(townhome_data$area_type)) # blue for coastal urban
clr[townhome_data$area_type=="coastal_not_populous"] <- "green" # green for coastal rural
clr[townhome_data$area_type=="inland_not_populous"] <- "red" # red for inland rural
clr[townhome_data$area_type=="inland_urban"] <- "purple" # purple for inland urban
table(clr,townhome_data$area_type)

CA_poly_3_plot_th <- ggplot(townhome_data,
                               aes(x = time_since,
                                   y = pred_lin_th,
                                   colour = area_type)) +
  geom_point(data = townhome_data, aes(y = median_sale_price), alpha=0.4) +
  geom_line(linewidth=1.2, aes(colour = area_type)) + 
  labs(#title="Median Sale Price vs Elapsed Time colored by Area Type",
    #subtitle="For Townhomes",
    x="Elapsed Time (monthly)",y="Median Sale Price") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_poly_3_plot_th.pdf",CA_poly_3_plot_th,
       width=5.85, height=4.39,units="in")


lin_townhome2 <- lm(median_sale_price ~ factor(area_type)*poly(time_since,5,raw=TRUE), 
                 townhome_data)
summary(lin_townhome2)

townhome_data$pred_lin_th2 <- fitted(lin_townhome2)

clr <- rep("blue", length(townhome_data$area_type)) # blue for coastal urban
clr[townhome_data$area_type=="coastal_not_populous"] <- "green" # green for coastal rural
clr[townhome_data$area_type=="inland_not_populous"] <- "red" # red for inland rural
clr[townhome_data$area_type=="inland_urban"] <- "purple" # purple for inland urban
table(clr,townhome_data$area_type)

CA_poly_5_plot_th <- ggplot(townhome_data,
                               aes(x = time_since,
                                   y = pred_lin_th2,
                                   colour = area_type)) +
  geom_point(data = townhome_data, aes(y = median_sale_price), alpha=0.4) +
  geom_line(linewidth=1.2, aes(colour = area_type)) + 
  labs(#title="Median Sale Price vs Elapsed Time colored by Area Type",
    #subtitle="For Townhomes",
    x="Elapsed Time (monthly)",y="Median Sale Price") +
  theme(legend.position = "bottom",
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  scale_colour_discrete(name="Area Type",
                        labels=c('Coastal Rural','Coastal Urban',
                                 'Inland Rural', 'Inland Urban'))

ggsave("CA_poly_5_plot_th.pdf",CA_poly_5_plot_th,
       width=5.85, height=4.39,units="in")


#----------------Final GAM models for Single Family Residential-----------------
townhome_data$region <- factor(townhome_data$region)
townhome_data$area_type <- factor(townhome_data$area_type)

# fixed effect model
gam_model_th1 <- gam(log(median_sale_price) ~ s(time_since),
                        data=townhome_data)
summary(gam_model_th1)
gam.check(gam_model_th1)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
townhome_data$pred_gam_th1 <- exp(gam_model_th1$fitted.values)

# This will plot the best line of fit for condo observations
CA_reg_line_fixed_gam_th <- ggplot(townhome_data, 
                                      aes(time_since, median_sale_price)) +
  geom_point(alpha=0.5) +
  geom_line(aes(time_since,pred_gam_th1),color="blue",linewidth=1) +
  labs(#title = "Median Sale price over Time",
    #subtitle = "For Townhomes",
    x = "Elapsed Time (by month)",
    y = "Median Sale Price") #+
#facet_wrap(~ area_type, scales = "free_y")


ggsave("CA_reg_line_fixed_gam_th.pdf",CA_reg_line_fixed_gam_th,
       width=5.85, height=4.39,units="in")



# random-intercept only
gam_model_th2 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re"),
                        data = townhome_data)
summary(gam_model_th2)
gam.check(gam_model_th2)

townhome_data$pred_gam_th2 <- exp(gam_model_th2$fitted.values)

CA_reg_line_rand_int_gam_th <- ggplot(townhome_data,
                                         aes(x = time_since,
                                             y = pred_gam_th2, group = region,
                                             colour = area_type)) +
  geom_point(data = townhome_data, aes(y = median_sale_price)) +
  geom_line(linewidth=0.5, color = "black") + 
  labs(#title="Median Sale Price vs Elapsed Time faceted by Area Type",
    #subtitle="Random Intercept by County for Townhomes",
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

ggsave("CA_reg_line_rand_int_gam_th.pdf",CA_reg_line_rand_int_gam_th,
       width=5.85, height=4.39,units="in")


# random-intercept and random slope
gam_model_th3 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re") + 
                          s(region, time_since, bs="re"),
                        data = townhome_data)
summary(gam_model_th3)
gam.check(gam_model_th3)

townhome_data$pred_gam_th3 <- exp(fitted(gam_model_th3))

CA_reg_line_rand_int_slope_gam_th <- ggplot(townhome_data, 
                                               aes(x = time_since, 
                                                   y = pred_gam_th3, group = region,
                                                   colour = area_type)) +
  geom_point(data = townhome_data, aes(y = median_sale_price)) +
  geom_line(linewidth=0.5, color = "black") +
  theme(legend.position = "bottom") +
  labs(#title="Median Sale Price vs Elapsed Time faceted by Area Type", 
    #subtitle="Random Intercept and Slope by County for Condomoniums",
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

ggsave("CA_reg_line_rand_int_slope_gam_th.pdf",
       CA_reg_line_rand_int_slope_gam_th,
       width=5.85, height=4.39,units="in")

#-------------------GAM including population-------------------
townhome_data_2$region <- factor(townhome_data_2$region)
population_t <- seq(50000,10000000,5000)
time_since <- 119
area_type_t <- unique(townhome_data_2$area_type)[1]
new_data_t <- data.frame(population_t, time_since, area_type_t)
colnames(new_data_t) <- c("population","time_since", "area_type")

population_t2 <- seq(50000,2500000,5000)
area_type_t2 <- unique(townhome_data_2$area_type)[2]
new_data_t2 <- data.frame(population_t2, time_since, area_type_t2)
colnames(new_data_t2) <- c("population","time_since", "area_type")

gam_model_th4 <- gam(log(median_sale_price) ~ s(time_since) + 
                          #s(region, bs="re") +
                          area_type +
                          s(log(population)),
                        data = townhome_data_2)

summary(gam_model_th4)

townhome_data_2$pred_gam_th4 <- exp(fitted(gam_model_th4))

new_data_t$pred_gam_th4 <- exp(predict(gam_model_th4, newdata = new_data_t))

new_data_t2$pred_gam_th4 <- exp(predict(gam_model_th4, newdata = new_data_t2))

combined_new_t <- rbind(new_data_t,new_data_t2)

CA_pop_line_gam_th4 <- ggplot(combined_new_t2, 
                                 aes(population, pred_gam_th4, 
                                     colour = area_type)) +
  geom_line() +
  #geom_line(linewidth=1, aes(colour=area_type)) +
  labs(#title="Median Sale Price vs Population colored by Area Type", 
    #    subtitle="Population for Single Family Residential",
    x="Population",y="Median Sale Price")

ggsave("CA_pop_line_gam_th4.pdf",CA_pop_line_gam_th4)



#-----------------------Population with no area_type---------------------

gam_model_th5 <- gam(log(median_sale_price) ~ s(time_since) + 
                          #s(region, bs="re") +
                          #area_type +
                          s(log(population)),
                        data = townhome_data_2)

summary(gam_model_th5)

townhome_data_2$pred_gam_th5 <- exp(fitted(gam_model_th5))

#new_data$pred_gam_th5 <- exp(predict(gam_model_th5, newdata = new_data))

#new_data_2$pred_gam_th5 <- exp(predict(gam_model_th5, newdata = new_data_2))

#combined_new_2 <- rbind(new_data,new_data_2)

combined_new_t$pred_gam_th5 <- exp(predict(gam_model_th5, newdata = combined_new_t))

dec_2021_t <- townhome_data_2 %>% 
            filter(time_since == 119) %>% 
            select(region, population, median_sale_price, area_type)

CA_pop_line_gam_th5 <- ggplot(combined_new_t2, 
                                 aes(population, pred_gam_th5, 
                                     colour = area_type)) +
  geom_line() +
  #geom_line(linewidth=1, aes(colour=area_type)) +
  labs(#title="Median Sale Price vs Population colored by Area Type", 
    #    subtitle="Population for Single Family Residential",
    x="Population",y="Median Sale Price")


CA_pop_line_gam_th5 <- ggplot() +
  geom_line(aes(population, pred_gam_th4, 
                colour = area_type), combined_new_t) + 
  geom_line(aes(population, pred_gam_th5, colour="both"), combined_new_t) +
  geom_point(aes(population,median_sale_price, colour=area_type), dec_2021_t) + 
  geom_text_repel(aes(population,median_sale_price, label=region), dec_2021_t, 
            nudge_y = 0.1, size = 2, vjust = 1) +
  #geom_line(linewidth=1, aes(colour=area_type)) +
  labs(#title="Median Sale Price vs Population colored by Area Type", 
    #    subtitle="Population for Single Family Residential",
    x="Population",y="Median Sale Price")

ggsave("CA_pop_line_gam_th5.pdf",CA_pop_line_gam_th5,
       width=5.85, height=4.39,units="in")


