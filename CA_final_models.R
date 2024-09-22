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

coastal_urban <- CA_data_new_3 %>% filter(area_type == "coastal_urban")
coastal_not_populous <- CA_data_new_3 %>% filter(area_type == "coastal_not_populous")
inland_urban <- CA_data_new_3 %>% filter(area_type == "inland_urban")
inland_not_populous <- CA_data_new_3 %>% filter(area_type == "inland_not_populous")

condo_data <- CA_data_new_3 %>% filter(property_type == "Condo/Co-op")
single_family_data <- CA_data_new_3 %>% filter(property_type == "Single Family Residential")
townhome_data <- CA_data_new_3 %>% filter(property_type == "Townhouse")

CA_data_new_3$population <- gsub(",","",CA_data_new_3$population)
CA_data_new_3$population <- as.numeric(CA_data_new_3$population)
hist(CA_data_new_3$population)
d <- density(CA_data_new_3$population)
plot(d, main="Kernel Density of Population")
hist(log(CA_data_new_3$population))
d2 <- density(log(CA_data_new_3$population))
plot(d2, main="Kernel Density of Population")

#-----------------------Final Models Linear Regression--------------------------

lin_m7 <- lm(median_sale_price ~ factor(area_type)*poly(time_since,3,raw=TRUE), 
             CA_data_new_3)
summary(lin_m7)

clr <- rep("blue", length(CA_data_new_3$area_type)) # blue for coastal urban
clr[CA_data_new_3$area_type=="coastal_not_populous"] <- "green" # green for coastal rural
clr[CA_data_new_3$area_type=="inland_not_populous"] <- "red" # red for inland rural
clr[CA_data_new_3$area_type=="inland_urban"] <- "purple" # purple for inland urban
table(clr,CA_data_new_3$area_type)


# model formula
# Y = B0 + B1*X_c + B2*X_in + B3*X_iu + B4*X_t + B5*X_t^2 + B6*X_t^3
#     + B7*(X_c:X_t) + B8*(X_in:X_t) + B9*(X_iu:X_t)
#     + B10*(X_c:X_t^2) + B11*(X_in:X_t^2) + B12*(X_iu:X_t^2)
#     + B13*(X_c:X_t^3) + B14*(X_in:X_t^3) + B15*(X_iu:X_t^3)

plot(CA_data_new_3$time_since, CA_data_new_3$median_sale_price, 
     pch = 16, 
     col = alpha(clr,0.5), 
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")
xs <- CA_data_new_3$time_since
# coastal urban line
# Y = B0 + B1 + B4*X_t + B5*X_t^2 + B6*X_t^3
#     + B7*(X_c:X_t)
#     + B10*(X_c:X_t^2)
#     + B13*(X_c:X_t^3)
ys <- cbind(1,1,0,0,xs,xs^2,xs^3,xs,0,0,xs^2,0,0,xs^3,0,0) %*% coef(lin_m7)
lines(xs,ys,col="blue",lwd=3)

# inland not populous
# Y = B0 + B2 + B4*X_t + B5*X_t^2 + B6*X_t^3
#     + B8*(X_in:X_t)
#     + B11*(X_in:X_t^2)
#     + B14*(X_in:X_t^3)
ys_2 <- cbind(1,0,1,0,xs,xs^2,xs^3,0,xs,0,0,xs^2,0,0,xs^3,0) %*% coef(lin_m7)
lines(xs,ys_2,col="red",lwd=3)

# inland urban
# Y = B0 + B3*X_iu + B4*X_t + B5*X_t^2 + B6*X_t^3
#     + B9*(X_iu:X_t)
#     + B12*(X_iu:X_t^2)
#     + B15*(X_iu:X_t^3)
ys_3 <- cbind(1,0,0,1,xs,xs^2,xs^3,0,0,xs,0,0,xs^2,0,0,xs^3) %*% coef(lin_m7)
lines(xs,ys_3,col="purple",lwd=3)

# coastal not populous
# Y = B0 + B4*X_t + B5*X_t^2 + B6*X_t^3
ys_4 <- cbind(1,0,0,0,xs,xs^2,xs^3,0,0,0,0,0,0,0,0,0) %*% coef(lin_m7)
lines(xs,ys_4,col="green",lwd=3)


lin_m8 <- lm(median_sale_price ~ factor(area_type)*poly(time_since,5,raw=TRUE), 
             CA_data_new_3)
summary(lin_m8)

clr <- rep("blue", length(CA_data_new_3$area_type)) # blue for coastal urban
clr[CA_data_new_3$area_type=="coastal_not_populous"] <- "green" # green for coastal rural
clr[CA_data_new_3$area_type=="inland_not_populous"] <- "red" # red for inland rural
clr[CA_data_new_3$area_type=="inland_urban"] <- "purple" # purple for inland urban
table(clr,CA_data_new_3$area_type)

# model formula
# Y = B0 + B1*X_c + B2*X_in + B3*X_iu + 
#.    + B4*X_t + B5*X_t^2 + B6*X_t^3 + B7*X_t^4 + B8*X_t^5
#     + B9*(X_c:X_t) + B10*(X_in:X_t) + B11*(X_iu:X_t)
#     + B12*(X_c:X_t^2) + B13*(X_in:X_t^2) + B14*(X_iu:X_t^2)
#     + B15*(X_c:X_t^3) + B16*(X_in:X_t^3) + B17*(X_iu:X_t^3)
#     + B18*(X_c:X_t^4) + B19*(X_in:X_t^4) + B20*(X_iu:X_t^4)
#     + B21*(X_c:X_t^5) + B22*(X_in:X_t^5) + B23*(X_iu:X_t^5)

plot(CA_data_new_3$time_since, CA_data_new_3$median_sale_price, 
     pch = 16, 
     col = alpha(clr,0.5), 
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")
# coastal urban line
# Y = B0 + B1*X_c
#.    + B4*X_t + B5*X_t^2 + B6*X_t^3 + B7*X_t^4 + B8*X_t^5
#     + B9*(X_c:X_t)
#     + B12*(X_c:X_t^2)
#     + B15*(X_c:X_t^3)
#     + B18*(X_c:X_t^4)
#     + B21*(X_c:X_t^5)
ys <- cbind(1,1,0,0,xs,xs^2,xs^3,xs^4,xs^5,xs,0,0,
            xs^2,0,0,xs^3,0,0,xs^4,0,0,xs^5,0,0) %*% coef(lin_m8)
lines(xs,ys,col="blue",lwd=3)

# inland not populous
# Y = B0 + B2*X_in
#.    + B4*X_t + B5*X_t^2 + B6*X_t^3 + B7*X_t^4 + B8*X_t^5
#     + B10*(X_in:X_t)
#     + B13*(X_in:X_t^2)
#     + B16*(X_in:X_t^3)
#     + B19*(X_in:X_t^4)
#     + B22*(X_in:X_t^5)
ys_2 <- cbind(1,0,1,0,xs,xs^2,xs^3,xs^4,xs^5,0,xs,0,
              0,xs^2,0,0,xs^3,0,0,xs^4,0,0,xs^5,0) %*% coef(lin_m8)
lines(xs,ys_2,col="red",lwd=3)

# inland urban
# Y = B0 + B3*X_iu
#.    + B4*X_t + B5*X_t^2 + B6*X_t^3 + B7*X_t^4 + B8*X_t^5
#     + B11*(X_iu:X_t)
#     + B14*(X_iu:X_t^2)
#     + B17*(X_iu:X_t^3)
#     + B20*(X_iu:X_t^4)
#     + B23*(X_iu:X_t^5)
ys_3 <- cbind(1,0,0,1,xs,xs^2,xs^3,xs^4,xs^5,0,0,xs,
              0,0,xs^2,0,0,xs^3,0,0,xs^4,0,0,xs^5) %*% coef(lin_m8)
lines(xs,ys_3,col="purple",lwd=3)

# coastal not populous
# Y = B0 + B4*X_t + B5*X_t^2 + B6*X_t^3 + B7*X_t^4 + B8*X_t^5
ys_4 <- cbind(1,0,0,0,xs,xs^2,xs^3,xs^4,xs^5,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0) %*% coef(lin_m8)
lines(xs,ys_4,col="green",lwd=3)


#--------------------------------Final GAM models-------------------------------
CA_data_new_3$region <- factor(CA_data_new_3$region)
CA_data_new_3$area_type <- factor(CA_data_new_3$area_type)
CA_data_new_3$property_type <- factor(CA_data_new_3$property_type)


# fixed effect model
gam_model0 <- gam(median_sale_price ~ time_since,
                  data=coastal_urban)
summary(gam_model0)
gam.check(gam_model0)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
plot(CA_data_new_3$time_since, CA_data_new_3$median_sale_price, 
     pch = 16, 
     col = alpha("black", 0.5),
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")

# vector of x's filtered by coastal urban area type
# This will plot the best line of fit for coastal urban observations
CA_data_new_3$pred_gam_m0 <- gam_model0$fitted.values
lines(CA_data_new_3$time_since, CA_data_new_3$pred_gam_m0, col = "blue", lwd = 3)

# random-intercept only
gam_model4 <- gam(median_sale_price ~ s(time_since) + s(region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model4)
gam.check(gam_model4)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, area_type),
                          time_since = unique(time_since))

pred_gam_m4 <- bind_cols(CA_data_fit,
                     as.data.frame(predict(gam_model4, newdata = CA_data_fit,
                                           se.fit = TRUE)))
ggplot(pred_gam_m4, aes(x = time_since, y = fit, group = region,
                    colour = area_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") + 
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept by County",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type)


# random-slope only
gam_model5 <- gam(median_sale_price ~ s(time_since)+ s(time_since,region,bs="re"),
                  data = CA_data_new_3)
summary(gam_model5)
gam.check(gam_model5)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m5 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model5, newdata = CA_data_fit,
                                               se.fit = TRUE)))
ggplot(pred_gam_m5, aes(x = time_since, y = fit, group = region,
                        colour = area_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type", 
       subtitle="Random Slope by County",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type)

# random-intercept and slope (this one is wrong)
gam_model6 <- gam(median_sale_price ~ s(time_since, region, bs ="re"),
                  data = CA_data_new_3)
summary(gam_model6)


# random-intercept and random slope
gam_model7 <- gam(median_sale_price ~ s(time_since) + s(region, bs="re") + 
                    s(time_since, region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model7)
gam.check(gam_model7)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m7 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model7, newdata = CA_data_fit,
                                               se.fit = TRUE)))
ggplot(pred_gam_m7, aes(x = time_since, y = fit, group = region,
                        colour = area_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type", 
       subtitle="Random Intercept and Slope by County",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type)

# Now we adding area type as an interaction to the model
gam_model8 <- gam(median_sale_price ~ time_since:area_type +
                    s(region, bs="re") +
                    s(time_since, region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model8)
gam.check(gam_model8)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m8 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model8, newdata = CA_data_fit,
                                               se.fit = TRUE)))
ggplot(pred_gam_m8, aes(x = time_since, y = fit, group = region,
                        colour = area_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept and Slope with Time and Area Type Interaction",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type)


# Now we adding property type as an interaction to the model
gam_model9 <- gam(median_sale_price ~ time_since:property_type +
                    s(region, bs="re") +
                    s(time_since, region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model9)
gam.check(gam_model9)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, property_type),
                             time_since = unique(time_since))

pred_gam_m9 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model9, newdata = CA_data_fit,
                                               se.fit = TRUE)))
ggplot(pred_gam_m9, aes(x = time_since, y = fit, group = region,
                        colour = property_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Property Type",
       subtitle="Random Intercept and Slope with Time and Property Type Interaction",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ property_type)



#-----------------------Final Linear Mixed Effect Models-----------------------

# random-intercept only
lme_model1 <- lmer(median_sale_price ~ time_since + (1|region),
                  data = CA_data_new_3)
summary(lme_model1)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, area_type),
                             time_since = unique(time_since))

pred_lme_m1 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(lme_model1, newdata = CA_data_fit)))

colnames(pred_lme_m1)[4] <- c("fit")

ggplot(pred_lme_m1, aes(x = time_since, y = fit, group = region,
                        colour = area_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept by County",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type)


# random-slope only
lme_model2 <- lmer(median_sale_price ~ time_since + (0 + time_since|region),
                  data = CA_data_new_3)
summary(lme_model2)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, area_type),
                             time_since = unique(time_since))

pred_lme_m2 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(lme_model2, newdata = CA_data_fit)))

colnames(pred_lme_m2)[4] <- c("fit")

ggplot(pred_lme_m2, aes(x = time_since, y = fit, group = region,
                        colour = area_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Slope by County",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  geom_line(linewidth=1, color = "black") +
  facet_wrap(~ area_type)


# random-intercept and random slope
lme_model3 <- lmer(median_sale_price ~ time_since + (1|region) +
                    (0 + time_since|region),
                  data = CA_data_new_3)
summary(lme_model3)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, area_type),
                             time_since = unique(time_since))

pred_lme_m3 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(lme_model3, newdata = CA_data_fit)))

colnames(pred_lme_m3)[4] <- c("fit")

ggplot(pred_lme_m3, aes(x = time_since, y = fit, group = region,
                        colour = area_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept and Slope by County",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type)

# Now we adding area type as an interaction to the model
lme_model4 <- lmer(median_sale_price ~ area_type:time_since + (1|region) +
                     (0 + time_since|region),
                   data = CA_data_new_3)
summary(lme_model4)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, area_type),
                             time_since = unique(time_since))

pred_lme_m4 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(lme_model4, newdata = CA_data_fit)))

colnames(pred_lme_m4)[4] <- c("fit")

ggplot(pred_lme_m4, aes(x = time_since, y = fit, group = region,
                        colour = area_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept and Slope by County with Time and Area Type Interaction",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type)


# Now we adding property type as an interaction to the model
lme_model5 <- lmer(median_sale_price ~ property_type:time_since + (1|region) +
                     (0 + time_since|region),
                   data = CA_data_new_3)
summary(lme_model5)

CA_data_fit <- tidyr::expand(CA_data_new_3, nesting(region, property_type),
                             time_since = unique(time_since))

pred_lme_m5 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(lme_model5, newdata = CA_data_fit)))

colnames(pred_lme_m5)[4] <- c("fit")

ggplot(pred_lme_m5, aes(x = time_since, y = fit, group = region,
                        colour = property_type)) +
  geom_point(data = CA_data_new_3, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept and Slope by County with Time and Property Type Interaction",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ property_type)

# Now we adding population into the model
lme_model6 <- lmer(median_sale_price ~ property_type:time_since + (1|region) +
                     (0 + time_since|region) + log(population),
                   data = CA_data_new_3)
summary(lme_model6)



#-----------------------Final GAM models for Condo/Co-op------------------------
condo_data$region <- factor(condo_data$region)
condo_data$area_type <- factor(condo_data$area_type)

# fixed effect model
gam_model0 <- gam(log(median_sale_price) ~ s(time_since),
                  data=condo_data)
summary(gam_model0)
gam.check(gam_model0)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
condo_data$pred_gam_m0 <- exp(gam_model0$fitted.values)

# This will plot the best line of fit for coastal urban observations
CA_reg_line_gam_m0_c <- ggplot(condo_data, aes(time_since, median_sale_price)) +
  geom_point(alpha=0.5) +
  geom_line(aes(time_since,pred_gam_m0),color="blue",linewidth=1) +
  labs(title = "Median Sale price over Time",
       subtitle = "For Condominiums",
       x = "Elapsed Time (by month)",
       y = "Median Sale Price")

ggsave("CA_reg_line_gam_m0_c.pdf",CA_reg_line_gam_m0_c)



# random-intercept only
gam_model4 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re"),
                  data = condo_data)
summary(gam_model4)
gam.check(gam_model4)

CA_data_fit <- tidyr::expand(condo_data, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m4 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model4, newdata = CA_data_fit,
                                               se.fit = TRUE)))

pred_gam_m4$fit_2 <- exp(pred_gam_m4$fit)
CA_reg_line_rand_int_gam_c <- ggplot(pred_gam_m4,
                                   aes(x = time_since,
                                       y = fit_2, group = region,
                                       colour = area_type)) +
  geom_point(data = condo_data, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") + 
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept by County for Condominiums",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y")

ggsave("CA_reg_line_rand_int_gam_c.pdf",CA_reg_line_rand_int_gam_c)


# random-intercept and slope (this one is wrong)
gam_model6 <- gam(median_sale_price ~ s(time_since, region, bs ="re"),
                  data = condo_data)
summary(gam_model6)


# random-intercept and random slope
gam_model7 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re") + 
                    s(time_since, region, bs="re"),
                  data = condo_data)
summary(gam_model7)
gam.check(gam_model7)

CA_data_fit <- tidyr::expand(condo_data, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m7 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model7, newdata = CA_data_fit,
                                               se.fit = TRUE)))

pred_gam_m7$fit_2 <- exp(pred_gam_m7$fit)

CA_reg_line_rand_int_slope_gam_c <- ggplot(pred_gam_m7, 
                                     aes(x = time_since, 
                                         y = fit_2, group = region,
                                         colour = area_type)) +
    geom_point(data = condo_data, aes(y = median_sale_price)) +
                      geom_line(linewidth=1, color = "black") +
       labs(title="Median Sale Price vs Elapsed Time faceted by Area Type", 
           subtitle="Random Intercept and Slope by County for Condominiums",
            x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y")

ggsave("CA_reg_line_rand_int_slope_gam_c.pdf",CA_reg_line_rand_int_slope_gam_c)

# Now we adding area type as an interaction to the model
gam_model8 <- gam(log(median_sale_price) ~ time_since:area_type +
                    s(region, bs="re") +
                    s(time_since, region, bs="re"),
                  data = condo_data)
summary(gam_model8)
gam.check(gam_model8)

CA_data_fit <- tidyr::expand(condo_data, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m8 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model8, newdata = CA_data_fit,
                                               se.fit = TRUE)))

pred_gam_m8$fit_2 <- exp(pred_gam_m8$fit)

ggplot(pred_gam_m8, aes(x = time_since, y = fit_2, group = region,
                        colour = area_type)) +
  geom_point(data = condo_data, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept and Slope with Time and Area Type Interaction for Condominiums",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type)


# Now we adding property type as an interaction to the model
gam_model9 <- gam(log(median_sale_price) ~ time_since:property_type +
                    s(region, bs="re") +
                    s(time_since, region, bs="re"),
                  data = condo_data)
summary(gam_model9)
gam.check(gam_model9)

CA_data_fit <- tidyr::expand(condo_data, nesting(region, property_type),
                             time_since = unique(time_since))

pred_gam_m9 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model9, newdata = CA_data_fit,
                                               se.fit = TRUE)))

pred_gam_m9$fit_2 <- exp(pred_gam_m9$fit)

ggplot(pred_gam_m9, aes(x = time_since, y = fit, group = region,
                        colour = property_type)) +
  geom_point(data = condo_data, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Property Type",
       subtitle="Random Intercept and Slope with Time and Property Type Interaction for Condominiums",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ property_type)

#----------------Final GAM models for Single Family Residential-----------------
single_family_data$region <- factor(single_family_data$region)
single_family_data$area_type <- factor(single_family_data$area_type)

# fixed effect model
gam_model10 <- gam(log(median_sale_price) ~ s(time_since),
                  data=single_family_data)
summary(gam_model10)
gam.check(gam_model10)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
single_family_data$gam_model10 <- exp(gam_model10$fitted.values)

# This will plot the best line of fit for coastal urban observations
CA_reg_line_gam_m10_s <- ggplot(single_family_data, aes(time_since, median_sale_price)) +
  geom_point(alpha=0.5) +
  geom_line(aes(time_since,gam_model10),color="blue",linewidth=1) +
  labs(title = "Median Sale price over Time",
       subtitle = "For Single Family Residential",
       x = "Elapsed Time (by month)",
       y = "Median Sale Price")

ggsave("CA_reg_line_gam_m10_s.pdf",CA_reg_line_gam_m10_s)



# random-intercept only
gam_model11 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re"),
                  data = single_family_data)
summary(gam_model11)
gam.check(gam_model11)

CA_data_fit <- tidyr::expand(single_family_data, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m11 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model11, newdata = CA_data_fit,
                                               se.fit = TRUE)))

pred_gam_m11$fit_2 <- exp(pred_gam_m11$fit)

CA_reg_line_rand_int_gam_s <- ggplot(pred_gam_m11,
                                   aes(x = time_since,
                                       y = fit_2, group = region,
                                       colour = area_type)) +
  geom_point(data = single_family_data, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") + 
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept by County for Single Family Residential",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y")

ggsave("CA_reg_line_rand_int_gam_s.pdf",CA_reg_line_rand_int_gam_s)


# random-intercept and random slope
gam_model12 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re") + 
                    s(time_since, region, bs="re"),
                  data = single_family_data)
summary(gam_model12)
gam.check(gam_model12)

CA_data_fit <- tidyr::expand(single_family_data, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m12 <- bind_cols(CA_data_fit,
                         as.data.frame(predict(gam_model12, newdata = CA_data_fit,
                                               se.fit = TRUE)))

pred_gam_m12$fit_2 <- exp(pred_gam_m12$fit)

CA_reg_line_rand_int_slope_gam_s <- ggplot(pred_gam_m12, 
                                         aes(x = time_since, 
                                             y = fit_2, group = region,
                                             colour = area_type)) +
  geom_point(data = single_family_data, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type", 
       subtitle="Random Intercept and Slope by County for Single Family Residential",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y")

ggsave("CA_reg_line_rand_int_slope_gam_s.pdf",CA_reg_line_rand_int_slope_gam_s)





#------------------------Final GAM models for Townhomes-------------------------
townhome_data$region <- factor(townhome_data$region)
townhome_data$area_type <- factor(townhome_data$area_type)

# fixed effect model
gam_model13 <- gam(log(median_sale_price) ~ s(time_since),
                   data=townhome_data)
summary(gam_model13)
gam.check(gam_model13)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
townhome_data$gam_model13 <- exp(gam_model13$fitted.values)

# This will plot the best line of fit for coastal urban observations
CA_reg_line_gam_m13_t <- ggplot(townhome_data, aes(time_since, median_sale_price)) +
  geom_point(alpha=0.5) +
  geom_line(aes(time_since,gam_model13),color="blue",linewidth=1) +
  labs(title = "Median Sale price over Time",
       subtitle = "For Townhomes",
       x = "Elapsed Time (by month)",
       y = "Median Sale Price")

ggsave("CA_reg_line_gam_m13_t.pdf",CA_reg_line_gam_m13_t)



# random-intercept only
gam_model14 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re"),
                   data = townhome_data)
summary(gam_model14)
gam.check(gam_model14)

CA_data_fit <- tidyr::expand(townhome_data, nesting(region, area_type),
                             time_since = unique(time_since))

pred_gam_m14 <- bind_cols(CA_data_fit,
                          as.data.frame(predict(gam_model14, newdata = CA_data_fit,
                                                se.fit = TRUE)))

pred_gam_m14$fit_2 <- exp(pred_gam_m14$fit)

CA_reg_line_rand_int_gam_t <- ggplot(pred_gam_m14,
                                     aes(x = time_since,
                                         y = fit_2, group = region,
                                         colour = area_type)) +
  geom_point(data = townhome_data, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") + 
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type",
       subtitle="Random Intercept by County for Townhomes",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y")

ggsave("CA_reg_line_rand_int_gam_t.pdf",CA_reg_line_rand_int_gam_t)


# random-intercept and random slope
gam_model15 <- gam(log(median_sale_price) ~ s(time_since) + s(region, bs="re") + 
                     s(time_since, region, bs="re"),
                   data = townhome_data)
summary(gam_model15)
gam.check(gam_model15)

CA_data_fit <- tidyr::expand(townhome_data, nesting(region, area_type),
                             time_since = unique(time_since))

CA_data_fit <- townhome_data %>% select(region,time_since,area_type)

pred_gam_m15 <- bind_cols(CA_data_fit,
                          as.data.frame(predict(gam_model15, newdata = CA_data_fit,
                                                se.fit = TRUE)))

pred_gam_m15$fit_2 <- exp(pred_gam_m15$fit)

pred_gam_m15 <- predict(gam_model15, newdata = townhome_data, se.fit = TRUE)

CA_reg_line_rand_int_slope_gam_t <- ggplot(pred_gam_m15, 
                                           aes(x = time_since, 
                                               y = fit_2, group = region,
                                               colour = area_type)) +
  geom_point(data = townhome_data, aes(y = median_sale_price)) +
  geom_line(linewidth=1, color = "black") +
  labs(title="Median Sale Price vs Elapsed Time faceted by Area Type", 
       subtitle="Random Intercept and Slope by County for Townhomes",
       x="Elapsed Time (monthly)",y="Median Sale Price") +
  facet_wrap(~ area_type, scales = "free_y")

ggsave("CA_reg_line_rand_int_slope_gam_t.pdf",CA_reg_line_rand_int_slope_gam_t)
