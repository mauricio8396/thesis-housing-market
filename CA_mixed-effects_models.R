library(broom)
library(dplyr)
library(classInt)
library(lme4)
library(nlme)
library(mgcv)
library(ggplot2)
library(cowplot)
library(scales)

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

#-------------------------Linear Regression models-------------------------


lm(median_sale_price ~ region + homes_sold + area_type, data = data_new)

CA_data_new_3$population <- gsub(",","",CA_data_new_3$population)
CA_data_new_3$population <- as.numeric(CA_data_new_3$population)
hist(CA_data_new_3$population)
d <- density(CA_data_new_3$population)
plot(d, main="Kernel Density of Population")
hist(log(CA_data_new_3$population))
d2 <- density(log(CA_data_new_3$population))
plot(d2, main="Kernel Density of Population")

lin_m0 <- lm(median_sale_price ~ time_since, data = CA_data_new_3)

CA_data_fit <- bind_cols(
  CA_data_new_3, pred_lin_m0 = predict(lin_m0, re.form = ~ 0)
)

plot(CA_data_fit$time_since, CA_data_fit$median_sale_price) + 
  abline(a = lin_m0$coefficients[1], b = lin_m0$coefficients[2], col="red", lwd= 5)

plot(CA_data_fit$time_since, CA_data_fit$median_sale_price, col = CA_data_fit$area_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black")
  #geom_abline(slope = coef(lin_m0)[2], intercept = coef(lin_m0)[1]) +

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
geom_abline(slope = coef(lin_m0)[2], intercept = coef(lin_m0)[1])

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  #geom_abline(slope = coef(lin_m0)[2], intercept = coef(lin_m0)[1]) +
  facet_wrap( ~ area_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), col = "black") +
  facet_wrap( ~ area_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,3), col = "black") +
  facet_wrap( ~ area_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  facet_wrap( ~ property_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), col = "black") +
  facet_wrap( ~ property_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,3), col = "black") +
  facet_wrap( ~ property_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  facet_wrap(property_type ~ area_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), col = "black") +
  facet_wrap(property_type ~ area_type)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,3), col = "black") +
  facet_wrap(property_type ~ area_type)


lin_m1 <- lm(median_sale_price ~ time_since + area_type + log(population), data = CA_data_new_3)

CA_data_fit$pred_lin_m1 <- predict(lin_m1, re.form = ~ 0)

plot(CA_data_fit$time_since, CA_data_fit$median_sale_price) + 
  abline(a = lin_m1$coefficients[1], b = lin_m1$coefficients[2], col="red", lwd= 5)

plot(CA_data_fit$population, CA_data_fit$median_sale_price) + 
  abline(a = lin_m1$coefficients[1], b = lin_m1$coefficients[6], col="red", lwd= 5)

ggplot(CA_data_fit, aes(time_since, median_sale_price)) + geom_point() +
  geom_abline(intercept = lin_m1$coefficients[1], slope = lin_m1$coefficients[2], color = "red", size = 1)
ggplot(CA_data_fit, aes(area_type, median_sale_price)) + geom_jitter() +
  geom_abline(intercept = lin_m1$coefficients[1], slope = lin_m1$coefficients[3] + lin_m1$coefficients[4] + lin_m1$coefficients[5])
ggplot(CA_data_fit, aes(population, median_sale_price)) + geom_point() +
  geom_abline(intercept = lin_m1$coefficients[1], slope = lin_m1$coefficients[6])

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black")
#geom_abline(slope = coef(lin_m0)[2], intercept = coef(lin_m0)[1]) +

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = property_type)) + 
  geom_point() +
  geom_abline(slope = coef(lin_m1)[2], intercept = coef(lin_m1)[1])


lin_m2 <- lm(median_sale_price ~ (time_since)^2 + area_type + log(population), data = CA_data_new_3)

CA_data_fit$pred_lin_m2 <- predict(lin_m2, re.form = ~ 0)

lin_m3 <- lm(median_sale_price ~ time_since + (area_type)^2 + log(population), data = CA_data_new_3)

CA_data_fit$pred_lin_m3 <- predict(lin_m3, re.form = ~ 0)

lin_m4 <- lm(median_sale_price ~ (time_since)^2 + (area_type)^2 + log(population), data = CA_data_new_3)

CA_data_fit$pred_lin_m4 <- predict(lin_m4, re.form = ~ 0)


lin_m5 <- lm(median_sale_price ~ time_since + factor(area_type), data = CA_data_new_3)
summary(lin_m5)

lin_m6 <- lm(median_sale_price ~ time_since + area_type, data = CA_data_new_3)
summary(lin_m6)

#---------------------------Stratified by property type-------------------------
lin_s <- lm(median_sale_price ~ time_since, 
              data = CA_data_new_3, 
              subset = (property_type == "Single Family Residential"))

p1 <- CA_data_new_3 %>% filter(property_type == "Single Family Residential") %>%
  ggplot(aes(time_since, median_sale_price)) + geom_point() +
  geom_abline(intercept = coef(lin_s)[1], slope = coef(lin_s)[2], 
              col = "red", linewidth = 1)


lin_c <- lm(median_sale_price ~ time_since, 
              data = CA_data_new_3, 
              subset = (property_type == "Condo/Co-op"))

p2 <- CA_data_new_3 %>% filter(property_type == "Condo/Co-op") %>%
  ggplot(aes(time_since, median_sale_price)) + geom_point() +
  geom_abline(intercept = coef(lin_s)[1], slope = coef(lin_s)[2], 
              col = "red", linewidth = 1)

lin_t <- lm(median_sale_price ~ time_since, 
              data = CA_data_new_3, 
              subset = (property_type == "Townhouse"))

p3 <- CA_data_new_3 %>% filter(property_type == "Townhouse") %>%
  ggplot(aes(time_since, median_sale_price)) + geom_point() +
  geom_abline(intercept = coef(lin_s)[1], slope = coef(lin_s)[2], 
              col = "red", linewidth = 1)



lin_s_2 <- lm(median_sale_price ~ time_since + area_type + log(population), 
            data = CA_data_new_3, 
            subset = (property_type == "Single Family Residential"))

p4 <- CA_data_new_3 %>% filter(property_type == "Single Family Residential") %>%
  ggplot(aes(time_since, median_sale_price)) + geom_point() +
  geom_abline(intercept = coef(lin_s_2)[1], slope = coef(lin_s_2)[2],
              col = "red", linewidth = 1)


lin_c_2 <- lm(median_sale_price ~ time_since + area_type + log(population), 
            data = CA_data_new_3, 
            subset = (property_type == "Condo/Co-op"))

p5 <- CA_data_new_3 %>% filter(property_type == "Condo/Co-op") %>%
  ggplot(aes(time_since, median_sale_price)) + geom_point() +
  geom_abline(intercept = coef(lin_s_2)[1], slope = coef(lin_s_2)[2],
              col = "red", linewidth = 1)

lin_t_2 <- lm(median_sale_price ~ time_since + area_type + log(population), 
            data = CA_data_new_3, 
            subset = (property_type == "Townhouse"))

p6 <- CA_data_new_3 %>% filter(property_type == "Townhouse") %>%
  ggplot(aes(time_since, median_sale_price)) + geom_point() +
  geom_abline(intercept = coef(lin_s_2)[1], slope = coef(lin_s_2)[2],
              col = "red", linewidth = 1)


plot_grid(p1, p2, p3, p4, p5, p5, p6)

#--------------Linear Mixed Effects models (Longitudinal Models)----------------

m1 <- lmer(median_sale_price ~ period_end + (period_end | region), CA_data_new_2)
summary(m1)

lin_0 <- lmer(median_sale_price ~ 1 + (1 | region), data = CA_data_new_2)
summary(lin_0)


m2 <- lmer(median_sale_price ~ homes_sold + (1 | region), CA_data_new_2)
summary(m2)


m3 <- lmer(median_sale_price ~ homes_sold + area_type + (1 | region), CA_data_new_2)
summary(m3)

m4 <- lmer(median_sale_price ~ homes_sold + area_type + property_type + (1 | region), CA_data_new_2)
summary(m4)

m5 <- lmer(median_sale_price ~ homes_sold*area_type + property_type + (1 | region), CA_data_new_2)
summary(m5)

m6 <- lmer(median_sale_price ~ homes_sold*property_type + area_type + (1 | region), CA_data_new_2)
summary(m6)

m7 <- lmer(median_sale_price ~ homes_sold*property_type + area_type + period_end + (1 | region), CA_data_new_2)
summary(m7)

m8 <- lmer(median_sale_price ~ homes_sold*property_type + area_type + (period_end | region), CA_data_new_2)
summary(m8)

CA_data_fit <- bind_cols(
  CA_data_new_2, pred_m2 = predict(m2, re.form = ~ 0)
)

m9 <- lmer(median_sale_price ~ time_since + (time_since | population), CA_data_new_3)
summary(m9)


#-------------------------Generalized Additive Models---------------------------
gam_m0 <- gam(median_sale_price ~ time_since,
              data = CA_data_new_3)
summary(gam_m0)

ggplot(CA_data_fit, aes(time_since, median_sale_price, col = area_type)) + 
  geom_point() +
  geom_smooth(method = "gam", col = "black")

gam_m1 <- gam(median_sale_price ~ s(time_since, bs = "cr"),
              data = CA_data_new_3)
summary(gam_m1)

gam_m2 <- gam(median_sale_price ~ time_since + area_type + log(population),
              data = CA_data_new_3)
summary(gam_m2)

gam_m3 <- gam(median_sale_price ~ time_since*log(population),
              data = CA_data_new_3)
summary(gam_m3)

gam_m4 <- gam(median_sale_price ~ s(time_since) + s(area_type) + s(log(population)),
              data = CA_data_new_3)
summary(gam_m4)

gam_m5 <- gam(median_sale_price ~ s(time_since, bs = "cr") + s(area_type) + s(log(population)),
              data = CA_data_new_3)
summary(gam_m5)

gam_m6 <- gam(median_sale_price ~ s(time_since, bs = "cr") + s(area_type, bs = "cr") + s(log(population), bs = "cr"),
              data = CA_data_new_3)
summary(gam_m6)
#---------------------------------Spatial Model---------------------------------




plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
})


par(mfrow = c(1,5))
for (i in 2:5) {
  plot(CA_HOMES[, i], CA_data_fit$median_sale_price, xlab = names(CA_HOMES)[i], ylab = "Response (y)", main = paste("Scatterplot of", names(CA_HOMES)[i]))
  abline(coef(lin_m1)[1], coef(lin_m1)[i + 1], col = "red")
}


p <- ggplot(mpg, aes(cty, hwy)) + geom_point()
p + geom_abline() + facet_wrap(~cyl)

plot(mpg~wt,mtcars,col=mtcars$cyl,pch=20)
curve(predict(lmfit,newdata=data.frame(wt=x,cyl=4)),col=4,add=T)
curve(predict(lmfit,newdata=data.frame(wt=x,cyl=6)),col=6,add=T)
curve(predict(lmfit,newdata=data.frame(wt=x,cyl=8)),col=8,add=T)


tidy(emmeans(m2, c("homes_sold")), conf.int = TRUE)

ggplot(CA_data_fit, aes(period_end, median_sale_price)) +
  geom_line(aes(group = factor(region))) +
  geom_point(aes(y = pred_m2), col = "blue", size = 2) + 
  labs(x = "Period (month)", y = "Median Sale Price")


dental_fit$pred_agesex <- predict(lin_agesex, re.form = ~ 0)
ggplot(dental_fit, aes(age, distance)) +
  geom_line(aes(group = factor(id))) +
  geom_point(aes(y = pred_agesex, col = sex), size = 2) + 
  labs(x = "Age, years", y = "Dental growth, mm", col = "Sex")


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


#------------------------------Final GAM models---------------------------------
CA_data_new_3$area_type <- factor(CA_data_new_3$area_type)
CA_data_new_3$region <- factor(CA_data_new_3$region)

gam_m7 <- gam(median_sale_price ~ s(time_since) + area_type,
              data = CA_data_new_3)
summary(gam_m7)
plot(gam_m7, se = TRUE)
plot(CA_data_new_3$time_since, CA_data_new_3$median_sale_price, 
     pch = 16, 
     col = clr,
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")
xs_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban") %>% select(time_since)
new_data_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban")
lines(xs_2[,1], predict(gam_m7, newdata = new_data_2), col = "blue", lwd = 3)


# Generate predictions from the GAM model for the smooth terms
CA_data_new_3$predicted <- predict(gam_m7, 
                                   newdata = data.frame(time_since = CA_data_new_3$time_since), 
                                   type = "response")


# Add the smooth terms to the plot
lines(CA_data_new_3$time_since, CA_data_new_3$predicted, col = "red", lwd = 2)

plot(gam_m7, select = 1, shade = TRUE, col = "blue", lwd = 2, main = "Fitted Smooth Term for x1")


#fitting model generalized additive model with two smooth terms and a factor variable
#a lower smoothing parameter k leads to a more flexible model
gam_model1 <- gam(median_sale_price ~ s(time_since, k=10) + s(population, k=350) + 
                   factor(area_type),
              data = CA_data_new_3)
summary(gam_model1)
par(mfrow=c(2,2))
gam.check(gam_model1)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
plot(CA_data_new_3$time_since, CA_data_new_3$median_sale_price, 
     pch = 16, 
     col = clr,
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")

# vector of x's filtered by coastal urban area type
# This will plot the best line of fit for coastal urban observations
xs_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban") %>% select(time_since)
new_data_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban")
lines(xs_2[,1], predict(gam_model1, newdata = new_data_2), col = "blue", lwd = 3)


#fitting model generalized additive model with two smooth terms and a factor variable
# note that in this model we set k=30 meaning this is a more constrained model
gam_model2 <- gam(median_sale_price ~ s(time_since, k=30) + s(population, k=30) + 
                    factor(area_type),
                  data = CA_data_new_3)
summary(gam_model2)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
plot(CA_data_new_3$time_since, CA_data_new_3$median_sale_price, 
     pch = 16, 
     col = alpha(clr,0.5), 
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")

# vector of x's filtered by coastal urban area type
# This will plot the best line of fit for coastal urban observations
xs_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban") %>% select(time_since)
lines(xs_2[,1], predict(gam_model2, newdata = new_data), col = "blue", lwd = 3)


plot(gam_m8, select = 1, shade = TRUE, col = "blue", lwd = 2, main = "Fitted Smooth Term for x1")
plot(gam_m8, select = 3, shade = TRUE, col = "blue", lwd = 2, main = "Fitted Smooth Term for x1")


#fitting model generalized additive model with two smooth terms and a factor variable
# note that in this model we set k=30 meaning this is a more constrained model
gam_model3 <- gam(median_sale_price ~ s(time_since, k=10, bs ="cr") + 
                    s(population, k=10, bs="re", by=region) +
                    s(region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model3)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
xs_2 <- CA_data_new_3 %>% filter(region == "Los Angeles County, CA") %>% select(time_since)
ys_2 <- CA_data_new_3 %>% filter(region == "Los Angeles County, CA") %>% select(median_sale_price)
subdata <- CA_data_new_3 %>% filter(region == "Los Angeles County, CA")

clr <- rep("blue", length(subdata$property_type)) # blue for single family residential
clr[subdata$property_type=="Condo/Co-op"] <- "red" # red for Condo/Co-op
clr[subdata$property_type=="Townhouse"] <- "purple" # purple for Townhouse
table(clr,subdata$property_type)

plot(subdata$time_since, subdata$median_sale_price, 
     pch = 16, 
     col = alpha(clr,0.5), 
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")

# vector of x's filtered by coastal urban area type
# This will plot the best line of fit for coastal urban observations
new_data <- CA_data_new_3 %>% filter(region == "Los Angeles County, CA")
lines(xs_2[,1], predict(gam_model3, newdata = new_data), col = "blue", lwd = 3)

xs_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban") %>% select(time_since)
new_data <- CA_data_new_3 %>% filter(area_type == "coastal_urban")
lines(xs_2[,1], predict(gam_model3, newdata = new_data), col = "blue", lwd = 3)


#plot residual value vs predicted value
plot(gam_model3$fitted.values,gam_model3$residuals,
     main = "Predicted Values vs Residuals",
     xlab = "Predicted Values",
     ylab = "Residuals")

#plot observed values vs predicted values
plot(gam_model3$fitted.values,CA_data_new_3$median_sale_price,
     main = "Predicted Values vs Observed Values",
     xlab = "Predicted Values",
     ylab = "Observed Values")


# Variables to include: time_since, region, area_type, population, area_type

# fixed effect model
gam_model0 <- gam(median_sale_price ~ time_since,
                  data=CA_data_new_3)
summary(gam_model0)
gam.check(gam_model0)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
plot(CA_data_new_3$time_since, CA_data_new_3$median_sale_price, 
     pch = 16, 
     col = clr,
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")

# vector of x's filtered by coastal urban area type
# This will plot the best line of fit for coastal urban observations
xs_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban") %>% select(time_since)
new_data_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban")
lines(xs_2[,1], predict(gam_model1, newdata = new_data_2), col = "blue", lwd = 3)

# random-intercept only
gam_model4 <- gam(median_sale_price ~ time_since + s(region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model4)
gam.check(gam_model4)

# random-slope only
gam_model5 <- gam(median_sale_price ~ time_since + s(time_since,region,bs="re"),
                  data = CA_data_new_3)
summary(gam_model5)
gam.check(gam_model5)

# random-intercept and slope (this one is wrong)
gam_model6 <- gam(median_sale_price ~ s(time_since, region, bs ="re"),
                  data = CA_data_new_3)
summary(gam_model6)


# random-intercept and random slope
gam_model7 <- gam(median_sale_price ~ time_since + s(region, bs="re") + 
                    s(time_since, region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model7)
gam.check(gam_model7)

# Now we adding area type as an interaction to the model
gam_model8 <- gam(median_sale_price ~ time_since:area_type +
                    s(region, bs="re") +
                    s(time_since, region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model8)
gam.check(gam_model8)

# Now we adding property type as an interaction to the model
gam_model9 <- gam(median_sale_price ~ time_since:property_type +
                    s(region, bs="re") +
                    s(time_since, region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model9)
gam.check(gam_model9)

# Lastly, now we add population to the last two models
gam_model10 <- gam(median_sale_price ~ time_since:area_type + 
                     log(population) +
                     s(region, bs="re") +
                     s(time_since, region, bs="re"),
                   data = CA_data_new_3)
summary(gam_model10)
gam.check(gam_model10)


gam_model11 <- gam(median_sale_price ~ time_since:area_type + 
                     s(population, by=region) +
                     s(region, bs="re") +
                     s(time_since, region, bs="re"),
                   data = CA_data_new_3)
summary(gam_model11)

gam_model12 <- gam(median_sale_price ~ time_since:property_type + 
                     log(population) +
                    s(region, bs="re") +
                    s(time_since, region, bs="re"),
                  data = CA_data_new_3)
summary(gam_model12)
gam.check(gam_model12)



gam_model13 <- gam(median_sale_price ~ time_since:property_type + 
                     s(time_since, population, region, bs="re") +
                     s(region, bs="re") +
                     s(time_since, region, bs="re"),
                   data = CA_data_new_3)
summary(gam_model13)

# Plotting observations: time on x-axis and median sale price on y-axis colored
# by area type
xs_2 <- CA_data_new_3 %>% filter(property_type == "Single Family Residential") %>% select(time_since)
ys_2 <- CA_data_new_3 %>% filter(region == "Los Angeles County, CA") %>% select(median_sale_price)
subdata <- CA_data_new_3 %>% filter(region == "Los Angeles County, CA")
subdata <- CA_data_new_3 %>% filter(property_type == "Single Family Residential")

clr <- rep("blue", length(CA_data_new_3$property_type)) # blue for single family residential
clr[CA_data_new_3$property_type=="Condo/Co-op"] <- "red" # red for Condo/Co-op
clr[CA_data_new_3$property_type=="Townhouse"] <- "purple" # purple for Townhouse
table(clr,CA_data_new_3$property_type)

plot(CA_data_new_3$time_since, CA_data_new_3$median_sale_price, 
     pch = 16, 
     col = alpha(clr,0.5), 
     main = "Median Sale price over Time",
     xlab = "Elapsed Time (by month)",
     ylab = "Median Sale Price")


# vector of x's filtered by coastal urban area type
# This will plot the best line of fit for coastal urban observations
lines(xs_2[,1], predict(gam_model13, newdata = subdata), col = "blue", lwd = 3)

xs_2 <- CA_data_new_3 %>% filter(area_type == "coastal_urban") %>% select(time_since)
new_data <- CA_data_new_3 %>% filter(area_type == "coastal_urban")
lines(xs_2[,1], predict(gam_model3, newdata = new_data), col = "blue", lwd = 3)

