####General Settings####

###Load Packages

library(foreign)
library(dplyr)
library(summarytools)
library(psych)
library(ggplot2)
library(corrr)
library(scales)
library(sjPlot)
library(cowplot)

###Load Functions for Two-Lines Tests

source("http://webstimate.org/twolines/twolines.R")

###Load Data

rcs21 <- read.spss("../data/ZA7703_v2-0-0_en.sav",
                   use.value.labels = FALSE, to.data.frame = TRUE)

rcs21[rcs21 < 0] <- NA

####Prepare Variables####

####Demographics

###Gender

rcs21$gend <- factor(rcs21$pre063,
                     levels = c(1, 2),
                     labels = c("male", "female"))

rcs21 <- rcs21 %>% mutate(gend_num = case_when(pre063 == 1 ~ 0,
                                               pre063 == 2 ~ 1))

###Age

rcs21$age <- 2021 - rcs21$pre062

###Education

rcs21$edu1 <- rcs21$pre064

rcs21 <- rcs21 %>% mutate(edu = case_when(edu1 == 1 | edu1 == 2 ~ 1,
                                          edu1 == 3 | edu1 == 6 | edu1 == 7 ~ 2,
                                          edu1 == 4 | edu1 == 5 ~ 3))

rcs21$edu <- factor(rcs21$edu,
                    levels = c(1, 2, 3),
                    labels = c("low", "medium", "high"))

rcs21 <- rcs21 %>% mutate(edu_d1 = case_when(edu1 == 1 | edu1 == 2 ~ 0,
                                          edu1 == 3 | edu1 == 6 | edu1 == 7 ~ 1,
                                          edu1 == 4 | edu1 == 5 ~ 0))

rcs21 <- rcs21 %>% mutate(edu_d2 = case_when(edu1 == 1 | edu1 == 2 ~ 0,
                                             edu1 == 3 | edu1 == 6 | edu1 == 7 ~ 0,
                                             edu1 == 4 | edu1 == 5 ~ 1))

###Region of residency

rcs21$region <- factor(rcs21$ostwest,
                       levels = c(0, 1),
                       labels = c("East Germany", "West Germany"))
####Affective Ambivalence

##Feelings

rcs21$Laschet_neg <- 6 - rcs21$pre032a
rcs21$Laschet_pos <- 6 - rcs21$pre033a

rcs21$Baerbock_neg <- 6 - rcs21$pre032b
rcs21$Baerbock_pos <- 6- rcs21$pre033b

rcs21$Scholz_neg <- 6- rcs21$pre032c
rcs21$Scholz_pos <- 6- rcs21$pre033c

##Ambivalence scores

rcs21$Laschet_aamb <- (rcs21$Laschet_pos + rcs21$Laschet_neg) / 2 - 
  abs(rcs21$Laschet_pos - rcs21$Laschet_neg)

rcs21$Baerbock_aamb <- (rcs21$Baerbock_pos + rcs21$Baerbock_neg) / 2 - 
  abs(rcs21$Baerbock_pos - rcs21$Baerbock_neg)

rcs21$Scholz_aamb <- (rcs21$Scholz_pos + rcs21$Scholz_neg) / 2 - 
  abs(rcs21$Scholz_pos - rcs21$Scholz_neg)

####Cognitive Ambivalence

##Strengths and Weaknesses

rcs21$Laschet_weak <- 6 - rcs21$pre030a
rcs21$Laschet_str <- 6 - rcs21$pre031a

rcs21$Baerbock_weak <- 6 - rcs21$pre030b
rcs21$Baerbock_str <- 6- rcs21$pre031b

rcs21$Scholz_weak <- 6- rcs21$pre030c
rcs21$Scholz_str <- 6- rcs21$pre031c

##Ambivalence scores

rcs21$Laschet_camb <- (rcs21$Laschet_str + rcs21$Laschet_weak) / 2 - 
  abs(rcs21$Laschet_str - rcs21$Laschet_weak)

rcs21$Baerbock_camb <- (rcs21$Baerbock_str + rcs21$Baerbock_weak) / 2 - 
  abs(rcs21$Baerbock_str - rcs21$Baerbock_weak)

rcs21$Scholz_camb <- (rcs21$Scholz_str + rcs21$Scholz_weak) / 2 - 
  abs(rcs21$Scholz_str - rcs21$Scholz_weak)

####Political Variables

##Attitudes toward candidates

rcs21$Laschet_att <- rcs21$pre029a
rcs21$Baerbock_att <- rcs21$pre029b
rcs21$Scholz_att <- rcs21$pre029c

##Political interest

rcs21$pol_int <- 6 - rcs21$pre001

##Symbolic Ideology

rcs21$symb_id <- rcs21$pre018

####Analyses####

####Zero-Order Correlations

rcs21 %>% select(symb_id, 
                 pol_int, 
                 Laschet_aamb, Baerbock_aamb, Scholz_aamb,
                 Laschet_camb, Baerbock_camb, Scholz_camb) %>% 
  correlate()

cor.test(rcs21$symb_id, rcs21$Laschet_aamb, use = "complete.obs")
cor.test(rcs21$symb_id, rcs21$Laschet_camb, use = "complete.obs")

cor.test(rcs21$symb_id, rcs21$Scholz_aamb, use = "complete.obs")
cor.test(rcs21$symb_id, rcs21$Scholz_camb, use = "complete.obs")

cor.test(rcs21$symb_id, rcs21$Baerbock_aamb, use = "complete.obs")
cor.test(rcs21$symb_id, rcs21$Baerbock_camb, use = "complete.obs")

####Regression Models

##Laschet

#Affective Ambivalence

Laschet_M1 <- lm(rescale(Laschet_aamb) ~ gend + age + edu + rescale(pol_int) + rescale(symb_id), data = rcs21)
Laschet_M2 <- update(Laschet_M1, .~. + I(rescale(symb_id)^2))
Laschet_M3 <- update(Laschet_M2, .~. + rescale(Laschet_att) + I(rescale(Laschet_att)^2))


#Cognitive Ambivalence

Laschet_M4 <- lm(rescale(Laschet_camb) ~ gend + age + edu + rescale(pol_int) + rescale(symb_id), data = rcs21)
Laschet_M5 <- update(Laschet_M4, .~. + I(rescale(symb_id)^2))
Laschet_M6 <- update(Laschet_M5, .~. + rescale(Laschet_att) + I(rescale(Laschet_att)^2))

##Scholz

#Affective Ambivalence

Scholz_M1 <- lm(rescale(Scholz_aamb) ~ gend + age + edu + rescale(pol_int) + rescale(symb_id), data = rcs21)
Scholz_M2 <- update(Scholz_M1, .~. + I(rescale(symb_id)^2))
Scholz_M3 <- update(Scholz_M2, .~. + rescale(Scholz_att) + I(rescale(Scholz_att)^2))

#Cognitive Ambivalence

Scholz_M4 <- lm(rescale(Scholz_camb) ~ gend + age + edu + rescale(pol_int) + rescale(symb_id), data = rcs21)
Scholz_M5 <- update(Scholz_M4, .~. + I(rescale(symb_id)^2))
Scholz_M6 <- update(Scholz_M5, .~. + rescale(Scholz_att) + I(rescale(Scholz_att)^2))

##Baerbock

#Affective Ambivalence

Baerbock_M1 <- lm(rescale(Baerbock_aamb) ~ gend + age + edu + rescale(pol_int) + rescale(symb_id), data = rcs21)
Baerbock_M2 <- update(Baerbock_M1, .~. + I(rescale(symb_id)^2))
Baerbock_M3 <- update(Baerbock_M2, .~. + rescale(Baerbock_att) + I(rescale(Baerbock_att)^2))

#Cognitive Ambivalence

Baerbock_M4 <- lm(rescale(Baerbock_camb) ~ gend + age + edu + rescale(pol_int) + rescale(symb_id), data = rcs21)
Baerbock_M5 <- update(Baerbock_M4, .~. + I(rescale(symb_id)^2))
Baerbock_M6 <- update(Baerbock_M5, .~. + rescale(Baerbock_att) + I(rescale(Baerbock_att)^2))


####Two-Lines Tests

###Laschet

##Ideology & Ambivalence

a = twolines(Laschet_amb ~ symb_id, data = rcs21)

##Ideology and Residual Ambivalence (not explained by attitude)

rcs21$Laschet_amb_resid <- resid(lm(Laschet_amb ~ Laschet_att + I(Laschet_att^2), data = rcs21, na.action = na.exclude))

a = twolines(Laschet_amb_resid ~ symb_id, data = rcs21)

###Baerbock

##Ideology & Ambivalence

a = twolines(Baerbock_aamb ~ symb_id, data = rcs21)

##Ideology and Residual Ambivalence (not explained by attitude)

rcs21$Baerbock_amb_resid <- resid(lm(Baerbock_amb ~ Baerbock_att + I(Baerbock_att^2), data = rcs21, na.action = na.exclude))

a = twolines(Baerbock_amb_resid ~ symb_id, data = rcs21)

###Scholz

##Ideology & Ambivalence

a = twolines(Scholz_amb ~ symb_id, data = rcs21)

##Ideology and Residual Ambivalence (not explained by attitude)

rcs21$Scholz_amb_resid <- resid(lm(Scholz_amb ~ Laschet_att + I(Scholz_att^2), data = rcs21, na.action = na.exclude))

a = twolines(Scholz_amb_resid ~ symb_id, data = rcs21)

####Zero-Order Correlations with Residual Ambivalence

rcs21 %>% select(symb_id, Laschet_amb_resid, Baerbock_amb_resid, Scholz_amb_resid) %>% 
  correlate()