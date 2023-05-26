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
library(apaTables)

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

####Political Variables

##Attitudes toward candidates

rcs21$Laschet_att <- rcs21$pre029a
rcs21$Baerbock_att <- rcs21$pre029b
rcs21$Scholz_att <- rcs21$pre029c

##Political interest

rcs21$pol_int <- 6 - rcs21$pre001

##Symbolic Ideology

rcs21$symb_id <- rcs21$pre018

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

rcs21$Scholz_aamb_resid <- resid(lm(Scholz_aamb ~ Laschet_att + I(Scholz_att^2), data = rcs21, na.action = na.exclude))

rcs21$Laschet_aamb_resid <- resid(lm(Laschet_aamb ~ Laschet_att + I(Laschet_att^2), data = rcs21, na.action = na.exclude))

rcs21$Baerbock_aamb_resid <- resid(lm(Baerbock_aamb ~ Baerbock_att + I(Baerbock_att^2), data = rcs21, na.action = na.exclude))

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

rcs21$Scholz_camb_resid <- resid(lm(Scholz_camb ~ Laschet_att + I(Scholz_att^2), data = rcs21, na.action = na.exclude))

rcs21$Laschet_camb_resid <- resid(lm(Laschet_camb ~ Laschet_att + I(Laschet_att^2), data = rcs21, na.action = na.exclude))

rcs21$Baerbock_camb_resid <- resid(lm(Baerbock_camb ~ Baerbock_att + I(Baerbock_att^2), data = rcs21, na.action = na.exclude))


####Sample Description####

rcs21 %>% filter(!is.na(symb_id) & !is.na(Baerbock_aamb)) %>% freq(., gend)

rcs21 %>% filter(!is.na(symb_id) & !is.na(Baerbock_aamb)) %>% descr(., age)

####Analyses####

####Correlations

rcs21 %>% select(pol_int, 
                 symb_id, 
                 Scholz_aamb, Scholz_camb,
                 Laschet_aamb, Laschet_camb,
                 Baerbock_aamb, Baerbock_camb) %>% 
  apa.cor.table(., filename = "tables/table_1.doc")

####Compare Affective and Cognitive Ambivalence

t.test(rcs21$Scholz_aamb, rcs21$Scholz_camb, paired = TRUE)

t.test(rcs21$Laschet_aamb, rcs21$Laschet_camb, paired = TRUE)

t.test(rcs21$Baerbock_aamb, rcs21$Baerbock_camb, paired = TRUE)

####Regression Models


##Scholz

#Affective Ambivalence

Scholz_M1a <- lm(rescale(Scholz_aamb) ~ rescale(symb_id) + I(rescale(symb_id)^2), data = rcs21)
Scholz_M1b <- lm(rescale(Scholz_aamb) ~ rescale(symb_id) + + I(rescale(symb_id)^2) + rescale(Scholz_att) + rescale(pol_int) + gend + age + edu + region, data = rcs21)

#Cognitive Ambivalence

Scholz_M2a <- lm(rescale(Scholz_camb) ~ rescale(symb_id) + I(rescale(symb_id)^2), data = rcs21)
Scholz_M2b <- lm(rescale(Scholz_camb) ~ rescale(symb_id) + + I(rescale(symb_id)^2) + rescale(Scholz_att) + rescale(pol_int) + gend + age + edu + region, data = rcs21)

##Baerbock

#Affective Ambivalence

Baerbock_M1a <- lm(rescale(Baerbock_aamb) ~ rescale(symb_id) + I(rescale(symb_id)^2), data = rcs21)
Baerbock_M1b <- lm(rescale(Baerbock_aamb) ~ rescale(symb_id) + + I(rescale(symb_id)^2) + rescale(Baerbock_att) + rescale(pol_int) + gend + age + edu + region, data = rcs21)

#Cognitive Ambivalence

Baerbock_M2a <- lm(rescale(Baerbock_camb) ~ rescale(symb_id) + I(rescale(symb_id)^2), data = rcs21)
Baerbock_M2b <- lm(rescale(Baerbock_camb) ~ rescale(symb_id) + + I(rescale(symb_id)^2) + rescale(Baerbock_att) + rescale(pol_int) + gend + age + edu + region, data = rcs21)

##Laschet

#Affective Ambivalence

Laschet_M1a <- lm(rescale(Laschet_aamb) ~ rescale(symb_id) + I(rescale(symb_id)^2), data = rcs21)
Laschet_M1b <- lm(rescale(Laschet_aamb) ~ rescale(symb_id) + + I(rescale(symb_id)^2) + rescale(Laschet_att) + rescale(pol_int) + gend + age + edu + region, data = rcs21)

#Cognitive Ambivalence

Laschet_M2a <- lm(rescale(Laschet_camb) ~ rescale(symb_id) + I(rescale(symb_id)^2), data = rcs21)
Laschet_M2b <- lm(rescale(Laschet_camb) ~ rescale(symb_id) + + I(rescale(symb_id)^2) + rescale(Laschet_att) + rescale(pol_int) + gend + age + edu + region, data = rcs21)

tab_model(Scholz_M1a, Scholz_M1b,
          Baerbock_M1a, Baerbock_M1b,
          Laschet_M1a, Laschet_M1b,
          show.intercept = FALSE,
          show.ci = FALSE,
          order.terms = c(1, 2, 3, 10, 11, 4, 5, 6, 7, 8, 9),
          pred.labels = c("Political Ideology",
                          "Political Ideology^2",
                          "Attitude Scholz",
                          "Political Interest",
                          "Gender (1 = female)", 
                          "Age", 
                          "Education (1 = medium)", 
                          "Education (1 = high)", 
                          "Region (1 = Western Germany)",
                          "Attitude Baerbock",
                          "Attitude Laschet"),
          dv.labels = c("Scholz (1)", "Scholz (2)", "Baerbock (1)", "Baerbock (2)", "Laschet (1)", "Laschet (2)"),
          file = "tables/table_A2.doc")

####Two-Lines Tests (affective ambivalence)

###Scholz

##Ideology & Ambivalence

a = twolines(Scholz_aamb ~ symb_id, data = rcs21)

##Ideology & Ambivalence with Controls

a = twolines(Scholz_aamb ~ symb_id + Scholz_att + pol_int + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = rcs21)

###Baerbock

##Ideology & Ambivalence

a = twolines(Baerbock_aamb ~ symb_id, data = rcs21)

##Ideology & Ambivalence with Controls

a = twolines(Baerbock_aamb ~ symb_id + Baerbock_att + pol_int + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = rcs21)

###Laschet

##Ideology & Ambivalence

a = twolines(Laschet_aamb ~ symb_id, data = rcs21)

##Ideology & Ambivalence with Controls

a = twolines(Laschet_aamb ~ symb_id + Laschet_att + pol_int + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = rcs21)


####Two-Lines Tests (cognitive ambivalence)

###Scholz

##Ideology & Ambivalence

a = twolines(Scholz_camb ~ symb_id, data = rcs21)

##Ideology & Ambivalence with Controls

a = twolines(Scholz_camb ~ symb_id + Scholz_att + pol_int + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = rcs21)

###Baerbock

##Ideology & Ambivalence

a = twolines(Baerbock_camb ~ symb_id, data = rcs21)

##Ideology & Ambivalence with Controls

a = twolines(Baerbock_camb ~ symb_id + Baerbock_att + pol_int + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = rcs21)

###Laschet

##Ideology & Ambivalence

a = twolines(Laschet_camb ~ symb_id, data = rcs21)

##Ideology & Ambivalence with Controls

a = twolines(Laschet_camb ~ symb_id + Laschet_att + pol_int + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = rcs21)





















#Table A3

tab_model(Scholz_M1a, Scholz_M1b,
          Scholz_M2a, Scholz_M2b,
          show.intercept = FALSE,
          show.ci = FALSE,
          pred.labels = c("Political Ideology",
                          "Gender (1 = female)", "Age", 
                          "Education (1 = medium)", "Education (1 = high)", 
                          "Region (1 = Western Germany)",
                          "Political Interest"),
          dv.labels = c("Affective Ambivalence (1)", "Affective Ambivalence (2)", "Cognitive Ambivalence (1)", "Cognitive Ambivalence (2)"),
          file = "tables/table_A3.doc")

##Laschet

#Affective Ambivalence

Laschet_M1a <- lm(rescale(Laschet_aamb) ~ rescale(symb_id), data = rcs21)
Laschet_M1b <- lm(rescale(Laschet_aamb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Cognitive Ambivalence

Laschet_M2a <- lm(rescale(Laschet_camb) ~ rescale(symb_id), data = rcs21)
Laschet_M2b <- lm(rescale(Laschet_camb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Table A4

tab_model(Laschet_M1a, Laschet_M1b,
          Laschet_M2a, Laschet_M2b,
          show.intercept = FALSE,
          show.ci = FALSE,
          pred.labels = c("Political Ideology",
                          "Gender (1 = female)", "Age", 
                          "Education (1 = medium)", "Education (1 = high)", 
                          "Region (1 = Western Germany)",
                          "Political Interest"),
          dv.labels = c("Affective Ambivalence (1)", "Affective Ambivalence (2)", "Cognitive Ambivalence (1)", "Cognitive Ambivalence (2)"),
          file = "tables/table_A4.doc")

##Baerbock

#Affective Ambivalence

Baerbock_M1a <- lm(rescale(Baerbock_aamb) ~ rescale(symb_id), data = rcs21)
Baerbock_M1b <- lm(rescale(Baerbock_aamb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Cognitive Ambivalence

Baerbock_M2a <- lm(rescale(Baerbock_camb) ~ rescale(symb_id), data = rcs21)
Baerbock_M2b <- lm(rescale(Baerbock_camb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Table A5

tab_model(Baerbock_M1a, Baerbock_M1b,
          Baerbock_M2a, Baerbock_M2b,
          show.intercept = FALSE,
          show.ci = FALSE,
          pred.labels = c("Political Ideology",
                          "Gender (1 = female)", "Age", 
                          "Education (1 = medium)", "Education (1 = high)", 
                          "Region (1 = Western Germany)",
                          "Political Interest"),
          dv.labels = c("Affective Ambivalence (1)", "Affective Ambivalence (2)", "Cognitive Ambivalence (1)", "Cognitive Ambivalence (2)"),
          file = "tables/table_A5.doc")

####Two-Lines Tests

###Scholz Affective

##Ideology & Ambivalence

a = twolines(Scholz_aamb ~ symb_id, data = rcs21)

Fig_A5a <- ggplot(rcs21, aes(x = symb_id, y = Scholz_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 0.3, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.03*", "b = - 0.03"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Scholz_att ~ symb_id, data = rcs21)

Fig_A5b <- ggplot(rcs21, aes(x = symb_id, y = Scholz_att)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 6.0, yend = 5.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 6.0, yend = 5.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 8.5), y = c(4.8, 4.8), label = c("b = - 0.15***", "b = - 0.1"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Scholz_aamb ~ Scholz_att, data = rcs21)

Fig_A5c <- ggplot(rcs21, aes(x = Scholz_att, y = Scholz_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 8.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 7.5), y = c(-0.3, -0.3), label = c("b = 0.36***", "b = - 0.37***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Scholz_aamb_resid ~ symb_id, data = rcs21)

Fig_A5d <- ggplot(rcs21, aes(x = symb_id, y = Scholz_aamb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = -0.3, yend = -0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = -0.3, yend = -0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 7.5), y = c(-1.0, -1.0), label = c("b = 0.01", "b = - 0.01"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A5a, Fig_A5b, Fig_A5c, Fig_A5d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA5.png")

###Scholz Cognitive

##Ideology & Ambivalence

a = twolines(Scholz_camb ~ symb_id, data = rcs21)

Fig_A6a <- ggplot(rcs21, aes(x = symb_id, y = Scholz_camb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Cognitive Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 0.3, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.02", "b = - 0.08*"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Scholz_att ~ symb_id, data = rcs21)

Fig_A6b <- ggplot(rcs21, aes(x = symb_id, y = Scholz_att)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 6.0, yend = 5.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 6.0, yend = 5.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 8.5), y = c(4.8, 4.8), label = c("b = - 0.15***", "b = - 0.1"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Scholz_camb ~ Scholz_att, data = rcs21)

Fig_A6c <- ggplot(rcs21, aes(x = Scholz_att, y = Scholz_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Cognitive Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 8.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 7.5), y = c(-0.3, -0.3), label = c("b = 0.31***", "b = - 0.34***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Scholz_camb_resid ~ symb_id, data = rcs21)

Fig_A6d <- ggplot(rcs21, aes(x = symb_id, y = Scholz_camb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = -0.3, yend = -0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = -0.3, yend = -0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 7.5), y = c(-1.0, -1.0), label = c("b = 0.04", "b = - 0.04*"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A6a, Fig_A6b, Fig_A6c, Fig_A6d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA6.png")


###Laschet Affective

##Ideology & Ambivalence

a = twolines(Laschet_aamb ~ symb_id, data = rcs21)

Fig_A7a <- ggplot(rcs21, aes(x = symb_id, y = Laschet_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 0.3, yend = 0.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.11***", "b = - 0.07**"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Laschet_att ~ symb_id, data = rcs21)

Fig_A7b <- ggplot(rcs21, aes(x = symb_id, y = Laschet_att)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 3.0, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 4.0, yend = 3.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 8.5), y = c(3.0, 3.0), label = c("b = 0.39***", "b = - 0.14"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Laschet_aamb ~ Laschet_att, data = rcs21)

Fig_A7c <- ggplot(rcs21, aes(x = Laschet_att, y = Laschet_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.5, xend = 9.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.35***", "b = - 0.29***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Laschet_aamb_resid ~ symb_id, data = rcs21)

Fig_A7d <- ggplot(rcs21, aes(x = symb_id, y = Laschet_aamb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 3.0, xend = 4.5, y = -0.4, yend = -0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = -0.3, yend = -0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 7.5), y = c(-1.0, -1.0), label = c("b = 0.03*", "b = - 0.02"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A7a, Fig_A7b, Fig_A7c, Fig_A7d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA7.png")


###Laschet Cognitive

##Ideology & Ambivalence

a = twolines(Laschet_camb ~ symb_id, data = rcs21)

Fig_A8a <- ggplot(rcs21, aes(x = symb_id, y = Laschet_camb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Cognitive Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 0.4, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.12***", "b = - 0.06*"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Laschet_att ~ symb_id, data = rcs21)

Fig_A8b <- ggplot(rcs21, aes(x = symb_id, y = Laschet_att)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.5, xend = 6.0, y = 3.5, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 4.5, yend = 4.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 8.5), y = c(3.0, 3.0), label = c("b = 0.39***", "b = - 0.14"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Laschet_camb ~ Laschet_att, data = rcs21)

Fig_A8c <- ggplot(rcs21, aes(x = Laschet_att, y = Laschet_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Cognitive Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.5, xend = 10.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 9.0), y = c(-0.3, -0.3), label = c("b = 0.30***", "b = - 0.30***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Laschet_camb_resid ~ symb_id, data = rcs21)

Fig_A8d <- ggplot(rcs21, aes(x = symb_id, y = Laschet_aamb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 3.0, xend = 4.5, y = -0.4, yend = -0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = -0.3, yend = -0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 7.5), y = c(-1.0, -1.0), label = c("b = 0.05*", "b = - 0.02"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A8a, Fig_A8b, Fig_A8c, Fig_A8d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA8.png")


###Baerbock Affective

##Ideology & Ambivalence

a = twolines(Baerbock_aamb ~ symb_id, data = rcs21)

Fig_A9a <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.0, xend = 6.5, y = 0.3, yend = 0.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.5), y = c(-0.3, -0.3), label = c("b = 0.02", "b = - 0.09***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Baerbock_att ~ symb_id, data = rcs21)

Fig_A9b <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_att)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 4.5, yend = 3.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 3.5, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 8.5), y = c(2.5, 2.5), label = c("b = - 0.53***", "b = - 0.17*"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Baerbock_aamb ~ Baerbock_att, data = rcs21)

Fig_A9c <- ggplot(rcs21, aes(x = Baerbock_att, y = Baerbock_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.5, xend = 9.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.35***", "b = - 0.46***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Baerbock_aamb_resid ~ symb_id, data = rcs21)

Fig_A9d <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_aamb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 1.0, xend = 2.5, y = -0.4, yend = -0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = -0.4, yend = -0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(1.5, 6.0), y = c(-1.0, -1.0), label = c("b = - 0.03", "b = 0.01"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A9a, Fig_A9b, Fig_A9c, Fig_A9d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA9.png")


###Baerbock Cognitive

##Ideology & Ambivalence

a = twolines(Baerbock_camb ~ symb_id, data = rcs21)

Fig_A10a <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_camb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Cognitive Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.1, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 0.3, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = - 0.13***", "b = - 0.08*"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Baerbock_att ~ symb_id, data = rcs21)

Fig_A10b <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_att)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 4.5, yend = 3.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 3.5, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 8.5), y = c(2.5, 2.5), label = c("b = - 0.53***", "b = - 0.17*"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Baerbock_camb ~ Baerbock_att, data = rcs21)

Fig_A10c <- ggplot(rcs21, aes(x = Baerbock_att, y = Baerbock_camb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Cognitive Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.5, xend = 10.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 9.5), y = c(-0.3, -0.3), label = c("b = 0.35***", "b = - 0.40***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Baerbock_camb_resid ~ symb_id, data = rcs21)

Fig_A10d <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_camb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.5, xend = 6.0, y = -0.3, yend = -0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = -0.4, yend = -0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 9.0), y = c(-1.0, -1.0), label = c("b = - 0.03*", "b = 0.05"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A10a, Fig_A10b, Fig_A10c, Fig_A10d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA10.png")


####Zero-Order Correlations with Residual Ambivalence

##Affective Ambivalence

rcs21 %>% select(symb_id, Scholz_aamb_resid, Laschet_aamb_resid, Baerbock_aamb_resid) %>% 
  correlate()

cor.test(rcs21$symb_id, rcs21$Scholz_aamb_resid, use = "complete.obs")

cor.test(rcs21$symb_id, rcs21$Laschet_aamb_resid, use = "complete.obs")

cor.test(rcs21$symb_id, rcs21$Baerbock_aamb_resid, use = "complete.obs")

##Cognitive Ambivalence

rcs21 %>% select(symb_id, Scholz_camb_resid, Laschet_camb_resid, Baerbock_camb_resid) %>% 
  correlate()

cor.test(rcs21$symb_id, rcs21$Scholz_camb_resid, use = "complete.obs")

cor.test(rcs21$symb_id, rcs21$Laschet_camb_resid, use = "complete.obs")

cor.test(rcs21$symb_id, rcs21$Baerbock_camb_resid, use = "complete.obs")