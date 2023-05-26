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

Scholz_M1 <- lm(rescale(Scholz_aamb) ~ rescale(symb_id), data = rcs21)
Scholz_M2 <- lm(rescale(Scholz_aamb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Cognitive Ambivalence

Scholz_M3 <- lm(rescale(Scholz_camb) ~ rescale(symb_id), data = rcs21)
Scholz_M4 <- lm(rescale(Scholz_camb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Table OA2

tab_model(Scholz_M1, Scholz_M2,
          Scholz_M3, Scholz_M4,
          show.intercept = FALSE,
          show.ci = FALSE,
          pred.labels = c("Political Ideology",
                          "Gender (1 = female)", "Age", 
                          "Education (1 = medium)", "Education (1 = high)", 
                          "Region (1 = Western Germany)",
                          "Political Interest"),
          dv.labels = c("Affective Ambivalence (1)", "Affective Ambivalence (2)", "Cognitive Ambivalence (1)", "Cognitive Ambivalence (2)"),
          file = "tables/table_OA2.doc")

##Laschet

#Affective Ambivalence

Laschet_M1 <- lm(rescale(Laschet_aamb) ~ rescale(symb_id), data = rcs21)
Laschet_M2 <- lm(rescale(Laschet_aamb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Cognitive Ambivalence

Laschet_M3 <- lm(rescale(Laschet_camb) ~ rescale(symb_id), data = rcs21)
Laschet_M4 <- lm(rescale(Laschet_camb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Table OA3

tab_model(Laschet_M1, Laschet_M2,
          Laschet_M3, Laschet_M4,
          show.intercept = FALSE,
          show.ci = FALSE,
          pred.labels = c("Political Ideology",
                          "Gender (1 = female)", "Age", 
                          "Education (1 = medium)", "Education (1 = high)", 
                          "Region (1 = Western Germany)",
                          "Political Interest"),
          dv.labels = c("Affective Ambivalence (1)", "Affective Ambivalence (2)", "Cognitive Ambivalence (1)", "Cognitive Ambivalence (2)"),
          file = "tables/table_OA3.doc")

##Baerbock

#Affective Ambivalence

Baerbock_M1 <- lm(rescale(Baerbock_aamb) ~ rescale(symb_id), data = rcs21)
Baerbock_M2 <- lm(rescale(Baerbock_aamb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Cognitive Ambivalence

Baerbock_M3 <- lm(rescale(Baerbock_camb) ~ rescale(symb_id), data = rcs21)
Baerbock_M4 <- lm(rescale(Baerbock_camb) ~ rescale(symb_id) + gend + age + edu + region + rescale(pol_int), data = rcs21)

#Table OA4

tab_model(Baerbock_M1, Baerbock_M2,
          Baerbock_M3, Baerbock_M4,
          show.intercept = FALSE,
          show.ci = FALSE,
          pred.labels = c("Political Ideology",
                          "Gender (1 = female)", "Age", 
                          "Education (1 = medium)", "Education (1 = high)", 
                          "Region (1 = Western Germany)",
                          "Political Interest"),
          dv.labels = c("Affective Ambivalence (1)", "Affective Ambivalence (2)", "Cognitive Ambivalence (1)", "Cognitive Ambivalence (2)"),
          file = "tables/table_OA4.doc")

####Two-Lines Tests

###Scholz Affective

##Ideology & Ambivalence

a = twolines(Scholz_aamb ~ symb_id, data = rcs21)

Fig_OA1a <- ggplot(rcs21, aes(x = symb_id, y = Scholz_aamb)) + 
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

Fig_OA1b <- ggplot(rcs21, aes(x = symb_id, y = Scholz_att)) + 
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

Fig_OA1c <- ggplot(rcs21, aes(x = Scholz_att, y = Scholz_aamb)) + 
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

Fig_OA1d <- ggplot(rcs21, aes(x = symb_id, y = Scholz_aamb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

plot_grid(Fig_OA1a, Fig_OA1b, Fig_OA1c, Fig_OA1d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figure_OA1.png")

###Scholz Cognitive

##Ideology & Ambivalence

a = twolines(Scholz_camb ~ symb_id, data = rcs21)

Fig_OA2a <- ggplot(rcs21, aes(x = symb_id, y = Scholz_camb)) + 
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
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.02", "b = - 0.08*"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Scholz_att ~ symb_id, data = rcs21)

Fig_OA2b <- ggplot(rcs21, aes(x = symb_id, y = Scholz_att)) + 
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

Fig_OA2c <- ggplot(rcs21, aes(x = Scholz_att, y = Scholz_aamb)) + 
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
  annotate("text", x = c(4.5, 7.5), y = c(-0.3, -0.3), label = c("b = 0.31***", "b = - 0.34***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Scholz_camb_resid ~ symb_id, data = rcs21)

Fig_OA2d <- ggplot(rcs21, aes(x = symb_id, y = Scholz_camb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

plot_grid(Fig_OA1a, Fig_OA1b, Fig_OA1c, Fig_OA1d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figure_OA2.png")


###Laschet Affective

##Ideology & Ambivalence

a = twolines(Laschet_aamb ~ symb_id, data = rcs21)

Fig_OA3a <- ggplot(rcs21, aes(x = symb_id, y = Laschet_aamb)) + 
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
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.11***", "b = - 0.07**"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Laschet_att ~ symb_id, data = rcs21)

Fig_OA3b <- ggplot(rcs21, aes(x = symb_id, y = Laschet_att)) + 
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
  annotate("text", x = c(5.0, 8.5), y = c(4.8, 4.8), label = c("b = - 0.39***", "b = - 0.14"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Laschet_aamb ~ Laschet_att, data = rcs21)

Fig_OA3c <- ggplot(rcs21, aes(x = Scholz_att, y = Scholz_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 8.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 7.5), y = c(-0.3, -0.3), label = c("b = 0.35***", "b = - 0.29***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Laschet_aamb_resid ~ symb_id, data = rcs21)

Fig_OA3d <- ggplot(rcs21, aes(x = symb_id, y = Laschet_aamb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

plot_grid(Fig_OA1a, Fig_OA1b, Fig_OA1c, Fig_OA1d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figure_OA3.png")


###Laschet Cognitive

##Ideology & Ambivalence

a = twolines(Laschet_camb ~ symb_id, data = rcs21)

Fig_OA4a <- ggplot(rcs21, aes(x = symb_id, y = Laschet_camb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 0.3, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.12***", "b = - 0.06*"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Laschet_att ~ symb_id, data = rcs21)

Fig_OA4b <- ggplot(rcs21, aes(x = symb_id, y = Laschet_att)) + 
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
  annotate("text", x = c(5.0, 8.5), y = c(4.8, 4.8), label = c("b = - 0.39***", "b = - 0.14"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Laschet_camb ~ Laschet_att, data = rcs21)

Fig_OA4c <- ggplot(rcs21, aes(x = Scholz_att, y = Scholz_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 8.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 7.5), y = c(-0.3, -0.3), label = c("b = 0.30***", "b = - 0.30***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Laschet_camb_resid ~ symb_id, data = rcs21)

Fig_OA4d <- ggplot(rcs21, aes(x = symb_id, y = Laschet_aamb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

plot_grid(Fig_OA1a, Fig_OA1b, Fig_OA1c, Fig_OA1d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figure_OA4.png")


###Baerbock Affective

##Ideology & Ambivalence

a = twolines(Baerbock_aamb ~ symb_id, data = rcs21)

Fig_OA5a <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.0, xend = 9.5, y = 0.3, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = 0.02", "b = - 0.09***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Baerbock_att ~ symb_id, data = rcs21)

Fig_OA5b <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_att)) + 
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
  annotate("text", x = c(5.0, 8.5), y = c(4.8, 4.8), label = c("b = - 0.53***", "b = - 0.17*"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Baerbock_aamb ~ Baerbock_att, data = rcs21)

Fig_OA5c <- ggplot(rcs21, aes(x = Baerbock_att, y = Baerbock_aamb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 8.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 7.5), y = c(-0.3, -0.3), label = c("b = 0.35***", "b = - 0.46***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Baerbock_aamb_resid ~ symb_id, data = rcs21)

Fig_OA5d <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_aamb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

plot_grid(Fig_OA1a, Fig_OA1b, Fig_OA1c, Fig_OA1d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figure_OA5.png")


###Baerbock Cognitive

##Ideology & Ambivalence

a = twolines(Baerbock_camb ~ symb_id, data = rcs21)

Fig_OA6a <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_camb)) + 
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
  annotate("text", x = c(4.5, 8.5), y = c(-0.3, -0.3), label = c("b = - 0.13***", "b = - 0.08*"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(Baerbock_att ~ symb_id, data = rcs21)

Fig_OA6b <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_att)) + 
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
  annotate("text", x = c(5.0, 8.5), y = c(4.8, 4.8), label = c("b = - 0.53***", "b = - 0.17*"), color = "red", size = 4, fontface = "bold")

##Attitude & Ambivalence

a = twolines(Baerbock_camb ~ Baerbock_att, data = rcs21)

Fig_OA6c <- ggplot(rcs21, aes(x = Baerbock_att, y = Baerbock_camb)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude",
       y = "Affective Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 8.0, y = 1.0, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 7.5), y = c(-0.3, -0.3), label = c("b = 0.35***", "b = - 0.40***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(Baerbock_camb_resid ~ symb_id, data = rcs21)

Fig_OA6d <- ggplot(rcs21, aes(x = symb_id, y = Baerbock_camb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

plot_grid(Fig_OA1a, Fig_OA1b, Fig_OA1c, Fig_OA1d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figure_OA6.png")


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