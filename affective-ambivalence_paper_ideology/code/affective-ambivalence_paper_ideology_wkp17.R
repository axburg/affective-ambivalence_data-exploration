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

wkp17 <- read.spss("../data/ZA6804_en_v7-0-0.sav",
                   use.value.labels = FALSE, to.data.frame = TRUE)

wkp17[wkp17 < 0] <- NA

####Prepare Variables####

####Demographics

###Gender

wkp17$gend <- factor(wkp17$kpx_2280,
                     levels = c(1, 2),
                     labels = c("male", "female"))

wkp17 <- wkp17 %>% mutate(gend_num = case_when(kpx_2280 == 1 ~ 0,
                                               kpx_2280 == 2 ~ 1))

###Age

wkp17$age <- 2017 - wkp17$kpx_2290

###Education

wkp17$edu1 <- coalesce(wkp17$kp1_2320, wkp17$kpa1_2320)

wkp17 <- wkp17 %>% mutate(edu = case_when(edu1 == 1 | edu1 == 2 ~ 1,
                                          edu1 == 3 | edu1 == 9 ~ 2,
                                          edu1 == 4 | edu1 == 5 ~ 3))

wkp17$edu <- factor(wkp17$edu,
                    levels = c(1, 2, 3),
                    labels = c("low", "medium", "high"))

wkp17 <- wkp17 %>% mutate(edu_d1 = case_when(edu1 == 1 | edu1 == 2 ~ 0,
                                             edu1 == 3 | edu1 == 6 | edu1 == 7 ~ 1,
                                             edu1 == 4 | edu1 == 5 ~ 0))

wkp17 <- wkp17 %>% mutate(edu_d2 = case_when(edu1 == 1 | edu1 == 2 ~ 0,
                                             edu1 == 3 | edu1 == 6 | edu1 == 7 ~ 0,
                                             edu1 == 4 | edu1 == 5 ~ 1))

###Income

wkp17$inc <- coalesce(wkp17$kp1_2591, wkp17$kpa1_2591)

###Region of residency

wkp17$region <- factor(wkp17$ostwest,
                       levels = c(0, 1),
                       labels = c("East Germany", "West Germany"))

####Affective Ambivalence

##Feelings

wkp17$Merkel_neg_w4 <- wkp17$kp4_662a
wkp17$Merkel_neg_w6 <- wkp17$kp6_662a
wkp17$Merkel_pos_w4 <- wkp17$kp4_663a
wkp17$Merkel_pos_w6 <- wkp17$kp6_663a

wkp17$Schulz_neg_w4 <- wkp17$kp4_662b
wkp17$Schulz_neg_w6 <- wkp17$kp6_662b
wkp17$Schulz_pos_w4 <- wkp17$kp4_663b
wkp17$Schulz_pos_w6 <- wkp17$kp6_663b

##Ambivalence scores

wkp17$Merkel_amb_w4 <- (wkp17$Merkel_pos_w4 + wkp17$Merkel_neg_w4) / 2 - 
  abs(wkp17$Merkel_pos_w4 - wkp17$Merkel_neg_w4)

wkp17$Merkel_amb_w6 <- (wkp17$Merkel_pos_w6 + wkp17$Merkel_neg_w6) / 2 - 
  abs(wkp17$Merkel_pos_w6 - wkp17$Merkel_neg_w6)

wkp17$Schulz_amb_w4 <- (wkp17$Schulz_pos_w4 + wkp17$Schulz_neg_w4) / 2 - 
  abs(wkp17$Schulz_pos_w4 - wkp17$Schulz_neg_w4)

wkp17$Schulz_amb_w6 <- (wkp17$Schulz_pos_w6 + wkp17$Schulz_neg_w6) / 2 - 
  abs(wkp17$Schulz_pos_w6 - wkp17$Schulz_neg_w6)

cor.test(wkp17$Merkel_amb_w4, wkp17$Merkel_amb_w6, use ="complete.obs") # correlation: r = -.55
cor.test(wkp17$Schulz_amb_w4, wkp17$Schulz_amb_w6, use ="complete.obs") # correlation: r = -.49

wkp17$mean_amb_Merkel <- rowMeans(wkp17[, c("Merkel_amb_w4", "Merkel_amb_w6")], na.rm = TRUE)

wkp17$mean_amb_Schulz <- rowMeans(wkp17[, c("Schulz_amb_w4", "Schulz_amb_w6")], na.rm = TRUE)

####Political Variables

##Political interest

wkp17$pol_int_w4 <- 6 - wkp17$kp4_010
wkp17$pol_int_w6 <- 6 - wkp17$kp6_010

wkp17$mean_pol_int <- rowMeans(wkp17[, c("pol_int_w4", "pol_int_w6")], na.rm = TRUE)

##Symbolic ideology

wkp17$symb_id_w4 <- wkp17$kp4_1500
wkp17$symb_id_w6 <- wkp17$kp6_1500

wkp17$mean_symb_id <- rowMeans(wkp17[, c("symb_id_w4", "symb_id_w6")], na.rm = TRUE)

##Economic Ideology

wkp17$econ_id_w4 <- 8 - wkp17$kp4_1090
wkp17$econ_id_w6 <- 8 - wkp17$kp6_1090

wkp17$mean_econ_id <- rowMeans(wkp17[, c("econ_id_w4", "econ_id_w6")], na.rm = TRUE)

##Socio-cultural Ideology (Immigration)

wkp17$soc_id_w4 <- wkp17$kp4_1130
wkp17$soc_id_w6 <- wkp17$kp6_1130

wkp17$mean_soc_id <- rowMeans(wkp17[, c("soc_id_w4", "soc_id_w6")], na.rm = TRUE)

##Attitudes towards candidates

wkp17$Merkel_att_w4 <- wkp17$kp4_650a
wkp17$Merkel_att_w6 <- wkp17$kp6_650a

wkp17$mean_att_Merkel <- rowMeans(wkp17[, c("Merkel_att_w4", "Merkel_att_w6")], na.rm = TRUE)

wkp17$Schulz_att_w4 <- wkp17$kp4_650z1
wkp17$Schulz_att_w6 <- wkp17$kp6_650z1

wkp17$mean_att_Schulz <- rowMeans(wkp17[, c("Schulz_att_w4", "Schulz_att_w6")], na.rm = TRUE)

####Sample Description####

wkp17 %>% filter(!is.na(mean_symb_id) & !is.na(mean_amb_Merkel)) %>% freq(., gend)

wkp17 %>% filter(!is.na(mean_symb_id) & !is.na(mean_amb_Merkel)) %>% descr(., age)

####Analyses with Mean Scores across Waves 4 and 6####

####Correlations

##Simple correlations

wkp17 %>% select(mean_symb_id, mean_pol_int, mean_amb_Merkel, mean_amb_Schulz) %>% 
  correlate()

cor.test(wkp17$mean_symb_id, wkp17$mean_amb_Merkel, use = "complete.obs")

cor.test(wkp17$mean_symb_id, wkp17$mean_amb_Schulz, use = "complete.obs")

wkp17 %>% select(mean_symb_id, mean_pol_int, mean_amb_Merkel, mean_amb_Schulz) %>% 
  tab_corr(., na.deletion = "pairwise", 
           var.labels = c("Ideology", "Political Interest", "Ambivalence toward Merkel", "Ambivalence toward Schulz"),
           triangle = "upper",
           file = "tables/table1.doc")

##Partial correlations

p_cor1 <- partial.r(wkp17, c("mean_symb_id", "mean_amb_Merkel", "mean_amb_Schulz"), c("age", "gend_num", "edu_d1", "edu_d2", "ostwest"))

corr.p(p_cor1, n = 1000, ci = TRUE)

####Regression Models

##Merkel

Merkel_mean_M1 <- lm(rescale(mean_amb_Merkel) ~ gend + age + edu + rescale(mean_pol_int) + rescale(mean_symb_id), data = wkp17)
Merkel_mean_M2 <- update(Merkel_mean_M1, .~. + I(rescale(mean_symb_id)^2))
Merkel_mean_M3 <- update(Merkel_mean_M2, .~. + rescale(mean_att_Merkel) + I(rescale(mean_att_Merkel)^2))

##Schulz

Schulz_mean_M1 <- lm(rescale(mean_amb_Schulz) ~ gend + age + edu + rescale(mean_pol_int) + rescale(mean_symb_id), data = wkp17)
Schulz_mean_M2 <- update(Schulz_mean_M1, .~. + I(rescale(mean_symb_id)^2))
Schulz_mean_M3 <- update(Schulz_mean_M2, .~. + rescale(mean_att_Schulz) + I(rescale(mean_att_Schulz)^2))

##Table 2

tab_model(Merkel_mean_M1, Merkel_mean_M2, Merkel_mean_M3,
          Schulz_mean_M1, Schulz_mean_M2, Schulz_mean_M3,
          show.intercept = FALSE,
          show.ci = FALSE,
          pred.labels = c("Gender (1 = female)", "Age", 
                          "Education (1 = medium)", "Education (1 = high)",
                          "Political Interest","Ideology", "Ideology squared",
                          "Attitude Merkel", "Attitude Merkel squared",
                          "Attitude Schulz", "Attitude Schulz squared"),
          dv.labels = c("Merkel (1)", "Merkel (2)", "Merkel (3)", "Schulz (1)", "Schulz (2)", "Schulz (3)"),
          file = "tables/table2.doc")

####Two-Lines Tests

###Merkel

##Ideology & Ambivalence

a = twolines(mean_amb_Merkel ~ mean_symb_id, data = wkp17)

Fig1a <- ggplot(wkp17, aes(x = mean_symb_id, y = mean_amb_Merkel)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.23***", "b = - 0.14***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(mean_att_Merkel ~ mean_symb_id, data = wkp17)

Fig1b <- ggplot(wkp17, aes(x = mean_symb_id, y = mean_att_Merkel)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 4.0, yend = 5.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.4, xend = 7.2, y = 5.0, yend = 4.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.8), y = c(3.2, 3.2), label = c("b = 0.62***", "b = - 0.29***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(mean_amb_Merkel ~ mean_att_Merkel, data = wkp17)

Fig1c <- ggplot(wkp17, aes(x = mean_att_Merkel, y = mean_amb_Merkel)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards Merkel",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.53***", "b = -0.38***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

wkp17$mean_amb_Merkel_resid <- resid(lm(mean_amb_Merkel ~ mean_att_Merkel + I(mean_att_Merkel^2), data = wkp17, na.action = na.exclude))

a = twolines(mean_amb_Merkel_resid ~ mean_symb_id, data = wkp17)

Fig1d <- ggplot(wkp17, aes(x = mean_symb_id, y = mean_amb_Merkel_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence     ") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

Fig1_plots <- plot_grid(Fig1a, Fig1b, Fig1c, Fig1d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

Fig1_title <- ggdraw() +
  draw_label("Predictors of Ambivalence about and Attitudes toward Angela Merkel",
             fontface = 'bold',
             x = 0, hjust = 0, size = 14) +
  theme(plot.margin = margin (0,0,0,7))

plot_grid(Fig1_title, Fig1_plots, ncol = 1, rel_heights = c(0.1,1))

ggsave("figures/figure1.png")

###Schulz [Abbildungen stimmen noch nicht]

##Ideology & Ambivalence

a = twolines(mean_amb_Schulz ~ mean_symb_id, data = wkp17)

Fig2a <- ggplot(wkp17, aes(x = mean_symb_id, y = mean_amb_Schulz)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 3.5, y = 0.3, yend = 0.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.0, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.0, 6.0), y = c(-0.3, -0.3), label = c("b = 0.11***", "b = - 0.14***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(mean_att_Schulz ~ mean_symb_id, data = wkp17)

Fig2b <- ggplot(wkp17, aes(x = mean_symb_id, y = mean_att_Schulz)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 1.5, xend = 3.5, y = 4.0, yend = 5.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 5.0, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.5), y = c(3.2, 3.2), label = c("b = 0.37***", "b = - 0.54***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(mean_amb_Schulz ~ mean_att_Schulz, data = wkp17)

Fig2c <- ggplot(wkp17, aes(x = mean_att_Schulz, y = mean_amb_Schulz)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards Schulz",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.8, yend = 1.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.3, yend = 0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.8, 7.5), y = c(0.5, 0.5), label = c("b = 0.44***", "b = - 0.32***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

wkp17$mean_amb_Schulz_resid <- resid(lm(mean_amb_Schulz ~ mean_att_Schulz + I(mean_att_Schulz^2), data = wkp17, na.action = na.exclude))

a = twolines(mean_amb_Schulz_resid ~ mean_symb_id, data = wkp17)

Fig2d <- ggplot(wkp17, aes(x = mean_symb_id, y = mean_amb_Schulz_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence     ") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

Fig2_plots <- plot_grid(Fig2a, Fig2b, Fig2c, Fig2d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

Fig2_title <- ggdraw() +
  draw_label("Predictors of Ambivalence about and Attitudes toward Martin Schulz",
             fontface = 'bold',
             x = 0, hjust = 0, size = 14) +
  theme(plot.margin = margin (0,0,0,7))

plot_grid(Fig2_title, Fig2_plots, ncol = 1, rel_heights = c(0.1,1))

ggsave("figures/figure2.png")

####Zero-Order Correlations with Residual Ambivalence

wkp17 %>% select(mean_symb_id, mean_pol_int, mean_amb_Merkel_resid, mean_amb_Schulz_resid) %>% 
  correlate()