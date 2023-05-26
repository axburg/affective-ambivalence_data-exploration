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
library(cocor)

###Load Functions for Two-Lines Tests

source("http://webstimate.org/twolines/twolines.R")

###Load Data

wkp13 <- read.spss("../data/ZA5704_en_v3-2-0.sav",
                   use.value.labels = FALSE, to.data.frame = TRUE)

wkp13[wkp13 < 0] <- NA

####Prepare Variables####

####Demographics

###Gender

wkp13$gend <- factor(wkp13$kpx_2280,
                     levels = c(1, 2),
                     labels = c("male", "female"))

wkp13 <- wkp13 %>% mutate(gend_num = case_when(kpx_2280 == 1 ~ 0,
                                               kpx_2280 == 2 ~ 1))

###Age

wkp13$age <- 2013 - wkp13$kpx_2290

###Education

wkp13$edu1 <- wkp13$kpx_2320

wkp13 <- wkp13 %>% mutate(edu = case_when(edu1 == 1 | edu1 == 2 ~ 1,
                                          edu1 == 3 | edu1 == 9 ~ 2,
                                          edu1 == 4 | edu1 == 5 ~ 3))

wkp13$edu <- factor(wkp13$edu,
                    levels = c(1, 2, 3),
                    labels = c("low", "medium", "high"))

wkp13 <- wkp13 %>% mutate(edu_d1 = case_when(edu1 == 1 | edu1 == 2 ~ 0,
                                             edu1 == 3 | edu1 == 6 | edu1 == 7 ~ 1,
                                             edu1 == 4 | edu1 == 5 ~ 0))

wkp13 <- wkp13 %>% mutate(edu_d2 = case_when(edu1 == 1 | edu1 == 2 ~ 0,
                                             edu1 == 3 | edu1 == 6 | edu1 == 7 ~ 0,
                                             edu1 == 4 | edu1 == 5 ~ 1))

###Region of residency

wkp13$region <- factor(wkp13$ostwest,
                       levels = c(1, 2),
                       labels = c("West Germany", "East Germany"))

####Political Variables

##Political interest

wkp13$pol_int_w1 <- 6 - wkp13$kp1_010
wkp13$pol_int_w3 <- 6 - wkp13$kp3_010
wkp13$pol_int_w6 <- 6 - wkp13$kp6_010

wkp13$pol_int_mean <- rowMeans(wkp13[, c("pol_int_w1", "pol_int_w3", "pol_int_w6")], na.rm = TRUE)

##Symbolic ideology

wkp13$symb_id_w1 <- wkp13$kp1_1500
wkp13$symb_id_w3 <- wkp13$kp3_1500
wkp13$symb_id_w6 <- wkp13$kp6_1500

wkp13$symb_id_mean <- rowMeans(wkp13[, c("symb_id_w1", "symb_id_w3", "symb_id_w6")], na.rm = TRUE)

##Economic Ideology

wkp13$econ_id_w1 <- 8 - wkp13$kp1_1090
wkp13$econ_id_w3 <- 8 - wkp13$kp3_1090
wkp13$econ_id_w6 <- 8 - wkp13$kp6_1090

wkp13$econ_id_mean <- rowMeans(wkp13[, c("econ_id_w1", "econ_id_w3", "econ_id_w6")], na.rm = TRUE)

##Socio-cultural Ideology (Immigration)

wkp13$soc_id_w1 <- wkp13$kp1_1130
wkp13$soc_id_w3 <- wkp13$kp3_1130
wkp13$soc_id_w6 <- wkp13$kp6_1130

wkp13$soc_id_mean <- rowMeans(wkp13[, c("soc_id_w1", "soc_id_w3", "soc_id_w6")], na.rm = TRUE)

##Attitudes towards parties

wkp13$CDU_att_w1 <- wkp13$kp1_430a
wkp13$CDU_att_w3 <- wkp13$kp3_430a
wkp13$CDU_att_w6 <- wkp13$kp6_430a

wkp13$CDU_att_mean <- rowMeans(wkp13[, c("CDU_att_w1", "CDU_att_w3", "CDU_att_w6")], na.rm = TRUE)

wkp13$CSU_att_w1 <- wkp13$kp1_430b
wkp13$CSU_att_w3 <- wkp13$kp3_430b
wkp13$CSU_att_w6 <- wkp13$kp6_430b

wkp13$Union_att_w1 <- rowMeans(wkp13[, c("CDU_att_w1", "CSU_att_w1")], na.rm = TRUE)
wkp13$Union_att_w3 <- rowMeans(wkp13[, c("CDU_att_w3", "CSU_att_w3")], na.rm = TRUE)
wkp13$Union_att_w6 <- rowMeans(wkp13[, c("CDU_att_w6", "CSU_att_w6")], na.rm = TRUE)

wkp13$Union_att_mean <- rowMeans(wkp13[, c("Union_att_w1", "Union_att_w3", "Union_att_w6")], na.rm = TRUE)

wkp13$SPD_att_w1 <- wkp13$kp1_430c
wkp13$SPD_att_w3 <- wkp13$kp3_430c
wkp13$SPD_att_w6 <- wkp13$kp6_430c

wkp13$SPD_att_mean <- rowMeans(wkp13[, c("SPD_att_w1", "SPD_att_w3", "SPD_att_w6")], na.rm = TRUE)

wkp13$FDP_att_w1 <- wkp13$kp1_430d
wkp13$FDP_att_w3 <- wkp13$kp3_430d
wkp13$FDP_att_w6 <- wkp13$kp6_430d

wkp13$FDP_att_mean <- rowMeans(wkp13[, c("FDP_att_w1", "FDP_att_w3", "FDP_att_w6")], na.rm = TRUE)

wkp13$GRUENE_att_w1 <- wkp13$kp1_430e
wkp13$GRUENE_att_w3 <- wkp13$kp3_430e
wkp13$GRUENE_att_w6 <- wkp13$kp6_430e

wkp13$GRUENE_att_mean <- rowMeans(wkp13[, c("GRUENE_att_w1", "GRUENE_att_w3", "GRUENE_att_w6")], na.rm = TRUE)

wkp13$LINKE_att_w1 <- wkp13$kp1_430f
wkp13$LINKE_att_w3 <- wkp13$kp3_430f
wkp13$LINKE_att_w6 <- wkp13$kp6_430f

wkp13$LINKE_att_mean <- rowMeans(wkp13[, c("LINKE_att_w1", "LINKE_att_w3", "LINKE_att_w6")], na.rm = TRUE)


####Affective Ambivalence

##Feelings

wkp13$CDU_neg_w1 <- wkp13$kp1_2800a
wkp13$CDU_neg_w3 <- wkp13$kp3_2800a
wkp13$CDU_neg_w6 <- wkp13$kp6_2800a

wkp13$SPD_neg_w1 <- wkp13$kp1_2800c
wkp13$SPD_neg_w3 <- wkp13$kp3_2800c
wkp13$SPD_neg_w6 <- wkp13$kp6_2800c

wkp13$FDP_neg_w1 <- wkp13$kp1_2800d
wkp13$FDP_neg_w3 <- wkp13$kp3_2800d
wkp13$FDP_neg_w6 <- wkp13$kp6_2800d

wkp13$GRUENE_neg_w1 <- wkp13$kp1_2800e
wkp13$GRUENE_neg_w3 <- wkp13$kp3_2800e
wkp13$GRUENE_neg_w6 <- wkp13$kp6_2800e

wkp13$LINKE_neg_w1 <- wkp13$kp1_2800f
wkp13$LINKE_neg_w3 <- wkp13$kp3_2800f
wkp13$LINKE_neg_w6 <- wkp13$kp6_2800f

wkp13$CDU_pos_w1 <- wkp13$kp1_2801a
wkp13$CDU_pos_w3 <- wkp13$kp3_2801a
wkp13$CDU_pos_w6 <- wkp13$kp6_2801a

wkp13$SPD_pos_w1 <- wkp13$kp1_2801c
wkp13$SPD_pos_w3 <- wkp13$kp3_2801c
wkp13$SPD_pos_w6 <- wkp13$kp6_2801c

wkp13$FDP_pos_w1 <- wkp13$kp1_2801d
wkp13$FDP_pos_w3 <- wkp13$kp3_2801d
wkp13$FDP_pos_w6 <- wkp13$kp6_2801d

wkp13$GRUENE_pos_w1 <- wkp13$kp1_2801e
wkp13$GRUENE_pos_w3 <- wkp13$kp3_2801e
wkp13$GRUENE_pos_w6 <- wkp13$kp6_2801e

wkp13$LINKE_pos_w1 <- wkp13$kp1_2801f
wkp13$LINKE_pos_w3 <- wkp13$kp3_2801f
wkp13$LINKE_pos_w6 <- wkp13$kp6_2801f

##Ambivalence scores

wkp13$CDU_amb_w1 <- (wkp13$CDU_pos_w1 + wkp13$CDU_neg_w1) / 2 - 
  abs(wkp13$CDU_pos_w1 - wkp13$CDU_neg_w1)

wkp13$CDU_amb_w3 <- (wkp13$CDU_pos_w3 + wkp13$CDU_neg_w3) / 2 - 
  abs(wkp13$CDU_pos_w3 - wkp13$CDU_neg_w3)

wkp13$CDU_amb_w6 <- (wkp13$CDU_pos_w6 + wkp13$CDU_neg_w6) / 2 - 
  abs(wkp13$CDU_pos_w6 - wkp13$CDU_neg_w6)

wkp13$SPD_amb_w1 <- (wkp13$SPD_pos_w1 + wkp13$SPD_neg_w1) / 2 - 
  abs(wkp13$SPD_pos_w1 - wkp13$SPD_neg_w1)

wkp13$SPD_amb_w3 <- (wkp13$SPD_pos_w3 + wkp13$SPD_neg_w3) / 2 - 
  abs(wkp13$SPD_pos_w3 - wkp13$SPD_neg_w3)

wkp13$SPD_amb_w6 <- (wkp13$SPD_pos_w6 + wkp13$SPD_neg_w6) / 2 - 
  abs(wkp13$SPD_pos_w6 - wkp13$SPD_neg_w6)

wkp13$FDP_amb_w1 <- (wkp13$FDP_pos_w1 + wkp13$FDP_neg_w1) / 2 - 
  abs(wkp13$FDP_pos_w1 - wkp13$FDP_neg_w1)

wkp13$FDP_amb_w3 <- (wkp13$FDP_pos_w3 + wkp13$FDP_neg_w3) / 2 - 
  abs(wkp13$FDP_pos_w3 - wkp13$FDP_neg_w3)

wkp13$FDP_amb_w6 <- (wkp13$FDP_pos_w6 + wkp13$FDP_neg_w6) / 2 - 
  abs(wkp13$FDP_pos_w6 - wkp13$FDP_neg_w6)

wkp13$GRUENE_amb_w1 <- (wkp13$GRUENE_pos_w1 + wkp13$GRUENE_neg_w1) / 2 - 
  abs(wkp13$GRUENE_pos_w1 - wkp13$GRUENE_neg_w1)

wkp13$GRUENE_amb_w3 <- (wkp13$GRUENE_pos_w3 + wkp13$GRUENE_neg_w3) / 2 - 
  abs(wkp13$GRUENE_pos_w3 - wkp13$GRUENE_neg_w3)

wkp13$GRUENE_amb_w6 <- (wkp13$GRUENE_pos_w6 + wkp13$GRUENE_neg_w6) / 2 - 
  abs(wkp13$GRUENE_pos_w6 - wkp13$GRUENE_neg_w6)

wkp13$LINKE_amb_w1 <- (wkp13$LINKE_pos_w1 + wkp13$LINKE_neg_w1) / 2 - 
  abs(wkp13$LINKE_pos_w1 - wkp13$LINKE_neg_w1)

wkp13$LINKE_amb_w3 <- (wkp13$LINKE_pos_w3 + wkp13$LINKE_neg_w3) / 2 - 
  abs(wkp13$LINKE_pos_w3 - wkp13$LINKE_neg_w3)

wkp13$LINKE_amb_w6 <- (wkp13$LINKE_pos_w6 + wkp13$LINKE_neg_w6) / 2 - 
  abs(wkp13$LINKE_pos_w6 - wkp13$LINKE_neg_w6)

psych::alpha(wkp13[, c("CDU_amb_w1", "CDU_amb_w3", "CDU_amb_w3")]) #alpha = .88
psych::alpha(wkp13[, c("SPD_amb_w1", "SPD_amb_w3", "SPD_amb_w3")]) #alpha = .86
psych::alpha(wkp13[, c("FDP_amb_w1", "FDP_amb_w3", "FDP_amb_w3")]) #alpha = .91
psych::alpha(wkp13[, c("GRUENE_amb_w1", "GRUENE_amb_w3", "GRUENE_amb_w3")]) #alpha = .87
psych::alpha(wkp13[, c("LINKE_amb_w1", "LINKE_amb_w3", "LINKE_amb_w3")]) #alpha = .89

wkp13$CDU_amb_mean <- rowMeans(wkp13[, c("CDU_amb_w1", "CDU_amb_w3", "CDU_amb_w6")], na.rm = TRUE)
wkp13$SPD_amb_mean <- rowMeans(wkp13[, c("SPD_amb_w1", "SPD_amb_w3", "SPD_amb_w6")], na.rm = TRUE)
wkp13$FDP_amb_mean <- rowMeans(wkp13[, c("FDP_amb_w1", "FDP_amb_w3", "FDP_amb_w6")], na.rm = TRUE)
wkp13$GRUENE_amb_mean <- rowMeans(wkp13[, c("GRUENE_amb_w1", "GRUENE_amb_w3", "GRUENE_amb_w6")], na.rm = TRUE)
wkp13$LINKE_amb_mean <- rowMeans(wkp13[, c("LINKE_amb_w1", "LINKE_amb_w3", "LINKE_amb_w6")], na.rm = TRUE)

wkp13$CDU_amb_resid <- resid(lm(CDU_amb_mean ~ Union_att_mean + I(Union_att_mean^2), data = wkp13, na.action = na.exclude))
wkp13$SPD_amb_resid <- resid(lm(SPD_amb_mean ~ SPD_att_mean + I(SPD_att_mean^2), data = wkp13, na.action = na.exclude))
wkp13$FDP_amb_resid <- resid(lm(FDP_amb_mean ~ FDP_att_mean + I(FDP_att_mean^2), data = wkp13, na.action = na.exclude))
wkp13$GRUENE_amb_resid <- resid(lm(GRUENE_amb_mean ~ GRUENE_att_mean + I(GRUENE_att_mean^2), data = wkp13, na.action = na.exclude))
wkp13$LINKE_amb_resid <- resid(lm(LINKE_amb_mean ~ LINKE_att_mean + I(LINKE_att_mean^2), data = wkp13, na.action = na.exclude))

####Sample Description####

wkp13 %>% filter(!is.na(symb_id_mean)) %>% descr(.,CDU_amb_mean)
wkp13 %>% filter(!is.na(symb_id_mean)) %>% descr(.,SPD_amb_mean)
wkp13 %>% filter(!is.na(symb_id_mean)) %>% descr(.,FDP_amb_mean)
wkp13 %>% filter(!is.na(symb_id_mean)) %>% descr(.,GRUENE_amb_mean)
wkp13 %>% filter(!is.na(symb_id_mean)) %>% descr(.,LINKE_amb_mean)

wkp13 %>% filter(!is.na(symb_id_mean) & !is.na(CDU_amb_mean)) %>% freq(., gend)
wkp13 %>% filter(!is.na(symb_id_mean) & !is.na(CDU_amb_mean)) %>% descr(., age)

####Correlations

##Simple correlations

cor.test(wkp13$pol_int_mean, wkp13$symb_id_mean, use = "complete.obs")

cor.test(wkp13$pol_int_mean, wkp13$CDU_amb_mean, use = "complete.obs")
cor.test(wkp13$pol_int_mean, wkp13$SPD_amb_mean, use = "complete.obs")
cor.test(wkp13$pol_int_mean, wkp13$FDP_amb_mean, use = "complete.obs")
cor.test(wkp13$pol_int_mean, wkp13$GRUENE_amb_mean, use = "complete.obs")
cor.test(wkp13$pol_int_mean, wkp13$LINKE_amb_mean, use = "complete.obs")

wkp13 %>% select(pol_int_mean, symb_id_mean, CDU_amb_mean, SPD_amb_mean, FDP_amb_mean, GRUENE_amb_mean, LINKE_amb_mean) %>% 
  apa.cor.table(., filename = "tables/table_2.doc")

####Regression Models

##CDU

CDU_M1a <- lm(rescale(CDU_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2), data = wkp13)
CDU_M1b <- lm(rescale(CDU_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2) + rescale(CDU_att_mean) + rescale(pol_int_mean) + gend + age + edu + region, data = wkp13)

##SPD

SPD_M1a <- lm(rescale(SPD_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2), data = wkp13)
SPD_M1b <- lm(rescale(SPD_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2) + rescale(SPD_att_mean) + rescale(pol_int_mean) + gend + age + edu + region, data = wkp13)

##FDP

FDP_M1a <- lm(rescale(FDP_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2), data = wkp13)
FDP_M1b <- lm(rescale(FDP_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2) + rescale(FDP_att_mean) + rescale(pol_int_mean) + gend + age + edu + region, data = wkp13)

##GRUENE

GRUENE_M1a <- lm(rescale(GRUENE_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2), data = wkp13)
GRUENE_M1b <- lm(rescale(GRUENE_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2) + rescale(GRUENE_att_mean) + rescale(pol_int_mean) + gend + age + edu + region, data = wkp13)

##LINKE

LINKE_M1a <- lm(rescale(LINKE_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2), data = wkp13)
LINKE_M1b <- lm(rescale(LINKE_amb_mean) ~ rescale(symb_id_mean) + I(rescale(symb_id_mean)^2) + rescale(LINKE_att_mean) + rescale(pol_int_mean) + gend + age + edu + region, data = wkp13)


####Two-Lines Tests

###CDU

##Ideology & Ambivalence

a = twolines(CDU_amb_mean ~ symb_id_mean, data = wkp13)

##Ideology & Ambivalence with Controls

a = twolines(CDU_amb_mean ~ symb_id_mean + CDU_att_mean + pol_int_mean + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = wkp13)

###SPD

##Ideology & Ambivalence

a = twolines(SPD_amb_mean ~ symb_id_mean, data = wkp13)

##Ideology & Ambivalence with Controls

a = twolines(SPD_amb_mean ~ symb_id_mean + SPD_att_mean + pol_int_mean + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = wkp13)

###FDP

##Ideology & Ambivalence

a = twolines(FDP_amb_mean ~ symb_id_mean, data = wkp13)

##Ideology & Ambivalence with Controls

a = twolines(FDP_amb_mean ~ symb_id_mean + FDP_att_mean + pol_int_mean + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = wkp13)

###GRUENE

##Ideology & Ambivalence

a = twolines(GRUENE_amb_mean ~ symb_id_mean, data = wkp13)

##Ideology & Ambivalence with Controls

a = twolines(GRUENE_amb_mean ~ symb_id_mean + GRUENE_att_mean + pol_int_mean + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = wkp13)

###LINKE

##Ideology & Ambivalence

a = twolines(LINKE_amb_mean ~ symb_id_mean, data = wkp13)

##Ideology & Ambivalence with Controls

a = twolines(LINKE_amb_mean ~ symb_id_mean + LINKE_att_mean + pol_int_mean + gend_num + age + edu_d1 + edu_d2 + as.numeric(region), data = wkp13)


























##CDU

CDU_M1a <- lm(rescale(CDU_amb_mean) ~ rescale(symb_id_mean), data = wkp13)
CDU_M1b <- lm(rescale(CDU_amb_mean) ~ rescale(symb_id_mean) + gend + age + edu + region + rescale(pol_int_mean), data = wkp13)

##SPD

SPD_M1a <- lm(rescale(SPD_amb_mean) ~ rescale(symb_id_mean), data = wkp13)
SPD_M1b <- lm(rescale(SPD_amb_mean) ~ rescale(symb_id_mean) + gend + age + edu + region + rescale(pol_int_mean), data = wkp13)

##FDP

FDP_M1a <- lm(rescale(FDP_amb_mean) ~ rescale(symb_id_mean), data = wkp13)
FDP_M1b <- lm(rescale(FDP_amb_mean) ~ rescale(symb_id_mean) + gend + age + edu + region + rescale(pol_int_mean), data = wkp13)

##GRUENE

GRUENE_M1a <- lm(rescale(GRUENE_amb_mean) ~ rescale(symb_id_mean), data = wkp13)
GRUENE_M1b <- lm(rescale(GRUENE_amb_mean) ~ rescale(symb_id_mean) + gend + age + edu + region + rescale(pol_int_mean), data = wkp13)

##LINKE

LINKE_M1a <- lm(rescale(LINKE_amb_mean) ~ rescale(symb_id_mean), data = wkp13)
LINKE_M1b <- lm(rescale(LINKE_amb_mean) ~ rescale(symb_id_mean) + gend + age + edu + region + rescale(pol_int_mean), data = wkp13)

##Table A6

tab_model(CDU_M1a, CDU_M1b,
          SPD_M1a, SPD_M1b,
          FDP_M1a, FDP_M1b,
          GRUENE_M1a, GRUENE_M1b,
          LINKE_M1a, LINKE_M1b,
          show.intercept = FALSE,
          show.ci = FALSE,
          pred.labels = c("Political Ideology",
                          "Gender (1 = female)", "Age", 
                          "Education (1 = medium)", "Education (1 = high)", 
                          "Region (1 = East Germany)",
                          "Political Interest"),
          dv.labels = c("CDU (1)", "CDU (2)", "SPD(1)", "SPD (2)", "FDP (1)", "FDP (2)", 
                        "Green Party (1)", "Green Party (2)", "The LEFT(1)", "The LEFT (2)"),
          file = "tables/table_A6.doc")


####Two-Lines Tests

###CDU

##Ideology & Ambivalence

a = twolines(CDU_amb_mean ~ symb_id_mean, data = wkp13)

Fig_A11a <- ggplot(wkp13, aes(x = symb_id_mean, y = CDU_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Ambivalence toward the CDU") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(-0.3, -0.3), label = c("b = 0.25***", "b = - 0.21***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(CDU_att_mean ~ symb_id_mean, data = wkp13)

Fig_A11b <- ggplot(wkp13, aes(x = symb_id_mean, y = CDU_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude toward the CDU") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 5.0, xend = 6.5, y = 4.0, yend = 5.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.5, xend = 10.0, y = 5.0, yend = 5.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(6.0, 9.5), y = c(3.2, 3.2), label = c("b = 0.72***", "b = - 0.01"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(CDU_amb_mean ~ CDU_att_mean, data = wkp13)

Fig_A11c <- ggplot(wkp13, aes(x = CDU_att_mean, y = CDU_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the CDU/CSU",
       y = "Ambivalence toward the CDU") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.5, xend = 6.0, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = 1.5, yend = 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 7.5), y = c(0.5, 0.5), label = c("b = 0.52***", "b = - 0.47***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(CDU_amb_resid ~ symb_id_mean, data = wkp13)

Fig_A11d <- ggplot(wkp13, aes(x = symb_id_mean, y = CDU_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence     ") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 5.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 3.5, xend = 5.0, y = - 1.0, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = - 1.0, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.0, 7.5), y = c(- 1.5, - 1.5), label = c("b = 0.05*", "b = 0"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A11a, Fig_A11b, Fig_A11c, Fig_A11d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA11.png")

###SPD

##Ideology & Ambivalence

a = twolines(SPD_amb_mean ~ symb_id_mean, data = wkp13)

Fig_A12a <- ggplot(wkp13, aes(x = symb_id_mean, y = SPD_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Ambivalence toward the SPD") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.3, yend = 0.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.5, xend = 9.0, y = 0.5, yend = 0.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(5.0, 8.0), y = c(-0.3, -0.3), label = c("b = 0.08***", "b = - 0.15***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(SPD_att_mean ~ symb_id_mean, data = wkp13)

Fig_A12b <- ggplot(wkp13, aes(x = symb_id_mean, y = SPD_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude toward the SPD") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 5.0, yend = 4.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.5, xend = 10.0, y = 4.3, yend = 4.1, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 9.0), y = c(3.2, 3.2), label = c("b = - 0.29***", "b = - 0.16"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(SPD_amb_mean ~ SPD_att_mean, data = wkp13)

Fig_A12c <- ggplot(wkp13, aes(x = SPD_att_mean, y = SPD_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the SPD",
       y = "Ambivalence towar the SPD") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.47***", "b = - 0.43***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(SPD_amb_resid ~ symb_id_mean, data = wkp13)

Fig_A12d <- ggplot(wkp13, aes(x = symb_id_mean, y = SPD_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence     ") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 3.5, xend = 5.0, y = - 1.0, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = - 1.0, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.0, 7.5), y = c(- 1.5, - 1.5), label = c("b = 0.02*", "b = - 0.1"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A12a, Fig_A12b, Fig_A12c, Fig_A12d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA12.png")


###FDP

##Ideology & Ambivalence

a = twolines(FDP_amb_mean ~ symb_id_mean, data = wkp13)

Fig_A13a <- ggplot(wkp13, aes(x = symb_id_mean, y = FDP_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Ambivalence toward the FDP") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 3.5, xend = 5.0, y = - 0.5, yend = 0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.5, xend = 9.0, y = 0.5, yend = 0.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.5), y = c(-0.7, -0.7), label = c("b = 0.29***", "b = - 0.04"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(FDP_att_mean ~ symb_id_mean, data = wkp13)

Fig_A13b <- ggplot(wkp13, aes(x = symb_id_mean, y = FDP_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude toward the FDP") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 7.5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 5.0, xend = 6.5, y = 2.5, yend = 3.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.5, xend = 10.0, y = 3.5, yend = 3.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(6.0, 9.0), y = c(2.0, 2.0), label = c("b = 0.57***", "b = - 0.04"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(FDP_amb_mean ~ FDP_att_mean, data = wkp13)

Fig_A13c <- ggplot(wkp13, aes(x = FDP_att_mean, y = FDP_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the FDP",
       y = "Ambivalence toward the FDP") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.55***", "b = - 0.30***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(FDP_amb_resid ~ symb_id_mean, data = wkp13)

Fig_A13d <- ggplot(wkp13, aes(x = symb_id_mean, y = FDP_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence     ") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 3.5, xend = 5.0, y = - 1.0, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = - 1.0, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.0, 7.5), y = c(- 1.7, - 1.7), label = c("b = 0.03*", "b = - 0.1"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A13a, Fig_A13b, Fig_A13c, Fig_A13d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA13.png")


###GRUENE

##Ideology & Ambivalence

a = twolines(GRUENE_amb_mean ~ symb_id_mean, data = wkp13)

Fig_A14a <- ggplot(wkp13, aes(x = symb_id_mean, y = GRUENE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Ambivalence toward the GREENS") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 0.3, yend = 0.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 8.0, y = 0.5, yend = 0.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 7.5), y = c(-0.3, -0.3), label = c("b = 0.08***", "b = - 0.15***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(GRUENE_att_mean ~ symb_id_mean, data = wkp13)

Fig_A14b <- ggplot(wkp13, aes(x = symb_id_mean, y = GRUENE_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude toward the GREENS") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 5.0, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.5, xend = 10.0, y = 3.5, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 9.5), y = c(2.5, 2.5), label = c("b = - 0.47***", "b = - 0.32**"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(GRUENE_amb_mean ~ GRUENE_att_mean, data = wkp13)

Fig_A14c <- ggplot(wkp13, aes(x = GRUENE_att_mean, y = GRUENE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the GREENS",
       y = "Ambivalence toward the GREENS") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.51***", "b = -0.50***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(GRUENE_amb_resid ~ symb_id_mean, data = wkp13)

Fig_A14d <- ggplot(wkp13, aes(x = symb_id_mean, y = GRUENE_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 3.5, xend = 5.0, y = - 1.0, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = - 1.0, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.0, 7.5), y = c(- 1.7, - 1.7), label = c("b = 0.03*", "b = - 0.2"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A14a, Fig_A14b, Fig_A14c, Fig_A14d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA14.png")


###LINKE

##Ideology & Ambivalence

a = twolines(LINKE_amb_mean ~ symb_id_mean, data = wkp13)

Fig_A15a <- ggplot(wkp13, aes(x = symb_id_mean, y = LINKE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Ambivalence toward Die LINKE") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.30***", "b = - 0.18***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(LINKE_att_mean ~ symb_id_mean, data = wkp13)

Fig_A15b <- ggplot(wkp13, aes(x = symb_id_mean, y = LINKE_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Attitude toward Die LINKE") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 3.0, xend = 4.5, y = 4.0, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 8.5, xend = 10.0, y = 2.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 9.5), y = c(2.0, 1.0), label = c("b = - 0.80***", "b = - 0.14"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(LINKE_amb_mean ~ LINKE_att_mean, data = wkp13)

Fig_A15c <- ggplot(wkp13, aes(x = LINKE_att_mean, y = LINKE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude toward Die LINKE",
       y = "Ambivalence doward Die LINKE") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.52***", "b = - 0.48***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(LINKE_amb_resid ~ symb_id_mean, data = wkp13)

Fig_A15d <- ggplot(wkp13, aes(x = symb_id_mean, y = LINKE_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Left-Right Self-placement",
       y = "Residual Ambivalence     ") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 4.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.0, y = - 1.0, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 7.0, xend = 8.5, y = - 1.0, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.0, 7.5), y = c(- 1.7, - 1.7), label = c("b = 0.04*", "b = 0"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A15a, Fig_A15b, Fig_A15c, Fig_A15d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA15.png")

####Zero-Order Correlations with Residual Ambivalence

wkp13 %>% select(symb_id_mean, CDU_amb_resid, SPD_amb_resid, FDP_amb_resid, GRUENE_amb_resid, LINKE_amb_resid) %>% 
  apa.cor.table(., filename = "tables/table_A7.doc")

cor.test(wkp13$symb_id_mean, wkp13$CDU_amb_resid, use = "complete.obs")
cor.test(wkp13$symb_id_mean, wkp13$SPD_amb_resid, use = "complete.obs")
cor.test(wkp13$symb_id_mean, wkp13$FDP_amb_resid, use = "complete.obs")
cor.test(wkp13$symb_id_mean, wkp13$GRUENE_amb_resid, use = "complete.obs")
cor.test(wkp13$symb_id_mean, wkp13$LINKE_amb_resid, use = "complete.obs")



####Testing the economic and the socio-cultural attitude dimension

##Simple correlations

wkp13 %>% select(symb_id_mean, soc_id_mean, econ_id_mean, 
                 CDU_amb_mean, SPD_amb_mean, FDP_amb_mean, GRUENE_amb_mean, LINKE_amb_mean) %>% 
  correlate()

wkp13 %>% select(symb_id_mean, soc_id_mean, econ_id_mean,CDU_amb_mean, SPD_amb_mean, FDP_amb_mean, GRUENE_amb_mean, LINKE_amb_mean) %>% 
  apa.cor.table(., filename = "tables/table_A8.doc")
  
  
  tab_corr(., na.deletion = "pairwise", 
           var.labels = c("Symbolic Ideology", "Social Ideology", "Economic Ideology", 
                          "Ambivalence toward CDU", "Ambivalence toward SPD", "Ambivalence toward FDP", 
                          "Ambivalence toward GRUENE", "Ambivalence toward LINKE"),
           triangle = "upper",
           file = "tables/table1.doc")

cocor(formula = ~CDU_amb_mean + soc_id_mean | CDU_amb_mean + econ_id_mean, data = wkp13)
cocor(formula = ~CDU_amb_mean + soc_id_mean | CDU_amb_mean + symb_id_mean, data = wkp13)
cocor(formula = ~CDU_amb_mean + econ_id_mean | CDU_amb_mean + symb_id_mean, data = wkp13)

cocor(formula = ~SPD_amb_mean + soc_id_mean | SPD_amb_mean + econ_id_mean, data = wkp13)
cocor(formula = ~SPD_amb_mean + soc_id_mean | SPD_amb_mean + symb_id_mean, data = wkp13)
cocor(formula = ~SPD_amb_mean + econ_id_mean | SPD_amb_mean + symb_id_mean, data = wkp13)

cocor(formula = ~FDP_amb_mean + soc_id_mean | FDP_amb_mean + econ_id_mean, data = wkp13)
cocor(formula = ~FDP_amb_mean + soc_id_mean | FDP_amb_mean + symb_id_mean, data = wkp13)
cocor(formula = ~FDP_amb_mean + econ_id_mean | FDP_amb_mean + symb_id_mean, data = wkp13)

cocor(formula = ~GRUENE_amb_mean + soc_id_mean | GRUENE_amb_mean + econ_id_mean, data = wkp13)
cocor(formula = ~GRUENE_amb_mean + soc_id_mean | GRUENE_amb_mean + symb_id_mean, data = wkp13)
cocor(formula = ~GRUENE_amb_mean + econ_id_mean | GRUENE_amb_mean + symb_id_mean, data = wkp13)

cocor(formula = ~LINKE_amb_mean + soc_id_mean | LINKE_amb_mean + econ_id_mean, data = wkp13)
cocor(formula = ~LINKE_amb_mean + soc_id_mean | LINKE_amb_mean + symb_id_mean, data = wkp13)
cocor(formula = ~LINKE_amb_mean + econ_id_mean | LINKE_amb_mean + symb_id_mean, data = wkp13)

####Two-Lines-Tests

###CDU (socio-cultural)

##Ideology & Ambivalence

a = twolines(CDU_amb_mean ~ soc_id_mean, data = wkp13)

Fig_A16a <- ggplot(wkp13, aes(x = soc_id_mean, y = CDU_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.30***", "b = - 0.13***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(CDU_att_mean ~ soc_id_mean, data = wkp13)

Fig_A16b <- ggplot(wkp13, aes(x = soc_id_mean, y = CDU_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 4.0, yend = 5.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.4, xend = 7.0, y = 5.0, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.8), y = c(3.2, 3.2), label = c("b = 0.54***", "b = - 0.42***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(CDU_amb_mean ~ CDU_att_mean, data = wkp13)

Fig_A16c <- ggplot(wkp13, aes(x = CDU_att_mean, y = CDU_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the CDU/CSU",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.52***", "b = -0.47***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(CDU_amb_resid ~ soc_id_mean, data = wkp13)

Fig_A16d <- ggplot(wkp13, aes(x = soc_id_mean, y = CDU_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 1.0, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.10***", "b = - 0.02"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A16a, Fig_A16b, Fig_A16c, Fig_A16d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA16.png")

###CDU (economic)

##Ideology & Ambivalence

a = twolines(CDU_amb_mean ~ econ_id_mean, data = wkp13)

Fig_A17a <- ggplot(wkp13, aes(x = econ_id_mean, y = CDU_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.32***", "b = - 0.19***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(CDU_att_mean ~ econ_id_mean, data = wkp13)

Fig_A17b <- ggplot(wkp13, aes(x = econ_id_mean, y = CDU_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 4.0, yend = 5.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.4, xend = 7.0, y = 5.0, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.8), y = c(3.2, 3.2), label = c("b = 0.65***", "b = - 0.39***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(CDU_amb_mean ~ CDU_att_mean, data = wkp13)

Fig_A17c <- ggplot(wkp13, aes(x = CDU_att_mean, y = CDU_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the CDU/CSU",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.52***", "b = - 0.47***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(CDU_amb_resid ~ econ_id_mean, data = wkp13)

Fig_A17d <- ggplot(wkp13, aes(x = soc_id_mean, y = CDU_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.10***", "b = - 0.06*"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A17a, Fig_A17b, Fig_A17c, Fig_A17d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA17.png")


###SPD (socio-cultural)

##Ideology & Ambivalence

a = twolines(SPD_amb_mean ~ soc_id_mean, data = wkp13)

Fig_A18a <- ggplot(wkp13, aes(x = soc_id_mean, y = SPD_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.18***", "b = - 0.18***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(SPD_att_mean ~ soc_id_mean, data = wkp13)

Fig_A18b <- ggplot(wkp13, aes(x = soc_id_mean, y = SPD_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 1.0, xend = 2.5, y = 4.0, yend = 5.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 3.5, xend = 5.0, y = 5.0, yend = 4.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(1.5, 4.0), y = c(3.2, 3.2), label = c("b = 0.37***", "b = - 0.41***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(SPD_amb_mean ~ SPD_att_mean, data = wkp13)

Fig_A18c <- ggplot(wkp13, aes(x = SPD_att_mean, y = SPD_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the SPD",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.47***", "b = - 0.43***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(SPD_amb_resid ~ soc_id_mean, data = wkp13)

Fig_A18d <- ggplot(wkp13, aes(x = soc_id_mean, y = SPD_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.10***", "b = - 0.04*"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A18a, Fig_A18b, Fig_A18c, Fig_A18d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA18.png")

###SPD (economic)

##Ideology & Ambivalence

a = twolines(SPD_amb_mean ~ econ_id_mean, data = wkp13)

Fig_A19a <- ggplot(wkp13, aes(x = econ_id_mean, y = SPD_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.20***", "b = - 0.17***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(SPD_att_mean ~ econ_id_mean, data = wkp13)

Fig_A19b <- ggplot(wkp13, aes(x = econ_id_mean, y = SPD_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 5.0, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.8, xend = 7.0, y = 4.0, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.5), y = c(3.2, 2.5), label = c("b = - 0.36***", "b = - 0.54***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(SPD_amb_mean ~ SPD_att_mean, data = wkp13)

Fig_A19c <- ggplot(wkp13, aes(x = SPD_att_mean, y = SPD_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the SPD",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.47***", "b = - 0.43***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(SPD_amb_resid ~ econ_id_mean, data = wkp13)

Fig_A19d <- ggplot(wkp13, aes(x = soc_id_mean, y = SPD_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 1.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.08***", "b = - 0.05*"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A19a, Fig_A19b, Fig_A19c, Fig_A19d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA19.png")


###FDP (socio-cultural)

##Ideology & Ambivalence

a = twolines(FDP_amb_mean ~ soc_id_mean, data = wkp13)

Fig_A20a <- ggplot(wkp13, aes(x = soc_id_mean, y = FDP_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.0, y = 0.0, yend = 0.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.5, xend = 7.0, y = 0.5, yend = 0.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.28***", "b = - 0.15***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(FDP_att_mean ~ soc_id_mean, data = wkp13)

Fig_A20b <- ggplot(wkp13, aes(x = soc_id_mean, y = FDP_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 2.5, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.4, xend = 7.0, y = 3.0, yend = 2.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.0), y = c(2.0, 2.0), label = c("b = 0.43***", "b = - 0.36***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(FDP_amb_mean ~ FDP_att_mean, data = wkp13)

Fig_A20c <- ggplot(wkp13, aes(x = FDP_att_mean, y = FDP_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the FDP",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.55***", "b = -0.30***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(FDP_amb_resid ~ soc_id_mean, data = wkp13)

Fig_A20d <- ggplot(wkp13, aes(x = soc_id_mean, y = FDP_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.10***", "b = 0"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A20a, Fig_A20b, Fig_A20c, Fig_A20d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA20.png")

###FDP (economic)

##Ideology & Ambivalence

a = twolines(FDP_amb_mean ~ econ_id_mean, data = wkp13)

Fig_A21a <- ggplot(wkp13, aes(x = econ_id_mean, y = FDP_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.0, y = 0.0, yend = 0.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 5.5), y = c(-0.3, -0.3), label = c("b = 0.47***", "b = - 0.10*"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(FDP_att_mean ~ econ_id_mean, data = wkp13)

Fig_A21b <- ggplot(wkp13, aes(x = econ_id_mean, y = FDP_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 2.5, yend = 3.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 5.4, xend = 7.0, y = 3.5, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.0), y = c(2.0, 2.0), label = c("b = - 0.76***", "b = - 0.57***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(FDP_amb_mean ~ FDP_att_mean, data = wkp13)

Fig_A21c <- ggplot(wkp13, aes(x = FDP_att_mean, y = FDP_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the FDP",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.55***", "b = -0.30***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(FDP_amb_resid ~ econ_id_mean, data = wkp13)

Fig_A21d <- ggplot(wkp13, aes(x = soc_id_mean, y = FDP_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.14***", "b = - 0.07**"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A21a, Fig_A21b, Fig_A21c, Fig_A21d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA21.png")


###GRUENE (socio-cultural)

##Ideology & Ambivalence

a = twolines(GRUENE_amb_mean ~ soc_id_mean, data = wkp13)

Fig_A22a <- ggplot(wkp13, aes(x = soc_id_mean, y = GRUENE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4.33, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.21***", "b = - 0.21***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(GRUENE_att_mean ~ soc_id_mean, data = wkp13)

Fig_A22b <- ggplot(wkp13, aes(x = soc_id_mean, y = GRUENE_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 6.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 5.0, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.8), y = c(3.2, 3.2), label = c("b = - 0.44***", "b = - 2.44***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(GRUENE_amb_mean ~ GRUENE_att_mean, data = wkp13)

Fig_A22c <- ggplot(wkp13, aes(x = GRUENE_att_mean, y = GRUENE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the Green Party",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.51***", "b = - 0.50***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(GRUENE_amb_resid ~ soc_id_mean, data = wkp13)

Fig_A22d <- ggplot(wkp13, aes(x = soc_id_mean, y = GRUENE_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.06*", "b = - 0.02"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A22a, Fig_A22b, Fig_A22c, Fig_A22d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA22.png")

###GRUENE (economic)

##Ideology & Ambivalence

a = twolines(GRUENE_amb_mean ~ econ_id_mean, data = wkp13)

Fig_A23a <- ggplot(wkp13, aes(x = econ_id_mean, y = GRUENE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.7, yend = 0.4, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.16***", "b = - 0.22***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(GRUENE_att_mean ~ econ_id_mean, data = wkp13)

Fig_A23b <- ggplot(wkp13, aes(x = econ_id_mean, y = GRUENE_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 5.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 5.0, yend = 4.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.0, xend = 7.0, y = 4.0, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.8), y = c(3.2, 2.5), label = c("b = - 0.45***", "b = - 0.98***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(GRUENE_amb_mean ~ GRUENE_att_mean, data = wkp13)

Fig_A23c <- ggplot(wkp13, aes(x = GRUENE_att_mean, y = GRUENE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards the GReen Party",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.51***", "b = - 0.50***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(GRUENE_amb_resid ~ econ_id_mean, data = wkp13)

Fig_A23d <- ggplot(wkp13, aes(x = soc_id_mean, y = GRUENE_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.03", "b = - 0.04*"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A23a, Fig_A23b, Fig_A23c, Fig_A23d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA23.png")


###LINKE (socio-cultural)

##Ideology & Ambivalence

a = twolines(LINKE_amb_mean ~ soc_id_mean, data = wkp13)

Fig_A24a <- ggplot(wkp13, aes(x = soc_id_mean, y = LINKE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.5, yend = 0.3, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 6.0), y = c(-0.3, -0.3), label = c("b = 0.12***", "b = - 0.11***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(LINKE_att_mean ~ soc_id_mean, data = wkp13)

Fig_A24b <- ggplot(wkp13, aes(x = soc_id_mean, y = LINKE_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 4.0, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.0, xend = 7.0, y = 3.0, yend = 2.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.5), y = c(2.5, 1.5), label = c("b = - 0.20***", "b = - 0.25***"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(LINKE_amb_mean ~ LINKE_att_mean, data = wkp13)

Fig_A24c <- ggplot(wkp13, aes(x = LINKE_att_mean, y = LINKE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude towards die LINKE",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.52***", "b = - 0.48***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(LINKE_amb_resid ~ soc_id_mean, data = wkp13)

Fig_A24d <- ggplot(wkp13, aes(x = soc_id_mean, y = LINKE_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Opposition to Immigration",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.08**", "b = - 0.03*"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A24a, Fig_A24b, Fig_A24c, Fig_A24d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA24.png")

###LINKE (economic)

##Ideology & Ambivalence

a = twolines(LINKE_amb_mean ~ econ_id_mean, data = wkp13)

Fig_A25a <- ggplot(wkp13, aes(x = econ_id_mean, y = LINKE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 3.67, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = 0.3, yend = 0.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = 0.5, yend = 0.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-0.3, -0.3), label = c("b = 0.10***", "b = - 0.20***"), color = "red", size = 4, fontface = "bold")

##Ideology & Attitude

a = twolines(LINKE_att_mean ~ econ_id_mean, data = wkp13)

Fig_A25b <- ggplot(wkp13, aes(x = econ_id_mean, y = LINKE_att_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Attitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.5, xend = 4.5, y = 4.0, yend = 3.0, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.0, xend = 7.0, y = 2.8, yend = 2.7, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(3.5, 6.5), y = c(2.5, 1.5), label = c("b = - 0.69***", "b = - 0.09"), color = "red", size = 4, fontface = "bold")

##Attitude and Ambivalence

a = twolines(LINKE_amb_mean ~ LINKE_att_mean, data = wkp13)

Fig_A25c <- ggplot(wkp13, aes(x = LINKE_att_mean, y = LINKE_amb_mean)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Attitude toward Die LINKE",
       y = "Ambivalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 4.0, xend = 5.5, y = 1.0, yend = 1.5, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 6.5, xend = 7.8, y = 1.5, yend = 1.2, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(4.5, 8.0), y = c(0.5, 0.5), label = c("b = 0.52***", "b = - 0.48***"), color = "red", size = 4, fontface = "bold")

##Ideology and Residual Ambivalence (not explained by attitude)

a = twolines(LINKE_amb_resid ~ econ_id_mean, data = wkp13)

Fig_A25d <- ggplot(wkp13, aes(x = soc_id_mean, y = LINKE_amb_resid)) + 
  geom_jitter(alpha = 0.02) + 
  geom_smooth() +
  labs(x = "Support for Low Taxes and Low Social Benefits",
       y = "Residual Ambivalence") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  xlim("1", "2", "3", "4", "5", "6", "7") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("segment", x = 2.0, xend = 3.5, y = - 1.0, yend = -0.8, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("segment", x = 4.5, xend = 6.0, y = - 0.8, yend = - 0.9, colour = "red", size = 1.0, alpha = 0.6, arrow = arrow()) +
  annotate("text", x = c(2.5, 5.0), y = c(-2.0, -2.0), label = c("b = 0.09***", "b = - 0.04"), color = "red", size = 4, fontface = "bold")

plot_grid(Fig_A25a, Fig_A25b, Fig_A25c, Fig_A25d, labels = c('A', 'B', 'C', 'D'), label_size = 16)

ggsave("figures/figureA25.png")


