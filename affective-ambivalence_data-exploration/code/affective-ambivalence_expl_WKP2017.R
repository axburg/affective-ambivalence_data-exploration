####General Settings####

###Load Packages

library(foreign)
library(dplyr)
library(summarytools)
library(psych)
library(ggplot2)
library(corrr)

###Load Functions for Two-Lines test

source("http://webstimate.org/twolines/twolines.R")


###Load Data

wkp17 <- read.spss("../data/ZA6804_en_v7-0-0.sav",
          use.value.labels = FALSE, to.data.frame = TRUE)

wkp17[wkp17 < 0] <- NA

####Prepare Variables####

####Demographics####

###Gender

wkp17$gend <- factor(wkp17$kpx_2280,
                     levels = c(1, 2),
                     labels = c("male", "female"))

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

###Income

wkp17$inc <- coalesce(wkp17$kp1_2591, wkp17$kpa1_2591)

###Region of residency

wkp17$region <- factor(wkp17$ostwest,
                       levels = c(0, 1),
                       labels = c("East Germany", "West Germany"))

####Affect####

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

####Political Variables####

##Political interest

wkp17$pol_int_w4 <- 6 - wkp17$kp4_010
wkp17$pol_int_w6 <- 6 - wkp17$kp6_010

##Symbolic ideology

wkp17$symb_id_w4 <- wkp17$kp4_1500
wkp17$symb_id_w6 <- wkp17$kp6_1500

##Ideological Extremity

wkp17 <- wkp17 %>% mutate(symb_extr_w4 = case_when(symb_id_w4 == 1 | symb_id_w4 == 11 ~ 5,
                                                   symb_id_w4 == 2 | symb_id_w4 == 10 ~ 4,
                                                   symb_id_w4 == 3 | symb_id_w4 == 9 ~ 3,
                                                   symb_id_w4 == 4 | symb_id_w4 == 8 ~ 2,
                                                   symb_id_w4 == 5 | symb_id_w4 == 7 ~ 1,
                                                   symb_id_w4 == 6 ~ 0))

##Economic Ideology

wkp17$econ_id_w4 <- 8 - wkp17$kp4_1090
wkp17$econ_id_w6 <- 8 - wkp17$kp6_1090

##Socio-cultural Ideology (Immigration)

wkp17$soc_id_w4 <- wkp17$kp4_1130
wkp17$soc_id_w6 <- wkp17$kp6_1130

##Authoritarianism

wkp17$aut1 <- coalesce(wkp17$kp2_060i, wkp17$kpa1_060i)
wkp17$aut2 <- coalesce(wkp17$kp2_060j, wkp17$kpa1_060j)
wkp17$aut3 <- coalesce(wkp17$kp2_060k, wkp17$kpa1_060k)

wkp17$aut_M <- rowMeans(wkp17[, c("aut1", "aut2", "aut3")], na.rm = TRUE)

psych::alpha(wkp17[, c("aut1", "aut2", "aut3")]) #alpha = .72

####Psychological Dispositions####

##Schwartz values##

for (x in c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u")) {
  wkp17[[paste0('schwartz_',x,'1')]] <- wkp17[[paste0('kp1_3320',x)]]
}

for (x in c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u")) {
  wkp17[[paste0('schwartz_',x,'2')]] <- wkp17[[paste0('kpa1_3320',x)]]
}

for (x in c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u")) {
  wkp17[[paste0('schwartz_',x)]] <- coalesce(wkp17[[paste0('schwartz_',x,'1')]], wkp17[[paste0('schwartz_',x,'2')]])
}

wkp17 <- wkp17 %>% rowwise() %>% mutate(cons = mean(c(schwartz_e, schwartz_m, schwartz_q, schwartz_n, schwartz_j, schwartz_t), 
                                                    na.rm = T))

wkp17 <- wkp17 %>% rowwise() %>% mutate(open = mean(c(schwartz_a, schwartz_i, schwartz_f, schwartz_s, schwartz_g, schwartz_r), 
                                                    na.rm = T))

wkp17 <- wkp17 %>% rowwise() %>% mutate(s_trans = mean(c(schwartz_c, schwartz_h, schwartz_u, schwartz_k, schwartz_p), 
                                                    na.rm = T))

wkp17 <- wkp17 %>% rowwise() %>% mutate(s_enh = mean(c(schwartz_b, schwartz_o, schwartz_d, schwartz_l), 
                                                      na.rm = T))

wkp17$open_cons <- wkp17$cons - wkp17$open

wkp17$trans_enh <- wkp17$s_enh - wkp17$s_trans

##Need for Closure

wkp17$nfc1 <- coalesce(wkp17$kp3_1570g, wkp17$kpa1_1570g)
wkp17$nfc2 <- coalesce(wkp17$kp3_1570h, wkp17$kpa1_1570h)
wkp17$nfc3 <- coalesce(wkp17$kp3_1570i, wkp17$kpa1_1570i)

wkp17$nfc_M <- rowMeans(wkp17[, c("nfc1", "nfc2", "nfc3")], na.rm = TRUE)

psych::alpha(wkp17[, c("nfc1", "nfc2", "nfc3")]) #alpha = .78

####Analyses####

###Explore Correlations

cor <- wkp17 %>% 
  select(symb_id_w4,symb_extr_w4, aut_M, open_cons,trans_enh, nfc_M, 
         Merkel_pos_w4, Merkel_neg_w4, Schulz_pos_w4, Schulz_neg_w4, 
         Merkel_amb_w4, Schulz_amb_w4) %>% 
  correlate()

###Visualize Association between negative and positive feelings

#Merkel
ggplot(wkp17, aes(x = Merkel_pos_w4, y = Merkel_neg_w4)) + geom_jitter(alpha = 0.3)
ggplot(wkp17, aes(x = Merkel_pos_w6, y = Merkel_neg_w6)) + geom_jitter(alpha = 0.3)

#Schulz
ggplot(wkp17, aes(x = Schulz_pos_w4, y = Schulz_neg_w4)) + geom_jitter(alpha = 0.3)
ggplot(wkp17, aes(x = Schulz_pos_w6, y = Schulz_neg_w6)) + geom_jitter(alpha = 0.3)

###Association between symbolic ideology and feelings

##Merkel

#W4

ggplot(wkp17, aes(x = symb_id_w4, y = Merkel_neg_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

ggplot(wkp17, aes(x = symb_id_w4, y = Merkel_pos_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

ggplot(wkp17, aes(x = symb_id_w4, y = Merkel_amb_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

twolines(Merkel_amb_w4 ~ symb_id_w4, data = wkp17)

#W6
ggplot(wkp17, aes(x = symb_id_w6, y = Merkel_amb_w6)) + geom_jitter(alpha = 0.3) + geom_smooth()

twolines(Merkel_amb_w6 ~ symb_id_w6, data = wkp17)


##Schulz

#W4

ggplot(wkp17, aes(x = symb_id_w4, y = Schulz_neg_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

ggplot(wkp17, aes(x = symb_id_w4, y = Schulz_pos_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

ggplot(wkp17, aes(x = symb_id_w4, y = Schulz_amb_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

twolines(Schulz_amb_w4 ~ symb_id_w4, data = wkp17)

#W6
ggplot(wkp17, aes(x = symb_id_w6, y = Schulz_amb_w6)) + geom_jitter(alpha = 0.3) + geom_smooth()

twolines(Schulz_amb_w6 ~ symb_id_w6, data = wkp17)

##Association between authoritarianism and feelings

##Merkel

#W4

ggplot(wkp17, aes(x = aut_M, y = Merkel_neg_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

ggplot(wkp17, aes(x = aut_M, y = Merkel_pos_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

ggplot(wkp17, aes(x = aut_M, y = Merkel_amb_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

twolines(Merkel_amb_w4 ~ aut_M, data = wkp17)

#W6

ggplot(wkp17, aes(x = aut_M, y = Merkel_amb_w6)) + geom_jitter(alpha = 0.3) + geom_smooth()

twolines(Merkel_amb_w6 ~ aut_M, data = wkp17)

##Schulz

#W4

ggplot(wkp17, aes(x = aut_M, y = Schulz_neg_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

ggplot(wkp17, aes(x = aut_M, y = Schulz_pos_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

ggplot(wkp17, aes(x = aut_M, y = Schulz_amb_w4)) + geom_jitter(alpha = 0.3) + geom_smooth()

twolines(Schulz_amb_w4 ~ aut_M, data = wkp17)




###Two-lines test

source("http://webstimate.org/twolines/twolines.R")

Schulz_w4 <- twolines(Schulz_amb_w4 ~ pol_int_w4, data = wkp17)
