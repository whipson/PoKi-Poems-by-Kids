library(tidyverse)

poems_full <- read_csv("https://raw.githubusercontent.com/whipson/Childrens_Poems/master/poems_full.csv")

#Descriptives

#How many distinct words are there?

words_nonstop %>%
  select(word) %>%
  n_distinct()

words_emotion %>%
  select(word) %>%
  n_distinct()

#Excluding poems with fewer than 5 words

poems_5 <- poems_full %>%
  filter(total_words >= 5)

#What scores are associated with 95% of the sample?

quantile(poems_5$valence, probs = c(.025, .975))

quantile(poems_5$arousal, probs = c(.025, .975))

quantile(poems_5$dominance, probs = c(.025, .975))

quantile(poems_5$anger, probs = c(.025, .975), na.rm = TRUE)

quantile(poems_5$fear, probs = c(.025, .975), na.rm = TRUE)

quantile(poems_5$sadness, probs = c(.025, .975), na.rm = TRUE)

quantile(poems_5$joy, probs = c(.025, .975), na.rm = TRUE)

#Descriptive Statistics

library(psych)

describe(poems_5)

describeBy(poems_5, "grade")

poems_5 %>%
  select(valence, arousal, dominance) %>%
  corr.test()

#T-tests

gender_t <- poems_5 %>%
  filter(total_words >= 5,
         gender %in% c("Male", "Female"))

describeBy(poems_5, "gender")

t.test(valence ~ gender, data = gender_t, var.equal = TRUE)

t.test(arousal ~ gender, data = gender_t, var.equal = TRUE)

t.test(dominance ~ gender, data = gender_t, var.equal = TRUE)

t.test(anger ~ gender, data = gender_t, var.equal = TRUE)

t.test(fear ~ gender, data = gender_t, var.equal = TRUE)

t.test(sadness ~ gender, data = gender_t, var.equal = TRUE)

t.test(joy ~ gender, data = gender_t, var.equal = TRUE)

#GAM Visuals

library(reshape2)

poems_5_melt <- poems_5 %>%
  select(grade, valence, arousal, dominance, anger, fear, sadness, joy) %>%
  rename(Valence = valence,
         Arousal = arousal,
         Dominance = dominance,
         Anger = anger,
         Fear = fear,
         Sadness = sadness,
         Joy = joy) %>%
  melt(id.vars = "grade")

#VAD

poems_5_melt %>%
  filter(variable %in% c("Valence", "Arousal", "Dominance")) %>%
  ggplot(aes(grade, value)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw(base_size = 26)

#EIL

poems_5_melt %>%
  filter(variable %in% c("Anger", "Fear", "Sadness", "Joy")) %>%
  ggplot(aes(grade, value, color = variable, shape = variable)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  scale_color_manual(values = c("#FF0000", "#FF7F00", "#0000CD", "#00CD00")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  labs(x = "Grade", y = "Intensity", color = "", shape = "") +
  theme_bw(base_size = 24)

#Trends by Gender

poems_gen_5 <- poems_full %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words >= 5)

poems_gen_melt <- poems_gen_5 %>%
  select(id, grade, total_words, gender, valence, arousal, dominance,
         anger, fear, sadness, joy) %>%
  rename(Valence = valence,
         Arousal = arousal,
         Dominance = dominance,
         Anger = anger,
         Fear = fear,
         Sadness = sadness,
         Joy = joy) %>%
  melt(id.vars = c("id", "grade", "total_words", "gender"))

#VAD

poems_gen_melt %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words >= 5,
         variable %in% c("Valence", "Arousal", "Dominance")) %>%
  ggplot(aes(grade, value, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_shape_manual(values = c(16, 15)) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(.99, .35))

#EIL

poems_gen_melt %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words >= 5,
         variable %in% c("Anger", "Fear", "Sadness", "Joy")) %>%
  ggplot(aes(grade, value, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_shape_manual(values = c(16, 15)) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~variable) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(.99, .25))

#GAMS

library(mgcv)

#VAD

val5 <- gam(valence ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(val5)

val_gen5 <- gam(valence ~  
                  s(grade, by = factor(gender), bs = 'cr') + 
                  total_words, data = poems_gen_5)
summary(val_gen5)

aro5 <- gam(arousal ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(aro5)

aro_gen5 <- gam(arousal ~
                  s(grade, by = factor(gender), bs = 'cr') +
                  total_words, data = poems_gen_5)
summary(aro_gen5)

dom5 <- gam(dominance ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(dom5)

dom_gen5 <- gam(dominance ~ 
                  s(grade, by = factor(gender), bs = 'cr') +
                  total_words, data = poems_gen_5)
summary(dom_gen5)

#diagnostics

gam.check(val5)
gam.check(val_gen5)
gam.check(aro5)
gam.check(aro_gen5)
gam.check(dom5)
gam.check(dom_gen5)

#EIL

anger5 <- gam(anger ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(anger5)

anger_gen5 <- gam(anger ~  
                    s(grade, by = factor(gender), bs = 'cr') + 
                    total_words, data = poems_gen_5)
summary(anger_gen5)

fear5 <- gam(fear ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(fear5)

fear_gen5 <- gam(fear ~  
                   s(grade, by = factor(gender), bs = 'cr') + 
                   total_words, data = poems_gen_5)
summary(fear_gen5)

sadness5 <- gam(sadness ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(sadness5)

sadness_gen5 <- gam(sadness ~
                      s(grade, by = factor(gender), bs = 'cr') + 
                      total_words, data = poems_gen_5)
summary(sadness_gen5)

joy5 <- gam(joy ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(joy5)

joy_gen5 <- gam(joy ~  
                  s(grade, by = factor(gender), bs = 'cr') + 
                  total_words, data = poems_gen_5)
summary(joy_gen5)

#diagnostics

gam.check(anger5)
gam.check(anger_gen5)
gam.check(fear5)
gam.check(fear_gen5)
gam.check(sadness5)
gam.check(sadness_gen5)
gam.check(joy5)
gam.check(joy_gen5)

#Bootstrapping: Sensitivity Analysis

set.seed(190327)

bootstrap_val <- replicate(50, {
  sample <- sample_n(poems_5, 8000, weight = grade)
  gam <- summary(gam(valence ~ s(grade, bs = 'cr') + total_words, data = sample))
  print(gam$r.sq)
})

summary(bootstrap_val)

bootstrap_aro <- replicate(50, {
  sample <- sample_n(poems_5, 8000, weight = grade)
  gam <- summary(gam(arousal ~ s(grade, bs = 'cr') + total_words, data = sample))
  print(gam$r.sq)
})

summary(bootstrap_aro)

bootstrap_dom <- replicate(50, {
  sample <- sample_n(poems_5, 8000, weight = grade)
  gam <- summary(gam(dominance ~ s(grade, bs = 'cr') + total_words, data = sample))
  print(gam$r.sq)
})

summary(bootstrap_dom)

#25 word count minimum

poems_25 <- poems_full %>%
  filter(total_words >= 25)

describe(poems_25)

describeBy(poems_25, "grade")

poems_gen_25 <- poems_full %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words >= 25)

#GAM visuals

#GAM Visuals

poems_25_melt <- poems_25 %>%
  select(grade, valence, arousal, dominance, anger, fear, sadness, joy) %>%
  rename(Valence = valence,
         Arousal = arousal,
         Dominance = dominance,
         Anger = anger,
         Fear = fear,
         Sadness = sadness,
         Joy = joy) %>%
  melt(id.vars = "grade")

poems_gen_melt25 <- poems_gen_25 %>%
  select(id, grade, total_words, gender, valence, arousal, dominance,
         anger, fear, sadness, joy) %>%
  rename(Valence = valence,
         Arousal = arousal,
         Dominance = dominance,
         Anger = anger,
         Fear = fear,
         Sadness = sadness,
         Joy = joy) %>%
  melt(id.vars = c("id", "grade", "total_words", "gender"))

#VAD

poems_25_melt %>%
  filter(variable %in% c("Valence", "Arousal", "Dominance")) %>%
  ggplot(aes(grade, value)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw(base_size = 26)

poems_gen_melt25 %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words >= 5,
         variable %in% c("Valence", "Arousal", "Dominance")) %>%
  ggplot(aes(grade, value, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_shape_manual(values = c(16, 15)) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(.99, .35))

#EIL

poems_25_melt %>%
  filter(variable %in% c("Anger", "Fear", "Sadness", "Joy")) %>%
  ggplot(aes(grade, value, color = variable, shape = variable)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  scale_color_manual(values = c("#FF0000", "#FF7F00", "#0000CD", "#00CD00")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~variable) +
  theme_bw(base_size = 26) +
  theme(legend.position = "none")

poems_gen_melt25 %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         variable %in% c("Anger", "Fear", "Sadness", "Joy")) %>%
  ggplot(aes(grade, value, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_shape_manual(values = c(16, 15)) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~variable) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5))

#GAMS

#VAD

val25 <- gam(valence ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(val25)

val_gen25 <- gam(valence ~  
                   s(grade, by = factor(gender), bs = 'cr') + 
                   total_words, data = poems_gen_25)
summary(val_gen25)

aro25 <- gam(arousal ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(aro25)

aro_gen25 <- gam(arousal ~  
                   s(grade, by = factor(gender), bs = 'cr') +
                   total_words, data = poems_gen_25)
summary(aro_gen25)

dom25 <- gam(dominance ~ s(grade, bs = 'cr') + total_words, data = poems_25)

summary(dom25)

dom_gen25 <- gam(dominance ~ 
                   s(grade, by = factor(gender), bs = 'cr') +
                   total_words, data = poems_gen_25)
summary(dom_gen25)

#diagnostics

gam.check(val25)
gam.check(val_gen25)
gam.check(aro25)
gam.check(aro_gen25)
gam.check(dom25)
gam.check(dom_gen25)

#EIL

anger25 <- gam(anger ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(anger25)

anger_gen25 <- gam(anger ~  
                     s(grade, by = factor(gender), bs = 'cr') + 
                     total_words, data = poems_gen_25)
summary(anger_gen25)

fear25 <- gam(fear ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(fear25)

fear_gen25 <- gam(fear ~ 
                    s(grade, by = factor(gender), bs = 'cr') + 
                    total_words, data = poems_gen_25)
summary(fear_gen25)

sadness25 <- gam(sadness ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(sadness25)

sadness_gen25 <- gam(sadness ~ 
                       s(grade, by = factor(gender), bs = 'cr') + 
                       total_words, data = poems_gen_25)
summary(sadness_gen25)

joy25 <- gam(joy ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(joy25)

joy_gen25 <- gam(joy ~  
                   s(grade, by = factor(gender), bs = 'cr') + 
                   total_words, data = poems_gen_25)
summary(joy_gen25)

#diagnostics

gam.check(anger25)
gam.check(anger_gen25)
gam.check(fear25)
gam.check(fear_gen25)
gam.check(sadness25)
gam.check(sadness_gen25)
gam.check(joy25)
gam.check(joy_gen25)
