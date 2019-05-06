#Opening the Library and Setting the Theme

library(tidyverse)
library(stringr)
library(dplyr)

#Importing and Cleaning the Data

poems_raw <- read_csv("https://raw.githubusercontent.com/whipson/Childrens_Poems/master/childrens_poem_raw.csv")

poems_raw$id <- str_remove(poems_raw$id, "\\{id: ")

poems_raw$grade <- gsub("country: Grade ", "", poems_raw$grade)

poems_raw$grade <- as.numeric(poems_raw$grade)

poems_raw$text <- tolower(poems_raw$text)

poems_raw$text <- gsub("\\.", ". ", poems_raw$text)

poems_raw$text <- gsub("\\,", ", ", poems_raw$text)

poems_raw$text <- gsub("?", "", poems_raw$text)

poems_raw$author <- str_replace(poems_raw$author, "By ", "")

poems_raw$author <- str_sub(poems_raw$author, 1, str_length(poems_raw$author)-3)

poems_raw$author <- tolower(poems_raw$author)

poems <- as_tibble(poems_raw)

#Removing Template Poems

strings <- c("awoke one morning, a stork was on my head",
             "awoke one morning a stork was on my head",
             "i eat pickles",
             "i eat pickles")

poems <- poems %>%
  filter(!str_detect(poems$text, paste(strings, collapse = "|")))

#Running above code produces childrens_poems_clean.csv

library(tidytext)

words_total <- poems %>%
  unnest_tokens(word, text)

#Stop words and Gender Data

#Download Cornell Stop list from: http://www.lextek.com/manuals/onix/stopwords2.html

cornell_stop <- read.delim("~/CornellStopList.txt", header = FALSE, stringsAsFactors = FALSE, col.names = c("word"))

other_stop <- bind_rows(data_frame(word = c("?", "i?'m", "im", "don?'t", "br", "it?'s", "gt", "x85", "2", "can?'t", "1", "3", "i?'ll",
                                            "ha", "la")))

#Download Gender Census data from: https://www.ssa.gov/oact/babynames/limits.html

gender <- read.delim("~/R/Sentiment Analysis/yob2017.txt", header = FALSE) %>%
  separate(V1, c("author", "gender", "frequency")) %>%
  mutate(author = tolower(author)) %>%
  arrange(author) %>%
  as_tibble()

gender_new <- gender %>%
  spread(key = gender, value = frequency) %>%
  mutate_at(c(2:3), funs(replace(., is.na(.), 0))) %>%
  rename("female" = "F",
         "male" = "M") %>%
  mutate(male = as.numeric(male),
         female = as.numeric(female)) %>%
  mutate(conf_female = round(female/(male + female), 2)) %>%
  mutate(gender = case_when(conf_female >= .95 ~ "Female",
                            conf_female <= .05 ~ "Male",
                            conf_female > .05 & conf_female < .95 ~ "Ambiguous"))

#Lexicons

#Download VAD and Emotion Intensity (AIL) from: http://saifmohammad.com/WebPages/lexicons.html

arousal <- read.delim("~/R/Sentiment Analysis/Lexicon/a.scores", header = FALSE) %>%
  rename(word = V1,
         arousal = V2) %>%
  mutate(word = str_to_lower(word))

valence <- read.delim("~/R/Sentiment Analysis/Lexicon/v.scores", header = FALSE) %>%
  rename(word = V1,
         valence = V2) %>%
  mutate(word = str_to_lower(word))

dominance <- read.delim("~/R/Sentiment Analysis/Lexicon/d.scores", header = FALSE) %>%
  rename(word = V1,
         dominance = V2) %>%
  mutate(word = str_to_lower(word))

AIL <- read.delim("~/R/Sentiment Analysis/Lexicon/AffectIntensity.txt", stringsAsFactors = FALSE) %>%
  rename(word = term,
         emotion = AffectDimension) %>%
  mutate(word = str_to_lower(word))

AIL_wide <- AIL %>%
  spread(emotion, score)

#Aligning Words with Lexicons

words_nonstop <- words_total %>%
  anti_join(cornell_stop) %>%
  anti_join(other_stop)

words_emotion <- words_nonstop %>%
  inner_join(valence) %>%
  left_join(arousal) %>%
  left_join(dominance) %>%
  left_join(AIL_wide)

#Computing scores at poem level

poems_full <- words_emotion %>%
  group_by(id, grade, author) %>%
  summarize(val_sd = sd(valence, na.rm = TRUE),
            aro_sd = sd(arousal, na.rm = TRUE),
            dom_sd = sd(dominance, na.rm = TRUE),
            anger_sd = sd(anger, na.rm = TRUE),
            fear_sd = sd(fear, na.rm = TRUE),
            sadness_sd = sd(sadness, na.rm = TRUE),
            joy_sd = sd(joy, na.rm = TRUE),
            valence = mean(valence, na.rm = TRUE),
            arousal = mean(arousal, na.rm = TRUE),
            dominance = mean(dominance, na.rm = TRUE),
            anger = mean(anger, na.rm = TRUE),
            fear = mean(fear, na.rm = TRUE),
            sadness = mean(sadness, na.rm = TRUE),
            joy = mean(joy, na.rm = TRUE),
            total_words = n()) %>%
  ungroup() %>%
  left_join(gender_new) %>%
  select(-c(male, female, conf_female)) %>%
  transmute_all(funs(ifelse(is.nan(.), NA, .)))

poems_full <- read_csv("")

#Descriptives

words_nonstop %>%
  select(word) %>%
  n_distinct()

words_emotion %>%
  select(word) %>%
  n_distinct()

poems_5 <- poems_full %>%
  filter(total_words >= 5)

quantile(poems_5$valence, probs = c(.025, .975))

quantile(poems_5$arousal, probs = c(.025, .975))

quantile(poems_5$dominance, probs = c(.025, .975))

quantile(poems_5$anger, probs = c(.025, .975), na.rm = TRUE)

quantile(poems_5$fear, probs = c(.025, .975), na.rm = TRUE)

quantile(poems_5$sadness, probs = c(.025, .975), na.rm = TRUE)

quantile(poems_5$joy, probs = c(.025, .975), na.rm = TRUE)

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

#Gender differences in words

words_prop_gender <- words_emotion %>%
  inner_join(gender_new) %>%
  filter(gender %in% c("Male", "Female")) %>%
  group_by(gender) %>%
  mutate(total = n()) %>%
  group_by(word, gender) %>%
  mutate(n = n(),
         prop = n / total) %>%
  select(word, gender, prop) %>%
  distinct(.) %>%
  ungroup() %>%
  spread(gender, prop, convert = TRUE) %>%
  mutate(Female = ifelse(is.na(Female), 0, Female),
         Male = ifelse(is.na(Male), 0, Male)) %>%
  mutate(diff = Male - Female,
         Male = scales::percent(Male),
         Female = scales::percent(Female))

words_prop_gender %>%
  arrange(desc(abs(diff))) %>%
  inner_join(valence) %>%
  inner_join(arousal) %>%
  View()

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

#AIL

poems_5_melt %>%
  filter(variable %in% c("Anger", "Fear", "Sadness", "Joy")) %>%
  ggplot(aes(grade, value, color = variable, shape = variable)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  scale_color_manual(values = c("#0000CD", "#FF0000", "#00CD00", "#FF7F00")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  theme_bw(base_size = 26)

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
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(.99, .35))

#AIL

poems_gen_melt %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words >= 5,
         variable %in% c("Anger", "Fear", "Sadness", "Joy")) %>%
  ggplot(aes(grade, value, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
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

val_gen5 <- gam(valence ~ s(grade, bs = 'cr') + 
                  s(grade, by = factor(gender), bs = 'cr') + 
                  total_words, data = poems_gen_5)
summary(val_gen5)

aro5 <- gam(arousal ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(aro5)

aro_gen5 <- gam(arousal ~ s(grade, bs = 'cr') + 
                  s(grade, by = factor(gender), bs = 'cr') +
                  total_words, data = poems_gen_5)
summary(aro_gen5)

dom5 <- gam(dominance ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(dom5)

dom_gen5 <- gam(dominance ~ s(grade, bs = 'cr') +
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

#AIL

anger5 <- gam(anger ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(anger5)

anger_gen5 <- gam(anger ~ s(grade, bs = 'cr') + 
                    s(grade, by = factor(gender), bs = 'cr') + 
                    total_words, data = poems_gen_5)
summary(anger_gen5)

fear5 <- gam(fear ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(fear5)

fear_gen5 <- gam(fear ~ s(grade, bs = 'cr') + 
                   s(grade, by = factor(gender), bs = 'cr') + 
                   total_words, data = poems_gen_5)
summary(fear_gen5)

sadness5 <- gam(sadness ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(sadness5)

sadness_gen5 <- gam(sadness ~ s(grade, bs = 'cr') + 
                      s(grade, by = factor(gender), bs = 'cr') + 
                      total_words, data = poems_gen_5)
summary(sadness_gen5)

joy5 <- gam(joy ~ s(grade, bs = 'cr') + total_words, data = poems_5)
summary(joy5)

joy_gen5 <- gam(joy ~ s(grade, bs = 'cr') + 
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

#25 word count

poems_25 <- poems_full %>%
  filter(total_words >= 25)

describe(poems_25)

describeBy(poems_25, "grade")

poems_gen_25 <- poems_full %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words >= 25)

#VAD

val25 <- gam(valence ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(val25)

val_gen25 <- gam(valence ~ s(grade, bs = 'cr') + 
                   s(grade, by = factor(gender), bs = 'cr') + 
                   total_words, data = poems_gen_25)
summary(val_gen25)

aro25 <- gam(arousal ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(aro25)

aro_gen25 <- gam(arousal ~ s(grade, bs = 'cr') + 
                   s(grade, by = factor(gender), bs = 'cr') +
                   total_words, data = poems_gen_25)
summary(aro_gen25)

dom25 <- gam(dominance ~ s(grade, bs = 'cr') + total_words, data = poems_25)

summary(dom25)

dom_gen25 <- gam(dominance ~ s(grade, bs = 'cr') +
                   s(grade, by = factor(gender), bs = 'cr') +
                   total_words, data = poems_gen_25)
summary(dom_gen25)

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

#AIL

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

poems_25_melt %>%
  filter(variable %in% c("Anger", "Fear", "Sadness", "Joy")) %>%
  ggplot(aes(grade, value, color = variable, shape = variable)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  scale_color_manual(values = c("#0000CD", "#FF0000", "#00CD00", "#FF7F00")) +
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
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~variable) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5))

poems_gen_melt25 %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words >= 5,
         variable %in% c("Valence", "Arousal", "Dominance")) %>%
  ggplot(aes(grade, value, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(.99, .35))

#GAMS

anger25 <- gam(anger ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(anger25)

anger_gen25 <- gam(anger ~ s(grade, bs = 'cr') + 
                     s(grade, by = factor(gender), bs = 'cr') + 
                     total_words, data = poems_gen_25)
summary(anger_gen25)

fear25 <- gam(fear ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(fear25)

fear_gen25 <- gam(fear ~ s(grade, bs = 'cr') + 
                    s(grade, by = factor(gender), bs = 'cr') + 
                    total_words, data = poems_gen_25)
summary(fear_gen25)

sadness25 <- gam(sadness ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(sadness25)

sadness_gen25 <- gam(sadness ~ s(grade, bs = 'cr') + 
                       s(grade, by = factor(gender), bs = 'cr') + 
                       total_words, data = poems_gen_25)
summary(sadness_gen25)

joy25 <- gam(joy ~ s(grade, bs = 'cr') + total_words, data = poems_25)
summary(joy25)

joy_gen25 <- gam(sadness ~ s(joy, bs = 'cr') + 
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
