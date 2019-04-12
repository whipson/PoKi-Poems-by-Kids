#Opening the Library and Setting the Theme

library(readr)
library(tidyverse)
library(stringr)
library(tidytext)
library(dplyr)
library(psych)
library(reshape2)
library(ggridges)

theme_set(theme_bw())

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

#Getting Gender Names

gender <- read.delim("~/R/Sentiment Analysis/yob2017.txt", header = FALSE) %>%
  separate(V1, c("author", "gender", "frequency")) %>%
  mutate(author = tolower(author)) %>%
  arrange(author) %>%
  as_tibble()

names <- gender[, 1]

gender_percent <- gender %>%
  spread(key = gender, value = frequency) %>%
  mutate_at(c(2:3), funs(replace(., is.na(.), 0))) %>%
  rename("female" = "F",
         "male" = "M") %>%
  mutate(male = as.numeric(male),
         female = as.numeric(female)) %>%
  mutate(conf_female = round(female/(male + female), 2))

gender_new <- gender_percent

gender_new$conf_female[gender_new$conf_female >= .95] <- "Female"
gender_new$conf_female[gender_new$conf_female <= .05] <- "Male"
gender_new$conf_female[gender_new$conf_female > .05 & gender_new$conf_female < .95] <- "Ambiguous"

#Loading up Stop Lists

cornell_stop <- read.delim("~/CornellStopList.txt", header = FALSE, stringsAsFactors = FALSE, col.names = c("word"))

other_stop <- bind_rows(data_frame(word = c("?", "i?'m", "im", "don?'t", "br", "it?'s", "gt", "x85", "2", "can?'t", "1", "3", "i?'ll",
                                            "ha", "la", "?")))

#Unnesting Tokens

tidy_poems <- poems %>%
  unnest_tokens(word, text) %>%
  anti_join(cornell_stop) %>%
  anti_join(other_stop) 

tidy_poems

#Number of unique words

length(unique(tidy_poems$word))

#Changing loves to love, likes to like, dogs to dog

tidy_poems <- tidy_poems %>%
  mutate(word = ifelse(word == "dogs", "dog", word),
         word = ifelse(word == "likes", "like", word),
         word = ifelse(word == "loves", "love", word),
         word = ifelse(word == "cats", "cat", word),
         word = ifelse(word == "playing", "play", word),
         word = ifelse(word == "ate", "eat", word),
         word = ifelse(word == "roses", "rose", word),
         word = ifelse(word == "days", "day", word),
         word = ifelse(word == "birds", "bird", word),
         word = ifelse(word == "hands", "hand", word),
         word = ifelse(word == "tears", "tear", word),
         word = ifelse(word == "makes", "make", word),
         word = ifelse(word == "made", "make", word),
         word = ifelse(word == "leaves", "leave", word))

#Loading up VAD

activation <- read.delim("~/R/Sentiment Analysis/Lexicon/a.scores", header=FALSE)
dominance <- read.delim("~/R/Sentiment Analysis/Lexicon/d.scores", header=FALSE)
valence <- read.delim("~/R/Sentiment Analysis/Lexicon/v.scores", header=FALSE)

colnames(activation) <- c("word", "activation")
colnames(dominance) <- c("word", "dominance")
colnames(valence) <- c("word", "valence")

activation <- activation %>%
  mutate(word = str_to_lower(word))

valence <- valence %>%
  mutate(word = str_to_lower(word))

dominance <- dominance %>%
  mutate(word = str_to_lower(word))

#Common lost words

tidy_poems %>%
  anti_join(valence) %>%
  count(word) %>%
  arrange(desc(n))

#Aligning text with VAD

tidy_poems_vad <- tidy_poems %>%
  inner_join(valence) %>%
  inner_join(activation) %>%
  inner_join(dominance) %>%
  mutate(val_split = cut(valence, c(0, .33, .66, 1), labels = c("low", "med", "high")),
         act_split = cut(activation, c(0, .33, .66, 1), labels = c("low", "med", "high")),
         dom_split = cut(dominance, c(0, .33, .66, 1), labels = c("low", "med", "high")))

tidy_poems_vad

#Number of unique words in tidy_poems_VAD

length(unique(tidy_poems_vad$word))

#Distribution of High, Med, Low affect terms by grade

valence_grouped <- tidy_poems_vad %>%
  group_by(grade) %>%
  count(val_split) %>%
  spread(val_split, n) %>%
  select(grade, low, med, high)

cbind(valence_grouped[, -1]/rowSums(valence_grouped[, -1])) %>%
  mutate(grade = row_number()) %>%
  melt("grade") %>%
  ggplot(aes(factor(grade), value, group = variable, fill = variable)) +
  geom_col(position = "dodge", width = .75) +
  labs(x = "Grade", y = "Proportion of Words", fill = "Valence Score") +
  theme_bw()

#Calculating Mean and 95% CI by Grade

tidy_poems_full <- tidy_poems_vad %>%
  group_by(id, grade, author) %>%
  summarize(val_sd = sd(valence, na.rm = TRUE),
            act_sd = sd(activation, na.rm = TRUE),
            dom_sd = sd(dominance, na.rm = TRUE),
            valence = mean(valence, na.rm = TRUE),
            activation = mean(activation, na.rm = TRUE),
            dominance = mean(dominance, na.rm = TRUE),
            total_words = n()) %>%
  ungroup() %>%
  left_join(gender_new) %>%
  select(-c(male, female)) %>%
  rename("gender" = "conf_female")

#Gender differences

gender_anova <- tidy_poems_full %>%
  filter(total_words > 4,
         !is.na(gender))

val_anova <- aov(valence ~ gender, data = gender_anova)
summary(val_anova)
TukeyHSD(val_anova)

aro_anova <- aov(activation ~ gender, data = gender_anova)
summary(aro_anova)
TukeyHSD(aro_anova)

dom_anova <- aov(dominance ~ gender, data = gender_anova)
summary(dom_anova)

describeBy(gender_anova, "gender")

#Boxplots for average word count by grade

tidy_poems_full %>%
  filter(total_words > 4) %>%
  ggplot(aes(as.factor(grade), total_words, fill = as.factor(grade))) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Grade", y = "Total words (log scale)") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

#Visualizing Change in Valence and Arousal over time > 4

melt_mean <- tidy_poems_full %>%
  filter(total_words > 4) %>%
  group_by(grade) %>%
  summarize(Valence = mean(valence),
            Arousal = mean(activation),
            Dominance = mean(dominance)) %>%
  ungroup() %>%
  melt(id = "grade",
       variable.name = "dimension_mean",
       value.name = "mean") 

melt_sd <- tidy_poems_full %>%
  filter(total_words > 4) %>%
  group_by(grade) %>%
  summarize(sdv = sd(valence),
            sda = sd(activation),
            sdd = sd(dominance)) %>%
  ungroup() %>%
  melt(id = "grade",
       variable.name = "dimension_sd",
       value.name = "sd") %>%
  select(-grade)

melt_n <- tidy_poems_full %>%
  filter(total_words > 4) %>%
  group_by(grade) %>%
  count() %>%
  ungroup() %>%
  select(-grade)

VAD_trends <- cbind(melt_mean, melt_sd, melt_n)

VAD_trends %>%
  ggplot(aes(x = as.numeric(grade), y = mean, color = dimension_mean)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean - 1.96*(sd/sqrt(n)), ymax = mean + 1.96*(sd/sqrt(n))), color = "black", size = 0, linetype = 0,
              alpha = .25) +
  geom_point(size = 5) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  facet_wrap(~ dimension_mean) +
  guides(color = FALSE) +
  labs(y = NULL,
       title = "Change in Valence, Arousal, & Dominance over Development",
       subtitle = "Grey area represents 95% confidence bands") +
  scale_color_manual(values = c("#1C86EE", "#FF4500", "#228B22")) +
  theme_bw(base_size = 28) +
  theme(plot.title = element_text(hjust = 0.5))

#Visualizing Change in Valence and Arousal over time any

melt_mean_any <- tidy_poems_full %>%
  group_by(grade) %>%
  summarize(Valence = mean(valence),
            Arousal = mean(activation)) %>%
  ungroup() %>%
  melt(id = "grade",
       variable.name = "dimension_mean",
       value.name = "mean") 

melt_sd_any <- tidy_poems_full %>%
  group_by(grade) %>%
  summarize(sdv = sd(valence),
            sda = sd(activation)) %>%
  ungroup() %>%
  melt(id = "grade",
       variable.name = "dimension_sd",
       value.name = "sd") %>%
  select(-grade)

melt_n_any <- tidy_poems_full %>%
  group_by(grade) %>%
  count() %>%
  ungroup() %>%
  select(-grade)

VAD_trends_any <- cbind(melt_mean_any, melt_sd_any, melt_n_any)

VAD_trends_any %>%
  ggplot(aes(x = as.numeric(grade), y = mean, color = dimension_mean)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean - 1.96*(sd/sqrt(n)), ymax = mean + 1.96*(sd/sqrt(n))), color = "black", size = 0, linetype = 0,
              alpha = .25) +
  geom_point(size = 5) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  facet_wrap(~ dimension_mean, scales = "free_y") +
  guides(color = FALSE) +
  labs(y = NULL,
       title = "Change in Valence and Arousal over Development",
       subtitle = "Grey area represents 95% confidence bands") +
  scale_color_manual(values = c("#1C86EE", "#FF4500")) +
  theme_bw(base_size = 28) +
  theme(plot.title = element_text(hjust = 0.5))

#Visualizing Change in Valence and Arousal over time > 9

melt_mean_10 <- tidy_poems_full %>%
  filter(total_words > 9) %>%
  group_by(grade) %>%
  summarize(Valence = mean(valence),
            Arousal = mean(activation)) %>%
  ungroup() %>%
  melt(id = "grade",
       variable.name = "dimension_mean",
       value.name = "mean") 

melt_sd_10 <- tidy_poems_full %>%
  filter(total_words > 9) %>%
  group_by(grade) %>%
  summarize(sdv = sd(valence),
            sda = sd(activation)) %>%
  ungroup() %>%
  melt(id = "grade",
       variable.name = "dimension_sd",
       value.name = "sd") %>%
  select(-grade)

melt_n_10 <- tidy_poems_full %>%
  filter(total_words > 9) %>%
  group_by(grade) %>%
  count() %>%
  ungroup() %>%
  select(-grade)

VAD_trends_10 <- cbind(melt_mean_10, melt_sd_10, melt_n_10)

VAD_trends_10 %>%
  ggplot(aes(x = as.numeric(grade), y = mean, color = dimension_mean)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean - 1.96*(sd/sqrt(n)), ymax = mean + 1.96*(sd/sqrt(n))), color = "black", size = 0, linetype = 0,
              alpha = .25) +
  geom_point(size = 5) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  facet_wrap(~ dimension_mean, scales = "free_y") +
  guides(color = FALSE) +
  labs(y = NULL,
       title = "Change in Valence and Arousal over Development",
       subtitle = "Grey area represents 95% confidence bands") +
  scale_color_manual(values = c("#1C86EE", "#FF4500")) +
  theme_bw(base_size = 28) +
  theme(plot.title = element_text(hjust = 0.5))

#GAM framework for visualizing

tidy_poems_5 <- tidy_poems_full %>%
  filter(total_words > 4)

describeBy(tidy_poems_5, "grade")

tidy_poems_5_melt <- tidy_poems_5 %>%
  select(grade, valence, activation, dominance) %>%
  rename(Valence = valence,
         Arousal = activation,
         Dominance = dominance) %>%
  melt(id.vars = "grade")

tidy_poems_5_melt %>%
  ggplot(aes(grade, value)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ variable) +
  theme_bw(base_size = 26)

tidy_poems_25 <- tidy_poems_full %>%
  filter(total_words > 24)

tidy_poems_25_melt <- tidy_poems_25 %>%
  select(grade, valence, activation, dominance) %>%
  rename(Valence = valence,
         Arousal = activation,
         Dominance = dominance) %>%
  melt(id.vars = "grade")

tidy_poems_25_melt %>%
  ggplot(aes(grade, value)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ variable) +
  theme_bw(base_size = 26)

#Visualizing SD by Grade

melt_sd_plot <- tidy_poems_full %>%
  filter(total_words > 4) %>%
  group_by(grade) %>%
  summarize('Valence SD' = mean(val_sd),
            'Arousal SD' = mean(act_sd),
            'Dominance SD' = mean(dom_sd)) %>%
  ungroup() %>%
  melt(id = "grade",
       variable.name = "sd",
       value.name = "mean") 

melt_sd_plot %>%
  ggplot(aes(x = as.numeric(grade), y = mean, color = sd)) +
  geom_line(size = 1.2) +
  geom_point(size = 5) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  facet_wrap(~ sd) +
  guides(color = FALSE) +
  labs(y = NULL) +
  scale_color_manual(values = c("#1C86EE", "#FF4500", "#008B00")) +
  theme_bw(base_size = 28) +
  theme(plot.title = element_text(hjust = 0.5))

#Visualizing Top 20 words

tidy_poems_vad %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "20 Most Common Words in Children's Poems",
       subtitle = "Stop words removed") +
  coord_flip() +
  guides(fill = FALSE) +
  theme_bw(base_size = 18)

#Visualizing Top 10 Words for two age groups

tidy_poems_vad %>%
  filter(grade %in% 1:5) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "dodgerblue3") +
  labs(x = NULL,
       y = NULL,
       title = "10 Most Common Words in Grades 1-5") +
  coord_flip() +
  guides(fill = FALSE) +
  theme_bw(base_size = 26)

tidy_poems_vad %>%
  filter(grade %in% 6:12) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkorange") +
  labs(x = NULL,
       y = NULL,
       title = "10 Most Common Words in Grades 6-12") +
  coord_flip() +
  guides(fill = FALSE) +
  theme_bw(base_size = 26)

#Trends by Gender

tidy_poems_full_gender <- tidy_poems_full %>%
  select(id, grade, total_words, gender, valence, activation, dominance) %>%
  rename(Valence = valence,
         Arousal = activation,
         Dominance = dominance) %>%
  melt(id.vars = c("id", "grade", "total_words", "gender"))

tidy_poems_full_gender %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words > 4) %>%
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
        legend.justification = c(1, 1), legend.position = c(1, .95))

tidy_poems_full_gender %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words > 24) %>%
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
        legend.justification = c(1, 1), legend.position = c(1, .95))

#95% of scores for valence and arousal

quantile(tidy_poems_5$valence, probs = c(.025, .975))

quantile(tidy_poems_5$activation, probs = c(.025, .975))

#Correlations between dimensions

tidy_poems_5 %>%
  select(valence, activation, dominance) %>%
  corr.test()

#GAMS

library(mgcv)

#At least 5 words

tidy_poems_gen5 <- tidy_poems_full %>%
  filter(!is.na(gender),
         gender != "Ambiguous",
         total_words > 4)

gam_val5 <- gam(valence ~ s(grade, bs = 'cr') + total_words, family = gaussian, data = tidy_poems_5)
summary(gam_val5)

gam_val_gen5 <- gam(valence ~ s(grade, bs = 'cr') + 
                     s(grade, by = factor(gender), bs = 'cr') + 
                     total_words, data = tidy_poems_gen5)
summary(gam_val_gen5)

gam_aro5 <- gam(activation ~ s(grade, bs = 'cr') + total_words, data = tidy_poems_5)
summary(gam_aro5)

gam_aro_gen5 <- gam(activation ~ s(grade, bs = 'cr') + 
                     s(grade, by = factor(gender), bs = 'cr') +
                     total_words, data = tidy_poems_gen5)

summary(gam_aro_gen5)

gam_dom5 <- gam(dominance ~ s(grade, bs = 'cr') + total_words, data = tidy_poems_5)

summary(gam_dom5)

gam_dom_gen5 <- gam(dominance ~ s(grade, bs = 'cr') +
                  s(grade, by = factor(gender), bs = 'cr') +
                  total_words, data = tidy_poems_gen5)

summary(gam_dom_gen5)

gam.check(gam_val5)

gam.check(gam_aro5)

gam.check(gam_val_gen5)

gam.check(gam_aro_gen5)

gam.check(gam_dom5)

gam.check(gam_dom_gen5)

#No poems removed

tidy_poems_gen <- tidy_poems_full %>%
  filter(!is.na(gender))

gam_val <- gam(valence ~ s(grade, bs = 'cr') + total_words, family = gaussian, data = tidy_poems_full)
summary(gam_val)

gam.check(gam_val)

gam_val_gen <- gam(valence ~ s(grade, bs = 'cr') + 
                     s(grade, by = factor(gender), bs = 'cr') + 
                     total_words, data = tidy_poems_gen)
summary(gam_val_gen)

gam_aro <- gam(activation ~ s(grade, bs = 'cr') + total_words, data = tidy_poems_full)
summary(gam_aro)

gam.check(gam_aro)

gam_aro_gen <- gam(activation ~ s(grade, bs = 'cr') + 
                     s(grade, by = factor(gender), bs = 'cr') +
                     total_words, data = tidy_poems_gen)

summary(gam_aro_gen)

gam.check(gam_val_gen)

gam.check(gam_aro_gen)

#At least 25 words

tidy_poems_25 <- tidy_poems_full %>%
  filter(total_words > 24)

tidy_poems_gen25 <- tidy_poems_full %>%
  filter(!is.na(gender),
         total_words > 24)

gam_val25 <- gam(valence ~ s(grade, bs = 'cr') + total_words, family = gaussian, data = tidy_poems_25)
summary(gam_val25)

gam.check(gam_val25)

gam_val_gen25 <- gam(valence ~ s(grade, bs = 'cr') + 
                      s(grade, by = factor(gender), bs = 'cr') + 
                      total_words, data = tidy_poems_gen25)
summary(gam_val_gen25)
gam.check(gam_val_gen25)

gam_aro25 <- gam(activation ~ s(grade, bs = 'cr') + total_words, data = tidy_poems_25)
summary(gam_aro25)

gam.check(gam_aro25)

gam_aro_gen25 <- gam(activation ~ s(grade, bs = 'cr') + 
                      s(grade, by = factor(gender), bs = 'cr') +
                      total_words, data = tidy_poems_gen25)

summary(gam_aro_gen25)

gam.check(gam_aro_gen25)

gam_dom25 <- gam(dominance ~ s(grade, bs = 'cr') + total_words, data = tidy_poems_25)

summary(gam_dom25)

gam.check(gam_dom25)

gam_dom_gen25 <- gam(dominance ~ s(grade, bs = 'cr') +
                      s(grade, by = factor(gender), bs = 'cr') +
                      total_words, data = tidy_poems_gen25)

summary(gam_dom_gen25)

gam.check(gam_dom_gen25)

# Plotting with minimum 25

tidy_poems_full %>%
  filter(total_words > 24) %>%
  ggplot(aes(grade, valence)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ "Valence") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(1, 1))

tidy_poems_full %>%
  filter(total_words > 24) %>%
  ggplot(aes(grade, activation)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ "Arousal") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(1, 1))

tidy_poems_full %>%
  filter(total_words > 24) %>%
  ggplot(aes(grade, dominance)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ "Dominance") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(1, 1))

tidy_poems_full %>%
  filter(!is.na(gender),
         total_words > 24) %>%
  ggplot(aes(grade, valence, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ "Valence") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(1, 1))

tidy_poems_full %>%
  filter(!is.na(gender),
         total_words > 24) %>%
  ggplot(aes(grade, activation, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ "Arousal") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(1, 0.4))

tidy_poems_full %>%
  filter(!is.na(gender),
         total_words > 24) %>%
  ggplot(aes(grade, dominance, group = gender, color = gender, shape = gender)) +
  stat_summary(fun.data = mean_cl_normal, size = .8) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("#0033FF", "#FF6600")) +
  scale_x_discrete(name = "Grade", limits = c("1", "2", "3",
                                              "4", "5", "6",
                                              "7", "8", "9",
                                              "10", "11", "12")) +
  labs(x = "Grade", y = NULL, color = "", shape = "") +
  facet_wrap(~ "Dominance") +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 1), legend.position = c(1, 0.4))

#Sampling

sample <- sample_n(tidy_poems_5, 8435, weight = grade)

sample_gam_val <- gam(valence ~ s(grade, bs = 'cr') + total_words, data = sample)
summary(sample_gam_val)

sample_gam_aro <- gam(activation ~ s(grade, bs = 'cr') + total_words, data = sample)
summary(sample_gam_aro)

sample_gam_dom <- gam(dominance ~ s(grade, bs = 'cr') + total_words, data = sample)
summary(sample_gam_dom)

#VADER

vader <- read.delim("~/R/Sentiment Analysis/vader_lexicon.txt", header = FALSE) %>%
  select(-V3, -V4) %>%
  rename("word" = V1, "vader" = V2)

tidy_poems_vader <- tidy_poems_vad %>%
  inner_join(vader, by = "word")

tidy_poems_vader

tidy_poems_full_vader <- tidy_poems_vader %>%
  group_by(id, grade, author) %>%
  summarize(vader = round(mean(vader, na.rm = TRUE), 2),
            total_words = n()) %>%
  ungroup() %>%
  left_join(gender_new) %>%
  select(-c(male, female)) %>%
  rename("gender" = "conf_female")

tidy_poems_full_vader %>%
  filter(total_words > 4) %>%
  ggplot(aes(vader)) +
  geom_histogram()

gam_vader_dt <- tidy_poems_full_vader %>%
  filter(total_words > 4)

gam_vader <- gam(vader ~ s(grade, bs = 'cr') + total_words, data = gam_vader_dt)
summary(gam_vader)
plot(gam_vader)
gam.check(gam_vader)

#Bootstrapping

set.seed(190327)

bootstrap_val <- replicate(50, {
  sample <- sample_n(tidy_poems_5, 8000, weight = grade)
  gam <- summary(gam(valence ~ s(grade, bs = 'cr') + total_words, data = sample))
  print(gam$r.sq)
})

summary(bootstrap_val)

bootstrap_aro <- replicate(50, {
  sample <- sample_n(tidy_poems_5, 8000, weight = grade)
  gam <- summary(gam(activation ~ s(grade, bs = 'cr') + total_words, data = sample))
  print(gam$r.sq)
})

summary(bootstrap_aro)

bootstrap_dom <- replicate(50, {
  sample <- sample_n(tidy_poems_5, 8000, weight = grade)
  gam <- summary(gam(dominance ~ s(grade, bs = 'cr') + total_words, data = sample))
  print(gam$r.sq)
})

summary(bootstrap_dom)
