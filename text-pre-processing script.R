library(tidyverse)
library(stringr)
library(dplyr)

#Importing and Cleaning the Data

poems_raw <- read_csv("childrens_poem_raw.csv")

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

#Tokenization

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

#Running all the code above produces poems_full.csv 
#(https://raw.githubusercontent.com/whipson/Childrens_Poems/master/poems_full.csv)

#Additional Analyses

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
  group_by(word) %>%
  mutate(n_word = n()) %>%
  distinct(.) %>%
  ungroup() %>%
  spread(gender, prop, convert = TRUE) %>%
  mutate(Female = ifelse(is.na(Female), 0, Female),
         Male = ifelse(is.na(Male), 0, Male)) %>%
  mutate(diff = Female - Male,
         Male = scales::percent(Male),
         Female = scales::percent(Female))

plots <- words_prop_gender %>%
  arrange(desc(abs(diff))) %>%
  inner_join(valence) %>%
  inner_join(arousal) %>%
  inner_join(dominance) %>%
  left_join(AIL_wide)

hi_val <- plots %>%
  filter(valence > .8) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "High Valence (> 0.8)") +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000FF", limits = c(-.0025, .0025), na.value = "#0000FF") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(valence, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5))

hi_val

lo_val <- plots %>%
  filter(valence < .2) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "Low Valence (< 0.2)") +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000FF", limits = c(-.001, .001), na.value = "#0000FF") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(valence, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5))

lo_val

hi_aro <- plots %>%
  filter(arousal > .8) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "High Arousal (> 0.8)",
       fill = NULL) +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000FF", limits = c(-.00035, .0015), breaks = c(-.00035, .0015), na.value = "#FF6600") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(arousal, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5),
        legend.position = "none")

hi_aro

lo_aro <- plots %>%
  filter(arousal < .2) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "Low Arousal (< 0.2)",
       fill = NULL) +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000FF", limits = c(-.00035, .00082), breaks = c(-.00035, .0015), na.value = "#FF6600") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(arousal, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5),
        legend.position = "none")

lo_aro

hi_dom <- plots %>%
  filter(dominance > .8) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "High Dominance (> 0.8)") +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000ff", limits = c(-.00083, .0009), na.value = "#FF6600") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(dominance, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5))

hi_dom

lo_dom <- plots %>%
  filter(dominance < .2) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "Low Dominance (< 0.2)") +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000ff", limits = c(-.00033, .00033), na.value = "#FF6600") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(dominance, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5))

lo_dom

anger_hi <- plots %>%
  filter(anger > .8) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "High Anger Intensity (> 0.8)") +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000ff", limits = c(-.00009, .00009), na.value = "#FF6600") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(anger, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5))

anger_hi

sadness_hi <- plots %>%
  filter(sadness > .8) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "High Sadness Intensity (> 0.8)") +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000ff", limits = c(-.0003, .0002), na.value = "#0000FF") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(sadness, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5))

sadness_hi

fear_hi <- plots %>%
  filter(fear > .8) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "High Fear Intensity (> 0.8)") +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000ff", limits = c(-.0003, .0002), na.value = "#0000FF") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(fear, 2)), position = position_stack(vjust = .5), size = 4, color = "white") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5))

fear_hi

joy_hi <- plots %>%
  filter(joy > .8) %>%
  top_n(15, abs(diff)) %>%
  ggplot(aes(x = reorder(word, desc(diff)), y = diff, fill = diff)) +
  geom_col() +
  labs(x = NULL,
       y = NULL,
       title = "High Joy Intensity (> 0.8)") +
  scale_fill_gradient2(low = "#FF6600", mid = "#8C8C8C", high = "#0000ff", limits = c(-.0003, .0002), na.value = "#0000FF") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  geom_text(aes(label = round(joy, 2)), position = position_nudge(y = .0005), size = 4.5, color = "black") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        plot.title = element_text(hjust = .5))

joy_hi

library(ggpubr)

VAD_plot <- ggarrange(hi_val, lo_val, hi_aro, lo_aro, hi_dom, lo_dom,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3)

VAD_plot

EI_plot <- ggarrange(anger_hi, sadness_hi, fear_hi, joy_hi,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

EI_plot

library(formattable)

sample_poem <- words_emotion %>%
  filter(id == 99545) %>%
  select(word, valence, arousal, dominance, anger, fear, sadness, joy) %>%
  group_by(word) %>%
  mutate('# of occurrences' = n()) %>%
  ungroup()

formattable(head(sample_poem),
            align =c("l","r","r","r","r", "r", "r", "r", "r"),
            list(`word` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 `valence` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                 `arousal` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                 `dominance` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                 `anger` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                 `fear`= formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                 `sadness` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                 `joy` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                 `# of occurrences` = formatter("span", style = ~ style(color = "black", font.weight = "bold"))))


