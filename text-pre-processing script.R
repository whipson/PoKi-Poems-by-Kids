#Text pre-processing of raw poems. Please cite: 

#Hipson, W. E., & Mohammad, S. M. (2019). Computational Analysis of Child and Adolescent Poetry.

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

#Running all the code above produces poems_full.csv (https://raw.githubusercontent.com/whipson/Childrens_Poems/master/poems_full.csv)