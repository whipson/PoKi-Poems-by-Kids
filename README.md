# Childrens_Poems
Raw and cleaned versions of Poems by Kids (PoKi) corpus.

Corpus is 66,250 poems submitted online by children in school grades 1-12. Permissions were obtained from
domain host prior to scraping. Poems were webscraped on June, 2018 from http://teacher.scholastic.com/writewit/poetry/jack_readall.asp.

Dataset #1: childrens_poems_raw

Description: Dataset containing the raw web-scraped poems from http://teacher.scholastic.com/writewit/poetry/jack_readall.asp. No text pre-processing at this point.

Variables:
id = unique identifier for each poem
title = title of poem (submitted by poem author)
author = first name of author
grade = school grade (1-12)
country = state or country of author
text = poem text
char = number of characters


Dataset #2: poems_full

Description: Clean, processed version of Children's Poems Corpus ready for analysis.

Variables:
id = unique identifier for each poem
grade = school grade (1-12)
author = first name of author
*_sd = within poem standard deviation for emotion tokens
valence = mean valence
arousal = mean arousal
dominance = mean dominance
anger = mean anger
fear = mean fear
sadness = mean sadness
joy = mean joy
total_words = total number of non stop words within poem
gender = probable gender based on author name

---

