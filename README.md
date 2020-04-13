# PoKi: A Large Dataset of Poems by Children
PoKi is a corpus of over 66,250 thousand poems written by children from grades 1 to 12. PoKi is especially useful in studying child language  because it comes with information about the age of the child authors (their grade). 

Poems were submitted online by children in school grades 1-12. Permissions were obtained from
domain host prior to scraping. Poems were webscraped on June, 2018 from http://teacher.scholastic.com/writewit/poetry/jack_readall.asp.

This repository includes two versions of PoKi and R analytic scripts for reproducing the results.

Dataset #1: poki

Description: Dataset containing the raw web-scraped poems from http://teacher.scholastic.com/writewit/poetry/jack_readall.asp. No text pre-processing at this point.

Variables:
id = unique identifier for each poem
title = title of poem (submitted by poem author)
author = first name of author
grade = school grade (1-12)
country = state or country of author
text = poem text
char = number of characters


Dataset #2: poki-analysis

Description: Clean, processed version of Children's Poems Corpus ready for analysis.

Variables:
id = unique identifier for each poem
grade = school grade (1-12)
author = first name of author
\_sd = within poem standard deviation for emotion tokens
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

