# PoKi: A Large Dataset of Poems by Children
PoKi is a corpus of over 66,250 thousand poems written by children from grades 1 to 12. PoKi is especially useful in studying child language because it comes with information about the age of the child authors (their grade). PoKi is made freely available for research with the condition that the research be used for the benefit of children. Anyone who uses PoKi must cite this paper (see BibTex below).

Poems were submitted online by children in school grades 1-12. Permissions were obtained from
domain host prior to scraping. Poems were webscraped on June, 2018 from http://teacher.scholastic.com/writewit/poetry/jack_readall.asp.

This repository includes two versions of PoKi and R analytic scripts for reproducing the results.

Details of the data are in this paper:
PoKi: A Large Dataset of Poems by Children. Will E. Hipson, Saif M. Mohammad. arXiv preprint arXiv: . April 2020.

BibTex:
@article{hipson2020poki,
  title={PoKi: A Large Dataset of Poems by Children},
  author={Hipson, W. E and Mohammad, S M.},
  journal={arXiv preprint arXiv:},
  year={2020}
}

## Overview of PoKi

The table below provides a brief glimpse of how the poems in PoKi are distributed by age (grade).

| Grade  | # of poems | Mean # of words per poem |
| ------------- | ------------- | ------------- | 
| 1  | Content Cell  | Content Cell  |
| 2  | Content Cell  | Content Cell  |
| 3 | Content Cell  | Content Cell  |
| 4  | Content Cell  | Content Cell  |
| 5  | Content Cell  | Content Cell  |
| 6  | Content Cell  | Content Cell  |
| 7  | Content Cell  | Content Cell  |
| 8  | Content Cell  | Content Cell  |
| 9  | Content Cell  | Content Cell  |
| 10 | Content Cell  | Content Cell  |
| 11 | Content Cell  | Content Cell  |
| 12  | Content Cell  | Content Cell  |

## Dataset Descriptions

Dataset #1: poki

Description: Dataset containing the raw web-scraped poems from http://teacher.scholastic.com/writewit/poetry/jack_readall.asp. No text pre-processing at this point.

Variables:
id = unique identifier for each poem;  
title = title of poem (submitted by poem author);
author = first name of author;
grade = school grade (1-12);
country = state or country of author;
text = poem text;
char = number of characters;


Dataset #2: poki-analysis

Description: Clean, processed version of Children's Poems Corpus ready for analysis.

Variables:
id = unique identifier for each poem;
grade = school grade (1-12);
author = first name of author;
\_sd = within poem standard deviation for emotion tokens;
valence = mean valence;
arousal = mean arousal;
dominance = mean dominance;
anger = mean anger;
fear = mean fear;
sadness = mean sadness;
joy = mean joy;
total_words = total number of non stop words within poem;
gender = probable gender based on author name;

---

