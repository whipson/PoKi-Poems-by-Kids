# PoKi: A Large Dataset of Poems by Children
PoKi is a corpus of 61,330 poems written by children from grades 1 to 12. PoKi is especially useful in studying child language because it comes with information about the age of the child authors (their grade). 

Poems were submitted online by children in school grades 1-12. Permissions were obtained from
domain host prior to scraping. Poems were webscraped on June, 2018 from http://teacher.scholastic.com/writewit/poetry/jack_readall.asp.

This repository includes three versions of PoKi and R analytic scripts for reproducing the results.

Details of the data are in this paper:  
PoKi: A Large Dataset of Poems by Children. Will E. Hipson, Saif M. Mohammad. arXiv preprint arXiv:2004.06188 . April 2020.

BibTex:  
@article{hipson2020poki,  
  title={PoKi: A Large Dataset of Poems by Children},  
  author={Hipson, W. E and Mohammad, S M.},  
  journal={arXiv preprint arXiv:2004.06188},  
  year={2020}  
}

## Terms of Use

PoKi is made freely available for research with the condition that the research be used for the benefit of children. Anyone who uses PoKi must cite this paper https://arxiv.org/abs/2004.06188 (see BibTex below for full citation). Should you intend to use PoKi in your research, please reach out to us (william.hipson@carleton.ca); we'd love to hear how you're using it!

## Overview of PoKi

The table below provides a brief glimpse of how the poems in PoKi are distributed by age (grade).

| Grade  | # of poems | Mean # of words per poem |
| ------------- | ------------- | ------------- | 
| 1  | 900  | 37.3  |
| 2  | 3174  | 32.1  |
| 3 | 6172  | 35.2  |
| 4  | 10899  | 39.3  |
| 5  | 11479  | 44.5  |
| 6  | 11011  | 49.6  |
| 7  | 7831  | 59.7  |
| 8  | 4546  | 67.6  |
| 9  | 1284  | 91.5  |
| 10 | 1171  | 91.8  |
| 11 | 667  | 103.0  |
| 12  | 1656  | 97.2  |
| All  | 61330  | 50.3  |

## Dataset Descriptions

### poki.csv

Description: Dataset containing the raw web-scraped poems from http://teacher.scholastic.com/writewit/poetry/jack_readall.asp. No text pre-processing at this point. Note that there may be a few extra poems beyond the number reported in the manuscript as some poems contain very little text and are eventually removed during the preprocessing steps.

Variables:  
id = unique identifier for each poem  
title = title of poem (submitted by poem author)  
author = first name of author  
grade = school grade (1-12)  
text = raw poem text  
char = number of characters  

### poki-lem.csv

Description: Lemmatized version of PoKi. We used lemmatization tools in the Natural Language Toolkit (http://www.nltk.org/) to lemmatize poem text (e.g., running, ran -> run | writing, wrote -> write). This dataset contains the same columns as in poki.csv, where 'text' now contains lemmatized text.

### poki-analysis.csv

Description: Data used for word-level analysis reported in Hipson and Mohammad (2020) PoKi: A Large Dataset of Poems by Children. We use the NRC VAD lexicon and the NRC Emotion Intensity lexicon.

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

