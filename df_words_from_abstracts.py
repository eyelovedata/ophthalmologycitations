# analogous to df_formatting.py, this file tokenizes all words from all abstracts of the papers listed

import os
import pandas as pd
import nltk

os.chdir('C:\\Users\\ynkar\\Desktop\\ophthalmology')

all_scopus = pd.read_csv('all_scopus_with_authorNum&paperLength.csv')
pd.set_option('display.max_columns', None)

# to-do: abstract, author keywords, index keywords, source title
all_scopus.drop(all_scopus.columns.difference(['title', 'abstract', 'author keywords', 'cited by',
                                             'year', 'funding details', 'index keywords', 'source title',
                                             'author number', 'paper length in pages'
                                             ]), 1, inplace=True)

# word tokenize - get all words and see which are more common, without rake (just nltk library)
def word_tokenize_whole_text(text):
    total_phrases = []

    for line in text:
        line_of_words = nltk.word_tokenize(line)
        total_phrases.append(line_of_words)

    return total_phrases

# title (and title subset) is the df that allows us to iterate through each paper
abstract = all_scopus['abstract']

words_from_all_abstracts = word_tokenize_whole_text(abstract)

# remove word if the word is a number
# https://stackoverflow.com/questions/1265665/how-can-i-check-if-a-string-represents-an-int-without-using-try-except
def RepresentsInt(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

master_list_all_words = []

# make list of sublists into one list, no repeats
for list in words_from_all_abstracts:
    for word in list:
        if word not in master_list_all_words:
            master_list_all_words.append(word)

# clean up words
# master_list_all_words is simply an array of words, can iterate through with a simple for loop
for word in master_list_all_words:
    if word.lower() == 'a' or word.lower() == 'an' or word.lower() == 'the': # articles
        master_list_all_words.remove(word)
    elif word.lower() == 'and' or word.lower() == 'or' or word.lower() == 'for' or word.lower() == 'from': # some conjunctions
        master_list_all_words.remove(word)
    elif word.lower() == 'if' or word.lower() == 'of' or word.lower() == 'in': # more common uninformative words
        master_list_all_words.remove(word)
    elif RepresentsInt(word): # number
        master_list_all_words.remove(word)
    elif not word.isalpha():
        master_list_all_words.remove(word)

print(master_list_all_words)

# same procedure as df_formatting.py
words_found = []

for word in master_list_all_words:
    for row in title:
        if word.lower() in row.lower():
            words_found.append(1)
        elif word.lower() not in row.lower():
            words_found.append(0)

split_words = []

max_range = len(title)
for i in range(0, max_range ** 2):
    if i % max_range == 0:
        list_range = words_found[i:i+max_range]
        split_words.append(list_range)

for i in range(max_range):
    all_scopus[master_list_all_words[i]] = split_words[i]

print(all_scopus)
all_scopus.to_csv('all_nlp_words_from_abstract.csv')
