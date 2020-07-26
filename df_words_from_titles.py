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
title = all_scopus['title']

words_from_all_titles = word_tokenize_whole_text(title)
print(words_from_all_titles)

# remove word if the word is a number
# https://stackoverflow.com/questions/1265665/how-can-i-check-if-a-string-represents-an-int-without-using-try-except
def RepresentsInt(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

print(words_from_all_titles)

master_list_all_words = []

# make list of sublists into one list, no repeats
for list in words_from_all_titles:
    for word in list:
        if word not in master_list_all_words:
            master_list_all_words.append(word)

# clean up words
for list in master_list_all_words:
    if word.lower() == 'a' or word.lower() == 'an' or word.lower() == 'the': # articles
        list.remove(word)
    elif word.lower() == 'and' or word.lower() == 'or' or word.lower() == 'for' or word.lower() == 'from': # some conjunctions
        list.remove(word)
    elif word.lower() == 'if' or word.lower() == 'of' or word.lower() == 'in': # more common uninformative words
        list.remove(word)
    elif RepresentsInt(word): # number
        list.remove(word)
    elif not word.isalpha():
        list.remove(word)

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
all_scopus.to_csv('all_nlp_words.csv')
