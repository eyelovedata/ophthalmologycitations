import os
import pandas as pd

os.chdir('C:\\Users\\veena\\Downloads')

all_scopus = pd.read_csv('all_scopus_with_authorNum&paperLength.csv')

master_df = all_scopus
subset_df = master_df # useful for creating a subset of the data to test df formatting later on

# to-do: abstract, author keywords, index keywords, source title
subset_df.drop(subset_df.columns.difference(['title', 'abstract', 'author keywords', 'cited by',
                                             'year', 'funding details', 'index keywords', 'source title',
                                             'author number', 'paper length in pages'
                                             ]), 1, inplace=True)

from rake_nltk import Rake, Metric

# https://pypi.org/project/rake-nltk/

# change max lengths depending on specific scope of RAKE analysis
    # find most common 'words' - i.e. length = 1
    # find most common 'phrases' - i.e. length > 1

# use Metric degree_to_frequency_ratio
    # we want common words while filtering out articles (the, an, a) and other uninformative terms

def get_all_keywords(text, word_boolean):

    if word_boolean: # word - i.e. length should be exactly 1
        r = Rake(ranking_metric=Metric.DEGREE_TO_FREQUENCY_RATIO, max_length=1)
    else: # phrase, i.e. made up of multiple words
        r = Rake(ranking_metric=Metric.DEGREE_TO_FREQUENCY_RATIO, min_length=2)

    total_phrases = []

    for local_text in text:
        r.extract_keywords_from_text(local_text) # use the Rake instance to find the keywords
        local_phrases = r.get_ranked_phrases_with_scores() # apply these keywords to a local variable
        total_phrases.append(local_phrases)

    all_keywords = []

    # get_ranked_phrases_with_scores gives a list of tuples - tuple[0] is score, 1 is word
    for list in total_phrases:
        for tuple in list:
            all_keywords.append(tuple[1])

    # clean some of the keywords - don't want a single number or small/uninformative words
    # amount goes from 200 keywords to 186 - len(df.columns) = 186, 1 column per keyword
    for word in all_keywords:
        if word.isdigit(): # remove numbers
            all_keywords.remove(word)
        elif len(word) < 5: # remove very short words
            all_keywords.remove(word)

    print(all_keywords)
    return all_keywords

# https://www.codespeedy.com/check-if-a-given-string-is-nan-in-python/
# use is_nan to strip away all rows which have NaN author keywords

def is_nan(string):
    return string != string # if true, value is NaN

# https://stackoverflow.com/questions/39068682/python-check-if-a-string-contains-multiple-words
# check if word or phrase, which is a boolean built into the function

def contains_multiple_words(string):
    return len(string.split()) > 1 # if true, the array has more than one element, i.e. more than one word

def top_n_author_keywords_or_phrases(keyword_text, word_boolean, n):

    # https://www.geeksforgeeks.org/python-string-split/

    master_list = []
    for line in keyword_text:
        if not is_nan(line): # if the line can be parsed
            local_element = line.split(';') # split the line based on the demarcation of ;
            master_list.append(local_element) # take each element a; b; c; d and append it [a, b, c, d]

    author_keywords_dict = {} # find frequencies of each element for all elements
    for list in master_list:
        for sublist in list:
            if sublist not in author_keywords_dict.keys():
                author_keywords_dict[sublist] = 1
            elif sublist in author_keywords_dict.keys():
                author_keywords_dict[sublist] += 1

    # https://stackoverflow.com/questions/613183/how-do-i-sort-a-dictionary-by-value
    author_keywords_dict = {k: v for k, v in
                                sorted(author_keywords_dict.items(), key=lambda item: item[1], reverse=True)}

    # same procedure as RAKE analysis
    sum_of_keyword_counts = 0
    for k, v in author_keywords_dict.items():
        sum_of_keyword_counts += v

    proportions = {}
    proportion_percentages = {}
    for k, v in author_keywords_dict.items():
        proportions[k] = (v / sum_of_keyword_counts)
        proportion_percentages[k] = proportions[k] * 100

    author_keywords = []
    author_keyword_labels = []

    author_key_phrases = []
    author_key_phrase_labels = []

    if word_boolean:
        for k, v in proportion_percentages.items():
            if not contains_multiple_words(k):
                k = k.lower() # ensure lowercase to get rid of case sensitivity concerns; 'oph' = 'Oph'...
                if k not in author_keyword_labels: # ensure no repetition of keywords; must be distinct in all ways
                    author_keyword_labels.append(k) # keyword
                    author_keywords.append(v) # frequency of keyword
    else:
        for k, v in proportion_percentages.items():
            if contains_multiple_words(k):
                k = k.lower()
                if k not in author_key_phrase_labels:
                    author_key_phrase_labels.append(k)
                    author_key_phrases.append(v)

    words = []
    phrases = []

    # use counters to ensure that we only receive the top n keywords, as supplied to the function
    word_count = 0
    phrase_count = 0

    for k, v in author_keywords_dict.items():
        if word_boolean:
            if not contains_multiple_words(k):
                # https://stackoverflow.com/questions/1801668/convert-a-python-list-with-strings-all-to-lowercase-or-uppercase
                words = [word.lower() for word in words] # list comprehension to eliminate case sensitivity

                if k.lower() in words:
                    pass # if the key is in the array already, or if a case-sensitive version is already, move on
                elif k.lower() not in words:
                    words.append(k.lower())
                    word_count += 1

                if word_count == n: # we have all the information we need
                    break

        elif not word_boolean:
            if contains_multiple_words(k):
                phrases = [phrase.lower() for phrase in phrases] # same list comprehension

                if k.lower() in phrases:
                    pass
                elif k.lower() not in phrases:
                    phrases.append(k.lower())
                    phrase_count += 1

                if phrase_count == n:
                    break

    # now that we have the top n words for any n, select only the words we need
    if word_boolean:
        del author_keywords[n:] # e.g. if n=5, then [5:] takes away all but elements 0-4
        del author_keyword_labels[n:]
        return author_keyword_labels # we care about the labels of the proportions (i.e. the words), not the proportions

    elif not word_boolean:
        del author_key_phrases[n:]
        del author_key_phrase_labels[n:]
        return author_key_phrase_labels # same as word_boolean branch - we want keywords, not proportions of keywords

# title (and title subset) is the df that allows us to iterate through each paper
title = subset_df['title']
all_key_words = get_all_keywords(title, word_boolean=True)
all_key_phrases = get_all_keywords(title, word_boolean=False)

# also add author and index keywords as their own boolean columns
author_keywords = subset_df['author keywords']
index_keywords = subset_df['index keywords']

# can dynamically change the top n keyword amounts
n1 = 10
n2 = 10

top_ten_author_keywords = top_n_author_keywords_or_phrases(keyword_text=author_keywords, word_boolean=True, n=n1)
top_ten_index_keywords = top_n_author_keywords_or_phrases(keyword_text=index_keywords, word_boolean=True, n=n2)
top_ten_author_and_index_keywords = top_ten_author_keywords + top_ten_index_keywords

max_index = 100
total_index = 200

# test a subset of ideal dataframe, with 100 words
all_key_words_subset = all_key_words[0:max_index] # slice of first 100 words
all_key_phrases_subset = all_key_phrases[0:max_index] # slice of first 100 phrases

# add together all data - title, abstract, keywords
all_key_data_subset = all_key_words_subset + all_key_phrases_subset + top_ten_author_and_index_keywords

total_index = total_index + n1 + n2 # necessary to add together len of df before assigning it in slice notation
subset_df_lenSubset = subset_df[0:total_index] # resize the df to match our test slice, and add top 10 author/index keywords

words_found = []

# look at all titles for each word for all words
for word in all_key_data_subset:
    for row in subset_df_lenSubset['title']:
        if word.lower() in row.lower(): # similar to Java ignoreCase(), works for our purposes
            words_found.append(1)
        elif word.lower() not in row.lower():
            words_found.append(0)

master_list = []
# simple enough to programmatically change, 100^2 -> len(data size) and 100 -> factor of data size (have to find equal groups)
for i in range(0, total_index**2):
    if i % total_index == 0:
        list = words_found[i:i+total_index] # i.e. list 1 is indices 0 to 100, list 2 is 101 to 200, etc.
        master_list.append(list)

print('Master list length', len(master_list))
print('Data frame subset length', len(subset_df_lenSubset))
print('Length of all data', len(all_key_data_subset))
print('\nMaster list', master_list)

pd.set_option('display.max_columns', None)

# for every paper in the whole df, add a new column for EACH keyword
# then, each column is assigned its respective boolean array (is the term of the column present or not in EACH paper?)
for i in range(len(subset_df_lenSubset)):
    subset_df_lenSubset[all_key_data_subset[i]] = master_list[i]

# add boolean columns that are not keyword columns (e.g. NIH funding)
funding_details = subset_df['funding details']
funding_details = funding_details[0:total_index]
funding_details_bool_array = []

for detail in funding_details:
    # check if object is a string
    # https: // stackoverflow.com / questions / 1303243 / how - to - find - out - if -a - python - object - is -a - string
    if isinstance(detail, str): # if the line is a string
        funding_details_bool_array.append(1)
    else:
        funding_details_bool_array.append(0)

subset_df_lenSubset['paper funding boolean'] = funding_details_bool_array

# add non-boolean columns (e.g. year)
year = subset_df['year']
subset_df_lenSubset['year published'] = year[0:total_index]

author_number = subset_df['author number']
subset_df_lenSubset['total author number'] = author_number[0:total_index]

page_count = subset_df_lenSubset['paper length in pages']
subset_df_lenSubset['page count'] = page_count[0:total_index]

# author and index keywords, as well as title and abstract, are above - source title can simply be appended
source_title = subset_df_lenSubset['source title']
subset_df_lenSubset['title of source'] = source_title[0:total_index]

# data frame
print(subset_df_lenSubset)

# test out variance threshold
from sklearn.feature_selection import VarianceThreshold
import numpy as np

selector = VarianceThreshold(.99 * (1 - .99)) # 99% non-zero

# https://stackoverflow.com/questions/34923728/type-error-unhashable-type-list-while-selecting-subset-from-specific-columns
# need to coerce the list to tuple so that pandas can hash the subset
# https://stackoverflow.com/questions/19371358/python-typeerror-unhashable-type-list

# create a fork of the variable to experiment with non-zero variance
wide = subset_df_lenSubset
wide.drop(wide.columns.difference(tuple(all_key_data_subset)), 1, inplace=True)

selector.fit_transform(np.array(wide)).shape

def variance_threshold_selector(data, threshold=0.5):
    selector = VarianceThreshold(threshold)
    selector.fit(data)
    return data[data.columns[selector.get_support(indices=True)]]

dfmedsfiltered_99_percent = variance_threshold_selector(wide, .99 * (1 - .99))
dfmedsfiltered_97_percent = variance_threshold_selector(wide, .97 * (1 - .97))
dfmedsfiltered_95_percent = variance_threshold_selector(wide, .95 * (1 - .95))

# For setting threshold for Boolean variables:
# if you want to remove variables which are 0 or 1 for more than 99% of observations,
# threshold=0.99*(1-0.99) assuming it’s a Bernoulli random variable

# without filter: 215 keyword columns
# with 99% filter: 47
# with 97% filter: 19
# with 95% filter: 8
print(dfmedsfiltered_99_percent)
print(dfmedsfiltered_97_percent)
print(dfmedsfiltered_95_percent)
