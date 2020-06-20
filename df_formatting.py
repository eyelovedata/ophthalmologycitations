import os
import pandas as pd

os.chdir('C:\\Users\\veena\\Downloads')

all_scopus = pd.read_csv('all_scopus_with_authorNum&paperLength.csv')

master_df = all_scopus
subset_df = master_df # useful for creating a subset of the data to test df formatting later on
subset_df.drop(subset_df.columns.difference(['title', 'abstract', 'author keywords', 'cited by']), 1, inplace=True)

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
        local_keywords = r.extract_keywords_from_text(local_text) # necessary, even though local_keywords is never used
        local_phrases = r.get_ranked_phrases_with_scores()
        total_phrases.append(local_phrases)

    all_keywords = []

    for list in total_phrases:
        for tuple in list:
            all_keywords.append(tuple[1])

    # clean some of the keywords - don't want a single number or small/uninformative words
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

def top_n_author_keywords_or_phrases(author_keywords, word_boolean, n):

    # https://www.geeksforgeeks.org/python-string-split/

    master_list = []
    for line in author_keywords:
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

    elif not word_boolean:
        del author_key_phrases[n:]
        del author_key_phrase_labels[n:]

title = subset_df['title']
all_key_words = get_all_keywords(title, word_boolean=True)

#all_key_phrases = get_all_keywords(title, word_boolean=False) # will do later

# test a small subset of ideal dataframe, with 9 words
all_key_words_subset = all_key_words[0:9] # slice of first 9 words
subset_df_lenSubset = subset_df[0:9] # resize the df to match our test slice

words_found = []
# look at all titles for each word for all words
for word in all_key_words_subset:
    for row in subset_df_lenSubset['title']:
        if word.lower() in row.lower(): # similar to Java ignoreCase(), works for our purposes
            words_found.append(1)
        elif word.lower() not in row.lower():
            words_found.append(0)

master_list = []
# simple enough to programmatically change, 81 -> len(data size) and 9 -> factor of data size (have to find equal groups)
for i in range(0, 81):
    # put every batch of n words together
    if i % 9 == 0:
        list = words_found[i:i+9]
        master_list.append(list)

print(master_list)

pd.set_option('display.max_columns', None)

for i in range(len(subset_df_lenSubset)): # for every element in the df
    # the df adds a new column as pointed to by keyword, then adds a column body from word batches earlier
    subset_df_lenSubset[all_key_words_subset[i]] = master_list[i]

print(subset_df_lenSubset)
