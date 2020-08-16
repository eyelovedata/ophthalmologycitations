# test out variance threshold
from sklearn.feature_selection import VarianceThreshold
import pandas as pd

import os
os.chdir('C:\\Users\\ynkar\\Desktop\\ophthalmology')

selector = VarianceThreshold(.99 * (1 - .99)) # 99% non-zero

df_scopus_2 = pd.read_csv('dfscopus-2.csv')

nlp_title = pd.read_csv('all_nlp_words_from_title.csv')
nlp_abstract = pd.read_csv('all_nlp_words_from_abstract.csv')

# there is a 1-1 bijection between papers in dfscopus-2.csv and nlp_title.csv and nlp_abstract.csv
# so, drop columns in nlp_*.csv that dfscopus-2.csv has, then add just the keyword columns to dfscopus-2.csv
# as a result, there are only 0/1 boolean columns for each tokenized word in each title & abstract respectively
nlp_title = nlp_title.drop(columns=['title', 'year', 'source title', 'cited by',
                        'abstract', 'author keywords', 'index keywords',
                        'funding details', 'author number',
                        'paper length in pages'])

nlp_abstract = nlp_abstract.drop(columns=['title', 'year', 'source title', 'cited by',
                        'abstract', 'author keywords', 'index keywords',
                        'funding details', 'author number',
                        'paper length in pages'])


print(len(nlp_title))
print(len(nlp_abstract))

def variance_threshold_selector(data, threshold=0.5):
    selector = VarianceThreshold(threshold)
    selector.fit(data)
    return data[data.columns[selector.get_support(indices=True)]]

nlp_title_filtered_95_percent = variance_threshold_selector(nlp_title, .95 * (1 - .95))
nlp_abstract_filtered_95_percent = variance_threshold_selector(nlp_abstract, .95 * (1 - .95))

nlp_title_filtered_95_percent.to_csv('nlp_words_from_title_95percent_filtered.csv')
nlp_abstract_filtered_95_percent.to_csv('nlp_words_from_abstract_95percent_filtered.csv')

nlp_title_95percent = pd.read_csv('nlp_words_from_title_95percent_filtered.csv')
nlp_abstract_95percent = pd.read_csv('nlp_words_from_abstract_95percent_filtered.csv')

dfscopus_2_csv = pd.read_csv('dfscopus-2.csv')

def clean_df_columns(df):
    for column in df.columns:
        if len(column) < 4:
            del df[column]
        elif not column.isalpha():
            del df[column]

print(nlp_title_95percent)
clean_df_columns(nlp_title_95percent)
print(nlp_title_95percent)

print(nlp_abstract_95percent)
clean_df_columns(nlp_abstract_95percent)
print(nlp_abstract_95percent)

#nlp_title_95percent.to_csv('cleaned_nlp_title_95percent.csv')
#nlp_abstract_95percent.to_csv('cleaned_nlp_abstract_95percent.csv')

# https://stackoverflow.com/questions/13411544/delete-column-from-pandas-dataframe
# https://stackoverflow.com/questions/16265831/merging-two-csv-files-using-python
# https://stackoverflow.com/questions/12168648/pandas-python-how-to-add-column-to-dataframe-for-index
cleaned_title = pd.read_csv('cleaned_nlp_title_95percent.csv')
cleaned_title['index'] = range(0, len(cleaned_title))
cleaned_abstract = pd.read_csv('cleaned_nlp_abstract_95percent.csv')
cleaned_abstract['index'] = range(0, len(cleaned_abstract))

# append nlp title/abstract tokenized & cleaned words to dfscopus-2.csv
# and merge the two csv files
merged_with_title = dfscopus_2_csv.merge(cleaned_title, on='index')
#merged_with_title.to_csv('dfscopus-2_with_nlp_title.csv')
merged_with_title_and_abstract = merged_with_title.merge(cleaned_abstract, on='index')
#merged_with_title_and_abstract.to_csv('dfscopus-2_with_nlp_title_and_abstract.csv')
