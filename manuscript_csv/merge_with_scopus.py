import os
import pandas as pd

os.chdir('C:\\Users\\ynkar\\Desktop\\ophthalmology\\manuscript')

title_95 = pd.read_csv('cleaned_nlp_title_95percent_titleTag.csv')
abstract_95 = pd.read_csv('cleaned_nlp_abstract_95percent_abstractTag.csv')
df_scopus = pd.read_csv('dfscopus-2.csv')

merged_title_and_abstract = title_95.merge(abstract_95, on='index')
merged_title_and_abstract.to_csv('merged_title_and_abstract.csv')

df_scopus_2_with_title_and_abstract_manuscript = df_scopus.merge(merged_title_and_abstract, on='index')
df_scopus_2_with_title_and_abstract_manuscript.to_csv('dfscopus-2_with_title_and_abstract_manuscript.csv')
