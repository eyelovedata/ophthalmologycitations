# this program is for finding variable importance for specific glaucoma keywords - in Python
# it uses dfscopus-2 ... _manuscript.csv, same as fork_for_glaucoma.R

import os
import pandas as pd

os.chdir('C:\\Users\\veena\\Desktop\\ophthalmology\\csv')
csv_superset_df = pd.read_csv("dfscopus-2_with_title_and_abstract_manuscript.csv") # most recent

# get rid of any NA columns - those won't go in the model
# https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.dropna.html
csv_superset_df = csv_superset_df.dropna(axis='columns')

# glaucoma, retina, uveitis, oculoplastics, cornea, neuroophthalmology
# find these words for the glaucoma-specific subset
glaucoma_words = ['glaucoma', 'retina', 'uveitis', 'oculoplastics', 'cornea', 'neuro']

glaucoma_subset_list = []
glaucoma_subset_factored_out_underscores = []

# iterate through all the df columns
# https://stackoverflow.com/questions/28218698/how-to-iterate-over-columns-of-pandas-dataframe-to-run-regression

for column in csv_superset_df:
    for word in glaucoma_words:
        if word in column:
            glaucoma_subset_list.append(column)

for item in glaucoma_subset_list:
    if '_x' in item or '_y' in item:
        pass
    else:
        glaucoma_subset_factored_out_underscores.append(item)

# these are the words R should look for
# minus journal of ... because some tc values are missing
print(glaucoma_subset_factored_out_underscores)
