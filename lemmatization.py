import sys
import pandas as pd
from rpy2.robjects import r
import rpy2.robjects.pandas2ri as pandas2ri
pandas2ri.activate() #MANDATORY !
file = str(sys.argv[1]) #"~/govWebsites/rfiles/d.Rdata"
rf = r['load'](file)
d = r['d']
docs = d['doc']

import spacy
nlp = spacy.load('en')

processed_docs = []
for doc in nlp.pipe(docs, n_threads=11, batch_size=100):

	# Lemmatize tokens, remove punctuation (also spaces, which makes it look nicer later)
	doc = [token.lemma_ for token in doc if token.is_alpha]
	doc = ' '.join(doc)
	processed_docs.append(doc)

d.doc = processed_docs


import rpy2
from rpy2 import robjects
from rpy2.robjects import pandas2ri
pandas2ri.activate()

def save_rdata_file(df, filename):
    r_data = pandas2ri.py2ri(df)
    robjects.r.assign("d", r_data)
    robjects.r("save(d, file='{}')".format(filename))

save_rdata_file(d, str(sys.argv[1]))


