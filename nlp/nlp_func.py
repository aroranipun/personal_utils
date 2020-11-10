# -*- coding: utf-8 -*-
"""
Created on Mon Sep 16 15:23:39 2019

@author: nipun
"""

import datetime
import re
import numpy as np
import unicodedata
import spacy
import gensim
import pkg_resources
from symspellpy import SymSpell, Verbosity

nlp = spacy.load("en_core_web_sm")  # python -m spacy download en_core_web_sm

sym_spell = SymSpell(max_dictionary_edit_distance=2, prefix_length=7)
dictionary_path = pkg_resources.resource_filename(
    "symspellpy", "frequency_dictionary_en_82_765.txt")

sym_spell.load_dictionary(dictionary_path, term_index=0, count_index=1)

def spell_correction(text):
    text = text.split()
    corrected =[]
    for i in text:
        suggestions = sym_spell.lookup(i, Verbosity.CLOSEST,
                                   max_edit_distance=2, ignore_token=r"\w+\d")
        corrected.append(suggestions[0].term)
    return ' '.join(corrected)

def add_stop_word(words):
    for w in words:
        nlp.Defaults.stop_words.add(w)

def deaccent(text):
    if not isinstance(text, str):
        text = text.decode('utf8')
    norm = unicodedata.normalize("NFD", text)
    result = ''.join(ch for ch in norm if unicodedata.category(ch) != 'Mn')
    return unicodedata.normalize("NFC", result)

def normalize(texts, custom_stop_words=None,
              min_length=3, max_length=15,
              allowed_postags=None, disallowed_postags=None,
              spell_check=False,
              return_tokenized=True,
              min_doc=1):
    """
    Notes:
    Spacy's POS-tagging depends on capitalization so word lists like 'Grain Bids, Cash Bids, Corn Prices, Cattle Feed' will be tagged as PROPN
    and won't be lemmatized. For such text, convert to lowercase before calling  this function.
    For topic modelling on larger corpora consider using allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV']
    :param min_doc: min number of documents
    :param return_tokenized: Whether it should return a string or list of tokens
    :param allowed_postags: part of speech filtering
    :param min_length: int fpr min token length
    :type max_length: int
    :param custom_stop_words: list of custom stop words

    """
    now = datetime.datetime.now()

    if spell_check:
        texts = spell_correction(texts)

    # Add custom stop words-----------------------
    if custom_stop_words is None:
        custom_stop_words = []

    add_stop_word([x.lower() for x in custom_stop_words])

    # Convert to array-------------------
    if isinstance(texts, str):
        texts = np.array([texts], dtype=np.object)
    else:
        texts = np.asarray(texts, dtype=np.object).flatten()

    if len(texts) == 1:
        result = [nlp(texts[0], disable=['ner', 'parser'])]
    else:
        result = nlp.pipe(texts, batch_size=100, disable=['ner', 'parser'])

    docs = []
    doc_count = -1
    invalid_docs = []
    for doc in result:
        words = []
        for token in doc:
            # if re.search('[^A-Za-z0-9]+',str(token)):
            #  continue #removes non-english and numerical characters
            if token.is_stop:
                continue
            if token.lemma_ in nlp.Defaults.stop_words:
                continue
            if allowed_postags and (token.pos_ not in allowed_postags or token.pos_ in disallowed_postags):
                continue
            if len(token.lemma_) < min_length or len(token.lemma_) > max_length:
                continue
            words.append(token.lemma_)

        phrases = gensim.models.phrases.Phrases(words, min_count=5, threshold=10)
        bigram = gensim.models.phrases.Phraser(phrases)
        words = bigram[words]

        if len(words) < min_doc:
            invalid_docs.append(doc_count)
            continue

        if return_tokenized:
            docs.append(deaccent(' '.join(words).lower()).split())
        else:
            docs.append(deaccent(' '.join(words).lower()))

        if len(texts) == 1:
            docs = docs[0]
        # print(doc_count)
        doc_count = doc_count + 1

    print(f'Runtime of pre_processing query was: {(datetime.datetime.now() - now)}')
    return docs, invalid_docs
