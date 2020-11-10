# -*- coding: utf-8 -*-

# Resources
# https://www.geeksforgeeks.org/nlp-synsets-for-a-word-in-wordnet/

import re
import nltk
from nltk.tokenize import word_tokenize
from nltk.corpus import wordnet
import spacy
DEFAULT_NLP = spacy.load('en_core_web_md')
DEFAULT_STOPWORDS = DEFAULT_NLP.Defaults.stop_words

def word_attr(word, print_results = False, limit = 5):

    # Test _Code
    # word_attr(word = "punch", print_results = True, limit = 5)

    output = []
    for i, syn in enumerate(wordnet.synsets(word),1):

        defnition = syn.definition()
        Synonyms = []
        Hyponyms = []
        Hypernyms = []

        for l in syn.lemmas():  # Get Synonyms
            Synonyms.append(l.name())

        for l in syn.hypernyms(): # More abstract terms around the particular definition
            Hypernyms.append(l.lemma_names())

        for l in syn.hyponyms(): # more specific words of ththat nature
            Hyponyms.append(l.lemma_names())

        if print_results:
            print(f"Definition {i}. {syn.definition()}")
            print(f"Synonyms are {Synonyms}"  )
            print(f"Hypernyms are {Hypernyms}"  )
            print(f"Hyponyms are {Hyponyms}"  )

        Hyponyms = [items for sublist in Hyponyms for items in sublist]
        out_i= {
            "Definition" : syn.definition(),
            "Synonyms"  : Synonyms,
            "Hypernyms" : Hypernyms,
            "Hyponyms"  : Hyponyms
            }
        output.append(out_i)
        if i == limit: break

    return(output)

def get_key_words(sentence,explained = False):
    # this function takes in a sentence and outputs a list of key words
    # explained = True is used to troubleshoot the process. It results
    #                  in a breakdown of all the subsentences to be printed with some of the analysis

    nlp = DEFAULT_NLP
    stopwords = DEFAULT_STOPWORDS

    # define a function that flattens a list of lists of items into one list of items
    def flatten(l):
        return [item for sublist in l for item in sublist]

    # returns a list of unique items in a list
    def unique(l):
        output = []
        for x in l:
            if x not in output:
                output.append(x)
        return(output)

    # adds an oxford comma to a sentence
    # this function does so by finding the longest list of conj tokens
    # and adds a comma between the last two tokens in the list only if the list is larger than
    # two tokens
    def add_oxford_comma(sentence):
        max_list_size = 0
        max_list = []
        nlp_sent = nlp(sentence)
        # find all tokens that have a "conj" dependancy
        w_list = [w for w in nlp_sent if w.dep_ == "conj"]
        w_list.reverse()
        for w in w_list:
            len_list = 0
            w_list = []
            w_temp = w
            # find all of the items that have the same dependancy
            while (w_temp.dep_=="conj"):
                len_list += 1
                w_list.append(w_temp)
                w_temp = w_temp.head
            w_list.append(w_temp)
            # overwrite the largest list to determine the longest list of conj's
            if len_list > max_list_size:
                max_list_size = len_list
                max_list = w_list
        # if the list is larger than 2 items add a comma
        if(len(max_list)>2):
            # find "or" or "and"
            for t in max_list[1].children:
                if t.dep_ == 'cc':
                    sentence = nlp_sent[:t.i].text+', '+nlp_sent[t.i:].text
        return sentence


    phrases = []

    sentence = add_oxford_comma(sentence)
    subsents = sentence.split(',')
    subsents = re.split(',|\n',sentence)

    all_verbs = []

    # process each subsentence alone
    for subsent in subsents:
        # remove all extra numbers and characters
        clean_subsent = nlp(re.sub(r"[0-9]+","",subsent.lower()))
        # find nouns
        nouns = [w.text for w in clean_subsent if (w.pos_ == 'NOUN' or (w.pos_ == 'PROPN' ))]
        # find verbs
        verbs = [w.text for w in clean_subsent if (w.pos_ == 'VERB')]
        # find amod relationships or conj relationships
        # amod ex: "mechanical breakdown" > "mechanical breakdown"
        # conj ex: "wear and tear" > "wear tear"
        relationship = [(w.text,w.head.text) for w in clean_subsent if((w.dep_ == 'amod' or w.dep_ == 'conj')and (w.text in nouns or w.head.text in nouns))]
        # find pobj relationships
        # pobj ex: "dampness of atmosphere" > "dampness atmosphere"
        relationship_2 = [[w.head.head.text,w.text] for w in clean_subsent if((w.dep_ == 'pobj')and (w.text in nouns or w.head.text in nouns))]

        for n in nouns:
            # if a noun has a relationship ignore it, otherwise add it to phrases
            if n not in flatten(relationship) and n not in flatten(relationship_2):
                phrases.append(n)
            # if a noun is in relationship 2 at second position then add the relationship to the phrases
            if n in flatten(relationship_2):
                for r in relationship_2:
                    if n == r[1]:
                        phrases.append(" ".join(r))
            # if a noun is in relationship then add the relationship to the phrases
            if n in flatten(relationship):
                for r in relationship:
                    if n in r:
                        phrases.append(" ".join(r))
            # if user requires details print all details for each subsentence
            if explained:
                print('-------subsentence')
                print(subsent)
                print("nouns: %"%nouns)
                print("verbs: %"%verbs)
                print("relationship_1: %"%relationship)
                print("relationship_2: %"%relationship_2)
                print('-------')
        all_verbs.extend(verbs)
    # after all nouns and relationships are done add verbs that have not been used in relationships
    for v in all_verbs:
        if v not in " ".join(phrases):
            phrases.append(v)
    # return a unique list of phrases
    return unique(phrases)

def get_questions(text):
    text = nlp(text)
    sents=list(text.sents)
    questions = []
    for i in sents:
        if('?' in i.text):
            questions.append(i.text)
    return(' '.join(questions))