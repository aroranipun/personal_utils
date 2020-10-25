import datetime
# Gensim
import io
import sys

import gensim.corpora as corpora
import matplotlib.pyplot as plt
from gensim.models import CoherenceModel
from gensim.models import ldamulticore
from gensim.models.wrappers import LdaMallet
from tqdm import tqdm


def corpus_dictionary_create(tokenized_docs,
                             filter_no_below=10,
                             filter_no_above=.35):
    # Dictionary and Corpus creation
    # Takes in normalized list of tokens
    id2word = corpora.Dictionary(tokenized_docs)
    id2word.filter_extremes(no_below=filter_no_below, no_above=filter_no_above)
    id2word.compactify()
    corpus = [id2word.doc2bow(text) for text in tokenized_docs]
    return id2word, corpus


################ Different LDA models ##################

def lda_model_mallet(corpus, id2word, num_topics, path_to_mallet_binary=None):
    text_trap = io.StringIO()
    sys.stdout = text_trap
    model = LdaMallet(mallet_path=path_to_mallet_binary,
                      corpus=corpus,
                      id2word=id2word,
                      num_topics=num_topics
                      )
    sys.stdout = sys.__stdout__
    return model


def lda_model_multi(corpus, id2word, num_topics, workers):
    model = ldamulticore.LdaMulticore(
        corpus=corpus,
        id2word=id2word,
        num_topics=num_topics,
        workers=workers,
        passes=50,
        chunksize=100,
        eval_every=1,
        per_word_topics=True)
    return model


def lda_perplexity(lda_model, corpus):
    # a measure of how good the model is. lower the better.
    Perplexity = lda_model.log_perplexity(corpus)
    # print('\nPerplexity: ', Perplexity)
    return (Perplexity)


def lda_coherence(lda_model, id2word, tokenized_docs):
    # Compute Coherence Score
    coherence_model_lda = CoherenceModel(model=lda_model, texts=tokenized_docs, dictionary=id2word, coherence='c_v')
    coherence_lda = coherence_model_lda.get_coherence()
    # print('\nCoherence Score: ', coherence_lda)
    return (coherence_lda)


def topic_model(tokenized_docs,
                use_mallet=False, path_to_mallet_binary=None,
                corpus=None, id2word=None,
                num_topics=5, filter_no_below=10, filter_no_above=.35, workers=3):
    now = datetime.datetime.now()

    if corpus is None and id2word is None:
        id2word, corpus = corpus_dictionary_create(tokenized_docs,
                                                   filter_no_below=filter_no_below,
                                                   filter_no_above=filter_no_above)

    if use_mallet:
        model = lda_model_mallet(corpus=corpus,
                                 id2word=id2word,
                                 num_topics=num_topics,
                                 path_to_mallet_binary=path_to_mallet_binary
                                 )
        Perplexity = None
        Coherence = round(lda_coherence(lda_model=model, id2word=id2word, tokenized_docs=tokenized_docs), 2)
    else:
        model = lda_model_multi(corpus=corpus,
                                id2word=id2word,
                                num_topics=num_topics,
                                workers=workers)
        Perplexity = round(lda_perplexity(lda_model=model, corpus=corpus), 2)
        Coherence = round(lda_coherence(lda_model=model, id2word=id2word, tokenized_docs=tokenized_docs), 2)
    print(
        f'Runtime of the query was: {(datetime.datetime.now() - now)}\nWith num_topics={num_topics}\nCohearance={Coherence} and Perplexity={Perplexity}')

    return model, corpus, Coherence, Perplexity


def topic_optimizer(tokenized_docs, stop, start, step,
                    filter_no_below=10, filter_no_above=.35,
                    use_mallet=False, path_to_mallet_binary=None,
                    workers=3, graph=True):
    """
    :param tokenized_docs: list of tokenized documents
    :type filter_no_below: int
    """
    now = datetime.datetime.now()
    coherence_values = []
    model_list = []

    id2word, corpus = corpus_dictionary_create(tokenized_docs,
                                               filter_no_below=filter_no_below,
                                               filter_no_above=filter_no_above)

    for num_topics in tqdm(range(start, stop, step)):
        model, corpus, cohearance, perplexity = topic_model(tokenized_docs,
                                                            corpus=corpus, id2word=id2word,
                                                            num_topics=num_topics,
                                                            workers=workers,
                                                            use_mallet=use_mallet,
                                                            path_to_mallet_binary=path_to_mallet_binary,
                                                            filter_no_below=filter_no_below,
                                                            filter_no_above=filter_no_above
                                                            )
        model_list.append(model)
        coherence_values.append(cohearance)
        print(len(model_list))
        print(len(coherence_values))

    print("Time taken for__topic optimization__ is " + str(datetime.datetime.now() - now))
    if graph:
        x = range(start, stop, step)
        plt.plot(x, coherence_values)
        plt.xlabel("Num Topics")
        plt.ylabel("Coherence score")
        plt.legend(("coherence_values"), loc='best')
        plt.savefig("output/coherence_values.png")
        plt.show()
    return model_list, coherence_values, corpus


def get_topic_prob_vectors(lda_model, tokenized_doc):
    num_topics = lda_model.num_topics
    new_corpus = [lda_model.id2word.doc2bow(text) for text in tokenized_doc]
    train_vecs = []

    for i in range(len(new_corpus)):
        topics = lda_model.get_document_topics(new_corpus[i], minimum_probability=0.0)
        topic_vec = [topics[i][1] for i in range(num_topics)]
        train_vecs.append(topic_vec)
    return train_vecs
