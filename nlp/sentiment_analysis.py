import spacy
from spacy import displacy
from spacymoji import Emoji
from textblob import TextBlob

nlp = spacy.load('en_core_web_md') # disable=['parser', 'ner'])
nlp_emoj= spacy.load('en_core_web_md') # disable=['parser', 'ner'])

# Emojis-----------------------------------
nlp_emoj= spacy.load('en_core_web_md') # disable=['parser', 'ner'])
from spacymoji import Emoji
import emoji as emoj
emoji = Emoji(nlp_emoj)
nlp_emoj.add_pipe(emoji, first=True)

def extract_emojis(str):
    return ''.join(c for c in str if c in emoj.UNICODE_EMOJI)

def describe_emojis(extracted_emojis):
    try:
        doc = nlp_emoj(extracted_emojis)
        emoji_desp = []
        for token in doc:
            emoji_desp.append(token._.emoji_desc)
            return(','.join(c for c in emoji_desp))
    except:
        return('')

def textualize_emojis(text):
    textulized_emojies = describe_emojis(extract_emojis(text))
    return (text + textulized_emojies)
