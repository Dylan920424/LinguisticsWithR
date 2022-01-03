# Chapter 4 Lecture

#======================
# Lecture 4.1
#======================
library(quanteda)
library(readtext)
library(tidytext)
library(dplyr)
packageVersion("quanteda")
#======================
# END
#======================

#======================
# Lecture 4.2: Building a corpus from character vector
#======================
data_corpus_inaugural
str(data_corpus_inaugural)
class(data_corpus_inaugural)
 corp_us <- corpus(data_corpus_inaugural) # save the `corpus` to a short obj name
summary(corp_us)

#Visualization
library(ggplot2)
library(ggrepel)
corp_us %>%
  summary %>%
  ggplot(aes(x = Year, y = Tokens, group = 1, label = President)) +
  geom_line() +
  geom_point(aes(color = Party)) +
  theme_bw() +
  geom_text_repel()
#try theme_classic() to change to layout
#======================
# END
#======================

#======================
# Lecture 4.2:Other types
#======================
#when you want to import a dataframe into a corpus
#specify docid_field = "doc_id", text_field = "text"
?corpus()
#======================
# END
#======================

#======================
# Lecture 4.2:Other types
#======================
require(ggplot2)

corp_us %>%
  summary %>%
  ggplot(aes(x = Year, y = Tokens, group = 1)) +
  geom_line() +
  geom_point() +
  theme_bw()
#======================
# END
#======================


#======================
# Lecture 4.3: Keyword-in-Context (KWIC)
#======================
View(kwic(corp_us, "terror"))
#with regex
kwic(corp_us, "terror.*", valuetype = "regex")
View(kwic(corp_us, phrase("our country"),window = 5))#window = 5
#======================
# END
#======================

#======================
# Lecture 4.5
#======================
library(tidytext)
corp_us_tidy <- tidy(corp_us) # convert `corpus` to `data.frame`
class(corp_us_tidy)

#======================
# END
#======================

#======================
# Lecture 4.6
#======================
corp_us_words <- corp_us_tidy %>%
  unnest_tokens(output = word, 
                input = text, 
                token = "words") # tokenize the `text` column into `word`

corp_us_words

#======================
# END
#======================

#======================
# Lecture 4.6
#======================
corp_us_words_freq <- corp_us_words %>% 
  count(word, sort=TRUE)

corp_us_words_freq

#======================
# END
#======================

#======================
# Lecture 4.6
#======================
corp_us_bigrams <- corp_us_tidy %>%
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2)

corp_us_bigrams

#======================
# END
#======================

#======================
# Lecture 4.6
#======================
corp_us_bigrams_freq <- corp_us_bigrams %>% 
  count(bigram, sort=TRUE)
corp_us_bigrams_freq
sum(corp_us_words_freq$n) # size of unigrams
sum(corp_us_bigrams_freq$n) # size of bigrams
#======================
# END
#======================

#======================
# Lecture 4.6
#======================
corp_us_trigrams <-  corp_us_tidy %>%
  unnest_tokens(trigrams, text, token = "ngrams", n = 3)

corp_us_trigrams

#======================
# END
#======================

#======================
# Lecture 4.6
#======================
corp_us_trigrams %>%
  count(President, trigrams) %>%
  group_by(President) %>%
  top_n(3, n) %>%
  arrange(President, desc(n))

#======================
# END
#======================

#======================
# Lecture 4.6
#======================
corp_us_trigrams %>%
  group_by(trigrams) %>%
  summarize(FREQ = n(), DISPERSION = n_distinct(President)) %>%
  filter(DISPERSION >= 5) %>%
  arrange(desc(DISPERSION))

#======================
# END
#======================

#======================
# Lecture 4.7
#======================
library(wordcloud)
with(corp_us_words_freq, wordcloud(word, n, 
                                   max.words = 400,
                                   min.freq = 20,
                                   scale = c(2,0.5),
                                   color = brewer.pal(8, "Dark2"),
                                   vfont=c("serif","plain")))

#======================
# END
#======================

#======================
# Lecture 4.8
#======================
corp_us_bigrams_freq %>% head(10)
corp_us_collocations <- corp_us_bigrams_freq %>%
  filter(n > 5) %>% # set bigram frequency cut-off
  rename(O11 = n) %>%
  tidyr::separate(bigram, c("w1", "w2"), sep="\\s") %>% # split bigrams into two columns
  mutate(R1 = corp_us_words_freq$n[match(w1, corp_us_words_freq$word)],
         C1 = corp_us_words_freq$n[match(w2, corp_us_words_freq$word)]) %>% # retrieve w1 w2 unigram freq
  mutate(E11 = (R1*C1)/sum(O11)) %>% # compute expected freq of bigrams
  mutate(MI = log2(O11/E11),
         t = (O11 - E11)/sqrt(E11)) %>% # compute associations
  arrange(desc(MI)) # sorting

corp_us_collocations
#======================
# END
#======================

#======================
# Lecture 4.9
#======================
corp_us_sents <- corp_us_tidy %>%
  unnest_tokens(output = sentence, 
                input = text, 
                token = "sentences") # tokenize the `text` column into `sentence`
corp_us_sents

#======================
# END
#======================

#======================
# Lecture 4.9
#======================
require(stringr)
# Perfect
corp_us_sents %>%
  unnest_tokens(perfect, 
                sentence,
                token = function(x) str_extract_all(x, "ha[d|ve|s] \\w+(en|ed)")) -> result_perfect
result_perfect

#======================
# END
#======================

#======================
# Lecture 4.9
#======================
require(tidyr)
# table
result_perfect %>%
  group_by(President) %>%
  summarize(TOKEN_FREQ = n(),
            TYPE_FREQ = n_distinct(perfect))
#======================
# END
#======================

#======================
# Lecture 4.9
#======================
# graph
result_perfect %>%
  group_by(President) %>%
  summarize(TOKEN_FREQ = n(),
            TYPE_FREQ = n_distinct(perfect)) %>%
  pivot_longer(c("TOKEN_FREQ", "TYPE_FREQ"), names_to = "STATISTIC", values_to = "NUMBER") %>%
  ggplot(aes(President, NUMBER, fill = STATISTIC)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  theme(axis.text.x = element_text(angle=90))
#======================
# END
#======================

