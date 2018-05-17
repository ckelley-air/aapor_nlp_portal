#################################
##   NLP for Social Science   ##
##        AAPOR 2018          ##
##      ckelley@air.org       ##
##       skelley@air.org      ##
################################

library(topicmodels)
library(tidytext)
library(dplyr)
library(sentimentr)

###########################
##   Text Preparation   ##
##########################

#setwd("location of the text file")
alice <- data_frame("text" = readLines("alice.txt"))

#select non empty rows
alice <- alice %>% subset(text != "")

#unnest - move each "token" to its own line 
tokens <- unnest_tokens(alice ,token, text,drop=F)
head(tokens)
dim(tokens)

#limit our data to paragraphs (ie > 10 words )
word_count <- tokens %>% group_by(text) %>% summarize(n_words=n())
tokens %<>% left_join(word_count) %>% subset(n_words > 10)

#caclcualte also ngrams for later 
ngrams <- unnest_tokens(alice ,token, text,token = "ngrams", n = 2)

#word freqency 
all_tokens <- tokens %>% count(token,sort=TRUE)
all_tokens %>% head()

clean_tokens <- tokens %>%  anti_join(get_stopwords(),by=c("token"="word")) 
clean_tokens  %>% count(token,sort=TRUE) %>% head()

###########################
##   Structuring Data   ##
##########################

#making a document term matrix (will need this later)
doc_term_mat <- cast_sparse(clean_tokens, text, token)

#td-idf 
#We are goign to look at each paragraph as a "document" 
word_freq <- tokens %>% anti_join(get_stopwords(),by=c("token"="word")) %>% count(text, token, sort = TRUE) %>%  ungroup()

# create term frequency 
# nb uses weighting 
tfidf <- word_freq %>% bind_tf_idf(token, text, n) %>%  arrange(desc(tf_idf))

###########################
##  Sentiment Analysis  ##
##########################

#Sentiment Analysis
sentiment("We have good surveys, the best surveys")$sentiment
sentiment("This presentation is excellent and Informative")$sentiment
sentiment("This presentation is not excellent")$sentiment 

#tokenize into sentences 
sentences <- get_sentences(alice$text)
sentiments <-  sentiment(sentences)$sentiment

sentiment_df <- data.frame("sentence"=unlist(sentences),"sentiment"= sentiments )

sentiment_df %>% subset(sentiment < -.90)
sentiment_df %>% subset(sentiment > .85 )

###########################
##   Topic Modelling    ##
##########################

#Topic Modelling by Latent Dirichlet Allocation 
#NB set a different seed and your results may differ
topics <- tidy(LDA(doc_term_mat, k = 10, control = list(seed = 1)),matrix = "beta")

top_five <- topics %>%  group_by(topic) %>%
      top_n(5, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

#Alice's feelings 
top_five %>% subset(topic ==3)

#Alice and the mythical beasts
top_five %>% subset(topic ==6)

#Alice and the monarchy 
top_five %>% subset(topic ==10)


##############################
##   Analysis of Our data   ##
#############################
aapor_results <- read.csv("aapor_results.csv")


