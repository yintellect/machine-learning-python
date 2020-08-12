

#Getting data in using tm package
library(tm)

#Create folder to store text data
dir.create("text_data_to_import")

setwd(paste(getwd(),"text_data_to_import",sep="/"))

data("crude")

#Write some some text data to use for practice in tutorial
writeCorpus(crude) # writes out txt files from documents in "crude" reuters
#articles corpus

#import data from separate txt files into corpus

newcorpus <- VCorpus(DirSource(getwd()))


#Other sources can be found with:
getSources()

#use VectorSource and refer to column name if you want to import
#to corpus from a data frame
#Example: theCorpus <- Corpus(VectorSource(x = rawData$text))

docs <- c("This is a text.", "This another one.")
(vs <- VectorSource(docs))
inspect(VCorpus(vs))


#use DataframeSource and refer to column names if you want to import

docs <- data.frame(doc_id = c("doc_1", "doc_2"),
                   text = c("This is a text.", "This another one."),
                                      stringsAsFactors = FALSE)
(ds <- DataframeSource(docs))
x <- Corpus(ds)


##Let's examine the meta data and information in the reuters data
##we imported using vector source
#look at meta data for particular documents
inspect(newcorpus[1:2])

inspect(newcorpus[1])


#look at actual data for particular documents

inspect(newcorpus[[1]])

#Clean up text 

newcorpus = tm_map(newcorpus,stripWhitespace)
newcorpus = tm_map(newcorpus,removeWords,stopwords("en"))
newcorpus = tm_map(newcorpus,removePunctuation)
newcorpus = tm_map(newcorpus,removeNumbers)

#Converting to and from non-tidy formats

dtm <- DocumentTermMatrix(newcorpus)

inspect(dtm)

freq = sort(colSums(as.matrix(dtm)),decreasing = T)


#Convert to dtm to tidy data

#tidy() turns a document-term matrix into a tidy data frame
library(dplyr)
library(tidytext)

tidy_data<-tidy(dtm)


##--------working with tidy text package------

#Example of character vector that we might want to import and analyze:
	text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")


#To turn it into a tidy text dataset, we first need to put it into a data frame.


text_df <- data_frame(line = 1:4, text = text)

text_df

#Now we need to convert this so that it has?one-token-per-document-per-row:


text_df<-text_df %>%
  unnest_tokens(word, text)

data(stop_words)

tidy_data<- text_df %>%
  anti_join(stop_words)



#Example Jane Austen books


library(janeaustenr)
library(dplyr)
library(stringr)


#organize data by text, book, and chapter with dplyr code
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books


#To work with this as a tidy dataset, we need to restructure it in the 
#one-token-per-row format, which as we saw earlier is done with the 
#unnest_tokens() function.

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

#Now we have data with each row representing a word in each book


#Let's remove stop words that aren't helpful for analysis
#like and, to, but etc.

data(stop_words)

#head(stop_words) #to see stop_words variables

tidy_books <- tidy_books %>%
  anti_join(stop_words)


#What are the most common words used in books?
#Use dplyr's count function for frequencies

tidy_books %>%
  count(word, sort = TRUE) # count args are count(data, column_name, sort=TRUE)

#Visualize most used words with ggplot2

library(ggplot2)

tidy_books2<-tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n))

 tidy_books2 %>%
 ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Sentiment analysis with tidy data

# The tidytext package contains several sentiment lexicons
# in the sentiments dataset.

library(tidytext)

sentiments  #uses nrc lexicon

#The three general-purpose lexicons are

#AFINN from Finn ?rup Nielsen,
#bing from Bing Liu and collaborators, and
#nrc from Saif Mohammad and Peter Turney.

#Lexicons take terms (or groups of terms) and define sentiments

#tidytext provides a function get_sentiments() to get specific 
#sentiment lexicons without the columns that are not used in that lexicon.

get_sentiments("afinn") #classified on -5 to 5 scale of positivity to negativity
get_sentiments("bing") #True/False Scale of positivity or negativity
get_sentiments("nrc") #True/False Scale of positivity or negativity

#Note:  Size of text matters to your sentiment score!

#A text the size of many paragraphs can often have positive and negative 
#sentiment averaged out to about zero, while sentence-sized or 
#paragraph-sized text often works better
#Tweets are good.

#Example: Find words denoting Joy in Emma by Jane Austen


library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%

#add columns with line number and row number, then create word count tidy text
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#Next filter to get words for Emma only, then use inner join to 
#do sentiment analysis

#Limit sentiment data to feelings of joy only
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>% #keep words that match both datasets
  count(word, sort = TRUE) # count frequency of joy words in Emma



#Example: Examine positive and negative sentiments from Trump Tweets

#Load trump_tweets_df dataset with large chunk of trump tweets from campaign
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

nrc #note that some words fit multiple sentiment categories


#First we need to grab the text we want from the dataset

library(tidyr)

tweets <- trump_tweets_df %>%
  select(id,text,statusSource)

#Subset data to Trump's phone rather than campaign staff's phones
tweets<-tweets[grep("android",tweets$statusSource),]

#convert to tidy text using unnest_tokens
tweet_words <-unnest_tokens(tweets ,word, text)

#Remove non letters with regex code
tweet_words<-tweet_words[grep("[[:punct:]]|[[:digit:]]|https|amp",tweet_words$word,invert=TRUE),]

#Remove stopwords

tweet_words<-tweet_words %>% anti_join(stop_words)

#View most common words in tweets 
library(forcats) #to reorder by word count using fct_reorder

tweetcount<-tweet_words %>%
count(word,sort=TRUE)%>%
filter(n>50) %>%
 mutate(word = fct_reorder(word, n, .desc = FALSE)) 

#visualize frequency

tweetcount %>%
 ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#Aggregate to level per tweet
tweets_by_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id)    #count per tweet id


#Count tweets per sentiment
tweets_per_sentiment<-tweets_by_sentiment %>%
count(sentiment, sort=TRUE)%>%
 mutate(sentiment = fct_reorder(sentiment, nn, .desc = FALSE)) 

#change name of count variable from nn to n
tweets_per_sentiment<-rename(tweets_per_sentiment,n=nn)

tweets_per_sentiment %>%
 ggplot(aes(sentiment, n,fill=sentiment)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#Gets more interesting if we compare to other data
#Did Trump's staff tweet more positive words than Trump?

#create source variable in original data set to group by in analysis


tweets <- trump_tweets_df %>%
  select(id,text,statusSource)

#Subset data to Trump's phone rather than campaign staff's phones
tweets$source<-"Staff"
tweets$source[grep("android",tweets$statusSource)]<-"Trump"

tweets<-select(tweets,-statusSource)
#convert to tidy text using unnest_tokens
tweet_words <-unnest_tokens(tweets ,word, text)

#Remove non letters with regex code
tweet_words<-tweet_words[grep("[[:punct:]]|[[:digit:]]|https|amp",tweet_words$word,invert=TRUE),]

#Remove stopwords

tweet_words<-tweet_words %>% anti_join(stop_words)

#calculate total words per source
sources <- tweet_words %>%
  group_by(source) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, source, total_words)

#add sentiment categories to id level data, merge in source total,

by_source_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%  #add sentiments to tweet words
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>% #replace missing w/ zeroes
  inner_join(sources, by="id") %>% #data still at word level, need to aggregate to id
  group_by(source, sentiment, total_words) %>%
  summarize(words = sum(n)) %>% #count words
  ungroup()

head(by_source_sentiment) #data by source and sentiment

#e.g. - 224 out of 4589 tweets associated with anger 

#add proportions

by_source_sentiment<-by_source_sentiment %>%
mutate(proportion=words/total_words) %>%
 mutate(sentiment = fct_reorder(sentiment, proportion, .desc = FALSE)) 


#Visualize differences
ggplot(by_source_sentiment, aes(sentiment, proportion, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Proportion of tweets", fill = "")


##Visualize Frequencies with Word Clouds----------------

library(wordcloud)

#10 most frequent words for Jane Austen books

tidy_books_wordcount<-tidy_books %>%
  anti_join(stop_words) %>%
  count(word)

wordcloud(tidy_books_wordcount$word, tidy_books_wordcount$n, max.words = 10)

#With colors
wordcloud(tidy_books_wordcount$word, tidy_books_wordcount$n, max.words = 10,
color= brewer.pal(8,"Spectral"))



#-----Mix sentiment analysis with word clouds----

library(reshape2) #Use acast function to format data for visualization

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% #words as row.names
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


#for Trump tweets
tweet_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% #words as row.names
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


#-----Counting and Correlating Pairs of Words-------#
#words that co-occur in documents (aka tweets)
# count words co-occuring within sections

library(widyr)
word_pairs <- tweet_words  %>%
  pairwise_count(word, id, sort = TRUE) #arg1 is word arg2 is tweet id


#filter by word to see covariation

word_pairs %>%
  filter(item1 == "hillary")

#Pairwise correlation

#Measures how often words appear together relative to how
# often they appear separately.
#calculated using phi coefficient

#For Trump 

word_cors <- tweet_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, id, sort = TRUE)

word_cors


word_cors %>%
  filter(item1 == "obama")


#Pick some words to visualize

word_cors %>%
  filter(item1 %in% c("obama", "hillary", "nytimes","cruz")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()



#Load data for inaugural speeches for all U.S. Presidents
library(quanteda)

data(data_corpus_inaugural)

speechdata<-tidy(data_corpus_inaugural)

#Question 1: Can you take this tidy data and create a dataset with one row per word from
#each speech?

#Question 2: Can you delete stop words? And preprocess the data in other ways
#if needed?

#Question 3: Subset the data to particular speeches for two presidents.  Can
#you count the most frequent words used by each president and compare them?

#Question 4: Can you visualize frequencies for two different presidents with
#word clouds?

#Question 5: What pairwise correlations are the strongest for all inaugural 
#addresses?  

#Question 6: Evaluate the inaugural address data using sentiment analysis.
#What are the most and least common sentiments?


