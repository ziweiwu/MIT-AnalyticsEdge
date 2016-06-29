

library(tm)

library(wordcloud)

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
#preprocessing tweeter words
corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

#create a word cloud

wordcloud(colnames(allTweets), colSums(allTweets), colors="blue",rot.per=0.5)



