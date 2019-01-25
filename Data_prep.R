library(tm)

#Data Cleaning
text = twitter_data$text
text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
text_clean = gsub("@\\w+", "", text_clean)
text_clean = gsub("[[:punct:]]", "", text_clean)
text_clean = gsub("[[:digit:]]", "", text_clean)
text_clean = gsub("http\\w+", "", text_clean)

#Data Transformation
text_corpus <- VCorpus(VectorSource(text_clean))
text_corpus=tm_map(text_corpus, content_transformer(tolower))
text_corpus = tm_map(text_corpus, removeWords, c(stopwords("english"), "olympics"))
text_corpus = tm_map(text_corpus, stripWhitespace)
text_corpus = tm_map(text_corpus, PlainTextDocument)


tdm = TermDocumentMatrix(text_corpus)
m = as.matrix(tdm)
# remove sparse terms (word frequency > 90% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]
# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]
# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1
# change it to a Boolean matrix
#m[m>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix = m1 %*% t(m1)
