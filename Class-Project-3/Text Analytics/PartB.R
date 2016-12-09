# libraries
library(tm) # Framework for text mining.
library(qdap) # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr) # Data wrangling, pipe operator %>%().
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.



# get data
data(acq)


# a: try functions in lecture 7
data(acq)
inspect(acq)
summary(acq)

as.list(acq)

wordcount(acq[[2]])

require(openNLP)

for(i in 1:50){
   wordcount(content(acq[[i]]))
}

dtm <- DocumentTermMatrix(acq)
dtm

inspect(dtm)

termFreq(acq[[1]])

dm2 <- TermDocumentMatrix(acq, control = list(wordLengths = c(1, Inf)))
dm2

findAssocs(dm2, "states", 0.25)

freq.terms <- findFreqTerms(dm2, lowfreq = 3)
freq.terms


# b: find 10 longest docs (in words)
inspect(acq)
dtm <- DocumentTermMatrix(acq)
wordCounts <- rowSums(as.matrix(dtm))
largest <- names(head(sort(wordCounts, decreasing = T), 10))
largest
# c: for each doc, work through examples in lect 7 to display dendrogram and Wordcloud

# we will do this right and remove stop words
d <- Corpus(VectorSource(acq))
dm3 <- tm_map(d, removeWords, stopwords('english'))

dm2 <- TermDocumentMatrix(dm3)
dm2 <- removeSparseTerms(dm2, sparse = 0.60)
distMatrix <- dist(scale(dm2))

fit <- hclust(distMatrix, method = "ward.D2")
plot(fit)
groups <- cutree(fit, k=4)
rect.hclust(fit, k=4, border="red")

library(wordcloud)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-1(1:4)]

dm3 <- TermDocumentMatrix(dm3)
m1 <- as.matrix(dm3)
word.freq <- sort(rowSums(m1), decreasing = T)
wordcloud(words = names(word.freq), freq=word.freq, min.freq = 3, random.order = F, max.words = 50)
# d: find longest word and longest sentence from 10 largest docs
# longest sentence/word
sents <- tokenize_sentences(acq[[largest[1]]]$content)
word <- tokenize_words(acq[[largest[1]]]$content)
# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[2]]]$content)
word <- tokenize_words(acq[[largest[2]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[3]]]$content)
word <- tokenize_words(acq[[largest[3]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[4]]]$content)
word <- tokenize_words(acq[[largest[1]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[1]]]$content)
word <- tokenize_words(acq[[largest[4]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[5]]]$content)
word <- tokenize_words(acq[[largest[5]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[6]]]$content)
word <- tokenize_words(acq[[largest[6]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[7]]]$content)
word <- tokenize_words(acq[[largest[7]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[8]]]$content)
word <- tokenize_words(acq[[largest[8]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[9]]]$content)
word <- tokenize_words(acq[[largest[9]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[largest[10]]]$content)
word <- tokenize_words(acq[[largest[10]]]$content)

# longest word by chars
word[which.max(nchar(word))]

# longest sentence by chars
sents[which.max(nchar(sents))]

# e: Print a table of the length of each sentence in each of the 10 documents.
library(formattable)
sents <- tokenize_sentences(acq[[largest[1]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[2]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[3]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[4]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[5]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[6]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[7]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[8]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[9]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[largest[10]]]$content)
formattable(sapply(sents, wordcount))

# f: remove punctuation and then display sentence
no_punct <- tm_map(acq, removePunctuation)
dataframe<-data.frame(text=unlist(sapply(no_punct, `[`, "content")), stringsAsFactors=F)
as.String(dataframe$text)

# g: pring Wordnet POS tags
library(wordnet)
library(openNLP)

tokenized <- tokenize_words(as.String(dataframe$text))

s <-as.String(dataframe$text)


posTagger <- Maxent_POS_Tag_Annotator(language = "en", probs = F, model = NULL)


sent_token_annotator <- Maxent_Sent_Token_Annotator ()
word_token_annotator <- Maxent_Word_Token_Annotator ()
pos_tag_annotator <- Maxent_POS_Tag_Annotator ()

y1 <- annotate(s, c(sent_token_annotator, word_token_annotator))

t <- annotate(s, posTagger, y1)
t

# h: analyze word freq using zipfR
require(zipfR)
?zipfR

tdm <- TermDocumentMatrix(acq)
temp <- inspect(tdm)
freq <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(freq) <- NULL
freq
mytfl <- tfl(freq$Freq, freq$ST)
acq.spc <- tfl2spc(mytfl)

# frequency spectrum
plot(acq.spc)
summary(acq.spc)

acq.fzm <- lnre("fzm", acq.spc)
summary(acq.fzm)

# Vocab Growth
Vm(acq.fzm,1)/N(acq.spc)
sample.sizes <- floor (N(acq.spc)/100*(1:100))
acq.vgc <- vgc.interp(acq.spc, sample.sizes)
plot(acq.vgc)
