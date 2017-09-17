source("R/packages.R")

# Data loading and preparation

# Read Emails and immidiately remove empty mails
data <- read.csv("../data/Emails.csv")
names(data)

# delete empty mails
data <- data %>% filter(!is.na(ExtractedBodyText) & ExtractedBodyText != "")


# Read other Metadata
persons <- read.csv("../data/Persons.csv")
receivers <- read.csv("../data/EmailReceivers.csv")
aliases <- read.csv("../data/Aliases.csv")

#Overview over data
names(data)

# Pick 3 frequent authors: 

data$MetadataFrom %>% table() %>% sort()
data$sender <- data$MetadataFrom %>% as.character() 
#ggplot(data = data) + geom_histogram(aes(x = sender),stat = "count")

# Of 290 unique authors, 16 wrote more than 50 E-Mails. 
# These 16 will be kept for different combinations of training datasets
data$sender %>% unique() %>% length()
data$sender %>% table() %>% sort() %>% .[.>50] 


# Advanced Data Management: tm-package/Corpus operations

data <- data[1:1000,]

# E-Mail Content: 
docs_raw <- (VCorpus(VectorSource(data$ExtractedBodyText)))
docs <- docs_raw 

summary(docs)   
#inspect(docs[1])
#writeLines(as.character(docs[2]))


# Removing Punctuation
docs <- tm_map(docs,removePunctuation)   

# Removing numbers
docs <- tm_map(docs, removeNumbers)   

# converting to lowercase
docs <- tm_map(docs, tolower)   

# Remove stopwords (do I really wanna do this?)
length(stopwords("english"))   
stopwords("english")   
# docs <- tm_map(docs, removeWords, stopwords("english"))   
# docs <- tm_map(docs, PlainTextDocument)

# Remove other particular words
docs <- tm_map(docs, removeWords, c("studienstiftung", "merkel"))   

## Optional: Stemming the end
#docs_st <- tm_map(docs, stemDocument)   
#docs_st <- tm_map(docs_st, PlainTextDocument)
#writeLines(as.character(docs_st[1])) # Check to see if it worked.
# docs <- docs_st


docs_final <- docs
# so far so good the processing

docs <- tm_map(docs_final, PlainTextDocument)

# DTM
dtm <- DocumentTermMatrix(docs)   
#dtm   
#as.matrix(dtm)

#TDM
tdm <- TermDocumentMatrix(docs)   
#tdm   
#as.matrix(tdm)

freq <- colSums(as.matrix(dtm))   
length(freq) 

#  Start by removing sparse terms:   
#dtms <- removeSparseTerms(dtm, 0.2) # This makes a matrix that is 20% empty space, maximum.   
#dtms

# Word freq
freq <- colSums(as.matrix(dtm))

head(table(freq), 20) # The ", 20" indicates that we only want the first 20 frequencies. Feel free to change that number.
tail(table(freq), 20) # The ", 20" indicates that we only want the last 20 frequencies.  Feel free to change that number, as needed.

freq <- colSums(as.matrix(dtm))   
freq   

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

# library(ggplot2)   
# 
# p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
#   geom_bar(stat = "identity") + 
#   theme(axis.text.x=element_text(angle=45, hjust=1))
# p   


############################################################


## M: Total number of words

allWords = function(d) {
  return((strsplit(d, " ")[[1]]))
}

countWords = function(d) {
  return(length(strsplit(d, " ")[[1]]))
}


data$number_words <- as.vector(sapply(docs,function(x) countWords(as.character(x[[1]]))))

##C = total number of characters in e-mail body.

data$total_char <- as.vector(sapply(docs,function(x) nchar(as.character(x[[1]]))))

## V = unique words

uniqueWords = function(d) {
  return(unique(strsplit(d, " ")[[1]]))
}

#the length of a vector containing all unique words
data$unique_words <- data$total_char <- as.vector(sapply(docs,function(x) length(uniqueWords(as.character(x[[1]])))))


##Number of blank lines/total number of lines
# yet to implement

## Average sentence length

sentenceLength <- function(x){
  return(sapply(unlist(str_split(x, boundary("sentence"))), function(x) nchar(x)) %>% mean(na.rm = TRUE))
}

data$sentence_length <-  as.vector(sapply(docs,function(x) sentenceLength(x)))

##Average word length (number of characters)

data$average_word_length <- as.vector(sapply(docs,function(x) mean(sapply(allWords(as.character(x[[1]])),function(x) nchar(x)),na.rm = TRUE)))


## Vocabulary richness i.e., V=M

data$vocab_richness <- data$unique_words/data$number_words

##Total number of function words/M
# yti
data$number_stopwords <- as.vector(sapply(docs,function(x) {length(allWords(x[[1]]) %>% .[.%in% stopwords("english")])} ))

data$rate_stopwords <- data$number_stopwords/data$number_words

##Function word frequency distribution (122 features)
# yet to implement!!!

##Total number of short words/M 

##Count of hapax legomena/M
# ??? 

##Count of hapax legomena/V
# ???

### Total number of characters in words/C
# yti

##Total number of alphabetic characters in words/C
# yti

##Total number of upper-case characters in words/C
# yti

##Total number of digit characters in words/C
# yti

##Total number of white-space characters/C
# yti

##Total number of space characters/C
# yti

##Total number of space characters/number white-space characters
# yti

##Total number of tab spaces/C
# yti

##Total number of tab spaces/number white-space characters
# yti

##Total number of punctuations/C
# yti

##Word length frequency distribution/M (30 features)
# ???




