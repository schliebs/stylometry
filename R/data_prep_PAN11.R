# Load packages
source("R/packages.R")

# Data loading and preparation

# PAN 11 Training Dataset





# Read Emails and immidiately remove empty mails
data <- read.csv("../data/Emails.csv")
names(data)

# delete empty mails
data <- data %>% filter(!is.na(ExtractedBodyText) & ExtractedBodyText != "")


#Overview over data
names(data)

# Pick 3 frequent authors: 


# Advanced Data Management: tm-package/Corpus operations


# E-Mail Content: 
docs_raw <- (VCorpus(VectorSource(data$ExtractedBodyText)))
docs <- docs_raw 

summary(docs)   
#inspect(docs[1])
#writeLines(as.character(docs[2]))


# Removing Punctuation (loss of information)
#docs <- tm_map(docs,removePunctuation)   

# Removing numbers (Do not want this: loss of information)
#docs <- tm_map(docs, removeNumbers)   

# converting to lowercase (loss of information)
#docs <- tm_map(docs, tolower)   

# Remove stopwords (do I really wanna do this?) => nope
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

# so far so good the processing
docs <- tm_map(docs_final, PlainTextDocument)





########## Document Term / Term Document Matrices ##########

# DTM
dtm <- DocumentTermMatrix(docs)   
#dtm   
#as.matrix(dtm)

freq <- colSums(as.matrix(dtm))   
freq   

wf <- data.frame(word=names(freq), freq=freq)   
wf_ordered <- wf[order(wf$freq,decreasing = T),]

#TDM
tdm <- TermDocumentMatrix(docs)   
#tdm   
tdm_matrix <- as.matrix(tdm)
sub_matrix <- tdm_matrix[rownames(tdm_matrix) %in% wf_ordered[1:122,]$word,]
colnames(sub_matrix) <- 1:ncol(sub_matrix)

freq <- colSums(as.matrix(dtm))   
length(freq) 

#  Start by removing sparse terms:   
#dtms <- removeSparseTerms(dtm, 0.2) # This makes a matrix that is 20% empty space, maximum.   
#dtms
#??? 

# Word freq
freq <- colSums(as.matrix(dtm))

head(table(freq), 20) # The ", 20" indicates that we only want the first 20 frequencies. Feel free to change that number.
tail(table(freq), 20) # The ", 20" indicates that we only want the last 20 frequencies.  Feel free to change that number, as needed.



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


## STOPWORDS ? 

data$number_stopwords <- as.vector(sapply(docs,function(x) {length(allWords(x[[1]]) %>% .[.%in% stopwords("english")])} ))
data$rate_stopwords <- data$number_stopwords/data$number_words


##Total number of function words/M
# yti


##Function word frequency distribution (122 features)
test = (sub_matrix / t(rep.col(data$number_words,nrow(sub_matrix)))) %>% t()

##Total number of short words/M 
# defined as <4

countShortWords = function(d) {
  return(length(strsplit(d, " ")[[1]] %>% .[str_length(.) < 4]) )
}

data$total_shortwords <- as.vector(sapply(docs,function(x) (countShortWords(as.character(x[[1]])))))
data$rate_shortwords <- data$total_shortwords/data$number_words


##Count of hapax legomena/M
# does not work because almost all mails to short!!!

##Count of hapax legomena/V
# does not work because almost all mails to short!!!

### Total number of characters in words/C
# 

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

### Structural Attributes (all not implementable)

## Has a greeting acknowledgment
## Uses a farewell acknowledgment
## Contains signature text
## Number of attachments
## Position of requoted text within e-mail body
## HTML tag frequency distribution/total number of HTML tags (16 features)


## Other structural stuff




#######################################################################
## Save Data

saveRDS(data,file = "../data/processed_data.rds")


