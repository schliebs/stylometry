source("R/packages.R")

# Data loading and preparation

# Read Emails and immidiately remove empty mails
data <- read.csv("../data/Emails.csv")
names(data)

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
ggplot(data = data) + geom_histogram(aes(x = sender),stat = "count")

# Of 290 unique authors, 16 wrote more than 50 E-Mails. 
# These 16 will be kept for different combinations of training datasets
data$MetadataFrom %>% unique() %>% length()
data$MetadataFrom %>% table() %>% sort() %>% .[.>50] 


# E-Mail Content: 


docs_raw <- (VCorpus(VectorSource(data$ExtractedBodyText)))
docs <- docs_raw

summary(docs)   
inspect(docs[1])
writeLines(as.character(docs[2]))


# Removing Punctuation
docs <- tm_map(docs,removePunctuation)   

# Email-specific
# for (j in seq(docs)) {
#   docs[[j]] <- gsub("/", " ", docs[[j]])
#   docs[[j]] <- gsub("@", " ", docs[[j]])
#   docs[[j]] <- gsub("\\|", " ", docs[[j]])
#   docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # This is an ascii character 
# }

# Removing numbers
docs <- tm_map(docs, removeNumbers)   

# converting to lowercase
docs <- tm_map(docs, tolower)   

# this does something i don't want
#docs2 <- tm_map(docs, PlainTextDocument)

# Remove stopwords (do I really wanna do this?)
length(stopwords("english"))   
stopwords("english")   
# docs <- tm_map(docs, removeWords, stopwords("english"))   
# docs <- tm_map(docs, PlainTextDocument)

# Remove other particular words
docs <- tm_map(docs, removeWords, c("studienstiftung", "merkel"))   


# Combine words/concepts that shall stay together (does not work until now)
# for (j in seq(docs)){
#   
#   docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
#   docs[[j]] <- gsub("hillary", "Trumpitrump", docs[[j]])
#   
#   docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
#   docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
# }



## Does not do what i want
#docs2 <- tm_map(docs, PlainTextDocument)


## Optional: Stemming the end
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1])) # Check to see if it worked.
# docs <- docs_st

docs_final <- docs


# so far so good the processing

docs <- tm_map(docs_final, PlainTextDocument)


# DTM
dtm <- DocumentTermMatrix(docs)   
dtm   

#TDM
tdm <- TermDocumentMatrix(docs)   
tdm   

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

library(ggplot2)   

p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p   






