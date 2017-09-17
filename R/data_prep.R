source("R/packages.R")

# Data loading and preparation

# Read Emails
data <- read.csv("../data/Emails.csv")

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


docs_raw <- (Corpus(VectorSource(data$ExtractedBodyText)))
docs <- docs_raw

summary(docs)   
inspect(docs[1])
writeLines(as.character(docs[2]))


# Remove Empty documents: 

empty <- sapply(docs, function(x) {x[[1]] == "" })
empty



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

docs <- docs_raw

for (j in 1:5)
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("Hillary", "Trumpitrump", docs[[j]])
  
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)


## Optional: Stemming the end
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1])) # Check to see if it worked.
# docs <- docs_st