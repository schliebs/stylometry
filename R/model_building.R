#######################################
##One-dimensional case: number of words
#######################################

# Read data

data <- readRDS(file = "../data/processed_data.rds")


# Covariates that work 

df <- 
  data %>% select(
    sender,
    number_words,
    total_char,
    unique_words,
    sentence_length,
    average_word_length ,
    vocab_richness,
    number_stopwords,
    rate_stopwords
)

df <- df[complete.cases(df),]

#List all authors with more than 50 mails
df$sender %>% table() %>% sort() %>% .[.>20] 

# Mean number words

mail_summary(df)


# Pick 3
filter <- df$sender %>% table() %>% sort() %>% .[.>50 ] %>% names() %>% as.vector() %>% .[c(1:3)]
filter 

df2 <- df %>% filter(sender %in% filter)
mail_summary(df2)


## classification mode
# default with factor response:
model <- svm(sender ~ 
             number_words +
             total_char +
             unique_words +
             sentence_length +
             average_word_length +
             vocab_richness +
             number_stopwords +
             rate_stopwords,
             data = df2,
             type = "C-classification")

print(model)
summary(model)

# Tuned: 

svm_tune <- 
  system.time(
  tune(svm, factor(sender)~ ., data = df2,
                 kernel="radial", type = "C-classification",
       ranges=list(cost=10^(-1:2), 
                   gamma=c(.5,1,2)))
  )

print(svm_tune)

# test with train data
#pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Calculate Decision Values and Probabilities
pred2 <- predict(model,df2,decision.values = TRUE)
attr(pred2, "decision.values")

# Check accuracy:
round(table(predicted = pred, actual_author = df2$sender) %>% prop.table(2),2)

