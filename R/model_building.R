#######################################
##One-dimensional case: number of words
#######################################

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

# Pick 3
three <- df$sender %>% table() %>% sort() %>% .[.>20 ] %>% names() %>% as.vector() %>% .[c(1:3)]
three 

df2 <- df %>% filter(sender %in% three)

# Mean number words
summary_table <- 
  df2 %>% group_by(sender) %>% summarize(number_words = mean(number_words,na.rm = T),
                                       total_char = mean(total_char,na.rm = T),
                                       unique_words = mean(unique_words,na.rm = T),
                                       total_char = mean(total_char,na.rm = T),
                                       sentence_length = mean(sentence_length,na.rm = T),
                                       total_char = mean(total_char,na.rm = T),
                                       unique_words = mean(unique_words,na.rm = T),
                                       sentence_length = mean(sentence_length,na.rm = T),
                                       average_word_length = mean(average_word_length,na.rm = T),
                                       vocab_richness = mean(vocab_richness,na.rm = T),
                                       number_stopwords = mean(number_stopwords,na.rm = T),
                                       rate_stopwords = mean(rate_stopwords,na.rm = T)
                                       )

#
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
                 kernel="radial", type = "C-classification",ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  )

print(svm_tune)

# test with train data
#pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Calculate Decision Values and Probabilities
pred2 <- predict(model,df,decision.values = TRUE)
attr(pred2, "decision.values")

# Check accuracy:
round(table(predicted = pred, actual_author = df2$sender) %>% prop.table(2),2)

