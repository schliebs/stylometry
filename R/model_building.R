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

# Pick 3
filter <- df$sender %>% table() %>% sort() %>% .[.>50 ] %>% names() %>% as.vector() #%>% .[c(1:3)]
filter 

df2 <- df %>% filter(sender %in% filter)

# Mean number words
summary_table <- 
  df2 %>% group_by(sender) %>% summarize(total_mails = n(),
                                         number_words = mean(number_words,na.rm = T) %>% round(1),
                                       total_char = mean(total_char,na.rm = T) %>% round(1),
                                       unique_words = mean(unique_words,na.rm = T)%>% round(1),
                                       total_char = mean(total_char,na.rm = T)%>% round(1),
                                       sentence_length = mean(sentence_length,na.rm = T)%>% round(1),
                                       total_char = mean(total_char,na.rm = T)%>% round(1),
                                       unique_words = mean(unique_words,na.rm = T)%>% round(1),
                                       sentence_length = mean(sentence_length,na.rm = T)%>% round(1),
                                       average_word_length = mean(average_word_length,na.rm = T)%>% round(1),
                                       vocab_richness = mean(vocab_richness,na.rm = T)%>% round(1),
                                       number_stopwords = mean(number_stopwords,na.rm = T)%>% round(1),
                                       rate_stopwords = mean(rate_stopwords,na.rm = T) %>% round(1)
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

