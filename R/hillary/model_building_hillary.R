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


# Look at authors/dep.variable
data$MetadataFrom %>% table() %>% sort()
data$sender <- data$MetadataFrom %>% as.character() 
#ggplot(data = data) + geom_histogram(aes(x = sender),stat = "count")

# Of 290 unique authors, 16 wrote more than 50 E-Mails. 
# These 16 will be kept for different combinations of training datasets
data$sender %>% unique() %>% length()
data$sender %>% table() %>% sort() %>% .[.>50] 


#List all authors with more than 50 mails
df$sender %>% table() %>% sort() %>% .[.>20] 

# Mean number words

mail_summary(df)


# Pick 3
filter <- df$sender %>% table() %>% sort() %>% .[.>50 ] %>% names() %>% as.vector() %>% .[c(1:3)]
filter 

df_all <- df %>% filter(sender %in% filter)
mail_summary(df_all)

# Splitting in training and test data

sample_training <- sample(1:nrow(df_all),round(nrow(df_all)*0.75),replace = FALSE)

training <- c(1:nrow(df_all)) %in% sample_training
prediction <- !c(1:nrow(df_all)) %in% training

df_training <- 
  df_all [training,]

df_prediction <- 
  df_all [prediction,]

# Tune 1
svm_tune1 <- 
    tune(svm, factor(sender)~ ., data = df_training,
         kernel="radial", type = "C-classification",
         ranges=list(cost=10^(-1:2), 
                     gamma=c(.5,1,2)))

# Tune 2
svm_tune2 <- 
  tune(svm, factor(sender)~ ., data = df_training,
       kernel="radial", type = "C-classification",
       ranges=list(cost=seq(0.1,1.9,by = 0.2), 
                   gamma=seq(0.1,1.9,by = 0.2)))


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
             data = df_training,
             gamma = 1.1,
             cost = 1.5,
             kernel = "radial",
             type = "C-classification")

print(model)
summary(model)


# Calculate Decision Values and Probabilities
pred2 <- predict(model,df_prediction,decision.values = TRUE)
attr(pred2, "decision.values")

# Check accuracy:
t1 <- round(table(predicted = pred2,
                  actual_author = df_prediction$sender) %>% prop.table(2),2); t1

