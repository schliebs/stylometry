# All Senders

formattable(mail_summary(df), 
            list(
  area(col = c(total_mails)
  ) ~ normalize_bar("lightgreen", 0.2),
  area(col = c(number_words)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(total_char)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(unique_words)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(sentence_length)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(average_word_length)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(vocab_richness)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(number_stopwords)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(rate_stopwords)
  ) ~ normalize_bar("pink", 0.2)
  )
) %>% as.htmlwidget() %>% 
  saveWidget(file = "descriptive_covariates.html",selfcontained = TRUE)



# Subset for first try

# c("MillsCD@state.gov" , "Verveer, Melanne S" ,"Muscatine, Lissa"  )

formattable(mail_summary(df2), 
            list(
              area(col = c(total_mails)
              ) ~ normalize_bar("lightgreen", 0.2),
              area(col = c(number_words)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(total_char)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(unique_words)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(sentence_length)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(average_word_length)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(vocab_richness)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(number_stopwords)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(rate_stopwords)
              ) ~ normalize_bar("pink", 0.2)
            )
) %>% as.htmlwidget() %>% 
  saveWidget(file = "covariates_subset_1.html",selfcontained = TRUE)

