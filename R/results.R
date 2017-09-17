formattable(summary_table, 
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
  saveWidget(file = "descriptive_covariates.html")

