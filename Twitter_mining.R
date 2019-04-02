# Importing data ####
rm(list = ls())
setwd("~/Twitter API")

# Importing recent tweets: "Indien" ####
twindien <- search_tweets(
  "Indien", n = 10000, type = "recent", include_rts = FALSE
)

twindien2 <- twindien %>% 
  filter(twindien[,"verified"] == TRUE)

twindien2 <- twindien2 %>% 
  filter(twindien2[,"lang"] == "de")

twindien2 <- twindien2[!table(twindien2$screen_name)[twindien2$screen_name] < 10,] 

twindien2 <- within(twindien2, 
                    screen_name <- factor(screen_name, 
                                          levels=names(sort(table(screen_name), 
                                                            decreasing=F))))

# Bar chart of number of tweets per account (must have count larger than 10) ####
ggplot(data.frame(twindien2), aes(x= screen_name)) +
  geom_bar(stat = "count") +
  coord_flip()

# Tokenization and word cloud of words contained in tweets ####
indtext <- paste(twindien2$text)
indwords <- tokenize_tweets(indtext, strip_url = T)
wordlist1 <- data.frame(matrix(unlist(indwords), byrow=T))

tab <- table(wordlist1)
tab <- tibble(word = names(tab), count = as.numeric(tab))
tab1 <-  tab
tab1 <- tab1[order(tab1$count, decreasing = TRUE), ]

tab1 <- tab1 %>% 
  filter(tab1[,"count"] >= 5)

wordcloud2(tab1, shape = 'star', size = 2, color = 'random-dark')

ggplot(tab1) +
  aes(x = 1, y = 1, size = count, label = word) +
  geom_text_repel(segment.size = 0, force = 10) +
  scale_size(range = c(5, 15), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  labs(x = '', y = '') +
  theme_classic()

# Bar chart of words' frequency ####
ggplot(tab1, aes(x= reorder(word, +count), y=count)) +
  geom_bar(stat = "identity") +
  coord_flip()
