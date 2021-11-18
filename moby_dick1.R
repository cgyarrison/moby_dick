library(tidyverse)
library(tidytext)
library(gutenbergr)

moby_dick <- gutenberg_download(15)

glimpse(moby_dick)

moby_dick <- moby_dick %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>% 
  filter(chapter != 0)

moby_dick <- moby_dick %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

moby_dick %>% count(word, sort = TRUE) %>% 
  top_n(10, n) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col()

# wordcloud

library(wordcloud)

moby_dick %>% count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

# sentiments

library(textdata)

sentiments
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran")
get_sentiments("nrc")

md_bing <- moby_dick %>% 
  inner_join(get_sentiments("bing"))
md_bing

md_bing %>% group_by(chapter, sentiment) %>% 
  count() %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(overall = positive - negative)

md_bing %>% group_by(chapter, sentiment) %>% 
  count() %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(overall = positive - negative) %>% 
  ggplot(aes(x = chapter, y = overall)) +
  geom_col()

md_bing %>% group_by(chapter, sentiment) %>% 
  count() %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(overall = positive - negative) %>% 
  ggplot(aes(x = chapter, y = overall)) +
  geom_area(fill = "dark blue") +
  theme_minimal()

md_bing %>% group_by(chapter, sentiment) %>% 
  count() %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(overall = positive - negative) %>% 
  ggplot(aes(x = chapter, y = overall)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, fill = "yellow") +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = "black") +
  geom_area(fill = "dark blue") +
  theme_minimal()
