library(tidyverse)
library(jiebaR)
library(lubridate)
library(tidytext)
install.packages("textdata")

### load data
# news article
df_pts_raw <- read_csv("data/TM01/pts_1016-1114_all.csv")
# df_pts_raw <- read_rds("data/TM01/pts_1016-1114_all.rds")
df_pts_raw

# stopword
df_stopword <- read_csv("data/TM01/df_stopword.csv")
# df_stopword <- read_rds("data/TM01/df_stopword.rds")

# jieba POS table
df_jieba_tabel <- read_csv("data/TM01/df_jieba_tabel.csv")
# df_jieba_tabel <- read_rds("data/TM01/df_jieba_tabel.rds")

# glimpse
df_pts_raw %>% glimpse()

# clean
df_pts_clean <- df_pts_raw %>%
  mutate(year = as.integer(str_sub(meta_time, 1, 4)),
         month = as.integer(str_sub(meta_time, 6, 7)),
         day = as.integer(str_remove(str_sub(meta_time, 9, 10), "日"))) %>%
  mutate(meta_text = str_remove_all(meta_text, "\\n|\\t|\\r| "))

# tidytext segment
df_pts_seg_tidytext <-
  df_pts_clean %>% rename(text = meta_text) %>%
  unnest_tokens(word, text) %>%
  anti_join(df_stopword)

# top 10
df_pts_seg_tidytext %>%
  count(word, sort = TRUE) %>%
  slice(1:10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

### sentiment
get_sentiments("afinn")

df_sentiment <- read_csv("data/TM01/df_sentiment.csv")
# df_sentiment <- read_rds("data/TM01/df_sentiment.rds")

df_pts_sentiment <- df_pts_seg_tidytext %>% select(id, word) %>%
  left_join(df_sentiment) %>%
  filter(!is.na(type)) %>% mutate(score = if_else(type == "pos", 1, -1)) %>%
  mutate(score_abs = abs(score)) %>%
  group_by(id) %>% summarise(score = sum(score), score_abs = sum(score_abs))

df_pts_sentiment

### tf-idf
book_tf_idf <- df_pts_seg_tidytext %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n)

book_tf_idf %>%
  arrange(desc(tf_idf))





