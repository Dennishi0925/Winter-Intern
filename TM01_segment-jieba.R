library(tidyverse)
# library(clipr)
library(jiebaR)
library(lubridate)
library(tidytext)

### load data
# news article
df_pts_raw <- read_csv("data/TM01/pts_1016-1114_all.csv")
# df_pts_raw <- read_rds("data/TM01/pts_1016-1114_all.rds")

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

### segment words
segment_not <- c("韓國瑜","民進黨","國民黨","柯文哲","北漂","賴清德")
# jieba with POS
cutter <- worker("tag")
new_user_word(cutter, segment_not)

# jieba segment and POS
df_pts_seg <-
  df_pts_clean %>% rename(text = meta_text) %>%
  mutate(word_segment = purrr::map(text, function(x)segment(x, cutter))) %>%
  mutate(word_POS = purrr::map(word_segment, function(x)names(x)))

# unnest
df_pts_unnest <- df_pts_seg %>%
  unnest(c(word_segment, word_POS))

df_pts_unnest_clean <- df_pts_unnest %>%
  anti_join(df_stopword %>% rename(word_segment = word))

### some topics

# bigram 
df_bigram <- df_pts_unnest_clean %>%
  filter(str_detect(word_POS, "n")) %>%
  select(id, word1 = word_segment)%>%
  group_by(id)%>%
  mutate(word2 = lead(word1,1))%>%
  ungroup()%>%
  filter(complete.cases(.))%>%
  count(word1,word2, sort = T)

df_bigram

# nouns
df_noun <- df_pts_unnest_clean %>%
  filter(str_detect(word_POS, "n")) %>%
  count(word_segment, word_POS, sort = T)

df_noun

# DPP
df_verb <- df_pts_unnest_clean %>%
  filter(str_detect(word_POS, "v")) %>%
  count(word_segment, word_POS, sort = T)

df_verb


