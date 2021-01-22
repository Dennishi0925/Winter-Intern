library(tidyverse)
library(clipr)
library(psych)
library(nnet)
library(rsample)
library(tidytext)
library(tm)

### load data
df_all_clean <- read_rds("data/TM02/df_all_clean.rds")
df_text_seg_unnest <- read_rds("data/TM02/df_text_segj_unnest.rds")
df_stopword <- read_rds("data/TM02/df_stopword.rds")
df_sentiment <- read_rds("data/TM02/df_sentiment.rds")

### stopwords
df_news_seg_clean <- 
  df_text_seg_unnest %>% #filter(text_POS != "FW") %>%
  # bind_rows(
  #   df_text_seg_unnest %>% filter(text_POS == "FW") %>%
  #     unnest_tokens(text_segment, text_segment)
  # ) %>%
  filter(!str_detect(text_segment, "[a-zA-Z0-9]+")) %>%
  # filter(!str_detect(text_POS, "space|ther")) %>%
  filter(!str_detect(text_POS, "space")) %>%
  filter(!str_detect(text_segment, "「|」|【|】|／")) %>%
  anti_join(df_stopword, by = c("text_segment" = "word"))

### sentiment data
df_news_sentiment <- df_news_seg_clean %>% select(id, text_segment) %>%
  left_join(df_sentiment %>% rename(text_segment = word)) %>%
  filter(!is.na(type)) %>% mutate(score = if_else(type == "pos", 1, -1)) %>%
  mutate(score_abs = abs(score)) %>%
  group_by(id) %>% summarise(score = sum(score), score_abs = sum(score_abs))

### POS wide data
df_pos_wide <- df_news_seg_clean %>% select(id, text_POS2) %>% select(id, text_POS = text_POS2) %>%
  mutate(text_POS = str_c("pos_", text_POS)) %>%
  count(id, text_POS) %>% pivot_wider(names_from = text_POS, values_from = n, values_fill = list(n=0))

### kick out sparse word segment
### data frame to document term matrix
# 留下出現五次以上的詞
df_news_seg_count <- df_news_seg_clean %>% 
  count(text_segment, sort = T) %>%
  filter(n > 3) %>%
  select(text_segment)

df_news_seg_kick <- df_news_seg_clean %>% 
  count(id, text_segment) %>%
  inner_join(df_news_seg_count, by = "text_segment") %>%
  left_join(df_all_clean, by = "id") ###串回標題

dtm_news <- df_news_seg_kick %>%
  cast_dtm(id, text_segment, n)

# remove sparse words
dtm_news_f <- removeSparseTerms(dtm_news, 0.997)
dim(dtm_news);dim(dtm_news_f)

# convert to dataframe
df_news_dtm <- tidy(dtm_news_f) %>% pivot_wider(id_cols = document , 
                                                names_from = term, 
                                                values_from = count,
                                                values_fill = list(count=0)) %>%
  rename(id = document) %>% mutate(id = as.integer(id))

df_news_dtm

### 串資料: POS_wide, sentiment, DFM
df_word_feature <- df_all_clean %>% 
  # 串資料
  left_join(df_pos_wide) %>% 
  left_join(df_news_sentiment) %>%
  left_join(df_news_dtm) %>%
  mutate_at(vars(matches("pos_")), ~ if_else(is.na(.),as.integer(0),.)) %>%
  mutate_at(vars(matches("score")), ~ if_else(is.na(.),as.integer(0),as.integer(.))) %>%
  mutate(label = as.character(label))

### modeling
# spliting and modeling
set.seed(100)
split_set <- initial_split(df_word_feature, prop = 3/4)
train_data <- training(split_set)
test_data <- testing(split_set)

# use nnet
fit_nn <- multinom(label ~ ., 
                   data = train_data %>% select(-id, -text), MaxNWts = 6000)
# 可能會出現底下的 error
# Error in nnet.default(X, Y, w, ...) too many (5457) weights
# 設定 MaxNWts 參數大於上面那行括弧當中的數值即可

# Predict and Convert probs to binary
nn_pred <- predict(fit_nn, newdata = test_data, type = "probs")
nn_pred %>% as_tibble()
test_data$nn_output <- predict(fit_nn, newdata = test_data, type = "class")

test_data %>% select(nn_output, label)
# Evaluation Metrics
nn.result    <- confusionMatrix(data = as.factor(test_data$nn_output), as.factor(test_data$label)); nn.result$table
nn.Kappa     <- nn.result$overall['Kappa']; nn.Kappa 
nn.acc       <- nn.result$overall['Accuracy']; nn.acc 
nn.result$table %>% write_clip()

