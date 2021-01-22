library(tidyverse)
library(tidytext)
library(jiebaR)
library(lubridate)
library(tsne)
# devtools::install_github("bmschmidt/wordVectors")
library(wordVectors)
# devtools::install_github("mukul13/rword2vec")
library(rword2vec)

# load data
# df_course <- read_rds("data/TM03/df_course.rds")
df_course <- read_csv("data/TM03/df_course.csv")
df_course %>% sample_n(10)
### segment words
segment_not <- c("韓國瑜","民進黨","國民黨","柯文哲","北漂","賴清德")
# jieba with POS
cutter <- worker("tag")
new_user_word(cutter, segment_not)

# stopword
df_stopword <- read_csv("data/TM03/df_stopword.csv")

tokenized_df <- df_course %>%
  mutate(詳細_課程概述_word = purrr::map(詳細_課程概述, function(x)segment(x, cutter)))

unnested_df <- tokenized_df %>%
  select(課程_ID, 詳細_課程概述, 詳細_課程概述_word) %>%
  unnest(詳細_課程概述_word) %>%
  anti_join(df_stopword, by = c("詳細_課程概述_word" = "word")) %>%
  filter(!str_detect(詳細_課程概述_word, "[a-zA-Z0-9]+"))

unnested_df %>% count(詳細_課程概述_word, sort = T) %>% head(50)

unnested_df %>%
  group_by(課程_ID) %>%
  summarise(w_w = paste(詳細_課程概述_word, collapse = " ")) %>%
  ungroup() %>% 
  .$w_w %>%
  write("data/TM03/text_course-description.txt")

### train the model(他會存在 output 裡面)
# model <- train_word2vec("data/TM03/text_course-description.txt",
#                        output="data/TM03/vec.bin",
#                        threads = 4, vectors = 300,
#                        window = 5, min_count = 12,
#                        iter = 10, force=TRUE)

model <- read.vectors("data/TM03/vec.bin")

par(family="黑體-繁 中黑")
plot(model, method = "tsne")

### 接近的字
nearest_to(model,model[["統計"]])
nearest_to(model,model[["分析"]])
nearest_to(model,model[["新聞"]])
nearest_to(model,model[["寫作"]])
nearest_to(model,model[["日文"]])
nearest_to(model,model[["財務"]])
nearest_to(model,model[["英文"]])
nearest_to(model,model[["文學"]])

### 找平均
language = closest_to(model,model[[c("中文","英文","日文","法文")]],150)
language_avg = model[[language$word,average=F]]
plot(language_avg,method="pca")

# 其他可以參考
# https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd

# rword2vec
rword2vec::bin_to_txt("data/TM03/vec.bin","data/TM03/vec.text")

word_vec <- read.table("data/TM03/vec.text", header = F, skip = 1, 
                       quote = "", row.names = NULL,
                       stringsAsFactors = F)

# word_vec 與 word_vec2 的差別在於後者的 column 數較少，不想放太多字
word_vec2 <- word_vec[2:4427,]

# 字多的看要不要跑都可以
word_vec_table <- word_vec %>%
  as_tibble() %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

word_vec2_table <- word_vec2 %>% 
  as_tibble() %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))

word_vec_table %>% dim()
word_vec2_table %>% dim()

# word_vec[is.na(word_vec),] <- 0
# word_vec2[is.na(word_vec2),] <- 0

# k means clustering
cluster.res <- kmeans(word_vec_table[,2:301], 50) # time-consuming
word_vec_table$cluster <- cluster.res$cluster
for(i in 1:30){
  print(paste0("---------------------clueter: ", i))
  print(word_vec_table$V1[word_vec$cluster==i])
}


cluster.res2 <- kmeans(word_vec2_table[,2:301], 50) # time-consuming
word_vec2_table$cluster <- cluster.res2$cluster
for(i in 1:10){
  print(paste0("---------------------clueter: ", i))
  print(word_vec2_table$V1[word_vec2_table$cluster==i])
}

