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

# model <- train_word2vec("data/TM03/text_course-description.txt",
#                        output="data/TM03/vec.bin",
#                        threads = 4, vectors = 300,
#                        window = 5, min_count = 12,
#                        iter = 10, force=TRUE)

model <- read.vectors("vec.bin")
# wordVectors::write.binary.word2vec(model = model, "data/TM03/text_course-description.bin")
model <- read.vectors("data/TM03/text_course-description.bin")

par(family="黑體-繁 中黑")
plot(model, method = "tsne")

nearest_to(model,model[["統計"]])
nearest_to(model,model[["分析"]])
nearest_to(model,model[["新聞"]])
nearest_to(model,model[["寫作"]])
nearest_to(model,model[["日文"]])
nearest_to(model,model[["財務"]])
nearest_to(model,model[["文學"]])
nearest_to(model,model[["翻譯"]])
nearest_to(model,model[["新聞"]])
nearest_to(model,model[["老師"]])

model %>% closest_to(~ "老師" - "同學" ,5)
model %>% closest_to(~ "同學" - "老師" ,5)

rword2vec::bin_to_txt("data/TM03/vec.bin","data/TM03/vec.text")

?readBin
# data <- readBin("vec.bin", character(), endian = "little")
word_vec <- read.table("data/TM03/vec.text", header = F, skip = 1, 
                       quote = "", row.names = NULL,
                       stringsAsFactors = F)

word_vec2 <- word_vec[2:4427,]

# word_vec <- word_vec %>% 
#   as_tibble() %>% 
#   mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
#   as.matrix()

word_vec2 <- word_vec2 %>% 
  as_tibble() %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  as.matrix()
# word_vec %>% dim()
word_vec2 %>% dim()
# ?read.table
# head(word_vec)

# word_vec[is.na(word_vec),] <- 0
# word_vec2[is.na(word_vec2),] <- 0

#further?---k means clustering
# cluster.res <- kmeans(word_vec[,2:301], 50) # time-consuming
# word_vec$cluster <- cluster.res$cluster
# for(i in 1:30){
#   print(paste0("---------------------clueter: ", i))
#   print(word_vec$V1[word_vec$cluster==i])
# }

cluster.res2 <- kmeans(word_vec2[,2:301], 50) # time-consuming
word_vec2$cluster <- cluster.res2$cluster
for(i in 1:10){
  print(paste0("---------------------clueter: ", i))
  print(word_vec2$V1[word_vec2$cluster==i])
}
