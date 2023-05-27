library(tidyverse)
library(keras)
library(text2vec)
library(uwot)
library(tidytext)
library(gridExtra)
library(furrr)



data =  read_rds("data/balanced_score5.rds")  

data_tidy =
  data %>% 
  unnest(comments) %>% 
  # filter(score_comment > 5) %>% 
  # select(-score_comment) %>% 
  dplyr::filter(body != "") %>% 
  group_by(subreddit) %>% 
  mutate(id = row_number()) %>%
  mutate(id = paste(subreddit, id, sep = "_")) %>% 
  ungroup() %>% 
  mutate(f_subreddit = factor(subreddit)) %>% 
  mutate(f_subreddit = as.numeric(f_subreddit))


stop_words2 = 
  read_csv("data/special_stop_words.csv") %>% 
  filter(!is.na(word))

comments_tokens = 
  data_tidy %>% 
  select(id, body) %>%
  unnest_tokens(word, body)


data_tidy =
  data_tidy %>% 
  select(-c(body))

data_tidy = 
  data_tidy %>% 
  left_join(comments_tokens %>%
              anti_join(stop_words2, by ="word") %>% 
              group_by(word) %>% 
              mutate(token_freq=n()) %>%  
              filter(token_freq>=5) %>% 
              group_by(id) %>% 
              summarise(body = str_c(word, collapse = " ")),
            by = "id") %>% 
  filter(!is.na(body))

rm(data, comments_tokens)
gc()

mask_word = function(sub, bod){
  bod %>% 
    str_replace_all(pattern = sub, replacement = paste0(sub, "_mask"))
}

glove_masking = function(word_masked){
  
  data_new =
    data_tidy %>% 
    mutate(body = map2_chr(subreddit, body, mask_word))
  
  
  # We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in reviews_new
  it <- itoken(data_new$body, 
               tokenizer = word_tokenizer,
               ids = data_new$id,
               progressbar = TRUE)
  
  # create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
  vocab <- create_vocabulary(it) # use uni-grams
  
  # text2vec has the option to prune the vocabulary of low-frequent words
  vocab <- prune_vocabulary(vocab, term_count_min = 10)
  
  # Vectorize word to integers
  vectorizer <- vocab_vectorizer(vocab)
  
  # Create a Term-Count-Matrix, by default it will use a skipgram window of 5 (symmetrical)
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
  
  # maximum number of co-occurrences to use in the weighting function, we choose the entire token set divided by 100
  x_max <- length(vocab$doc_count)/100
  
  # set up the embedding matrix and fit model
  glove_model <- GloVe$new(rank = 60, x_max = x_max) 
  glove_embedding = glove_model$fit_transform(tcm, n_iter = 20, convergence_tol = 0.01, n_threads = 4)
  
  # combine main embedding and context embeddings (sum) into one matrix
  glove_embeddingt = glove_embedding + t(glove_model$components) # the transpose of the context matrix
  
  return(glove_embeddingt)
}

glove_relationship = glove_masking("relationship")
glove_women = glove_masking("women")
glove_sex = glove_masking("sex")
glove_love = glove_masking("love")
glove_men = glove_masking("men")

helper = function(goes){
  goes[,1] %>% 
    sort(., decreasing = TRUE) %>% 
    as.data.frame() %>% 
    rownames_to_column("words") %>%
    rename_all(~c("words", "cosine")) %>% 
    as_tibble() %>% 
    filter(row_number() <= 30)
}

get_similarities = function(glove_word){
  similarity = 
    bind_cols(
      sim2(x = glove_word, 
           y = glove_word["relationships_mask", , drop = FALSE], 
           method = "cosine", norm = "l2") %>% 
        helper(),
      sim2(x = glove_word, 
           y = glove_word["dating_advice_mask", , drop = FALSE], 
           method = "cosine", norm = "l2") %>% 
        helper(),
      sim2(x = glove_word, 
           y = glove_word["TheRedPill_mask", , drop = FALSE], 
           method = "cosine", norm = "l2") %>% 
        helper(),
      sim2(x = glove_word, 
           y = glove_word["relationship_advice_mask", , drop = FALSE], 
           method = "cosine", norm = "l2") %>% 
        helper(),
      sim2(x = glove_word, 
           y = glove_word["ForeverAlone_mask", , drop = FALSE], 
           method = "cosine", norm = "l2") %>% 
        helper(),
      sim2(x = glove_word, 
           y = glove_word["datingoverthirty_mask", , drop = FALSE], 
           method = "cosine", norm = "l2") %>% 
        helper(),
      sim2(x = glove_word, 
           y = glove_word["dating_mask", , drop = FALSE], 
           method = "cosine", norm = "l2") %>% 
        helper(),
      sim2(x = glove_word, 
           y = glove_word["FemaleDatingStrategy_mask", , drop = FALSE], 
           method = "cosine", norm = "l2") %>% 
        helper()
    )
  
}


# get_similarities = function(glove_word){
#   similarity = 
#     bind_cols(
#       sim2(x = glove_word, 
#            y = glove_word["relationships_mask", , drop = FALSE], 
#            method = "cosine", norm = "l2") %>% 
#         helper(),
#       sim2(x = glove_word, 
#            y = glove_word["datingoverthirty_mask", , drop = FALSE], 
#            method = "cosine", norm = "l2") %>% 
#         helper(),
#       sim2(x = glove_word, 
#            y = glove_word["dating_mask", , drop = FALSE], 
#            method = "cosine", norm = "l2") %>% 
#         helper()
#     )
#     
# }

get_similarities(glove_men)

# get_similarities(glove_relationship)%>% 
#   write_csv(paste0("glove_similarities_relationship.csv"))

# get_similarities(glove_women)%>% 
#   write_csv(paste0("glove_similarities_women.csv"))


# get_similarities(glove_sex)%>% 
#   write_csv(paste0("glove_similarities_sex.csv"))


# get_similarities(glove_men)%>%
#   write_csv(paste0("glove_similarities_men.csv"))


# Word2Vec dimension reduction
word2vec_umap <- umap(word2vec_embedding, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)
# Put results in a dataframe for ggplot, starting with Word2Vec
df_word2vec_umap <- 
  word2vec_umap %>% 
  data.frame() %>% 
  rownames_to_column("word") %>%
  rename(UMAP1 = X1, UMAP2 = X2) %>% 
  as_tibble()%>% 
  mutate(technique = "Word2Vec")






# GloVe dimension reduction
glove_umap <- umap(glove_embedding, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)
df_glove_umap <- 
  glove_umap %>% 
  data.frame() %>% 
  rownames_to_column("word") %>%
  rename(UMAP1 = X1, UMAP2 = X2) %>% 
  as_tibble() %>% 
  mutate(technique = "Glove")



# Combine the datasets
df_umap <- bind_rows(df_word2vec_umap, df_glove_umap)



# Plot the UMAP dimensions for both Word2Vec and GloVe
ggplot(df_umap) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 0.05) +
  facet_wrap(~technique, scales = "free") +
  labs(title = "Word embedding in 2D using UMAP.") +
  theme(plot.title = element_text(hjust = .5, size = 14)) +
  theme_minimal()+
  ylab("") +
  xlab("")

ggsave("plots/umap_glove.png",
       width = 10, height = 4, units = "in", dpi = 320)
