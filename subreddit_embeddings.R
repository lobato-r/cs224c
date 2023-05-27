library(tidyverse)
library(keras)
library(text2vec)
library(uwot)
library(tidytext)
library(gridExtra)


data_tidy =
  read_rds("data/balanced_score5.rds")   %>% 
  unnest(comments)  %>% 
  # filter(score_comment > 5) %>% 
  select(-score_comment) %>% 
  filter(body != "") %>% 
  group_by(subreddit) %>% 
  mutate(id = row_number()) %>%
  mutate(id = paste(subreddit, id, sep = "_")) %>% 
  ungroup() %>% 
  mutate(f_subreddit = factor(subreddit)) %>% 
  mutate(f_subreddit = as.numeric(f_subreddit))

data_tidy = 
  data_tidy %>% 
  mutate(p = runif(nrow(data_tidy))) %>% 
  mutate(train = p < 0.8) %>% 
  select(-p)




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
              filter(token_freq>=15) %>% 
              group_by(id) %>% 
              summarise(body = str_c(word, collapse = " ")),
            by = "id") %>% 
  filter(!is.na(body))
  

comments_tokens = 
  data_tidy %>% 
  select(id, body) %>%
  unnest_tokens(word, body)


x_train = 
  data_tidy %>% 
  filter(train) %>% 
  pull(body)

x_test = 
  data_tidy %>% 
  filter(!train) %>% 
  pull(body)

y_train = 
  data_tidy %>% 
  filter(train) %>% 
  pull(f_subreddit) %>% 
  as.array()

y_test = 
  data_tidy %>% 
  filter(!train) %>% 
  pull(f_subreddit) %>% 
  as.array()


# maximum number of words for a review
max_length =
  comments_tokens %>% 
  count(id) %>%
  pull(n) %>% 
  max()


# Vectorize the tokens, each token will receive a unique integer, the index of that token in a dictionary. 
# Remember, we already restricted the corpus to 37.520 unique words.
tokenizer_train <- 
  text_tokenizer() %>% 
  fit_text_tokenizer(x_train)

tokenizer_test <- 
  text_tokenizer() %>% 
  fit_text_tokenizer(x_test)

# and put these integers into a sequence
sequences_train <- texts_to_sequences(tokenizer_train, x_train)
sequences_test <- texts_to_sequences(tokenizer_train, x_test)

# and make sure that every sequence has the same length (Keras requirement)
input_train <- pad_sequences(sequences_train, maxlen = max_length)
input_test <- pad_sequences(sequences_test, maxlen = max_length)

# show an example from the created index (word and vector)
tokenizer_train$word_index[200:204]


word2vecdim <- 100

# how many words are in the index
num_tokens <- length(unique(tokenizer_train$word_index))

model <- keras_model_sequential() %>% 
  # Specify the maximum input length (150) and input_dim (unique tokens+1) and choose 32 dimensions
  layer_embedding(input_dim = num_tokens+1, 
                  output_dim = word2vecdim, 
                  input_length = max_length,
                  mask_zero = TRUE,                 
  ) %>% 
  # Flatten the 3D tensor of embeddings into a 2D tensor >
  # Shape `(samples, max_length * word2vecdim)`
  layer_flatten() %>% 
  # add a dense layer with 32 units
  layer_dense(units = 60, activation = "relu") %>%
  # add the classifier on top
  layer_dense(units = 1, activation = "sigmoid") 

model %>% compile(
  optimizer = "rmsprop",
  # we have a binary classification
  loss = "binary_crossentropy",
  # retrieve accuracy as measure
  metrics = c("acc")
)


history <- model %>% keras::fit(
  input_train, y_train,
  # maximum number of iterations
  epochs = 50,
  # how many reviews do we offer in each batch
  batch_size = 500,
  # check train results againts test data
  validation_data = list(input_test, y_test)
)




# get embedding matrix, the weights from the model
word2vec_embedding <- get_weights(model)[[1]]

# give the index back the name of the word for looking up a word embedding (NA for blanks)
rownames(word2vec_embedding) <- c('NA',as.vector(unlist(tokenizer_train$index_word)))

# let's look up word 495 ("aanraden") again, the index shifted with 1 as NAs are now on top of the list 
print(rownames(word2vec_embedding)[496])
word2vec_embedding[495,]


# find words that are related to another word 
token <- "relationship"
embedding_vector <- t(matrix(word2vec_embedding[token,])) 
cos_sim = sim2(x = word2vec_embedding, y = embedding_vector, method = "cosine", norm = "l2")
cat(paste0('Words from the embedding layer similar to "relationship":', '\n'))
print(head(sort(cos_sim[,1], decreasing = TRUE), 10))

token <- "love"
embedding_vector <- t(matrix(word2vec_embedding[token,])) 
cos_sim = sim2(x = word2vec_embedding, y = embedding_vector, method = "cosine", norm = "l2")
cat(paste0('\n', 'Words from the embedding layer similar to "love":', '\n'))
print(head(sort(cos_sim[,1], decreasing = TRUE), 10))




# We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in reviews_new
it <- itoken(data_tidy$body, 
             tokenizer = word_tokenizer,
             ids = data_tidy$id,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab <- create_vocabulary(it) # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
vocab <- prune_vocabulary(vocab, term_count_min = 10)

# What's in the vocabulary?
# print(vocab)



# Vectorize word to integers
vectorizer <- vocab_vectorizer(vocab)

# Create a Term-Count-Matrix, by default it will use a skipgram window of 5 (symmetrical)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

# maximum number of co-occurrences to use in the weighting function, we choose the entire token set divided by 100
x_max <- length(vocab$doc_count)/100

# set up the embedding matrix and fit model
glove_model <- GloVe$new(rank = word2vecdim, x_max = x_max) 
glove_embedding = glove_model$fit_transform(tcm, n_iter = 50, convergence_tol = 0.01, n_threads = 4)

# combine main embedding and context embeddings (sum) into one matrix
glove_embeddingt = glove_embedding + t(glove_model$components) # the transpose of the context matrix

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

ggplot(df_glove_umap) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 0.05) +
  labs(title = "Word embedding in 2D using UMAP.") +
  theme(plot.title = element_text(hjust = .5, size = 14)) +
  theme_minimal()+
  ylab("") +
  xlab("")

ggsave("plots/umap_glove_complete.png", dpi = 320)





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

ggsave("plots/umap_glove_w2v_complete.png", dpi = 320)
