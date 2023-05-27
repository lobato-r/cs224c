library(tidyverse)
library(keras)
library(text2vec)
library(uwot)
library(tidytext)
library(gridExtra)
library(doc2vec)


# data =  read_rds("data/very_smol.rds")
data =  read_rds("data/balanced_score5.rds") 


data_tidy =
  data %>% 
  unnest(comments) %>% 
  # filter(score_comment > 5) %>% 
  select(-score_comment) %>% 
  filter(body != "") %>% 
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


comments_tokens = 
  data_tidy %>% 
  select(id, body) %>%
  unnest_tokens(word, body)

max_length =
  comments_tokens %>% 
  count(id) %>%
  pull(n) %>% 
  max()

max_length < 1e3

rm(comments_tokens, data)

data_tidy =
  data_tidy %>% 
  rename( 
    doc_id  = id,
    text = body
    )

# model -------------------------------------------------------------------


model <- paragraph2vec(x = data_tidy, type = "PV-DBOW", dim = 150, iter = 50, 
                       min_count = 5, lr = 0.05, threads = 4)



topic_embedding = as.matrix(model, which = "docs")
rownames(topic_embedding) = summary(model,   which = "docs")


d2v_umap <- umap(topic_embedding, n_components = 2, metric = "cosine", n_neighbors = 40, min_dist = 0.5, spread=3)

data_plot <- 
  d2v_umap %>% 
  data.frame() %>% 
  rownames_to_column("id") %>%
  rename(UMAP1 = X1, UMAP2 = X2) %>% 
  as_tibble() %>% 
  mutate(technique = "Doc2Vec")

data_plot =
  data_plot %>% 
  separate(id, into = c("subreddit", "id"), sep = "_")

# Plot the UMAP dimensions for both Word2Vec and GloVe
data_plot %>% 
  rename(Subreddit = subreddit) %>% 
  ggplot() +
  geom_point(aes(x = UMAP1, y = UMAP2, color = Subreddit), size = 0.05) +
  labs(title = "Topic embedding in 2D using UMAP.") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_minimal()+
  theme(legend.key.size = unit(3, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        plot.title = element_text(hjust = .5, size = 14))+
  ylab("") +
  xlab("")

ggsave("plots/umap_doc2vec_balanced3.png", dpi = 320,
       width = 13, height = 10, units = "in")


