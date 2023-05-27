library(tidyverse)
library(tidytext)
library(furrr)
library(topicmodels)


# data = read_rds("sample_2019.rds")
# data =  read_rds("data/smol.rds")
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


data_token = 
  data_tidy %>% 
  select(subreddit, body) %>% 
  unnest_tokens(word, body, token = "ngrams", n =1) %>% 
  # unnest_tokens(word, comments, token = "ngrams", n =2) %>% 
  count(subreddit, word, sort = TRUE) %>% 
  filter(n > 2) %>% 
  anti_join(stop_words2, by ="word") 
  


popularity = 
  data_token %>% 
  group_by(word) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total))

least_popular = 
  popularity %>% 
  filter(total < 15)

most_popular =
  popularity %>% 
  arrange(desc(total)) %>% 
  filter(row_number() < 33)


data_token2 = 
  data_token %>% 
  anti_join(most_popular, by ="word")  %>%
  anti_join(least_popular, by ="word")  


# data_token2 %>% 
#   group_by(word) %>% summarize(token_freq=n()) %>% 
#   mutate(token_freq_binned = case_when(token_freq>20~20,TRUE~as.numeric(token_freq))) %>% 
#   group_by(token_freq_binned) %>% summarise(n_tokens = n()) %>% 
#   mutate(pct_tokens = n_tokens/sum(n_tokens),
#          cumpct_tokens = cumsum(n_tokens)/sum(n_tokens)) %>% 
#   ggplot(aes(x=token_freq_binned)) + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
#   geom_bar(aes(y=pct_tokens),stat='identity',fill='blue') +  
#   geom_line(aes(y=cumpct_tokens),stat='identity',color='orange',linetype='dashed') + 
#   geom_text(aes(y=cumpct_tokens,label=scales::percent(cumpct_tokens,accuracy=1)),
#             size=3) + theme_minimal() + 
#   ggtitle("Frequency of token in Corpus (all reviews)") + xlab("token frequency") +
#   ylab("% of all tokens")

subreddits_dtm =
  data_token2 %>%
  cast_dtm(subreddit, word, n)

lda_n = function(n_topics){
  subreddits_lda = LDA(subreddits_dtm, k = n_topics, control = list(seed = 1234))
  
  ap_topics = tidy(subreddits_lda, matrix = "beta")
  
  
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()  +
    theme_minimal()+
    labs(
      title =  "Most Common Words per Topic."
    )+
    ylab("") +
    xlab("")
  
  ggsave(paste0("plots/topic_", n_topics,"_common.png"),,
         width = 15, height = 7, units = "in", dpi = 320)
  
  
  agg_beta <- ap_topics %>%
    group_by(term) %>% 
    summarise(total_beta = sum(beta)) %>% 
    filter(total_beta > 0.005) 
  
  beta_wide = ap_topics %>% 
    inner_join(agg_beta, by = "term") %>% 
    mutate(relative_beta = log(beta) -log(total_beta)) %>% 
    group_by(topic) %>% 
    arrange(desc(relative_beta)) %>% 
    filter(row_number() <= 10) %>% 
    ungroup() %>% 
    arrange(topic, desc(relative_beta))
  
  beta_wide %>% 
    mutate(term = reorder_within(term, relative_beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() +
    theme_minimal()+
    labs(
      title =  "Most different words per topic."
    )+
    ylab("") +
    xlab("")
  
  ggsave(paste0("plots/topic_", n_topics,"_different.png"),
         width = 15, height = 7, units = "in", dpi = 320)
  
  ap_documents <- tidy(subreddits_lda, matrix = "gamma") 
  
  
  ap_documents=
    ap_documents %>% 
    arrange(document, desc(gamma)) %>% 
    group_by(document) %>% 
    mutate(sum_gumma = cumsum(gamma)) %>% 
    ungroup()
  
  
  ap_documents %>% 
    mutate(Topic = factor(topic, levels = c(1:n_topics), labels = 1:n_topics)) %>% 
    # mutate(topic = reorder_within(topic, gamma, topic)) %>%
    ggplot(aes(gamma, document, fill = Topic)) +
    geom_col() +
    scale_y_reordered() +
    theme_minimal()+
    labs(
      title =  "Distribution of Topics per Subreddit."
    )+
    ylab("") +
    xlab("")
  
  ggsave(paste0("plots/topic_", n_topics,"_distribution_balance.png"),
         width = 10, height = 4, units = "in", dpi = 320)
}

c(2:14) %>% 
  walk(lda_n)
# agregar bigramas
  