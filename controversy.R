library(tidytext)
library(tidymodels)
library(tidyverse)
library(textrecipes)
library(LiblineaR)

set.seed(12294)

reviews <- read_csv("./data/reviews.csv")
works <- read_csv("./data/goodreads_works.csv")

controversy <- reviews %>%
  group_by(book_id) %>%
  filter(rating == 5 | rating <=2) %>%
  group_by(book_id) %>%
  # positive reviews == 5 stars, negative are 2 or less
  summarize(p = sum(rating == 5), n = sum(rating <= 2) ) %>%
  ungroup() %>%
  filter(p > 15 & n > 5) %>%
  # The numerator increases with popularity, while the denominator penalizes books
  # with an uneven balance of positive and negative reviews
  mutate(c = (p*n) / (abs(p-n)+1)) %>%
  select(book_id, c) %>%
  arrange(desc(c))

controversy <- controversy %>% 
  left_join(works, by=join_by(book_id==best_book_id))


# Most (and least) controversial titles

controversial_titles <- controversy %>%
  filter( title != "Dark Money: The Hidden History of the Billionaires Behind the Rise of the Radical Right") %>%
  filter( title != "The Passage of Power (The Years of Lyndon Johnson, Volume 4)") %>%
  filter( title != "The Little Mouse, the Red Ripe Strawberry, and the Big Hungry Bear") %>%
  filter(row_number() > max(row_number()) - 10 | row_number() <= 10)

write_csv(controversial_titles, "./data/controversial_titles.csv")

controversial_titles %>% 
  mutate( c = c - mean(c)) %>%
  mutate(title = reorder(title, c), c_color = ifelse(c>0,"red", "blue")) %>%
  ggplot(aes(title, c, fill = c_color)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Controversy Score", x="Title", title="Most and Least Controversial Texts")

# Most (and least) controversial words


# Machine Learning

# Compute the top 20% most and least controversial
controversey_quantiles <- quantile(controversy$c, c(.2,.8))
controversial_limit <- controversey_quantiles[[2]]
uncontroversial_limit <- controversey_quantiles[[1]]

# Use only the most and least controversial
controversy_ids <- controversy %>%
  filter(c >= controversial_limit | c <= uncontroversial_limit)

# Here's out data
c_data <- reviews %>%
  left_join(controversy_ids, by="book_id") %>%
  filter(!is.na(c)) %>%
  filter(nchar(review_text) >= 500) %>%
  group_by(book_id) %>%
  add_count() %>%
  filter(n >= 100) %>%
  slice_sample(n=100)

# Split into test and train
c_split <- c_data %>%
  initial_split()
c_train <- training(c_split)
c_test <- testing(c_split)


# Define preprocessing steps
c_rec <- recipe(c ~ review_text, data = c_train) %>%
  step_tokenize(review_text) %>%
  step_stopwords(review_text) %>%
  step_tokenfilter(review_text, max_tokens = 1e3) %>%
  step_tfidf(review_text) %>%
  step_normalize(all_predictors())

# Define workflow
c_workflow <- workflow() %>%
  add_recipe(c_rec)

# Define the model
svm_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")

# Fit the model
svm_fit <- c_workflow %>%
  add_model(svm_spec) %>%
  fit(data = c_train)

# Extract most controversial words
controversial_words <- svm_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(-estimate) 

write_csv(controversial_words, "./data/controversial_words.csv")

slice_tail(controversial_words, n=-2) %>% 
  filter(row_number() > max(row_number()) - 20 | row_number() <= 20) %>% 
  mutate(
    term = reorder(sub("tfidf_review_text_", "", term), estimate),
    term_color = ifelse(estimate>0,"controversial", "uncontroversial"),
    ) %>%
  ggplot(aes(term, estimate, fill = term_color)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Controversy Score", x="Title", title="Controversial Review Language",
       fill = ""
       )
  
  

