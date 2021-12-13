# Loading packages
library(ggplot2)
library(quanteda.sentiment)
library(quanteda)
library(tidyr)


# Loading and organizing necessary files
d_course_info <- read.csv("support_docs/div_statements_revGL_revGL.csv")

d_course_info[16, "Key"] <- "GRAD-C3H" # Differentiating GRAD-C3 keys (same course, different Professors)
d_course_info[17, "Key"] <- "GRAD-C3P" # Differentiating GRAD-C3 keys (same course, different Professors)
d_course_info[18, "Key"] <- "GRAD-C3W" # Differentiating GRAD-C3 keys (same course, different Professors)

d_course_info <- d_course_info[-c(10,11), ] # Removing classes taught jointly by both male and female Professor

vec_word_count <- sapply(strsplit(d_course_info$unlist.a., " "), length) # Creating vector with diversity statement word count
d_course_info$word_count <- vec_word_count  # Including vector in data frame


# Plotting relationships
p_ave_count_gender <- ggplot(data = d_course_info, aes(x = Gender, y = word_count)) +
  geom_bar(stat = "summary", fun = "mean")

p_ave_count_method <- ggplot(data = d_course_info, aes(x = Method, y = word_count)) +
  geom_bar(stat = "summary", fun = "mean")

p_count_class_gender <- ggplot(data = d_course_info, aes(x = reorder(Key, word_count), y = word_count, fill = Gender)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))

p_count_class_method <- ggplot(data = d_course_info, aes(x = reorder(Key, word_count), y = word_count, fill = Method)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))

p_count_class_field <- ggplot(data = d_course_info, aes(x = reorder(Key, word_count), y = word_count, fill = Field)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))


# Applying dictionaries

## Working with Quanteda package's valence-based sentiment dictionary
d_sentiment <- d_course_info$unlist.a. %>%
  textstat_valence(dictionary = data_dictionary_AFINN) # Creating new data frame with valence-based sentiment values

d_course_info$sentiment <- d_sentiment$sentiment # Inserting sentiment column in original data frame

p_sentiment <- ggplot(data = d_course_info, ## Plotting sentiment by class
         aes(x = sentiment, y = reorder(Key, sentiment), color = Gender)) +
  geom_point() +
  ylab("")

## Creating diversity-specific dictionary
dict_div <-
  dictionary(list(
    diversity_words = c(
      "race",
      "gender",
      "ethnicity",
      "ability",
      "discrimination",
      "culture",
      "belief"
    ),
    speech_words = c("speech", "freedom", "thought")
  ))

## Creating a dfm using the dictionary above, weighting it by document length and computing the share of words of interest
dfm_div_wgt <- d_course_info$unlist.a. %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  dfm() %>%
  dfm_weight(scheme = "prop") %>%
  dfm_lookup(dict_div)

## Transforming the dfm into a data frame (and tidy it up) in order to plot the findings
d_div_wgt <- dfm_div_wgt %>%
  convert(to = "data.frame") %>%
  cbind(docvars(dfm_div_wgt)) %>%
  tidyr::gather(unlist.a., share, diversity_words:speech_words)

## Including the courses' keys to the new data frame
d_div_wgt$Key <- d_course_info$Key

p_div_dict <- d_div_wgt %>%
  ggplot(aes(x = Key, y = share, color = unlist.a.)) +
  geom_point() +
  ylab("Share of Document") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90))
