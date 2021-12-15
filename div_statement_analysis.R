# Loading packages
library(ggplot2)
library(quanteda.sentiment)
library(quanteda)
library(tidyr)
library(gridExtra)
library(tm)


# Loading and organizing necessary files
d_course_info <- read.csv("support_docs/div_statements_revGL_revGL.csv")

d_course_info[16, "Key"] <- "GRAD-C3H" # Differentiating GRAD-C3 keys (same course, different Professors)
d_course_info[17, "Key"] <- "GRAD-C3P" # Differentiating GRAD-C3 keys (same course, different Professors)
d_course_info[18, "Key"] <- "GRAD-C3W" # Differentiating GRAD-C3 keys (same course, different Professors)

d_course_info <- d_course_info[-c(10,11), ] # Removing classes taught jointly by both male and female Professor

vec_word_count <- sapply(strsplit(d_course_info$unlist.a., " "), length) # Creating vector with diversity statement word count
d_course_info$word_count <- vec_word_count  # Including vector in data frame


# PLOTTING RELATIONSHIPS

p_ave_count_gender <- ggplot(data = d_course_info, aes(x = Gender, y = word_count, fill = Gender)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Length of Diversity Statement",
       subtitle = "Comparison Between Professors' Gender",
       x = "Grouping of Professors  \n by Gender",
       y = "Length of Statement (Words)") +
  scale_fill_manual(values = c("Female" = "#a52a2a", "Male" = "#2aa5a5")) +
  theme(axis.text.x=element_blank(), legend.position="bottom",
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 90))

p_ave_count_method <- ggplot(data = d_course_info, aes(x = Method, y = word_count, fill = Method)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Length of Diversity Statement",
       subtitle = "Comparison Between Professors' Main Research Method",
       x = "Grouping of Professors  \n by Research Method",
       y = "Length of Statement (Words)") +
  scale_fill_manual(values = c("Quali" = "#a0740a", "Quant" = "#0a36a0")) +
  theme(axis.text.x=element_blank(), legend.position="bottom",
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 90))

p_ave_count_field <- ggplot(data = d_course_info, aes(x = Field, y = word_count, fill = Field)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Length of Diversity Statement",
       subtitle = "Comparison Between Professors' Fields",
       x = "Grouping of Professors  \n  by Field",
       y = "Length of Statement (Words)") +
  scale_fill_manual(values = c("STEM" = "#cd5c5c", "Social science" = "#5ccdcd")) +
  theme(axis.text.x=element_blank(), legend.position="bottom",
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 90))

p_comparison_averages <- grid.arrange(p_ave_count_gender,  ## Combining plots for summary image (average length)
                                     p_ave_count_method,
                                     ncol = 2)
## Decision: not to include field as they carry the same information as method.

p_count_class_gender <- ggplot(data = d_course_info, aes(x = reorder(Key, word_count), y = word_count, fill = Gender)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Length of Diversity Statement",
       subtitle = "Filling according to \nProfessors' Gender",
       x = "Mandatory Courses",
       y = "Length of Statement (Words)") +
  scale_fill_manual(values = c("Female" = "#a52a2a", "Male" = "#2aa5a5")) +
  theme_classic() +
  coord_cartesian(ylim = c(0, 150)) +
  theme(axis.text.x = element_text(angle = 90))

p_count_class_method <- ggplot(data = d_course_info, aes(x = reorder(Key, word_count), y = word_count, fill = Method)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Length of Syllabi Diversity Statement",
       subtitle = "Filling according to \nProfessors' Main Research Method",
       x = "Mandatory Courses",
       y = "Length of Statement (Words)") +
  scale_fill_manual(values = c("Quali" = "#a0740a", "Quant" = "#0a36a0")) +
  theme_classic() +
  coord_cartesian(ylim = c(0, 150)) +
  theme(axis.text.x = element_text(angle = 90))

p_count_class_field <- ggplot(data = d_course_info, aes(x = reorder(Key, word_count), y = word_count, fill = Field)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))

p_comparison_courses <- grid.arrange(p_count_class_gender, ## Combining plots for summary image (word count per class)
             p_count_class_method, ncol = 2)
## Decision: not to include field as they carry the same information as method.


# APPLYING DICTIONARIES

## Removing classes without diversity statement from the analysis
d_course_with_statement <- d_course_info[!(d_course_info$word_count=="1"), ] # Removing classes with no diversity statement

## Working with Quanteda package's valence-based sentiment dictionary
d_sentiment <- d_course_with_statement$unlist.a. %>%
  textstat_valence(dictionary = data_dictionary_AFINN) # Creating new data frame with valence-based sentiment values

d_course_with_statement$sentiment <- d_sentiment$sentiment # Inserting sentiment column in original data frame

p_sentiment <- ggplot(data = d_course_with_statement, ## Plotting sentiment by class
         aes(x = sentiment, y = reorder(Key, sentiment), color = Gender)) +
  geom_point() +
  ylab("")

p_sentiment # Plot with "sentiment" by class according to Quanteda package


## Creating diversity-specific dictionary
dict_div <-
  dictionary(list(
    diversity_words = c(
      "race", "racism", "racist", "racial", "racially",
      "gender",
      "ethnicity", "ethnical", "ethnically",
      "ability", "ableism",
      "discrimination", "discriminatory",
      "culture", "cultural", "culturally",
      "belief",
      "equity",
      "underrepresented"
    ),
    speech_words = c("speech",
                     "freedom",
                     "thought",
                     "debate",
                     "disagreement", "agreement",
                     "respectful", "respecftully"),
    practice_words = c("projects",
                       "governments",
                       "countries"
  )))

## Creating a dfm using the dictionary above, weighting it by document length and computing the share of words of interest
dfm_div_wgt <- d_course_with_statement$unlist.a. %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  dfm() %>%
  dfm_weight(scheme = "prop") %>%
  dfm_lookup(dict_div)

## Transforming the dfm into a data frame (and tidying it up) in order to plot the findings
d_div_wgt <- dfm_div_wgt %>%
  convert(to = "data.frame") %>%
  cbind(docvars(dfm_div_wgt)) %>%
  tidyr::gather(unlist.a., share, c(diversity_words,speech_words,practice_words))

## Including the courses' keys to the new data frame
d_div_wgt$Key <- d_course_with_statement$Key

p_div_dict_wgt <- d_div_wgt %>%
  ggplot(aes(x = Key, y = share, color = unlist.a.)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Share of Diversity Statement Composed by Key Words",
       subtitle = "Filling according to \nthe Approach to Diversity",
       y = "Share of Document",
       x = "Mandatory courses") +
  scale_color_manual(name = "Approaches to Diversity",
                     labels=c(diversity_words = "Representativeness and Equity",
                              practice_words = "Practical experience",
                              speech_words = "Freedom of Speech"),
                     values = palette())

p_div_dict_wgt

############# Same process, now for absolute numbers of key words

## Creating a dfm using the same dictionary, weighting it by document length and computing the share of words of interest
dfm_div_abs <- d_course_with_statement$unlist.a. %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  dfm() %>%
  dfm_lookup(dict_div)

## Transforming a second dfm into a data frame (and tidying it up) in order to plot the findings
d_div_abs <- dfm_div_abs %>%
  convert(to = "data.frame") %>%
  cbind(docvars(dfm_div_abs)) %>%
  tidyr::gather(unlist.a., share, c(diversity_words,speech_words,practice_words))

## Including the courses' keys to the new data frame
d_div_abs$Key <- d_course_with_statement$Key

p_div_dict_abs <- d_div_abs %>%
  ggplot(aes(x = Key, y = share, color = unlist.a.)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
                      plot.subtitle = element_text(hjust = 0.5),
                      plot.title = element_text(hjust = 0.5)) +
  labs(title = "Absolute # of Key Words Employed on Diversity Statement",
       subtitle = "Filling according to \nthe Approach to Diversity",
       y = "# of Key Words Employed (Occurrence)",
       x = "Mandatory courses") +
  scale_color_manual(name = "Approaches to Diversity",
                     labels=c(diversity_words = "Representativeness and Equity",
                              practice_words = "Practical experience",
                              speech_words = "Freedom of Speech"),
                     values = palette())

p_div_dict_abs

## For comparison image - still missing a fixed panel size 

p_div_dict_abs_2 <- p_div_dict_abs +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p_comparison_diversity <- grid.arrange(p_div_dict_abs_2,
                                       p_div_dict_wgt, ncol = 1, nrow = 2)