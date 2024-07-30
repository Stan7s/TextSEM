#' Perform Latent Dirichlet Allocation on a Data Frame
#'
#' This function takes a data frame and performs text preprocessing followed by Latent Dirichlet Allocation (LDA) for topic modeling.
#'
#' @param data A data frame containing the data.
#' @param text_var A variable in the data frame containing the text data to be analyzed.
#' @param n_topic Number of topics to be extracted.
#' @param method The method to be used for LDA fitting; currently method = "VEM" or method= "Gibbs" are supported.
#' @param sparse A numeric for the maximal allowed sparsity in the range from bigger zero to smaller one.
#' @param seed Random seed for LDA estimation
#'
#' @return A topic model object of class "LDA" from the `topicmodels` package.
#' @import dplyr
#' @importFrom tidytext unnest_tokens cast_dtm
#' @importFrom tm removeSparseTerms
#' @importFrom topicmodels LDA
#' @importFrom SnowballC wordStem
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' data(prof.nest)
#' lda.model <- sem.lda(df, text_var = c("comments"), n_topic = c(6))
#' lda.model
#' }
sem.lda <- function(data, text_var, n_topic, method = "VEM", sparse = .995, seed = 42){

  df <- data
  df["row_index"] <- 1:nrow(df)

  # Split text into terms (words)
  df.tm <- unnest_tokens(df, word, {{text_var}})

  ## Remove stopwords
  data(stopwords, envir = environment())
  df.tm <- df.tm %>% anti_join(filter(stopwords, lexicon == "evaluation"), by = join_by(word))

  ## Stem words
  df.tm$word <- SnowballC::wordStem(df.tm$word)
  df.tm <- df.tm %>%
    filter(!grepl("[[:digit:]]", word))

  ## Build Document-term matrix: https://en.wikipedia.org/wiki/Document-term_matrix
  df.dtm <- df.tm %>%
    count(.data[["row_index"]], word) %>%    ## word frequency
    tidytext::cast_dtm(.data[["row_index"]], word, n)  ## convert to dtm matrix
  df.dtm <- tm::removeSparseTerms(df.dtm, sparse)

  ## Latent Dirichlet Allocation (LDA): https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation
  topicmodels::LDA(df.dtm, k = n_topic, control=list(seed = seed))
}


#' Perform Structural Equation Modeling with Latent Dirichlet Allocation
#'
#' This function performs structural equation modeling (SEM) combined with Latent Dirichlet Allocation (LDA) to analyze text data.
#'
#' @param model A description of the user-specified model. Typically, the model is described using the lavaan model syntax. See model.syntax for more information. Alternatively, a parameter table (eg. the output of the lavaanify() function) is also accepted.
#' @param data A data frame containing the data.
#' @param text_vars A character vector of text variable names in the data frame containing the text data to be analyzed.
#' @param n_topics A numeric vector containing number of topics to be extracted for each text variable.
#' @param method The method to be used for LDA fitting; currently method = "VEM" or method= "Gibbs" are supported.
#' @param sparse A numeric for the maximal allowed sparsity in the range from bigger zero to smaller one.
#' @param seed Random seed for LDA estimation
#'
#' @return A list containing four elements:
#' \item{model}{A character string representing the modified SEM with added topic variables.}
#' \item{data}{A data frame with added topic statistics.}
#' \item{estimates}{The fitted SEM model object.}
#' \item{lda}{A vector of LDA model objects.}
#' @import dplyr
#' @importFrom tidytext tidy
#' @importFrom lavaan sem
#' @importFrom tidyr spread
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#' data(prof.nest)
#' model <- 'rating ~ book + difficulty + comments + tags'
#' res <- sem.topic(model = model,
#'                  data = prof.nest,
#'                  text_vars = c('comments', 'tags'),
#'                  n_topics = c(6, 3))
#' summary(res$model, fit=TRUE)
#' }
sem.topic <- function(model, data, text_vars, n_topics, method = "VEM", sparse = .995, seed = 42){

  df <- data
  df["row_index"] <- 1:nrow(df)

  lda_objects = c()
  for(i in 1:length(text_vars)){
    # print(i)

    # Get LDA matrix
    df.lda <- sem.lda(df, text_vars[i], n_topics[i], method = method)
    lda_objects <- c(lda_objects, df.lda)

    ## Gamma (per-document-per-topic probability): the proportion of the document that is made up of words from the assigned topic
    document.prob <- tidytext::tidy(df.lda, matrix = "gamma")
    document.prob <- document.prob %>%
      tidyr::spread(key=topic, value=gamma, sep='')

    ## Combine the data with gamma
    # Rename the columns: topic_i -> text_var.topic_i
    names(document.prob)[2:(n_topics[i] + 1)] <- paste(rep(text_vars[i], n_topics[i]), names(document.prob)[2:(n_topics[i] + 1)], sep = ".")
    document.prob$document <- as.numeric(document.prob$document)
    df <- left_join(df, document.prob, by=join_by(row_index==document))
  }

  lda_objects <- setNames(lda_objects, text_vars)

  ## Rewrite the lavaan model by replacing text_var with text_var.topic_i
  model_lavaanify <- lavaanify(model)
  model_user <- model_lavaanify[model_lavaanify$user==1, ]
  model_new <- c()

  # Remove the last topic component
  df_topic <- setNames(n_topics - 1, text_vars)

  for(i in 1:nrow(model_user)){
    row <- model_user[i,]
    # print(row)
    if((row['lhs'] %in% text_vars) && (row['rhs'] %in% text_vars)){
      left <- paste0(rep(paste0(row['lhs'], '.topic'), df_topic[as.character(row$lhs)]), 1:df_topic[as.character(row$lhs)])
      right <- paste0(rep(paste0(row['rhs'], '.topic'), df_topic[as.character(row$rhs)]), 1:df_topic[as.character(row$rhs)])
    } else if(row['lhs'] %in% text_vars){
      left <- paste0(rep(paste0(row['lhs'], '.topic'), df_topic[as.character(row$lhs)]), 1:df_topic[as.character(row$lhs)])
      right <- as.character(row$rhs)
      model_new <- c(model_new, paste0(row['lhs'], '.topic', text_stats, ' ', row['op'], ' ', row['rhs']))
    } else if(row['rhs'] %in% text_vars){
      left <- as.character(row$lhs)
      right <- paste0(rep(paste0(row['rhs'], '.topic'), df_topic[as.character(row$rhs)]), 1:df_topic[as.character(row$rhs)])
    } else{
      left <- as.character(row$lhs)
      right <- as.character(row$rhs)
    }
    combinations <- expand.grid(left, right)
    model_new <- c(model_new, paste(combinations$Var1, row['op'], combinations$Var2))
  }

  # print(model_new)
  # model_new <- paste0(model_new, collapse = '\n')

  estimates <- lavaan::sem(model = model_new, data = df)

  list(model = model_new, data = df, estimates = estimates, lda = lda_objects)
}

#' Plot Top Terms in LDA Topics
#'
#' This function plots the top terms in each topic from a Latent Dirichlet Allocation (LDA) model.
#'
#' @param df.lda A fitted LDA model object.
#'
#' @return A ggplot object showing the top terms in each topic.
#' @import dplyr
#' @import ggplot2
#' @importFrom tidytext tidy scale_x_reordered
#' @importFrom stats reorder
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'lda_model' is a fitted LDA model object
#' sem.topic.plot(lda_model)
#' }
sem.topic.plot <- function(df.lda){

  df.topics <- tidy(df.lda, matrix = "beta")

  ## terms & topics
  df.terms <- df.topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  # df.terms %>% print(n=60)

  reorder_within <- function (x, by, within, fun = mean, sep = "___", ...)
  {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }

  ## plot the topics and terms
  df.terms %>%
    mutate(topic=as.factor(topic), term = reorder_within(term, beta, topic, sep="")) %>%
    ggplot(aes(term, beta, fill = topic)) +
    geom_col(show.legend = FALSE) + facet_wrap(~topic, scales = "free", labeller = "label_both") +
    xlab("Terms") + ylab("Topics") + coord_flip() + tidytext::scale_x_reordered() + scale_fill_grey()+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12))
}


############

# sem.lda.old <- function(df, id_var, text_var, n_topics){
#
#   # Split text into terms (words)
#   df.tm <- unnest_tokens(df, word, {{text_var}})
#
#   ## Remove stopwords
#   data(stopwords, envir = environment())
#   df.tm <- df.tm %>% anti_join(filter(stopwords, lexicon == "evaluation"))
#
#   ## Stem words
#   df.tm$word <- SnowballC::wordStem(df.tm$word)
#   df.tm <- df.tm %>%
#     filter(!grepl("[[:digit:]]", word))
#
#   ## Build Document-term matrix: https://en.wikipedia.org/wiki/Document-term_matrix
#   df.dtm <- df.tm %>%
#     count(.data[[id_var]], word) %>%    ## word frequency
#     tidytext::cast_dtm(.data[[id_var]], word, n)  ## convert to dtm matrix
#   df.dtm <- tm::removeSparseTerms(df.dtm, .995)
#
#   ## Latent Dirichlet Allocation (LDA): https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation
#   topicmodels::LDA(df.dtm, k = n_topics, control=list(seed=42))
# }
#
# sem.topic.old <- function(df, id_var, text_var, n_topics, model){
#
#   # Get LDA metrix
#   df.lda <- sem.lda.old(df, id_var, text_var, n_topics)
#
#   ## Gamma (per-document-per-topic probability): the proportion of the document that is made up of words from the assigned topic
#   document.prob <- tidytext::tidy(df.lda, matrix = "gamma")
#   print(document.prob)
#   document.prob <- document.prob %>%
#     tidyr::spread(key=topic, value=gamma, sep='')
#
#   ## Combine the data with gamma
#   document.prob$document <- as.numeric(document.prob$document)
#   sem.data <- left_join(df, document.prob, by=join_by({{id_var}}==document))
#   print(sem.data)
#
#   list(model = lavaan::sem(model = model, data = sem.data), lda = df.lda)
# }
#
# model <- ' rating ~ book + difficulty +
#                   topic1 + topic2 + topic3 + topic4 + topic5
#          '
# res <- sem.topic.old(prof.nest, id_var = 'profid', text_var = 'comments', n_topics = 6, model = model)
# summary(res$model, fit=TRUE)
