#' Perform Latent Dirichlet Allocation on a Data Frame
#'
#' This function takes a data frame and performs text preprocessing followed by Latent Dirichlet Allocation (LDA) for topic modeling.
#'
#' @param df A data frame containing the data.
#' @param id_var A variable in the data frame that uniquely identifies each document.
#' @param text_var A variable in the data frame containing the text data to be analyzed.
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
#' lda.model <- sem.lda(df, id_var = "profid", text_var = "comments")
#' lda.model
#' }
sem.lda <- function(df, id_var, text_var){

  # Split text into terms (words)
  df.tm <- unnest_tokens(df, word, {{text_var}})

  ## Remove stopwords
  data(stopwords, envir = environment())
  df.tm <- df.tm %>% anti_join(filter(stopwords, lexicon == "evaluation"))

  ## Stem words
  df.tm$word <- SnowballC::wordStem(df.tm$word)
  df.tm <- df.tm %>%
    filter(!grepl("[[:digit:]]", word))

  ## Build Document-term matrix: https://en.wikipedia.org/wiki/Document-term_matrix
  df.dtm <- df.tm %>%
    count(.data[[id_var]], word) %>%    ## word frequency
    tidytext::cast_dtm(.data[[id_var]], word, n)  ## convert to dtm matrix
  df.dtm <- tm::removeSparseTerms(df.dtm, .995)

  ## Latent Dirichlet Allocation (LDA): https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation
  topicmodels::LDA(df.dtm, k = 6, control=list(seed=20240305))
}


#' Perform Structural Equation Modeling with Latent Dirichlet Allocation
#'
#' This function performs structural equation modeling (SEM) combined with Latent Dirichlet Allocation (LDA) to analyze text data.
#'
#' @param df A data frame containing the data.
#' @param id_var A variable in the data frame that uniquely identifies each document.
#' @param text_var A variable in the data frame containing the text data to be analyzed.
#' @param model A description of the user-specified model. Typically, the model is described using the lavaan model syntax. See model.syntax for more information. Alternatively, a parameter table (eg. the output of the lavaanify() function) is also accepted.
#'
#' @return A list containing two elements: `model` (the fitted SEM model) and `lda` (the LDA model).
#' @import dplyr
#' @importFrom tidytext tidy
#' @importFrom lavaan sem
#' @importFrom tidyr spread
#' @export
#'
#' @examples
#' \dontrun{
#' data(prof.nest)
#' res <- sem.topic(prof.nest, 'profid', 'comments')
#' summary(res$model, fit=TRUE)
#' model <- 'rating ~ book + difficulty +
#'           topic1 + topic2 + topic3 + topic4 + topic5
#'          '
#' }
sem.topic <- function(df, id_var, text_var, model){

  # Get LDA metrix
  df.lda <- sem.lda(df, id_var, text_var)

  ## Gamma (per-document-per-topic probability): the proportion of the document that is made up of words from the assigned topic
  document.prob <- tidytext::tidy(df.lda, matrix = "gamma")
  document.prob <- document.prob %>%
    tidyr::spread(key=topic, value=gamma, sep='')

  ## Combine the data with gamma
  document.prob$document <- as.numeric(document.prob$document)
  sem.data <- left_join(df, document.prob, by=join_by({{id_var}}==document))

  list(model = lavaan::sem(model = model, data = sem.data), lda = df.lda)
}

#' Plot SEM Path Diagram with topic model
#'
#' This function plots the path diagram of a Structural Equation Modeling (SEM) with topic model.
#'
#' @param sem.model A fitted SEM model object.
#'
#' @return A rendered graph visualization of the SEM path diagram.
#' @import dplyr
#' @import lavaan
#' @importFrom RAMpath lavaan2ram ramPathBridge
#' @importFrom DiagrammeR grViz
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'fit' is a fitted SEM model object
#' sem.topic.path(fit)
#' }
sem.topic.path <- function(sem.model){

  plot.model <- RAMpath::lavaan2ram(sem.model, ram.out = F)
  plot.model.path <- RAMpath::ramPathBridge(plot.model, F, F)
  plot(plot.model.path, 'topic_model', output.type='dot')
  DiagrammeR::grViz('topic_model.dot')
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




