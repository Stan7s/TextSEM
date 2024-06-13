#' Structural Equation Modeling with Sentiment Analysis
#'
#' This function integrates sentiment analysis into a structural equation model (SEM) by calculating sentiment scores for specified text variables and incorporating them as additional variables in the SEM.
#'
#' @param model The structural equation model specified as a character string.
#' @param data A data frame containing the input data.
#' @param text_var A character vector of text variable names in the data frame for which sentiment analysis should be performed.
#' @param text_stats A character vector of text sentiment statistics to be added to the SEM. Currently supports only 'OverallSenti' (overall sentiment).
#' @param polarity_dt A data table for polarity lexicon to be used for sentiment analysis. Defaults to `lexicon::hash_sentiment_jockers_rinker`.
#' @param valence_shifters_dt A data table for valence shifters to be used for sentiment analysis. Defaults to `lexicon::hash_valence_shifters`.
#' @param missing The method for handling missing data in the SEM. Defaults to 'ML' (maximum likelihood).
#' @param fixed.x Logical. If `TRUE`, the exogenous variables are treated as fixed. Defaults to `FALSE`.
#' @param ... Additional arguments passed to the `sentiment_by` function from the `sentimentr` package and to the `sem` function from the `lavaan` package.
#'
#' @return A list containing three items:
#' \item{model}{A character string representing the modified SEM with added sentiment variables.}
#' \item{data}{A data frame with added text sentiment statistics.}
#' \item{estimates}{The fitted SEM model object.}
#' @importFrom lavaan lavParseModelString lavaanify sem
#' @importFrom sentimentr sentiment_by
#' @importFrom data.table rbindlist
#' @export
#'
sem.sentiment <- function(model,
                     data,
                     text_var,
                     text_stats=c('OverallSenti'),
                     polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                     valence_shifters_dt = lexicon::hash_valence_shifters,
                     missing = 'ML',
                     fixed.x = FALSE,
                     ...){

  ## parse the model
  model_info <- lavParseModelString(model)
  model_var <- unique(c(model_info$lhs, model_info$rhs))

  ## get the list of text variables in the model
  text_var <- text_var[text_var %in% model_var]
  # print("text_var")
  # print(text_var)

  N <- length(text_var) # Number of text variables
  if (N > 0){
    ## now get the sentiment score of the text
    text_score <- list()

    batch_sentiment_by <- function(reviews, batch_size = 200, ...) {
      review_batches <- split(reviews, ceiling(seq_along(reviews)/batch_size))
      x <- data.table::rbindlist(lapply(review_batches, sentiment_by))
      x[, element_id := .I]
      x[]
    }

    for(i in 1:N){
      sentiment_result <- batch_sentiment_by(data[, text_var[i]]) # Compute sentiment scores
      text_score[[i]] <- sentiment_result$ave_sentiment
    }
    names(text_score) <- text_var
    # print("text_score")
    # print(as.data.frame(text_score))

    data_new <- cbind(data, as.data.frame(text_score))
    names(data_new) <- c(names(data), paste0(rep(text_var, each = length(text_stats)), '.', text_stats))
    # print("data_new")
    # print(names(data_new))

    model_lavaanify <- lavaanify(model)
    model_user <- model_lavaanify[model_lavaanify$user==1, ]
    # print("model_user")
    # print(model_user)

    model_new <- c()
    for(i in 1:nrow(model_user)){
      row <- model_user[i,]
      # print(row)
      if((row['lhs'] %in% text_var) && (row['rhs'] %in% text_var)){
        model_new <- c(model_new, paste0(rep(paste0(row['lhs'], '.', text_stats), each = length(text_stats)),
                                         ' ', row['op'], ' ', rep(paste0(row['rhs'], '.', text_stats), length(text_stats))))
      } else if(row['lhs'] %in% text_var){
        model_new <- c(model_new, paste0(row['lhs'], '.', text_stats, ' ', row['op'], ' ', row['rhs']))
      } else if(row['rhs'] %in% text_var){
        model_new <- c(model_new, paste0(row['lhs'], ' ', row['op'], ' ', row['rhs'], '.', text_stats))
      } else{
        model_new <- c(model_new, paste0(row['lhs'],  ' ', row['op'], ' ', row['rhs']))
      }
    }
    # print(model_new)
    # model_new <- paste0(model_new, collapse = '\n')
  }
  model_res <- sem(model=model_new, data=data_new,
                   missing = missing, fixed.x = fixed.x, ...)

  return(list(model=model_new, data=data_new, estimates=model_res))
}
