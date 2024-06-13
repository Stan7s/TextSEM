#' Generate Sentence Embeddings using Sentence Transformers
#'
#' This function generates sentence embeddings for a given vector of text using a specified pre-trained model from the `sentence_transformers` Python package.
#'
#' @param text_vector A character vector containing the text data to be embedded.
#' @param model_name A character string specifying the name of the pre-trained model to be used for generating embeddings.
#'
#' @return A matrix of sentence embeddings with each row corresponding to a sentence from `text_vector` and each column representing a dimension of the embedding space.
#' @import reticulate
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' text_vector <- c("This is a sentence.", "This is another sentence.")
#' model_name <- "paraphrase-MiniLM-L6-v2"
#' embeddings <- sem.emb(text_vector, model_name)
#' print(embeddings)
#' }
sem.encode <- function(text_vector, model_name = "all-mpnet-base-v2"){
  sbert <- import("sentence_transformers")
  model <- sbert$SentenceTransformer(model_name)
  embeddings <- model$encode(text_vector)

  # rename the columns
  colnames(embeddings) <- paste0('v', 1:ncol(embeddings))
  rownames(embeddings) <- 1:nrow(embeddings)
  embeddings
}


#' Structural Equation Modeling with Embeddings
#'
#' This function performs Structural Equation Modeling (SEM) using text embeddings. It checks if the specified `.rda` file with embeddings exists. If the file exists, it loads the embeddings; otherwise, it generates the embeddings using the `sem.encode` function. The embeddings are then incorporated into the SEM model.
#'
#' @param df A data frame containing the input data.
#' @param text_var A character string specifying the name of the text variable in the data frame.
#' @param sem_model A character string specifying the SEM model.
#' @param emb_file_path A character string specifying the path to the `.rda` file containing the embeddings. If `NULL`, embeddings are generated using `sem.encode`.
#' @param encoder_model A character string specifying the encoder model to be used for generating embeddings. Defaults to "all-mpnet-base-v2".
#'
#' @return The result of the `lavaan::sem` function, which is an object of class `lavaan`.
#' @importFrom lavaan sem
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(text = c("I love this!", "I hate that!"), rating = c(5, 1))
#' sem_model <- "rating ~ text"
#' result <- sem.emb(df, text_var = "text", sem_model = sem_model, emb_file_path = "embeddings.rda")
#' summary(result)
#' }
sem.emb <- function(df, text_var, sem_model, emb_file_path = NULL, encoder_model = "all-mpnet-base-v2"){

  # Check if the file path ends with .rda
  load_flag = FALSE
  if (!is.null(emb_file_path)) {
    if (grepl("\\.rda$", emb_file_path)) {
      if (file.exists(emb_file_path)) {
        # Load the file
        print("Loading embeddings from file...")
        embeddings <- get(load(emb_file_path))
        if (is.matrix(embeddings)) {
          if (nrow(embeddings) == nrow(df)) {
            print("Success.")
            load_flag = TRUE
          } else {
            print("Incorrect dimension.")
          }
        } else {
          print("Loaded object is not a matrix.")
        }
      } else {
        print("File doesn't exist.")
      }
    } else {
      stop("The specified file is not an `.rda` file.")
    }
  }

  if (!load_flag){
    print("Generating embeddings, this might take a while...")
    embeddings <- TextSEM::sem.encode(df[[text_var]])
    print("Success.")
  }
  replace_vars_in_model <- function(model, var_old, var_new) {
    replacement <- paste(var_new, collapse = " + ")
    updated_model <- gsub(paste0("\\b", var_old, "\\b"), replacement, model)
    return(updated_model)
  }

  # Reduce dimension for test use
  embeddings <- embeddings[,1:10]

  colnames(embeddings) <- paste0(text_var, '.', colnames(embeddings))
  sem_model <- replace_vars_in_model(sem_model, text_var, colnames(embeddings))

  df <- cbind(df, embeddings)
  df <- as.data.frame(df)
  lavaan::sem(model = sem_model, data = df)
}
