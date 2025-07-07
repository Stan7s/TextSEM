#' Install Environment
#'
#' This function, `textsem_install`, is designed to create a Python virtual environment and install essential Python packages required for advanced text semantics analysis.
#'
#' @details
#' The function sets up a virtual environment and installs Python packages commonly used in machine learning and natural language processing. These include `pandas`, `torch`, `transformers`, and several other libraries tailored for specific tasks in text analysis and machine learning.
#'
#' @param envname A character string specifying the name of the virtual environment. Default is `"r-reticulate"`.
#'
#' @return
#' This function is used for side effects: it creates a virtual environment and installs necessary Python packages.
#'
#' @examples
#' \dontrun{
#' # Install the environment and necessary packages
#' textsem_install()
#' }
#'
#' @importFrom reticulate virtualenv_create py_install
#' @export
textsem_install <- function(envname = "r-reticulate"){
  # Create a new virtual environment
  virtualenv_create(envname)

  # Install necessary Python packages
  py_install("pandas", envname = envname)
  py_install("torch", envname = envname)
  py_install("transformers", envname = envname)
  py_install("sentence_transformers", envname = envname)
  py_install("openai", envname = envname)
  py_install("scikit-learn", envname = envname)
  py_install("matplotlib", envname = envname)
  py_install("tensorflow", envname = envname)
  py_install("tensorflow_text", envname = envname)
}

#' Initialize Environment
#'
#' The `textsem_init` function activates the virtual environment created by `textsem_install` and initializes the `sentiment.ai` library, enabling sentiment analysis capabilities in R.
#'
#' @details
#' This function activates the specified virtual environment and initializes necessary libraries for conducting text sentiment analysis. It is typically called after `textsem_install` to prepare the environment for specific analysis sessions.
#'
#' @param envname A character string specifying the name of the virtual environment previously created. Default is `"r-reticulate"`.
#'
#' @return
#' This function is used for side effects: it activates the virtual environment and initializes the `sentiment.ai` library.
#'
#' @examples
#' \dontrun{
#' # Activate and initialize the environment
#' textsem_init()
#' }
#'
#' @importFrom reticulate use_virtualenv
#' @importFrom sentiment.ai init_sentiment.ai
#' @export
textsem_init <- function(envname = "r-reticulate"){
  # Activate the virtual environment
  use_virtualenv(envname)

  # Initialize sentiment analysis module (sentiment.ai) in the specified environment
  init_sentiment.ai(envname = envname)
}
