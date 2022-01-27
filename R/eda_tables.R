#' Text summary of the data dimensions
#' 
#' @description Returns a character string, summarizing the number of features 
#'   and samples in the training, validation, and test data sets.
#'   
#' @param Xtrain Training data matrix or data frame.
#' @param Xvalid Validation data matrix or data frame.
#' @param Xtest Test data matrix or data frame.
#' @param print Logical indicating whether or not to print the text summary to
#'   the console.
#' 
#' @returns A character string with the text summary of the data dimensions.
#' 
#' @export
dataDimensions <- function(Xtrain, Xvalid, Xtest, print = TRUE) {
  text_out <- paste(
    sprintf("Number of features: %s", ncol(Xtrain)),
    sprintf("Number of training samples: %s", nrow(Xtrain)),
    sprintf("Number of validaiton samples: %s", nrow(Xvalid)),
    sprintf("Number of test samples: %s", nrow(Xtest)),
    sep = "\n"
  )
  
  if (print) {
    cat(text_out)
  }
  
  return(invisible(text_out))
}

#' Summary table of data types in (X, y)
#' 
#' @description Computes the frequency of each data type in the provided (X, y)
#'   data and returns an html or latex table with this information.
#' 
#' @param X Data matrix or data frame.
#' @param y Response vector.
#' @param html Logical indicating whether or not the output is an html table
#'   or a latex table.
#' @param ... Additional arguments to pass to pretty_DT() if
#'   \code{html = TRUE} or pretty_kable() if \code{html = FALSE}.
#' 
#' @returns Returns an html table (i.e., the output of pretty_DT()) or
#'   a latex table (i.e., the output of pretty_kable()), 
#'   containing the frequency of each data type in the given (X, y) data.
#' 
#' @export
dataTypes <- function(X, y, html = knitr::is_html_output(), ...) {
  group <- NULL  # to fix no visible binding for global variable error
  dtype <- NULL
  Freq <- NULL
  
  data <- cbind(.y = y, X)
  dtypes_df <- data.frame(
    var = colnames(data),
    dtype = as.factor(sapply(data, class)),
    group = c("y", rep("X", ncol(X)))
  ) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(Freq = c(table(dtype)), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Class = stringr::str_to_title(names(Freq))) %>%
    tidyr::spread(key = "Class", value = "Freq") %>%
    tibble::column_to_rownames("group")
  if (html) {
    tab_out <- pretty_DT(
      dtypes_df, 
      caption = shiny::tags$caption(
        style = "color: black; font-weight: bold; font-size: 125%",
        "Frequency of Column Types"
      ),
      options = list(dom = "t"),
      ...
    )
  } else {
    tab_out <- pretty_kable(dtypes_df, caption = "Frequency of column",
                                     format = "latex", ...)
  }
  return(tab_out)
}

#' Summary table of (X, y) data
#' 
#' @description Provides a summary of the given (X, y) data in table form. 
#'   Serves as a wrapper function around skimr::skim(), which skims a data 
#'   frame and returns a broad overview of useful summary statistics. This 
#'   wrapper can currently handle columns of type "factor", "numeric", 
#'   "character", "logical", "complex", "Date", and "POSIXct". All other column
#'   types are ignored.
#' 
#' @inheritParams dataTypes
#' @inheritParams pretty_DT
#' @param features (Optional) vector of features to include in summary. Default
#'   (\code{NULL}) is to include all features.
#' @param max_features (Optional) maximum number of features to include in 
#'   summary. Only used if \code{features = NULL}. Default is 1000. If the 
#'   number of features in X exceeds `max_features`, the features kept in the 
#'   summary are chosen randomly.
#' 
#' @returns Returns an html table (i.e., the output of pretty_DT()) or
#'   a latex table (i.e., the output of pretty_kable()), 
#'   containing a broad overview of summary statistics for each data column.
#' 
#' @export
dataSummary <- function(X, y = NULL, digits = 2, sigfig = FALSE,
                        features = NULL, max_features = 1000, 
                        html = knitr::is_html_output(), ...) {
  skim_type <- NULL  # to fix no visible binding for global variable error
  complete_rate <- NULL
  factor.ordered <- NULL
  logical.count <- NULL
  
  if (is.null(features)) {
    features <- colnames(X)
    if (length(features) > max_features) {
      features <- sample(features, size = max_features, replace = FALSE)
    }
  }
  
  if (!is.null(y)) {
    skim_out <- skimr::skim(cbind(`.y` = y, X[, features]))
  } else {
    skim_out <- skimr::skim(X[, features])
  }
  tab_ls <- list()
  dtypes <- c("factor", "numeric", "character", "logical", "complex", "Date",
              "POSIXct")
  for (dtype in dtypes) {
    if (sum(skim_out$skim_type == dtype) > 0) {
      # clean up and reformat output of skim() to show particular summary stats
      skim_df <- skim_out %>%
        dplyr::filter(skim_type == dtype) %>%
        dplyr::mutate(
          complete_rate = formatC((1 - complete_rate) * 100, 
                                  digits = digits, flag = "#",
                                  format = if (sigfig) "g" else "f")
        )
      
      keep_cols <- c("Variable Name" = "skim_variable",
                     "# Missing" = "n_missing",
                     "% Missing" = "complete_rate")
      
      if (dtype == "factor") {
        skim_df <- skim_df %>%
          dplyr::mutate(factor.ordered = tolower(factor.ordered) %>%
                          stringr::str_to_title())
        keep_cols <- c(keep_cols,
                       "Ordered Factor" = "factor.ordered",
                       "# Unique Factors" = "factor.n_unique",
                       "Top Factor Counts" = "factor.top_counts")
      } else if (dtype == "numeric") {
        skim_df <- skim_df %>%
          dplyr::mutate(
            dplyr::across(
              c("numeric.mean", "numeric.sd", "numeric.p0", "numeric.p25", 
                "numeric.p50", "numeric.p75", "numeric.p100"),
              formatC, 
              digits = digits, flag = "#", format = if (sigfig) "g" else "f"
            )
          )
        keep_cols <- c(keep_cols,
                       "Mean" = "numeric.mean",
                       "SD" = "numeric.sd",
                       "Minimum" = "numeric.p0",
                       "Q1" = "numeric.p25",
                       "Median" = "numeric.p50",
                       "Q3" = "numeric.p75",
                       "Maximum" = "numeric.p100")
        if (html) {
          keep_cols <- c(keep_cols, "Histogram" = "numeric.hist")
        }
      } else if (dtype == "character") {
        keep_cols <- c(keep_cols,
                       "# Empty" = "character.empty",
                       "Min Length" = "character.min",
                       "Max Length" = "character.max",
                       "# Unique" = "character.n_unique",
                       "Whitespace" = "character.whitespace")
      } else if (dtype == "logical") {
        skim_df <- skim_df %>%
          dplyr::mutate(
            logical.count = stringr::str_replace(
              stringr::str_replace(logical.count, "FAL", "False"),
              "TRU", "True"
            )
          )
        keep_cols <- c(keep_cols,
                       "Mean" = "logical.mean",
                       "Count" = "logical.count")
      } else if (dtype == "complex") {
        keep_cols <- c(keep_cols, "Mean" = "complex.mean")
      } else if ((dtype == "Date") | (dtype == "POSIXct")) {
        keep_cols <- c(keep_cols,
                       "Minimum" = paste0(dtype, ".min"),
                       "Maximum" = paste0(dtype, ".max"),
                       "Median" = paste0(dtype, ".median"),
                       "# Unique" = paste0(dtype, ".n_unique"))
      }
      
      grouped <- !identical(attr(skim_out, "groups"), list())
      if (grouped) {
        keep_cols <- c(keep_cols[1],
                       purrr::map_chr(attr(skim_out, "groups"), 
                                      ~rlang::as_string(.x)),
                       keep_cols[2:length(keep_cols)])
      }
      
      caption <- paste("Summary of", stringr::str_to_title(dtype), "Variables")
      if (html) {
        tab_ls[[dtype]] <- pretty_DT(
          skim_df %>% dplyr::select(tidyselect::all_of(keep_cols)),
          caption = shiny::tags$caption(
            style = "color: black; font-weight: bold; font-size: 125%", caption
          ), 
          rownames = F,
          options = list(dom = if (nrow(skim_df) > 10) "tip" else "t", 
                         scrollX = TRUE),
          ...
        )
      } else {
        tab_ls[[dtype]] <- pretty_kable(
          skim_df %>% dplyr::select(tidyselect::all_of(keep_cols)), 
          caption = caption, format = "latex", row.names = FALSE, escape = TRUE, 
          ...
        )
      }
    }
  }
  return(tab_ls)
}



