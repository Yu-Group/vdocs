data_types <- function(Xtrain, ytrain) {
  skim_out <- skimr::skim(cbind(`.y` = ytrain, Xtrain))
  dtypes_df <- skim_out %>%
    dplyr::mutate(`.group` = ifelse(skim_variable == ".y", "y", "X"),
                  skim_type = as.factor(skim_type)) %>%
    dplyr::group_by(`.group`) %>%
    dplyr::summarise(Freq = c(table(skim_type))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Class = capitalize(names(Freq))) %>%
    tidyr::spread(key = "Class", value = "Freq") %>%
    tibble::column_to_rownames(".group")
  dt_out <- DT::datatable(
    dtypes_df, rownames = T,
    caption = shiny::tags$caption(
      style = "color: black; font-weight: bold; font-size: 125%",
      "Frequency of Column Types"
    ), 
    options = list(
      columnDefs = list(list(className = "dt-center",
                             targets = "_all")),
      dom = "t"
    )
  )
  return(dt_out)
}

data_summary <- function(Xtrain, ytrain, digits = 2, sigfig = FALSE) {
  skim_out <- skimr::skim(cbind(`.y` = ytrain, Xtrain))
  dt_ls <- list()
  types <- c("factor", "numeric", "character", "logical", "complex", "Date",
             "POSIXct")
  for (type in types) {
    if (sum(skim_out$skim_type == type) > 0) {
      dt_ls[[type]] <- skim_wrapper(skim_out, type, digits, sigfig)
    }
  }
  return(dt_ls)
}

skim_wrapper <- function(skim_out, dtype, digits, sigfig) {
  if (sigfig) {
    sigfig <- "g"
  } else {
    sigfig <- "f"
  }
  
  skim_df <- skim_out %>%
    dplyr::filter(skim_type == dtype) %>%
    dplyr::mutate(complete_rate = formatC((1 - complete_rate) * 100, 
                                          digits = digits,
                                          format = sigfig, flag = "#"))
  
  keep_cols <- c("Variable Name" = "skim_variable",
                 "# Missing" = "n_missing",
                 "% Missing" = "complete_rate")
  
  if (dtype == "factor") {
    skim_df <- skim_df %>%
      dplyr::mutate(factor.ordered = tolower(factor.ordered) %>%
                      R.utils::capitalize())
    keep_cols <- c(keep_cols,
                   "Ordered Factor" = "factor.ordered",
                   "# Unique Factors" = "factor.n_unique",
                   "Top Factor Counts" = "factor.top_counts")
  } else if (dtype == "numeric") {
    skim_df <- skim_df %>%
      dplyr::mutate(dplyr::across(c("numeric.mean", "numeric.sd", "numeric.p0",
                                    "numeric.p25", "numeric.p50",
                                    "numeric.p75", "numeric.p100"),
                                  formatC, digits = digits, format = sigfig, 
                                  flag = "#"))
    keep_cols <- c(keep_cols,
                   "Mean" = "numeric.mean",
                   "SD" = "numeric.sd",
                   "Minimum" = "numeric.p0",
                   "Q1" = "numeric.p25",
                   "Median" = "numeric.p50",
                   "Q3" = "numeric.p75",
                   "Maximum" = "numeric.p100",
                   "Histogram" = "numeric.hist")
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
  
  if (nrow(skim_df) > 10) {
    dt_dom <- "tip"
  } else {
    dt_dom <- "t"
  }
  dt_out <- DT::datatable(
    skim_df %>% dplyr::select(keep_cols),
    caption = shiny::tags$caption(
      style = "color: black; font-weight: bold; font-size: 125%",
      paste("Summary of", R.utils::capitalize(dtype), "Variables")
    ), 
    rownames = F,
    options = list(
      columnDefs = list(list(className = "dt-center", targets = "_all")),
      dom = dt_dom,
      scrollX = TRUE
    )
  )
  return(dt_out)
}


