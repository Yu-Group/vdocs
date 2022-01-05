#' Create pretty tables for html and latex outputs.
#' 
#' @description Wrapper function around `simChef::pretty_DT()` and 
#'   `simChef::pretty_kable()` that automatically chooses to return a DT table 
#'   or a kable table depending on the output type (html or latex). 
#'   Specifically, if the requested output type is "html", a pretty DT table is 
#'   returned, and if the requested output type is "latex", a pretty kable table
#'   is returned.
#' 
#' @inheritParams simChef::pretty_DT
#' @param html Logical indicating whether the output should be in html format.
#'   If \code{FALSE}, output is in latex format.
#' @param html_options List of additional named arguments to pass to 
#'   `simChef::pretty_DT()`.
#' @param latex_options List of additional named arguments to pass to 
#'   `simChef::pretty_kable()`.
#' 
#' @returns A DT table in html format if `html = TRUE` and a kable table in 
#'   latex format if `html = FALSE`. In addition, if `return_df = TRUE`, the 
#'   data frame that was used to create the table is returned.
#'   
#' @export
prettyTable <- function(X, html = knitr::is_html_output(),
                        digits = 3, sigfig = TRUE, 
                        rownames = TRUE, caption = "", na_disp = "NA", 
                        bold_function = NULL, bold_margin = NULL, 
                        bold_scheme = TRUE, bold_color = NULL,
                        html_options = NULL, latex_options = NULL,
                        return_df = FALSE) {
  if (html) {
    tab_args <- list(X = X, digits = digits, sigfig = sigfig,
                     rownames = rownames, caption = caption, na_disp = na_disp,
                     bold_function = bold_function, bold_margin = bold_margin,
                     bold_scheme = bold_scheme, bold_color = bold_color,
                     return_df = return_df)
    tab <- do.call(simChef::pretty_DT, args = c(tab_args, html_options))
  } else {
    tab_args <- list(X = X, digits = digits, sigfig = sigfig,
                     row.names = rownames, caption = caption, na_disp = na_disp,
                     bold_function = bold_function, bold_margin = bold_margin,
                     bold_scheme = bold_scheme, bold_color = bold_color,
                     return_df = return_df, format = "latex")
    tab <- do.call(simChef::pretty_kable, args = c(tab_args, latex_options))
  }
  return(tab)
}
