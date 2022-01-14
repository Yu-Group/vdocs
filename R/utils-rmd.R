#' Subchunkify R chunks in R Markdown.
#' 
#' @description Helper function that splits up a single written R chunk (in R
#'   Markdown) into multiple subchunks. This can be used to enable plotting
#'   multiple plots of different sizes and captions from within a single written 
#'   R code chunk in R Markdown.
#'
#' @param g Plot or object to display in R Markdown output.
#' @param i Chunk id. Should be unique for each plot or subchunk.
#' @param fig_height Height of plot or output of subchunk (in inches). See 
#'   \code{fig.height} in the knitr chunk options.
#' @param fig_width Width of plot or output of subchunk (in inches). See 
#'   \code{fig.width} in the knitr chunk options.
#' @param caption Figure or subchunk caption. Should be surrounded by two sets 
#'   of quotes, e.g., "'This is a valid caption.'" See \code{fig.cap} in the
#'   knitr chunk options.
#' @param add_class Vector of css classes to add to object.
#' @param other_args Other arguments to pass to knitr chunk header. Should be
#'   surrounded by two sets of quotes, e.g., "results='asis'".
#' 
#' @references Adapted from \href{http://michaeljw.com/blog/post/subchunkify/}{Michael James Williams}.
#'
#' @export
subchunkify <- function(g, i, fig_height = 12, fig_width = 10, caption = "''",
                        add_class = NULL, other_args = "") {
  
  g_deparsed <- paste0(deparse(function() {g}), collapse = '')
  
  if (!identical(other_args, "")) {
    if (!startsWith(other_args, ",")) {
      other_args <- paste0(", ", other_args)
    }
  }
  
  sub_chunk <- paste0("\n```{r subchunk_", i, ", fig.height=", fig_height, 
                      ", fig.width=", fig_width, ", fig.cap=", caption,
                      ", echo=FALSE", other_args, "}", "\n(", g_deparsed, ")()",
                      "\n```\n")
  
  if (!is.null(add_class)) {
    cat(sprintf("<div class='%s'>", paste(add_class)))
  }
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
  if (!is.null(add_class)) {
    cat("</div>")
  }
}
