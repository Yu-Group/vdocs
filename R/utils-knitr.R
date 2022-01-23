#' Knitr options setup for vdocs Rmd template
#'
#' @description Helper function to set up knitr options and hooks for the
#'   vdocs Rmarkdown template.
#'
#' @param echo Logical indicating whether to display the source code in the
#'   output document.
#' @param warning Logical indicating whether to display warnings in the
#'   output document.
#' @param message Logical indicating whether to display messages in the
#'   output document.
#' @param cache Logical indicating whether to cache code chunks.
#' @param collapse Logical indicating whether to collapse all the source and
#'   output blocks from one code chunk into a single block.
#' @param fig.align Alignment of figures in the output document. Possible values
#'   are "default", "left", "right", and "center".
#' @param fig.pos Character string for the figure position arrangement to be
#'   used in `\\begin{figure}[]`.
#' @param fig.show How to show/arrange the plots. Possible values are "asis",
#'   "hold", "animate", and "hide".
#' @param comment Character string. Prefix to be added before each line of the
#'   text output.
#' @param ... Additional knitr options to set via `knitr::opts_chunk$set()`.
#'   See \url{https://yihui.org/knitr/options/} for possible options.
#'
#' @export
vdocs_knitr_setup <- function(echo = TRUE,
                             warning = FALSE,
                             message = FALSE,
                             cache = FALSE,
                             collapse = TRUE,
                             fig.align = "center",
                             fig.pos = "H",
                             fig.show = "hold",
                             comment = "#>",
                             ...) {
  knitr::opts_chunk$set(
    echo = echo,
    warning = warning,
    message = message,
    cache = cache,
    collapse = collapse,
    fig.align = fig.align,
    fig.pos = fig.pos,
    fig.show = fig.show,
    comment = comment,
    ...
  )

  if (knitr::is_html_output()) {
    options(width = 10000)

    # knitr hook for interactive textboxes (tinyMCE) and collapsible help sections
    knitr::knit_hooks$set(
      interactive_text = function(before, options) {
        if (before) {
          out <- sprintf('<textarea class="tinymce-text">%s</textarea><br>',
                         options$code)
        }
      },
      help = function(before, options) {
        if (before) {
          out <- sprintf('<div class="help-info">\n>%s\n</div>',
                         paste(options$code, collapse = "\n>"))
        }
      }
    )
    knitr::opts_hooks$set(
      interactive_text = function(options) {
        options$echo <- FALSE
        options$cache <- FALSE
        return(options)
      },
      help = function(options) {
        options$echo <- FALSE
        return(options)
      }
    )
  } else if (knitr::is_latex_output()) {
    knitr::knit_hooks$set(
      # knitr hook for styled textboxes
      interactive_text = function(before, options) {
        if (before) {
          out <- sprintf('\\begin{tcolorbox} %s \\end{tcolorbox}',
                         options$code)
        }
      },
      # hack to fix missing new line after some subsections
      add_new_line = function(before, options) {
        if (before) {
          out <- "\\phantom{.}"
        }
      }
    )
    knitr::opts_hooks$set(
      interactive_text = function(options) {
        options$echo <- FALSE
        return(options)
      },
      help = function(options) {
        # don't show help info/tips in pdf output
        options$echo <- FALSE
        options$eval <- FALSE
        return(options)
      }
    )
  }
}

#' Load in saved responses in vdocs Rmd template
#'
#' @description Helper function to load in previously saved responses into vdocs
#'   Rmarkdown html report.
#'
#' @export
load_saved_responses <- function() {
  if (file.exists("responses.zip")) {
    utils::unzip("responses.zip", exdir = "responses")
    checkboxes <- readLines(file.path("responses", "checkboxes.txt"),
                            warn = FALSE)
    cat(sprintf('<div id="checkbox-data" style="display: none;">%s</div>',
                checkboxes))
    for (fname in list.files("responses", pattern = "^mce\\_.*\\.html$")) {
      mce_id <- substr(fname, start = 1, stop = nchar(fname) - 5)
      textbox <- paste(readLines(file.path("responses", fname), warn = FALSE),
                       collapse = "\n")
      cat(sprintf('<div id="%s-data" style="display: none;">%s</div>',
                  mce_id, textbox))
    }
    unlink("responses/", recursive = TRUE)
  } else {
    cat('<div id="checkbox-data" style="display: none;"></div>')
    for (mce_id in paste0("mce_", 0:23)) {
      cat(sprintf('<div id="%s-data" style="display: none;"></div>', mce_id))
    }
  }
}
