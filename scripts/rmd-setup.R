knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  cache = TRUE,
  fig.align = "center",
  fig.pos = "H",
  fig.show = "hold",
  comment = "#>"
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
                       options$code)
      }
    }
  )
  knitr::opts_hooks$set(
    interactive_text = function(options) {
      options$echo <- FALSE
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

