options(width = 10000)
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  fig.align = "center",
  fig.pos = "H",
  fig.show = "hold",
  comment = "#>"
)

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
