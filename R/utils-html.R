#' Shared HTML template function
#'
#' @description Function copied from `rmdformats` since it is a non-exported
#'   function
#'
#' @keywords internal
html_template <- function(
  template_name,
  template_path,
  template_dependencies,
  pandoc_args,
  ...) {

  args <- list(...)
  ## For compatibility with pkgdown
  args$template <- NULL

  code_folding <- args[["code_folding"]]
  code_folding <- ifelse(is.null(code_folding), "none", code_folding)
  code_download <- args[["code_download"]]
  code_download <- ifelse(is.null(code_download), FALSE, code_download)
  code_menu <- !identical(code_folding, "none") || code_download

  ## js and css dependencies
  extra_dependencies <- c(
    list(
      rmarkdown::html_dependency_jquery(),
      rmarkdown::html_dependency_jqueryui(),
      html_dependency_navigation(
        code_menu = code_menu,
        source_embed = code_download
      ),
      html_dependency_bootstrap("bootstrap"),
      html_dependency_magnific_popup()
    ),
    template_dependencies
  )
  ## Merge "extra_dependencies"
  if ("extra_dependencies" %in% names(args)) {
    extra_dependencies <- append(extra_dependencies, args[["extra_dependencies"]])
    args[["extra_dependencies"]] <- NULL
    args[["mathjax"]] <- NULL
  }

  ## Force mathjax arguments
  if (!is.null(args[["mathjax"]])) {
    pandoc_args <- c(pandoc_args,
                     "--mathjax",
                     "--variable", paste0("mathjax-url:", default_mathjax()))
  }
  ## Other arguments
  pandoc_args <- c(pandoc_args,
                   "--variable", paste0(template_name, ":true"))
  if (args[["lightbox"]]) {
    pandoc_args <- c(pandoc_args, "--variable", "lightbox:true")
  }
  if (args[["thumbnails"]]) {
    pandoc_args <- c(pandoc_args, "--variable", "thumbnails:true")
  }
  if (args[["gallery"]]) {
    pandoc_args <- c(pandoc_args, "--variable", "gallery:true")
  } else {
    pandoc_args <- c(pandoc_args, "--variable", "gallery:false")
  }
  if (!is.null(args[["cards"]])) {
    if (args[["cards"]]) {
      pandoc_args <- c(pandoc_args, "--variable", "cards:true")
    }
  }
  ## downcute default style
  if (!is.null(args[["default_style"]])) {
    if (args[["default_style"]] == "dark") {
      toggler_checked <- "checked"
    } else {
      toggler_checked <- ""
    }
    pandoc_args <- c(pandoc_args, "--variable", paste0("dark_toggler_status:", toggler_checked))
  }
  ## downcute theme
  if (!is.null(args[["downcute_theme"]])) {
    pandoc_args <- c(pandoc_args, "--variable", paste0("downcute_theme:", args[["downcute_theme"]]))
  }


  ## Call rmarkdown::html_document
  html_document_args <- list(
    template = system.file(template_path, package = "rmdformats"),
    extra_dependencies = extra_dependencies,
    pandoc_args = pandoc_args
  )
  html_document_args <- append(html_document_args, args)
  if (args[["use_bookdown"]]) {
    html_document_func <- bookdown::html_document2
  } else {
    html_document_func <- rmarkdown::html_document
  }

  do.call(html_document_func, html_document_args)

}

#' create an html dependency for Magnific popup
#'
#' @description Function copied from `rmdformats` since it is a non-exported
#'   function
#'
#' @keywords internal
html_dependency_magnific_popup <- function() {
  htmltools::htmlDependency(name = "magnific-popup",
                            version = "1.1.0",
                            src = system.file("templates/magnific-popup-1.1.0", package = "rmdformats"),
                            script = "jquery.magnific-popup.min.js",
                            stylesheet = "magnific-popup.css")
}


#' create an html dependency for bootstrap (function copied from rmarkdown)
#'
#' @description Function copied from `rmdformats` since it is a non-exported
#'   function
#'
#' @keywords internal
html_dependency_bootstrap <- function(theme = "bootstrap") {
  htmltools::htmlDependency(name = "bootstrap",
                            version = "3.3.7",
                            src = system.file("templates/bootstrap-3.3.7", package = "rmdformats"),
                            meta = list(viewport = "width=device-width, initial-scale=1"),
                            script = c(
                              "js/bootstrap.min.js"
                              # These shims are necessary for IE 8 compatibility
                              #"shim/html5shiv.min.js",
                              #"shim/respond.min.js"
                            ),
                            stylesheet = paste("css/", theme, ".min.css", sep = ""))
}

#' Mathjax (functions copied from rmarkdown)
#'
#' @description Function copied from `rmdformats` since it is a non-exported
#'   function
#'
#' @keywords internal
default_mathjax <- function() {
  paste0("https://mathjax.rstudio.com/latest/", mathjax_config())
}
mathjax_config <- function() {
  "MathJax.js?config=TeX-AMS-MML_HTMLorMML"
}

#' Navigation dependency
#'
#' @description Function copied from `rmdformats` since it is a non-exported
#'   function
#'
#' @keywords internal
html_dependency_navigation <- function(code_menu = TRUE, source_embed = FALSE) {
  # dynamically build script list
  script <- c("tabsets.js")
  if (code_menu)
    script <- c(script, "codefolding.js")
  if (source_embed)
    script <- c(script, "FileSaver.min.js", "sourceembed.js")
  htmltools::htmlDependency(name = "navigation",
                            version = "1.1",
                            src = system.file("templates/navigation-1.1", package = "rmdformats"),
                            script = script)
}



