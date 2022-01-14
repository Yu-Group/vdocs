#' veridical design - bootstrap HTML output format
#'
#' @description Format for converting from R Markdown to an HTML document
#'   using the veridical design theme. The veridical theme is largely based upon the
#'   material design theme from the `rmdformats` R package. See
#'   \url{https://github.com/juba/rmdformats} for the source code.
#'
#' @details
#' JavaScript and CSS taken and adapted from the Material design theme
#' for Bootstrap 3 project : \url{https://github.com/FezVrasta/bootstrap-material-design}.
#'
#' @inheritParams rmdformats::material
#' @inheritParams rmarkdown::html_document
#' @param lab_notebook Logical indicating whether the veridical theme is being
#'   used for a PCS lab notebook. If \code{TRUE}, the veridical theme will
#'   also load additional scripts to enable interactivity of the notebook.
#' @param ... Additional function arguments passed to `rmdformats::material()`.
#'
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#'
#' @export
veridical <- function(lab_notebook = FALSE,
                      fig_width = 10,
                      fig_height = 8,
                      number_sections = FALSE,
                      code_folding = "hide",
                      code_download = TRUE,
                      use_bookdown = TRUE,
                      includes = NULL,
                      fig_caption = TRUE,
                      highlight = "kate",
                      lightbox = TRUE,
                      thumbnails = TRUE,
                      gallery = FALSE,
                      cards = TRUE,
                      pandoc_args = NULL,
                      md_extensions = NULL,
                      mathjax = "rmdformats",
                      ...) {
  if (is.null(includes)) {
    header_files <- c(
      system.file("templates/veridical/header/veridicalHeader.html",
                  package = "vdocs"),
      system.file("templates/veridical/header/vdocsHeader.html",
                  package = "vdocs"),
      system.file("templates/veridical/header/tinymce.html",
                  package = "vdocs")
    )
    if (!lab_notebook) {
      header_files <- header_files[1]
    }
    includes <- rmarkdown::includes(before_body = header_files)
  }
  html_template(
    template_name = "material",
    template_path = "templates/material/material.html",
    template_dependencies = list(
      html_dependency_bootstrap_material(),
      html_dependency_material(),
      html_dependency_veridical(lab_notebook = lab_notebook)
    ),
    pandoc_args = pandoc_args,
    fig_width = fig_width,
    fig_height = fig_height,
    fig_caption = fig_caption,
    number_sections = number_sections,
    code_folding = code_folding,
    code_download = code_download,
    includes = includes,
    highlight = highlight,
    lightbox = lightbox,
    thumbnails = thumbnails,
    gallery = gallery,
    cards = cards,
    toc = TRUE,
    toc_depth = 1,
    use_bookdown = use_bookdown,
    md_extensions = md_extensions,
    mathjax = mathjax,
    ...
  )
}

#' bootstrap material design js and css
#'
#' @description Function copied from `rmdformats` since it is a non-exported
#'   function. Originally from
#'   \url{https://github.com/FezVrasta/bootstrap-material-design}
#'
#' @keywords internal
html_dependency_bootstrap_material <- function() {
  htmltools::htmlDependency(name = "bootstrap_material",
                            version = "0.1",
                            src = system.file("templates/material/lib",
                                              package = "rmdformats"),
                            script = c("material.min.js", "ripples.min.js"),
                            stylesheet = c("bootstrap-material-design.min.css",
                                           "ripples.min.css"))
}


#' material js and css
#'
#' @description Function copied from `rmdformats` since it is a non-exported
#'   function.
#'
#' @keywords internal
html_dependency_material <- function() {
  htmltools::htmlDependency(name = "material",
                            version = "0.1",
                            src = system.file("templates/material",
                                              package = "rmdformats"),
                            script = "material.js",
                            stylesheet = "material.css")
}

#' veridical dependencies
#'
#' @description Dependencies on top of base material design theme.
#'
#' @param lab_notebook Logical indicating whether the veridical theme is being
#'   used for a PCS lab notebook. If \code{TRUE}, the veridical theme will
#'   also load additional scripts to enable interactivity of the notebook.
#'
#' @keywords internal
html_dependency_veridical <- function(lab_notebook = FALSE) {
  if (lab_notebook) {
    scripts <- c("js/checkbox.js", "js/collapsibleInfo.js")
    stylesheets <- c("css/veridical.css", "css/checkbox.css")
  } else {
    scripts <- NULL
    stylesheets <- "css/veridical.css"
  }
  htmltools::htmlDependency(name = "veridical",
                            version = "0.1",
                            src = system.file("templates/veridical",
                                              package = "vdocs"),
                            script = scripts,
                            stylesheet = stylesheets)
}


