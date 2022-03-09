#' veridical design - bootstrap HTML output format
#'
#' @description Format for converting from R Markdown to an HTML document
#'   using the veridical design theme. The veridical theme is a wrapper around
#'   the vmodern design theme from the `vthemes` R package with the difference
#'   that the veridical theme is tailored specifically for the PCS lab
#'   notebook and thus adds interactive capabilities such as clickable
#'   checkboxes, editable textboxes, and collapsible tips. Note that the vmodern
#'   design theme and hence the veridical design theme are largely based upon
#'   the material design theme from the `rmdformats` R package. See
#'   \url{https://github.com/juba/rmdformats} for the source code.
#'
#' @details
#' JavaScript and CSS taken and adapted from the Material design theme
#' for Bootstrap 3 project : \url{https://github.com/FezVrasta/bootstrap-material-design}.
#'
#' @inheritParams vthemes::vmodern
#' @inheritParams rmarkdown::html_document
#' @param ... Additional function arguments passed to `vthemes::vmodern()`.
#'
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#'
#' @export
veridical <- function(fig_width = 10,
                      fig_height = 8,
                      number_sections = FALSE,
                      code_folding = "hide",
                      code_download = TRUE,
                      use_bookdown = TRUE,
                      includes = NULL,
                      css = NULL,
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
      system.file("templates/vmodern/header/vmodern.html",
                  package = "vthemes"),
      system.file("templates/veridical/header/veridical.html",
                  package = "vdocs"),
      system.file("templates/veridical/header/tinymce.html",
                  package = "vdocs"),
      system.file("templates/veridical/header/saveResponses.html",
                  package = "vdocs")
    )
    includes <- rmarkdown::includes(before_body = header_files)
  }
  if (is.null(css)) {
    css <- system.file("templates/veridical/css/checkbox.css",
                       package = "vdocs")
  }

  vthemes::vmodern(
    fig_width = fig_width,
    fig_height = fig_height,
    number_sections = number_sections,
    code_folding = code_folding,
    code_download = code_download,
    use_bookdown = use_bookdown,
    includes = includes,
    css = css,
    fig_caption = fig_caption,
    highlight = highlight,
    lightbox = lightbox,
    thumbnails = thumbnails,
    gallery = gallery,
    cards = cards,
    pandoc_args = pandoc_args,
    md_extensions = md_extensions,
    mathjax = mathjax,
    ...
  )
}
