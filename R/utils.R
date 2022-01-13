#' Get path to pkg example
#'
#' pkg comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' pkg_example()
#' pkg_example("dm.xpt")
pkg_example <- function(file = NULL) {
   if (is.null(file)) {
      dir(system.file("extdata", package = "plaster"))
   } else {
      system.file("extdata", file, package = "plaster", mustWork = TRUE)
   }
}
