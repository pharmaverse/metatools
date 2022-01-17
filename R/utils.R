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


#' Convert metacore objec to just a single dataset
#'
#'
#' @param metacore metacore object, which may or may not be subsetted
#' @param dataset_name name of datasets which may or may not be null. If it is
#'   not null then it will be used to subset.
#'
#' @return metacore object
#' @noRd
make_lone_dataset <- function(metacore, dataset_name){
   if(!(nrow(metacore$ds_spec) == 1 | !is.null(dataset_name))){
      stop("Requires either a subsetted metacore object or a dataset name")
   }
   if(!is.null(dataset_name)){
      metacore <- select_dataset(metacore, dataset_name)
   }
   metacore

}
