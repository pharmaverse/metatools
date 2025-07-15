#' Handle Deprecation of the `dataset_name` argument
#'
#' This handler outputs the lifecycle deprecation information for the `dataset_name`
#' argument that was deprecated in metatools 0.2.0 and will be removed in a future
#' release.
#'
#' @param what What is being deprecated i.e., the calling function.
#' @noRd
handle_deprecate_dataset_name <- function(what) {
   lifecycle::deprecate_warn(
      when = "0.2.0",
      what = what,
      details = cli_inform(c("i" = col_red("The {.arg dataset_name} argument will be removed in a future release.
      Please use {.fn metacore::select_dataset} to subset the {.obj metacore} object to obtain
      metadata for a single dataset.")))
   )
}
