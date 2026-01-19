#' Apply labels to multiple variables on a data frame
#'
#' This function allows a user to apply several labels to a dataframe at once.
#'
#' @param data A data.frame or tibble
#' @param ... Named parameters in the form of variable = 'label'
#'
#' @return data with variable labels applied
#'
#' @export
#'
#' @examples
#' add_labels(
#'   mtcars,
#'   mpg = "Miles Per Gallon",
#'   cyl = "Cylinders"
#' )
#'
add_labels <- function(data, ...) {
  # Pull out ellipsis to list
  args <- list2(...)

  # Check params
  if (!inherits(data, "data.frame")) stop("Labels must be applied to a data.frame or tibble")
  if (!is_named(args)) stop("Must provide variable name and label as named arguments")
  if (!all(names(args) %in% names(data))) {
    stop("All variable names supplied to label must be variables in data")
  }
  if (!all(map_lgl(args, is.character))) stop("All labels must be character")

  # Iterate the args supplied and update the variable labels in place
  walk2(names(args), args, ~ {
    attr(data[[.x]], "label") <<- .y
  })

  data
}


#' Remove labels to multiple variables on a data frame
#'
#' This function allows a user to removes all labels to a dataframe at once.
#'
#' @param data A data.frame or tibble
#'
#' @return data with variable labels applied
#' @export
#'
#' @examples
#' library(haven)
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' remove_labels(data)
#'
remove_labels <- function(data) {
  # Check data
  if (!inherits(data, "data.frame")) stop("Labels must be removed from a data.frame or tibble")

  map_dfr(data, function(x) {
    attr(x, "label") <- NULL
    x
  })
}

#' Apply labels to a data frame using a metacore object
#'
#' This function leverages metadata available in a metacore object to apply
#' labels to a data frame.
#'
#' @param data A dataframe or tibble upon which labels will be applied
#' @param metacore metacore object that contains the specifications for the
#'   dataset of interest.
#' @param dataset_name `r lifecycle::badge("deprecated")` Optional string to
#'   specify the dataset that is being built. This is only needed if the metacore
#'   object provided hasn't already been subsetted.\cr
#'   Note: Deprecated in version 0.2.0. The `dataset_name` argument will be removed
#'   in a future release. Please use `metacore::select_dataset` to subset the
#'   `metacore` object to obtain metadata for a single dataset.
#' @param verbose Character string controlling message verbosity. One of:
#'   \describe{
#'     \item{`"message"`}{Show both warnings and messages (default)}
#'     \item{`"warn"`}{Show warnings but suppress messages}
#'     \item{`"silent"`}{Suppress all warnings and messages}
#'   }
#'
#' @return Dataframe with labels applied
#' @export
#'
#' @examples
#'
#' mc <- metacore::spec_to_metacore(
#'   metacore::metacore_example("p21_mock.xlsx"),
#'   quiet = TRUE
#' )
#' dm <- haven::read_xpt(metatools_example("dm.xpt"))
#' set_variable_labels(dm, mc, dataset_name = "DM")
set_variable_labels <- function(data, metacore, dataset_name = deprecated(),
                                verbose = c("message", "warn", "silent")) {
  if (is_present(dataset_name)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "check_unique_keys(dataset_name)",
      details = cli_inform(c("i" = col_red("The {.arg dataset_name} argument will be removed in a future release.
      Please use {.fn metacore::select_dataset} to subset the {.obj metacore} object to obtain
      metadata for a single dataset.")))
    )
    metacore <- make_lone_dataset(metacore, dataset_name)
  }
  verify_DatasetMeta(metacore)

  verbose <- validate_verbose(verbose)

  # Grab out the var names and labels
  var_spec <- metacore$var_spec %>%
    select(variable, label)


  ns <- var_spec$variable
  labs <- var_spec$label
  dns <- names(data)

  # Are there any variables in data not in the metadata
  in_meta <- setdiff(ns, dns) # Variables in metadata not in data
  in_data <- setdiff(dns, ns) # Variables in data not in metadata

  if (length(in_meta) > 0 && check_warn(verbose)) {
    wrn <- paste0("Variables in metadata not in data:\n\t", paste0(in_meta, collapse = "\n\t"))
    warning(wrn, call. = FALSE)
  }

  if (length(in_data) > 0 && check_warn(verbose)) {
    wrn <- paste0("Variables in data not in metadata:\n\t", paste0(in_data, collapse = "\n\t"))
    warning(wrn, call. = FALSE)
  }

  # Pick out only the variables which exist in both and build list
  match <- intersect(ns, dns)
  ind <- which(ns %in% match)

  # Subset and create a named list
  ns <- ns[ind]
  labs <- labs[ind]
  names(labs) <- ns
  labs <- as.list(labs)

  # Apply the labels to the data
  args <- append(list(data), labs)
  do.call(add_labels, args)
}
