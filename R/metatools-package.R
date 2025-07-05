#' @keywords internal
#' @family internal
#'
#' @importFrom cli cli_inform cli_text cli_warn
#' @importFrom dplyr across add_count all_of anti_join any_of arrange as_tibble
#'   bind_cols bind_rows case_when distinct everything filter full_join group_by
#'   group_split if_else inner_join left_join mutate pick pull rename select
#' @importFrom lifecycle deprecated deprecate_warn is_present
#' @importFrom metacore get_control_term get_keys select_dataset verify_DatasetMeta
#' @importFrom purrr discard keep map map2 map_dfr map_lgl map pmap_dfr reduce
#'   safely walk2
#' @importFrom rlang !! := as_label as_name enexpr expr is_named list2 set_names
#'   sym
#' @importFrom stringr str_c str_count str_detect str_extract str_match_all str_remove
#'   str_remove_all str_trim str_to_lower str_to_upper str_split
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider unnest
#' @importFrom utils capture.output
#'
"_PACKAGE"
