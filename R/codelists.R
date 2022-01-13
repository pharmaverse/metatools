
#' Dash to Equation
#'
#' Converts strings that are #-# style to a logical expression (but in a string format)
#' @param string
#'
#' @return string
#' @noRd
#' @importFrom stringr str_extract str_detect
#' @importFrom dplyr if_else
dash_to_eq <- function(string){
   front <- str_extract(string, "^.*(?=\\-)")
   front_eq <- if_else(str_detect(front, "<|>|="), front, paste0(">=", front))
   back <- str_extract(string, "(?<=\\-).*$")
   back_eq <- if_else(str_detect(back, "<|>|="), back, paste0("<=", back))
   paste0("x", front_eq, " & x", back_eq)
}


#' Create Subgroups
#'
#' @param ref_vec vector of numeric values
#' @param grp_defs vector of strings with groupings defined. Format must be
#'   either: <00, >=00, 00-00, or  00-<00
#'
#' @return Character vector of the values in the subgroups
#' @export
#' @importFrom  stringr str_detect str_c str_count
#' @importFrom purrr map reduce keep
#' @importFrom  dplyr case_when
#'
#' @examples
#' create_subgrps(c(1:10), c("<2", "2-5", ">5"))
#' create_subgrps(c(1:10), c("<=2", ">2-5", ">5"))
#' create_subgrps(c(1:10), c("<2", "2-<5", ">=5"))
create_subgrps <- function(ref_vec, grp_defs){
   if(!is.numeric(ref_vec)){
      stop("ref_vec must be numeric")
   }

   equations<- case_when(
      str_detect(grp_defs, "-") ~ paste0("function(x){if_else(", dash_to_eq(grp_defs), ", '",grp_defs, "','')}"),
      str_detect(grp_defs, "^(<\\s?=|>\\s?=|<|>)\\s?\\d+") ~ paste0("function(x){if_else(x", grp_defs, ",'", grp_defs, "', '')}"),
      TRUE ~ NA_character_
   )

   if(all(!is.na(equations))){
      functions <- equations %>%
         map(~eval(parse(text = .)))
      out <- functions %>%
         map(~.(ref_vec)) %>%
         reduce(str_c)
   }else {
      stop("Unable to deciver all groups please update and try again")
   }
   all_options <- str_c(grp_defs, collapse = "|")
   too_many_grps <- str_count(out, all_options) %>%
      keep(~. > 1)
   if(length(too_many_grps) > 0){
      stop("Grouping is not exclusive. Please look at the groups and try again")
   }
   out
}



#' Create Variable from Codelist
#'
#' This functions uses code/decode pairs from a metacore object to create new
#' variables in the data
#'
#' @param data dataset that contains the input variable
#' @param metacore a metacore object to get the codelist from. If the `out_var`
#'   has different codelists for different datasets the metacore object will
#'   need to be subsetted using `select_dataset` from the metacore package.
#' @param input_var Name of the variable that will be translated for the new
#'   column
#' @param out_var Name of the output variable. Note: the codelist will always be
#'   the codelist associates with `out_var`
#' @param decode_to_code Direction of the translation. By default assumes the
#'   `input_var` is the decode column of the codelist. Set to `FALSE` if the
#'   `input_var` is the code column of the codelist
#'
#' @return Dataset with a new column added
#' @export
#'
#' @importFrom rlang !! enquo enexpr as_label set_names :=
#' @importFrom dplyr left_join rename
#'
#' @examples
#' library(metacore)
#' data <- tribble(
#'    ~USUBJID, ~VAR1,  ~VAR2,
#'    1,         "M",    "Male",
#'    2,         "F",    "Female",
#'    3,         "F",    "Female",
#'    4,         "U",    "Unknown",
#'    5,         "M",    "Male",
#' )
#' var_from_codelist(data, spec, SEX, VAR2)
#' var_from_codelist(data, spec, SEX, VAR1, decode_to_code = FALSE)
var_from_codelist <- function(data, metacore, input_var, out_var,
                              decode_to_code = TRUE){
   code_translation <- get_control_term(metacore, !!enquo(out_var))
   input_var_str <- ifelse(mode(enexpr(input_var)) == "character",
                           input_var, as_label(enexpr(input_var)))
   if(decode_to_code){
      data %>%
         left_join(code_translation, by =  set_names("decode", input_var_str)) %>%
         rename(!!enquo(out_var) := code)
   }else if (!decode_to_code){
      data %>%
         left_join(code_translation, by =  set_names("code", input_var_str)) %>%
         rename(!!enquo(out_var) := decode)
   } else {
      stop("decode_to_code must be either TRUE or FALSE")
   }
}



#' Create Categorical Variable from Codelist
#'
#' Using the grouping from either the `decode_var` or `code_var` and a reference
#' variable (`ref_var`) it will create a categorical variable and the numeric
#' version of that categorical variable.
#'
#' @param data dataset with reference variable in it
#' @param metacore_subset a metacore object to get the codelist from. If the
#'   variable has different codelists for different datasets the metacore object
#'   will need to be subsetted using `select_dataset` from the metacore package.
#' @param ref_var Name of variable to be used as the reference i.e AGE when
#'   creating AGEGR1
#' @param grp_var Name of the new grouped variable
#' @param num_grp_var Name of the new numeric decode for the grouped variable.
#'   This is optional if no value given no variable will be created
#' @importFrom rlang enquo !! enexpr :=
#' @importFrom dplyr %>% pull mutate
#'
#' @return dataset with new column added
#' @export
#' @examples
#' library(metacore)
#' library(haven)
#' spec <- define_to_MetaCore(metacore_example("ADaM_define.xml")) %>%
#'  select_dataset("ADSL")
#' dm <- read_xpt(pkg_example("dm.xpt"))
#' # Grouping Column Only
#' create_cat_var(dm, spec, AGE, AGEGR1)
#' # Grouping Column and Numeric Decode
#' create_cat_var(dm, spec, AGE, AGEGR1, AGEGR1N)
#'
create_cat_var <- function(data, metacore_subset, ref_var, grp_var,
                           num_grp_var = NULL){
   grp_defs <- get_control_term(metacore_subset, !!enquo(grp_var)) %>%
      pull(decode)

   out <- data %>%
      mutate(!!enquo(grp_var) := create_subgrps(!!enquo(ref_var), grp_defs))

   if(!is.null(enexpr(num_grp_var))){
      out <- out %>%
         var_from_codelist(metacore_subset, !!enquo(grp_var), !!enquo(num_grp_var))
   }
   out
}



