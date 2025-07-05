
#' Dash to Equation
#'
#' Converts strings that are #-# style to a logical expression (but in a string format)
#' @param string
#'
#' @return string
#' @noRd
#' @importFrom stringr str_extract str_detect
#' @importFrom dplyr if_else
dash_to_eq <- function(string) {
   front <- str_extract(string, "^.*(?=\\-)")
   front_eq <- if_else(str_detect(front, "<|>|="), front, paste0(">=", front))
   back <- str_extract(string, "(?<=\\-).*$")
   back_eq <- if_else(str_detect(back, "<|>|="), back, paste0("<=", back))
   paste0("x", front_eq, " & x", back_eq)
}


#' Create Subgroups
#'
#' @param ref_vec Vector of numeric values
#' @param grp_defs Vector of strings with groupings defined. Format must be
#'   either: <00, >=00, 00-00, or  00-<00
#' @param grp_labs Vector of strings with labels defined. The labels correspond
#'   to the associated `grp_defs`. i.e., "12-17" may translate to "12-17 years".
#'   If no `grp_labs` specified then `grp_defs` will be used.
#'
#' @return Character vector of the values in the subgroups
#' @export
#' @importFrom stringr str_detect str_c str_count
#' @importFrom purrr map reduce keep map_chr
#' @importFrom dplyr case_when
#' @importFrom cli cli_abort cli_warn qty cli_text cli_bullets
#'
#' @examples
#' create_subgrps(c(1:10), c("<2", "2-5", ">5"))
#' create_subgrps(c(1:10), c("<=2", ">2-5", ">5"))
#' create_subgrps(c(1:10), c("<2", "2-<5", ">=5"))
#' create_subgrps(c(1:10), c("<2", "2-<5", ">=5"), c("<2 years", "2-5 years", ">=5 years"))
create_subgrps <- function(ref_vec, grp_defs, grp_labs = NULL) {
   if (!is.numeric(ref_vec)) { cli_abort("ref_vec must be numeric") }
   if (is.null(grp_labs)) { grp_labs <- grp_defs }

   # Create equations used to derive the subgroups
   equations <- case_when(
      str_detect(grp_defs, "-") ~ paste0("function(x){if_else(", dash_to_eq(grp_defs), ", '", grp_labs, "', '')}"),
      str_detect(grp_defs, "^(<\\s?=|>\\s?=|<|>)\\s?\\d+") ~ paste0("function(x){if_else(x", grp_defs, ",'", grp_labs, "', '')}"),
      TRUE ~ NA_character_
   )

   # Apply equations
   if (all(!is.na(equations))) {
      functions <- equations %>%
         map(~ eval(parse(text = .)))
      out <- functions %>%
         map(~ .(ref_vec)) %>%
         reduce(str_c) %>%
         replace(. == "", NA)
   } else {
      na_index <- which(is.na(equations))
      bad_defs <- grp_defs[na_index]
      cli_abort(paste("Unable to decipher the following group definition{?s}: {bad_defs}.",
                      "Please check your controlled terminology."))
   }
   # Find non-exclusive subgroups i.e., values that have been mapped to two groups
   non_excl <- out |>
      discard(is.na) |>
      map(~ grp_labs[str_detect(.x, grp_labs)]) |>
      keep(~ length(.) > 1) |>
      unique()

   # Throw error if groups are not exclusive
   if (length(non_excl) > 0) {
      msg <- map_chr(non_excl, ~ {
         items <- paste(.x, collapse = ", ")
      }) %>%
         paste0(seq_along(.), ". ", .)

      cli_abort(c(
         "Group definitions are not exclusive. Please check your controlled terminology",
         "The following group definitions overlap:",
         msg
      ))
   }
   out
}



#' Create Variable from Codelist
#'
#' This functions uses code/decode pairs from a metacore object to create new
#' variables in the data
#'
#' @param data Dataset that contains the input variable
#' @param metacore A metacore object to get the codelist from. If the `out_var`
#'   has different codelists for different datasets the metacore object will
#'   need to be subsetted using `select_dataset` from the metacore package.
#' @param input_var Name of the variable that will be translated for the new
#'   column
#' @param out_var Name of the output variable. Note: the grouping will always be
#'   from the code of the codelist associates with `out_var`
#' @param decode_to_code Direction of the translation. By default assumes the
#'   `input_var` is the decode column of the codelist. Set to `FALSE` if the
#'   `input_var` is the code column of the codelist
#'
#' @return Dataset with a new column added
#' @export
#'
#' @importFrom rlang enexpr as_label set_names := as_name
#' @importFrom dplyr left_join rename
#' @importFrom metacore get_control_term
#' @importFrom stringr str_remove_all
#'
#' @examples
#' library(metacore)
#' library(tibble)
#' data <- tribble(
#'   ~USUBJID, ~VAR1, ~VAR2,
#'   1, "M", "Male",
#'   2, "F", "Female",
#'   3, "F", "Female",
#'   4, "U", "Unknown",
#'   5, "M", "Male",
#' )
#' spec <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
#' create_var_from_codelist(data, spec, VAR2, SEX)
#' create_var_from_codelist(data, spec, "VAR2", "SEX")
#' create_var_from_codelist(data, spec, VAR1, SEX, decode_to_code = FALSE)
create_var_from_codelist <- function(data, metacore, input_var, out_var,
                                     decode_to_code = TRUE) {
   code_translation <- get_control_term(metacore, {{ out_var }})
   input_var_str <- as_label(enexpr(input_var)) %>%
      str_remove_all("\"")
   if (is.vector(code_translation) | !("decode" %in% names(code_translation))) {
      stop("Expecting 'code_decode' type of control terminology. Please check metacore object")
   }
   if (decode_to_code) {
      out <- data %>%
         left_join(code_translation, by = set_names("decode", input_var_str)) %>%
         rename({{ out_var }} := code)
      if(all(str_detect(code_translation$code, "^\\d*$"))){
         out <- out %>%
            mutate({{ out_var }} := as.numeric({{ out_var }}))
      }
   } else if (!decode_to_code) {
      out <- data %>%
         left_join(code_translation, by = set_names("code", input_var_str)) %>%
         rename({{ out_var }} := decode)
      if(all(str_detect(code_translation$decode, "^\\d*$"))){
         out <- out %>%
            mutate({{ out_var }} := as.numeric({{ out_var }}))
      }
   } else {
      stop("decode_to_code must be either TRUE or FALSE")
   }
   out
}




#' Create Categorical Variable from Codelist
#'
#' Using the grouping from either the `decode_var` or `code_var` and a reference
#' variable (`ref_var`) it will create a categorical variable and the numeric
#' version of that categorical variable.
#'
#' @param data Dataset with reference variable in it
#' @param metacore A metacore object to get the codelist from. If the
#'   variable has different codelists for different datasets the metacore object
#'   will need to be subsetted using `select_dataset` from the metacore package.
#' @param ref_var Name of variable to be used as the reference i.e AGE when
#'   creating AGEGR1
#' @param grp_var Name of the new grouped variable
#' @param num_grp_var Name of the new numeric decode for the grouped variable.
#'   This is optional if no value given no variable will be created
#' @param create_from_decode Sets the `decode` column of the codelist as the column
#'   from which the variable will be created. By default the column is `code`.
#' @importFrom rlang enexpr := as_character enquo
#' @importFrom dplyr %>% pull mutate
#' @importFrom metacore get_control_term
#'
#' @return dataset with new column added
#' @export
#' @examples
#' library(metacore)
#' library(haven)
#' library(dplyr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' dm <- read_xpt(metatools_example("dm.xpt")) %>%
#'   select(USUBJID, AGE)
#' # Grouping Column Only
#' create_cat_var(dm, spec, AGE, AGEGR1)
#' # Grouping Column and Numeric Decode
#' create_cat_var(dm, spec, AGE, AGEGR1, AGEGR1N)
create_cat_var <- function(data, metacore, ref_var, grp_var, num_grp_var = NULL,
                           create_from_decode = FALSE, strict = TRUE) {
   ct <- get_control_term(metacore, {{ grp_var }})
   if (is.vector(ct) | !("decode" %in% names(ct))) {
      cli_abort("Expecting 'code_decode' type of control terminology. Please check metacore object")
   }

   # Assign group definitions and labels
   grp_defs <- pull(ct, code)
   grp_labs <- if (create_from_decode) pull(ct, decode) else grp_defs

   out <- data %>%
      mutate({{ grp_var }} := create_subgrps({{ ref_var }}, grp_defs, grp_labs))

   if (!is.null(enexpr(num_grp_var))) {
      out <- out %>%
         create_var_from_codelist(metacore, {{ grp_var }}, {{ num_grp_var }})
   }

   missing <- out |> pull({{ grp_var }}) |> is.na() |> which()|> length()
   if (strict && missing > 0) {
      cli_warn(paste(
         "There {qty(missing)} {?is/are} {missing} {qty(missing)} observation{?s}",
         "in {as_name(enquo(ref_var))} that {qty(missing)} {?does/do} not fit into",
         "the provided categories for {as_name(enquo(grp_var))}. Please check your",
         "controlled terminology.")
      )
   }
   out
}


#' Convert Variable to Factor with Levels Set by Control Terms
#'
#' This functions takes a dataset, a metacore object and a variable name. Then
#' looks at the metacore object for the control terms for the given variable and
#' uses that to convert the variable to a factor with those levels. If the
#' control terminology is a code list, the code column will be used. The
#' function fails if the control terminology is an external library
#' @param data A dataset containing the variable to be modified
#' @param metacore A metacore object to get the codelist from. If the
#'   variable has different codelists for different datasets the metacore object
#'   will need to be subsetted using `select_dataset` from the metacore package
#' @param var Name of variable to change
#' @importFrom rlang as_label enexpr as_name
#' @importFrom stringr str_remove_all
#' @importFrom dplyr mutate
#'
#' @return Dataset with variable changed to a factor
#' @export
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(dplyr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' dm <- read_xpt(metatools_example("dm.xpt")) %>%
#'   select(USUBJID, SEX, ARM)
#' # Variable with codelist control terms
#' convert_var_to_fct(dm, spec, SEX)
#' # Variable with permitted value control terms
#' convert_var_to_fct(dm, spec, ARM)
convert_var_to_fct <- function(data, metacore, var) {
   code_translation <- get_control_term(metacore, {{ var }})
   var_str <- as_label(enexpr(var)) %>%
      str_remove_all("\"")
   if (is.vector(code_translation)) {
      levels <- code_translation
   } else if ("code" %in% names(code_translation)) {
      levels <- code_translation$code
   } else {
      stop("We currently don't have the ability to use external libraries")
   }
   if (!var_str %in% names(data)) {
      stop(paste(var_str, "cannot be found in the dataset. Please create variable before converting to factor"))
   }
   data %>%
      mutate({{ var }} := factor({{ var }}, levels = levels))
}

