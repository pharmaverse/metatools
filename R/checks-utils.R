#' Resolve a column name for CT checks
#'
#' Converts a user-supplied variable (bare name or string) into a validated
#' character column name.
#'
#' @param data A data.frame to check against.
#' @param var Column name, supplied as a bare name or string.
#'
#' @return A character string containing the validated column name.
#'
#' @noRd
resolve_var <- function(data, var = NULL) {
   if (!is.data.frame(data)) {
      cli::cli_abort(c("x" = "Argument {.arg data} must be a data frame"))
   }

   # Capture both string and symbols
   var_str <- as.character(rlang::ensym(var))

   if (length(var_str) == 0 || !nzchar(var_str)) {
      cli::cli_abort(c("x" = "Argument {.arg var} must be provided as a string or bare column name"))
   }

   if (!var_str %in% names(data)) {
      cli::cli_abort(c("x" = "Column {.var {var_str}} not found in dataset"))
   }

   var_str
}

#' Build controlled terminology evaluation context
#'
#' Constructs a standardised context object used for controlled terminology
#' (CT) and value-level metadata (VLM) validation.
#'
#' @param data A dataset to validate.
#' @param metacore A metacore object containing dataset metadata.
#' @param var Column name (character string; already resolved).
#' @param na_acceptable Logical indicating whether missing values are allowed,
#'   or `NULL` to infer from CDISC "core" metadata.
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{Input dataset}
#'   \item{metacore}{Metacore object}
#'   \item{var}{Validated column name}
#'   \item{vlm}{Logical; whether variable has value-level metadata}
#'   \item{core}{CDISC core requirement for the variable}
#'   \item{na_ok}{Logical; whether NA/blank values are permitted}
#' }
#'
#' @noRd
ct_context <- function(data, metacore, var, na_acceptable = NULL) {
   # Resolve tidy-evaluated variable name into a character column name
   col_name <- rlang::as_label({{ var }}) |>
      stringr::str_remove_all("\"")

   # Extract CDISC "core" requirement (e.g. Required / Expected / Permissible)
   core <- metacore$ds_vars |>
      dplyr::filter(.data$variable == col_name) |>
      dplyr::pull(.data$core)

   attr(core, "label") <- NULL

   # Extract value_spec to check for VLM (>1)
   value_spec <- metacore$value_spec |>
      dplyr::filter(.data$variable == col_name)

   # Determine whether missing values are allowed:
   # - If user supplies explicit override, use it
   # - Otherwise infer from CDISC core requirement
   na_ok <- if (is.null(na_acceptable)) {
      !identical(core, "Required")
   } else {
      na_acceptable
   }

   list(
      data = data,
      metacore = metacore,
      var = col_name,
      vlm = nrow(value_spec) > 1,
      core = core,
      na_ok = na_ok
   )
}

#' Extract VLM where-clauses for a variable
#'
#' Retrieves all value-level metadata (VLM) where-clauses associated with a
#' given variable from the metacore specification.
#'
#' @param metacore A metacore object containing metadata definitions.
#' @param var Variable name (character).
#'
#' @return A character vector of unique, non-NA where-clauses.
#'
#' @details
#' - Removes duplicates
#' - Removes NA values
#' - Does not validate clause syntax (handled downstream)
#'
#' @noRd
vlm_clauses <- function(metacore, var) {

   vs <- metacore$value_spec

   vs |>
      filter(variable == var) |>
      pull(where) |>
      unique() |>
      na.omit()
}

#' Compile a VLM where-clause into an evaluable expression
#'
#' Parses a CDISC-style VLM where-clause and converts it into an R expression
#' suitable for use in dplyr filtering operations.
#'
#' @param where_clause Character string specifying a VLM rule in the form
#'   `"VAR OP VALUE"`.
#' @param var Variable name (currently unused, reserved for future alignment).
#'
#' @return An R language object representing the condition, or `NULL` if the
#'   clause cannot be parsed or contains an unsupported operator.
#'
#' @details
#' Supports the following operators:
#' \itemize{
#'   \item EQ, =, ==
#'   \item NE, !=
#'   \item GT, LT, GE, LE
#'   \item IN, NOTIN
#' }
#'
#' Multi-value expressions (IN / NOTIN) are parsed into vector membership
#' checks.
#'
#' Unsupported operators are skipped with a warning.
#'
#' @noRd
compile_vlm_clause <- function(where_clause, var) {

   # Split clause into components
   parts <- str_split(where_clause, "\\s+", simplify = TRUE)

   var_name <- toupper(parts[1])
   op <- toupper(parts[2])

   # Reconstruct value portion (supports multi-word values)
   val <- paste(parts[3:length(parts)], collapse = " ") |> trimws()

   # Map CDISC operators to R equivalents
   op_map <- c(
      EQ = "==", "=" = "==", "==" = "==",
      NE = "!=", "!=" = "!=",
      GT = ">", LT = "<",
      GE = ">=", LE = "<=",
      IN = "%in%",
      NOTIN = "!%in%"
   )

   op_resolved <- unname(op_map[op])

   # Fail gracefully for unsupported operators
   if (is.na(op_resolved)) {
      cli_warn(c(
         "x" = "Unsupported operator {.val {op}} in clause {.val {where_clause}}",
         "i" = "Clause will be skipped"
      ))
      return(NULL)
   }

   # ---- IN / NOTIN handling ----
   # These require vector membership logic rather than scalar comparison
   if (op %in% c("IN", "NOTIN")) {

      vals <- parse_vlm_values(val)

      expr <- expr(.data[[!!var_name]] %in% !!vals)

      # NOTIN becomes negated membership test
      if (op == "NOTIN") {
         expr <- expr(! (!!expr))
      }

      return(expr)
   }

   # ---- Scalar comparison handling ----
   # Attempt numeric conversion; fallback to character comparison
   is_num <- suppressWarnings(!is.na(as.numeric(val)))
   val <- if (is_num) as.numeric(val) else val

   call2(
      op_resolved,
      expr(.data[[!!var_name]]),
      val
   )
}

#' Parse a VLM IN / NOTIN value list into a character vector
#'
#' Converts the value portion of a CDISC-style `IN` or `NOTIN` where-clause
#' into a character vector suitable for membership testing.
#'
#' @param val Character string containing the raw value list extracted from a
#'   VLM where-clause (e.g. `"c(A, B, C)"` or `"(A, B, C)"`).
#'
#' @return A character vector of individual values.
#'
#' @details
#' Two input formats are accepted:
#' \itemize{
#'   \item **Valid R syntax** — strings of the form `c(...)` are parsed
#'     directly via [rlang::parse_expr()]; symbols are coerced to character.
#'   \item **Bare / corrected syntax** — parentheses are stripped and the
#'     remaining text is split on commas or whitespace runs, with empty strings
#'     discarded.
#' }
#'
#' @noRd
parse_vlm_values <- function(val) {

   val <- trimws(val)

   # VLM passed with correct syntax, already c(...)
   if (grepl("^c\\s*\\(.*\\)$", val)) {

      expr <- rlang::parse_expr(val)
      vals <- as.list(expr)[-1]

      out <- vapply(vals, function(x) {
         if (is.symbol(x)) {
            as.character(x)
         } else {
            rlang::as_string(x)
         }
      }, character(1))

      return(out)
   }

   # Try to correct invalid syntax
   vals <- stringr::str_remove_all(val, "[()]")
   vals <- stringr::str_split(vals, "[,\\s]+")[[1]]
   vals <- vals[vals != ""]

   vals
}

#' Evaluate a single VLM clause against dataset and controlled terminology
#'
#' Applies a single VLM where-clause to the dataset, compares observed values
#' against allowed controlled terminology, and returns any violations.
#'
#' @param ctx A CT evaluation context created by `ct_context()`.
#' @param where_clause Character string containing a VLM rule expression.
#'
#' @return
#' `NULL` if no violations are found; otherwise a list containing:
#' \describe{
#'   \item{clause}{The evaluated where-clause}
#'   \item{bad_values}{Vector of values not permitted by CT}
#' }
#'
#' @details
#' - Safely evaluates the clause against the dataset
#' - Retrieves allowed controlled terminology for the clause context
#' - Compares observed values against allowed codes
#' - Returns only violations (clean results are dropped)
#'
#' @noRd
eval_vlm_clause <- function(ctx, where_clause) {

   # Convert clause string into executable filter expression
   expr <- compile_vlm_clause(where_clause, ctx$var)
   if (is.null(expr)) return(NULL)

   # Apply filter safely (guard against malformed expressions)
   subset_data <- tryCatch(
      filter(ctx$data, !!expr),
      error = function(e) {
         cli_warn(c(
            "x" = "Failed to evaluate VLM clause {.val {where_clause}}",
            "i" = "Skipping clause for variable {.var {ctx$var}}"
         ))
         return(NULL)
      }
   )

   # Skip empty subsets (no rows match condition)
   if (is.null(subset_data) || nrow(subset_data) == 0) {
      return(NULL)
   }

   # Retrieve controlled terminology for this clause context
   ct <- get_control_term(ctx$metacore, !!ctx$var, where = where_clause)
   check <- pull(ct, code)

   # Standardised NA handling
   if (ctx$na_ok) {
      check <- if (is.character(check)) {
         c(check, NA_character_, "")
      } else {
         c(check, NA)
      }
   }

   # Identify values not present in allowed CT set
   vals <- pull(subset_data, .data[[ctx$var]])
   bad <- unique(vals[!vals %in% check])

   # Return NULL for clean results
   if (!length(bad)) return(NULL)

   list(
      clause = where_clause,
      bad_values = bad
   )
}

#' Summarise VLM evaluation results
#'
#' Aggregates results from multiple VLM clause evaluations and produces a
#' structured summary of controlled terminology violations.
#'
#' @param results A list of clause-level evaluation results returned by
#'   `eval_vlm_clause()`.
#' @param var Variable name being evaluated.
#'
#' @return
#' A named list where each element corresponds to a VLM clause and contains
#' the values that violate controlled terminology rules. Returns an empty list
#' if no violations are found.
#'
#' @details
#' - Groups violations by VLM clause
#' - Formats a consolidated CLI warning message
#' - Returns structured output for programmatic use
#'
#' @noRd
summarise_vlm_results <- function(results, var) {

   # No violations → return empty structure
   if (!length(results)) return(list())

   # Build named list: clause → bad values
   bad_vals <- setNames(
      lapply(results, `[[`, "bad_values"),
      paste0("Codelist: ", vapply(results, `[[`, character(1), "clause"))
   )

   # Format human-readable warning message
   msg <- unlist(lapply(names(bad_vals), function(nm) {
      vals <- bad_vals[[nm]]
      paste0(
         nm, ": ",
         toString(cli::format_inline("{.val {vals}}"))
      )
   }))

   # Emit single consolidated warning
   cli::cli_warn(c(
      "x" = "Invalid controlled terminology detected",
      "i" = "Variable: {.var {var}}",
      setNames(msg, rep("i", length(msg))),
      ""
   ))

   bad_vals
}

#' Run VLM evaluation pipeline
#'
#' Executes all VLM where-clauses for a variable by evaluating each clause
#' against the dataset and controlled terminology rules.
#'
#' @param ctx A CT evaluation context created by `ct_context()`.
#' @param where_clauses Character vector of VLM where-clauses.
#'
#' @return A list of non-NULL clause evaluation results representing VLM
#' violations. If no violations are found, returns an empty list.
#'
#' @details
#' Pipeline flow:
#' context → clauses → compile → evaluate → aggregate
#'
#' @noRd
run_vlm_pipeline <- function(ctx, where_clauses) {

   purrr::map(where_clauses, function(clause) {
      eval_vlm_clause(ctx, clause)
   }) |>
      purrr::compact()
}

check_vars_in_data <- function(vars, vars_name, data) {
   missing_vars <- setdiff(vars, names(data))
   if (length(missing_vars) > 1) {
      cli_warn(c(
         "!" = "Not all variables from {.arg {vars_name}} are in the data and will not be checked",
         "i" = "Variables not present in the data: {.val {missing_vars}}",
         ""
      ),
      call. = FALSE
      )
   }
   return(setdiff(vars, missing_vars))
}

#' Print Messages to Console
#'
#' This function prints formatted messages to the console, either as errors (stopping
#' execution) or as warnings. It is designed as a helper function to provide informative
#' messages during validation checks.
#'
#' @param messages A character vector of messages to be printed. Each element corresponds
#'   to a separate message.
#' @param data_list A list of character vectors. Each element in the list corresponds
#'   to a message in `messages` and provides associated data (e.g., column names).
#'   If an element in `messages` has no corresponding data, include a `NULL`.
#' @param strict A logical value indicating whether to print messages as
#'   errors (\code{TRUE}, default) or warnings (\code{FALSE}).
#'
#' @details The function constructs a formatted message string including the calling
#' function's name, the individual messages provided in `messages`, and associated data
#' from `data_list`. The function uses \code{switch} to call either `stop()` or `warning()`
#' based on `strict` and prints the full message string to the console.
#'
#' @return None. The function's primary purpose is its side effect of printing a message.
#' It does not return a meaningful value.
#'
#' @noRd
#'
print_to_console <- function(messages, data_list, strict = TRUE) {
   calling_function <- paste(deparse(sys.call(-1)), collapse = " ")
   output_string <- paste0("In: [", calling_function, "]")

   for (i in seq_along(messages)) {
      message <- paste0(messages[i], ": ",
                        paste(data_list[[i]], collapse = ", "),
                        sep = "\n"
      )

      output_string <- paste(output_string, message, sep = "\n\n")
   }

   options(deparse.max.lines = 2000L)
   switch(as.character(strict),
          "TRUE"  = cli::cli_abort(c(output_string), call = NULL),
          "FALSE" = cli::cli_warn(c(output_string), call = NULL)
   )
}
