#' Prepare a variable name for CT evaluation
#'
#' Resolves a user-supplied variable (either unquoted or quoted) into a
#' standard character column name and performs basic validation required
#' for controlled terminology (CT) checks.
#'
#' This function is intended as a lightweight helper used at the start of
#' CT validation pipelines to ensure the requested variable exists and is
#' properly formatted for downstream processing.
#'
#' @param data Data to check.
#' @param metacore A metacore object containing dataset metadata.
#' @param var A column name provided either as:
#'   - an unquoted variable name (tidy evaluation), or
#'   - a string (e.g. "ARM")
#'
#' @return A character string giving the resolved column name.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Validates that `metacore` is a valid dataset-level object
#'   \item Converts `var` into a character column name
#'   \item Ensures the variable argument is not empty
#'   \item Checks that the column exists in the dataset
#' }
#'
#' @seealso
#' \code{\link{verify_DatasetMeta}}
#'
#' @examples
#' \dontrun{
#' prepare_ct_check(metacore, ARM)
#' prepare_ct_check(metacore, "ARM")
#' }
#'
#' @noRd
prepare_ct_check <- function(data, metacore, var) {
   verify_DatasetMeta(metacore)

   # Check data is supplied
   if (!is.data.frame(data)) {
      cli::cli_abort(c(
         "x" = "Argument {.arg data} must be a dataframe"
      ))
   }

   # Check argument was supplied
   if (missing(var)) {
      cli::cli_abort(c(
         "x" = "Argument {.arg var} must be provided"
      ))
   }

   # Capture NSE or string safely
   var_expr <- rlang::enexpr(var)
   var <- rlang::as_string(rlang::ensym(var_expr))

   # Basic validation
   if (length(var) == 0 || !nzchar(var)) {
      cli::cli_abort(c(
         "x" = "Argument {.arg var} must resolve to at least one column name"
      ))
   }

   # Check column exists
   if (!var %in% names(data)) {
      cli::cli_abort(c(
         "x" = "Column {.var {var}} not found in dataset"
      ))
   }
   var
}

#' Build controlled terminology evaluation context
#'
#' Creates a standardized evaluation context used across controlled
#' terminology (CT) and value-level metadata (VLM) validation.
#'
#' This function centralises:
#' - dataset variable resolution
#' - metadata lookup for CDISC "core" requirement
#' - NA handling rules
#' - shared objects required by downstream pipeline functions
#'
#' @param data A dataset to validate.
#' @param metacore A metacore object containing dataset metadata.
#' @param var Column name.
#' @param na_acceptable Logical. If `NULL`, NA handling is inferred from
#'   the variable's CDISC "core" requirement.
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{Input dataset}
#'   \item{metacore}{Metacore object}
#'   \item{var}{Resolved variable name (character)}
#'   \item{core}{CDISC core requirement for variable}
#'   \item{na_ok}{Logical indicating whether NA/blank values are allowed}
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
      dplyr::filter(.data$variable == var) |>
      dplyr::pull(.data$where) |>
      unique() |>
      stats::na.omit()
}

#' Compile a VLM where-clause into a filter expression
#'
#' Converts a CDISC-style where clause string into an R expression
#' suitable for evaluation in `dplyr::filter()`.
#'
#' @param where_clause Character string in format:
#'   "VAR OP VALUE"
#'
#' @param var Optional variable override (currently unused but reserved
#'   for future alignment with external variable mapping systems).
#'
#' @return An R expression or `NULL` if the clause cannot be parsed.
#'
#' @details
#' Supported operators:
#' EQ, NE, GT, LT, GE, LE, IN, NOTIN
#'
#' Examples:
#' - "AGE GE 18"
#' - "SEX IN c('M','F')"
#'
#' @noRd
compile_vlm_clause <- function(where_clause, var) {

   # Split clause into components
   parts <- stringr::str_split(where_clause, "\\s+", simplify = TRUE)

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
      cli::cli_warn(c(
         "x" = "Unsupported operator {.val {op}} in clause {.val {where_clause}}",
         "i" = "Clause will be skipped"
      ))
      return(NULL)
   }

   # ---- IN / NOTIN handling ----
   # These require vector membership logic rather than scalar comparison
   if (op %in% c("IN", "NOTIN")) {

      vals <- parse_vlm_values(val)

      expr <- rlang::expr(.data[[!!var_name]] %in% !!vals)

      # NOTIN becomes negated membership test
      if (op == "NOTIN") {
         expr <- rlang::expr(! (!!expr))
      }

      return(expr)
   }

   # ---- Scalar comparison handling ----
   # Attempt numeric conversion; fallback to character comparison
   is_num <- suppressWarnings(!is.na(as.numeric(val)))
   val <- if (is_num) as.numeric(val) else val

   rlang::call2(
      op_resolved,
      rlang::expr(.data[[!!var_name]]),
      val
   )
}

#' Evaluate a single VLM clause against dataset and CT rules
#'
#' Executes a single VLM rule:
#' 1. Compiles the where-clause into a filter expression
#' 2. Subsets the dataset
#' 3. Retrieves controlled terminology (CT)
#' 4. Compares observed values against allowed CT values
#'
#' @param ctx A CT evaluation context created by `ct_context()`.
#' @param where_clause A single VLM where-clause string.
#'
#' @return
#' - `NULL` if no violations are found or clause is invalid
#' - A list with:
#'   - clause: original where-clause
#'   - bad_values: vector of invalid values
#'
#' @noRd
eval_vlm_clause <- function(ctx, where_clause) {

   # Convert clause string into executable filter expression
   expr <- compile_vlm_clause(where_clause, ctx$var)
   if (is.null(expr)) return(NULL)

   # Apply filter safely (guard against malformed expressions)
   subset_data <- tryCatch(
      dplyr::filter(ctx$data, !!expr),
      error = function(e) {
         cli::cli_warn(c(
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
   ct <- get_control_term(ctx$metacore, ctx$var, where = where_clause)
   check <- dplyr::pull(ct, code)

   # Standardised NA handling across CT systems
   if (ctx$na_ok) {
      check <- if (is.character(check)) {
         c(check, NA_character_, "")
      } else {
         c(check, NA)
      }
   }

   # Identify values not present in allowed CT set
   vals <- dplyr::pull(subset_data, .data[[ctx$var]])
   bad <- unique(vals[!vals %in% check])

   # Return NULL for clean results (keeps pipeline simple)
   if (!length(bad)) return(NULL)

   list(
      clause = where_clause,
      bad_values = bad
   )
}

#' Summarise VLM evaluation results and emit warning
#'
#' Aggregates results from multiple VLM clause evaluations and
#' generates a structured warning message if violations exist.
#'
#' @param results A list of clause evaluation outputs from
#'   `eval_vlm_clause()`.
#' @param var Variable name being evaluated.
#'
#' @return A named list of bad values grouped by clause,
#'   or an empty list if no violations exist.
#'
#' @details
#' - Collapses clause-level results into named structure
#' - Emits a single consolidated CLI warning
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
      vals <- paste0("'", bad_vals[[nm]], "'", collapse = ", ")
      paste0(nm, ": ", vals)
   }))

   # Emit single consolidated warning
   cli::cli_warn(c(
      "x" = "Invalid controlled terminology detected",
      "i" = "Variable: {var}",
      setNames(msg, rep("i", length(msg)))
   ))

   bad_vals
}

#' Run full VLM evaluation pipeline
#'
#' Executes all VLM clauses for a variable by:
#' 1. Iterating over each where-clause
#' 2. Evaluating clause against dataset + CT rules
#' 3. Removing NULL (non-violations)
#'
#' @param ctx CT evaluation context from `ct_context()`.
#' @param where_clauses Character vector of VLM where-clauses.
#'
#' @return A list of clause-level violation results.
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
