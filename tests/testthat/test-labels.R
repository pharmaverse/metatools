# Suppress cli output during testing
options(cli.default_handler = function(...) {})

# Mock up metacore data
starwars_short <- dplyr::starwars %>% select(1:5)
var_spec <- tibble::tibble(
  variable = names(starwars_short),
  length = rep(1, 5),
  label = c("Name", "Height", "Mass", "Hair Color", "Skin Color"),
  type = c("text", "int", "int", "int", "text"),
  format = rep(NA_character_, 5),
  common = rep(FALSE, 5)
)

ds_spec <- tibble::tibble(
  dataset = "Starwars",
  structure = c(""),
  label = "Star Wars"
)

ds_vars <- tibble::tibble(
  dataset = rep("Starwars", 5),
  variable = names(starwars_short),
  order = 1:5,
  keep = rep(TRUE, 5),
  key_seq = 1:5,
  core = rep(NA_character_, 5),
  supp_flag = rep(FALSE, 5)
)

value_spec <- tibble::tibble(
  dataset = character(0),
  variable = character(0),
  type = character(0),
  origin = character(0),
  sig_dig = character(0),
  code_id = character(0),
  where = character(0),
  derivation_id = character(0)
)

derivations <- tibble::tibble(
  derivation_id = character(0),
  derivation = character(0)
)

code_id <- tibble::tibble(
  code_id = character(0),
  name = character(0),
  type = character(0),
  codes = list()
)

# This is loud and I don't want it - just need the metacore object
mc <- suppressWarnings(
  suppressMessages(
    metacore::metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, code_id)
  )
)

test_that("Check that add_labels applies labels properly", {
  x <- mtcars %>%
    add_labels(
      mpg = "Miles Per Gallon",
      cyl = "Cylinders"
    )

  expect_equal(attr(x$mpg, "label"), "Miles Per Gallon")
  expect_equal(attr(x$cyl, "label"), "Cylinders")
})

test_that("Check that add_labels errors properly", {
  expect_error(add_labels(TRUE, x = "label"))
  expect_error(add_labels(mtcars, "label"))
  expect_error(add_labels(mtcars, bad = "label"))
  expect_error(add_labels(mtcars, mpg = 1))
})

test_that("set_variable_labels applies labels properly", {
  # Load in the metacore test object and example data
  suppressMessages(
    mc <- metacore::spec_to_metacore(metacore::metacore_example("p21_mock.xlsx"), quiet = TRUE) %>%
      metacore::select_dataset("DM", quiet = TRUE)
  )
  dm <- haven::read_xpt(metatools_example("dm.xpt"))

  # Set the variable labels
  dm_labeled <- set_variable_labels(dm, mc)

  # Pull out the data to check against
  labs <- purrr::map_chr(names(dm_labeled), ~ attr(dm_labeled[[.]], "label"))
  attr(labs, "label") <- "Variable Label" # This is labelled in the metacore object

  expect_equal(labs, mc$var_spec$label)
})

test_that("set_variable_labels raises warnings properly", {
  # This is metadata for the dplyr::starwars dataset
  mc <- suppressWarnings(
    suppressMessages(
      metacore::metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, code_id)
    )
  ) %>% select_dataset("Starwars", quiet = TRUE)

  starwars_short2 <- starwars_short
  starwars_short2$new_var <- ""

  # Variables in data not in metadata
  expect_warning(set_variable_labels(starwars_short2, mc))

  mc <- suppressWarnings(
    suppressMessages(
      metacore::metacore(ds_spec, ds_vars[1:4, ], var_spec[1:4, ], value_spec, derivations, code_id) %>%
        metacore::select_dataset("Starwars", quiet = TRUE)
    )
  )
  expect_warning(set_variable_labels(starwars_short, mc))
})

test_that("set_variable_labels respects verbose parameter", {
  load(metacore::metacore_example("pilot_SDTM.rda"))
  spec <- metacore |> select_dataset("DM", quiet = TRUE)
  dm <- haven::read_xpt(metatools_example("dm.xpt"))
  
  # Create data with mismatch to trigger warnings
  dm_mismatch <- dm |> 
    select(-RACE) |>  
    mutate(EXTRAVAR = "test")
  
  # verbose = "silent" suppresses warnings
  expect_silent(
    set_variable_labels(dm_mismatch, spec, verbose = "silent")
  )
  
  # verbose = "message" shows warnings  
  expect_warning(
    set_variable_labels(dm_mismatch, spec, verbose = "message"),
    "Variables in"
  )
  
  # verbose = "warn" shows warnings
  expect_warning(
    set_variable_labels(dm_mismatch, spec, verbose = "warn"),
    "Variables in"
  )
  
  # Invalid verbose value errors
  expect_error(
    set_variable_labels(dm, spec, verbose = "invalid"),
    "should be one of"
  )
})

test_that("remove_labels removes labels properly", {
  # Add labels first
  x <- mtcars |>
    add_labels(
      mpg = "Miles Per Gallon",
      cyl = "Cylinders"
    )
  
  # Verify labels exist
  expect_equal(attr(x$mpg, "label"), "Miles Per Gallon")
  expect_equal(attr(x$cyl, "label"), "Cylinders")
  
  # Remove labels
  x_no_labels <- remove_labels(x)
  
  # Verify labels are gone
  expect_null(attr(x_no_labels$mpg, "label"))
  expect_null(attr(x_no_labels$cyl, "label"))
})

test_that("remove_labels errors on invalid input", {
  expect_error(
    remove_labels("not a dataframe"),
    "Labels must be removed from a data.frame or tibble"
  )
})