# Suppress cli output during testing
options(cli.default_handler = function(...) { })

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

   expect_equal(attr(x$mpg, 'label'), "Miles Per Gallon")
   expect_equal(attr(x$cyl, 'label'), "Cylinders")
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
      mc <- metacore::spec_to_metacore(metacore::metacore_example("p21_mock.xlsx"), quiet=TRUE) %>%
         metacore::select_dataset("DM", quiet = TRUE)
   )
   dm <- haven::read_xpt(metatools_example("dm.xpt"))

   # Set the variable labels
   dm_labeled <- set_variable_labels(dm, mc)

   # Pull out the data to check against
   labs <- purrr::map_chr(names(dm_labeled), ~ attr(dm_labeled[[.]], 'label'))
   attr(labs, 'label') <- 'Variable Label' # This is labelled in the metacore object

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

test_that("removal_labels works to remvoe all labels", {
   data <- tibble::tibble(a = 1:5,
                          b = letters[1:5])
   data_lab <- data %>%
      purrr::map2_dfr(c("apple", "pear"), function(x, y ){
         attr(x, "label") <- y
         x
      })
   remove_labels(data_lab) %>%
      expect_equal(., data)

   expect_error(remove_labels(c(1:10)))
})
test_that("set_variable_labels verbose parameter", {
  load(metacore::metacore_example("pilot_SDTM.rda"))
  spec <- metacore %>% select_dataset("DM", quiet = TRUE)
  
  dm <- haven::read_xpt(metatools_example("dm.xpt"))
  
  # Get the actual variables in the metadata
  meta_vars <- spec$var_spec$variable
  data_vars <- names(dm)
  
  # Find a variable that exists in metadata to remove
  var_to_remove <- intersect(meta_vars, data_vars)[1]
  
  # Create mismatch: add a variable not in metadata, remove a variable that is in metadata
  dm_mismatch <- dm %>%
    select(-all_of(var_to_remove)) %>%  # Remove a variable that's in metadata
    mutate(EXTRAVAR = "test")  # Add a variable not in metadata
  
  # Test verbose = "message" or "warn" - should show warnings about mismatches
  expect_warning(
    set_variable_labels(dm_mismatch, spec, verbose = "message"),
    "Variables in"
  )
  
  expect_warning(
    set_variable_labels(dm_mismatch, spec, verbose = "warn"),
    "Variables in"
  )
  
  # Test verbose = "silent" - suppress all warnings
  expect_silent(
    result_silent <- set_variable_labels(dm_mismatch, spec, verbose = "silent")
  )
  
  # Verify all verbose levels return same result (labels applied the same way)
  result_message <- suppressWarnings(
    set_variable_labels(dm_mismatch, spec, verbose = "message")
  )
  
  result_warn <- suppressWarnings(
    set_variable_labels(dm_mismatch, spec, verbose = "warn")
  )
  
  expect_equal(result_message, result_warn)
  expect_equal(result_message, result_silent)
  
  # Verify labels were actually applied to variables that exist in both
  common_vars <- intersect(names(result_message), meta_vars)
  if (length(common_vars) > 0) {
    expect_true(!is.null(attr(result_message[[common_vars[1]]], "label")))
  }
  
  # Test with perfect match - no warnings with any verbose level
  # Only keep variables that are in metadata
  dm_matched <- dm %>% select(all_of(intersect(names(dm), meta_vars)))
  
  expect_silent(
    set_variable_labels(dm_matched, spec, verbose = "message")
  )
  
  expect_silent(
    set_variable_labels(dm_matched, spec, verbose = "warn")
  )
  
  expect_silent(
    set_variable_labels(dm_matched, spec, verbose = "silent")
  )
  
  # Test invalid verbose value
  expect_error(
    set_variable_labels(dm, spec, verbose = "invalid"),
    "'arg' should be one of"
  )
})