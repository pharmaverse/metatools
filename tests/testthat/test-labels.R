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
         metacore::select_dataset("DM")
   )
   dm <- haven::read_xpt(metatools_example("dm.xpt"))

   # Set the variable labels
   dm_labeled <- set_variable_labels(dm, mc)

   # Pull out the data to check against
   labs <- purrr::map_chr(names(dm_labeled), ~ attr(dm_labeled[[.]], 'label'))
   attr(labs, 'label') <- 'Variable Label' # This is labelled in the metacore object

   expect_equal(labs, mc$var_spec$label)
})
