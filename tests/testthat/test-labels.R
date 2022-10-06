
# Mock up metacore data
var_spec <- tibble::tibble(
   variable = names(iris),
   length = rep(1, 5),
   label = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width", "Species"),
   type = c("float", "float", "float", "float", "text"),
   format = rep(NA_character_, 5),
   common = rep(FALSE, 5)
)

ds_spec <- tibble::tibble(
   dataset = "Iris",
   structure = c(""),
   label = "Iris"
)

ds_vars <- tibble::tibble(
   dataset = rep("Iris", 5),
   variable = names(iris),
   order = 1:5,
   keep = rep(TRUE, 5),
   key_seq = 1:5,
   core = rep(NA_character_, 5),
   supp_flag = rep(FALSE, 5)
)

value_spec <- tibble::tibble(
   dataset = character(0),
   variable = character(0),
   origin = character(0),
   type = character(0),
   code_id = character(0),
   derivation_id = character(0),
   where = character(0)
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

test_that("set_variable_labels raises warnings properly", {
   # This is metadata for the Iris dataset
   mc <- suppressWarnings(
      suppressMessages(
         metacore::metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, code_id)
      )
   )

   iris2 <- iris
   iris2$new_var <- ""

   expect_warning(set_variable_labels(iris2, mc))

   mc <- suppressWarnings(
      suppressMessages(
         metacore::metacore(ds_spec, ds_vars, var_spec[1:4, ], value_spec, derivations, code_id)
      )
   )

   expect_warning(set_variable_labels(iris, mc))

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
