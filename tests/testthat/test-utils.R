# Suppress cli output during testing
options(cli.default_handler = function(...) { })

test_that("metatools_example", {
  expect_equal(metatools_example(), c("adsl.xpt", "dm.xpt"))
  expect_equal(
    file.exists(metatools_example("dm.xpt")),
    TRUE
  )

  expect_equal(
    file.exists(metatools_example("adsl.xpt")),
    TRUE
  )
})

test_that("make_lone_dataset", {
   load(metacore::metacore_example("pilot_ADaM.rda"))
   # Test deprecated function. Deprecated warning suppressed.
   suppressWarnings(expect_error(make_lone_dataset(metacore, NULL)))
})

test_that("metatools_example handles invalid file", {
  # Test with non-existent file
  expect_error(
    metatools_example("nonexistent.xpt"),
    "does not exist"
  )
})

test_that("make_lone_dataset with valid dataset_name", {
  load(metacore::metacore_example("pilot_ADaM.rda"))
  
  # Test with a valid dataset name
  suppressWarnings({
    result <- make_lone_dataset(metacore, "ADSL")
    expect_equal(nrow(result$ds_spec), 1)
    expect_equal(result$ds_spec$dataset, "ADSL")
  })
})

test_that("make_lone_dataset with already subsetted metacore", {
  load(metacore::metacore_example("pilot_ADaM.rda"))
  spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
  
  # Test with already subsetted metacore (should work without dataset_name)
  suppressWarnings({
    result <- make_lone_dataset(spec, NULL)
    expect_equal(result, spec)
  })
})
