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
