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
   expect_error(make_lone_dataset(metacore, NULL), "Requires either a subsetted metacore object or a dataset name")

   ds_spec <- make_lone_dataset(metacore, "ADSL")$ds_spec
   expect_equal(nrow(ds_spec), 1)
})
