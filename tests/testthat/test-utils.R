test_that("metatools_example", {
   expect_equal(metatools_example(), c("adsl.xpt", "dm.xpt"))
   expect_equal(
      file.exists( metatools_example("dm.xpt")),
      TRUE)

   expect_equal(
      file.exists( metatools_example("adsl.xpt")),
      TRUE)

})
