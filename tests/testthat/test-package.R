test_that("package metadata can be read", {
  desc <- utils::packageDescription("pubdeck")

  expect_identical(desc$Package, "pubdeck")
  expect_identical(desc$License, "MIT + file LICENSE")
})

test_that("the test template still matches the default configuration", {
  expect_message(
    result <- deck_check_template(config = test_config()),
    "every layout"
  )
  expect_true(all(result$found))
})
