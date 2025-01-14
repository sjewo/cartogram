test_that("R Markdown documents can be rendered", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  rmarkdown::render("test.Rmd", quiet = TRUE)
  expect_true(file.exists("test.html"))
  unlink("test.html")
})