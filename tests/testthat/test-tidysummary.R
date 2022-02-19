generate_complex_data <- function() {
  n <- 100
  df <- data.frame(
    id = seq.int(n),
    fct = factor(sample(1:5, n, replace=TRUE),
                 levels = 1:5,
                 labels = c("One", "Two", "Three", "Four", "Five")),
    chr = sample(letters[1:5], n, replace=TRUE),
    lst = list(1),
    bol = sample(c(TRUE, FALSE), n, replace=TRUE),
    df = rep(data.frame(y=rnorm(n)), n)
  )
  # Some missing data
  df$id[sample(seq.int(n), 5)] <- NA
  df$fct[sample(seq.int(n), 5)] <- NA
  df$chr[sample(seq.int(n), 5)] <- NA
  df
}

test_that("tidysummary works on complex data", {
  data <- generate_complex_data()
  output <- tidysummary(data, "large")
  expect_true(is_tidysummary(output))
})

test_that("tidysummary works on many datasets", {
  # more data can be found in datasets::
  output1 <- tidysummary(mtcars, "large")
  output2 <- tidysummary(iris, "large")
  output3 <- tidysummary(state.name, "large")
  expect_true(is_tidysummary(output1))
  expect_true(is_tidysummary(output2))
  expect_true(is_tidysummary(output3))
})

test_that("empty data frame works", {
  output <- tidysummary(data.frame(), "l")
  expect_true(NROW(output) == 0)
  expect_true(length(output) > 0)
})

test_that("atomic numeric works", {
  output <- tidysummary(iris$Sepal.Length, "l")
  expect_equal(output$column, "iris$Sepal.Length")
  expect_true(is_tidysummary(output))
  expect_true(NROW(output) > 0)
  expect_true(length(output) > 0)
})

test_that("atomic factor works", {
  output <- tidysummary(iris$Species, "l")
  expect_equal(output$column, "iris$Species")
  expect_true(is_tidysummary(output))
  expect_true(NROW(output) > 0)
  expect_true(length(output) > 0)
})

test_that("size argument works", {
  s <- tidysummary(iris, "small")
  m <- tidysummary(iris, "medium")
  l <- tidysummary(iris, "large")
  expect_true(length(s) < length(m))
  expect_true(length(m) < length(l))

  s <- tidysummary(iris, "SMALL")
  m <- tidysummary(iris, "MEDIUM")
  l <- tidysummary(iris, "LARGE")
  expect_true(length(s) < length(m))
  expect_true(length(m) < length(l))

  s <- tidysummary(iris, "s")
  m <- tidysummary(iris, "m")
  l <- tidysummary(iris, "l")
  expect_true(length(s) < length(m))
  expect_true(length(m) < length(l))

  s <- tidysummary(iris, size="s")
  m <- tidysummary(iris, size="m")
  l <- tidysummary(iris, size="l")
  expect_true(length(s) < length(m))
  expect_true(length(m) < length(l))
})

test_that("digits argument works", {
  output0 <- tidysummary(iris, digits=0)
  output1 <- tidysummary(iris, digits=1)
  output2 <- tidysummary(iris, digits=2)
  output3 <- tidysummary(iris, digits=3)
  outputN <- tidysummary(iris, digits=NULL)
  expect_equal(output0[1, "sd"], 1)
  expect_equal(output1[1, "sd"], 0.8)
  expect_equal(output2[1, "sd"], 0.83)
  expect_equal(output3[1, "sd"], 0.828)

  # This assumes that the target test system has at least 6 decimal places
  # which probably isn't unreasonable but an assumption nonetheless
  expect_equal(format(outputN[1, "sd"], digits=6), "0.828066")
})

  test_that("bad arguments throw exceptions", {
  expect_error(tidysummary(iris, "doesn't exist"))
  expect_error(tidysummary(iris, size="doesn't exist"))
  expect_error(tidysummary(iris, digits=-1))
  expect_error(tidysummary(iris, digits=NA))
  expect_error(tidysummary(lm(1 ~ 1)))
})

test_that("text converters warn if bad input", {
  pseudoiris <- iris
  class(pseudoiris) <- "shit type"
  expect_warning(expect_error(to_markdown(pseudoiris)))
})

test_that("text converters work", {
  output <- tidysummary(iris, "l")
  md <- to_markdown(output)
  html <- to_html(output)
  latex <- to_latex(output)
  expect_s3_class(md, "knitr_kable")
  expect_s3_class(html, "knitr_kable")
  expect_s3_class(latex, "knitr_kable")
})

save_file_and_cleanup <- function(text, func) {
  file <- tempfile(fileext = ".txt")
  func(text, file=file)
  size <- file.size(file)
  if (file.exists(file)) file.remove(file)
  expect_true(size > 0)
}

test_that("text converter save work", {
  output <- tidysummary(iris, "l")
  save_file_and_cleanup(output, to_markdown)
  save_file_and_cleanup(output, to_latex)
  save_file_and_cleanup(output, to_html)
  save_file_and_cleanup(output, to_json)
  save_file_and_cleanup(output, to_csv)
})

test_that("Excel converter works", {
  output <- tidysummary(iris, "l")
  file <- tempfile(fileext = ".xlsx")
  to_excel(output, file=file)
  xlsx <- openxlsx::read.xlsx(file)
  if (file.exists(file)) file.remove(file)
  expect_s3_class(xlsx, "data.frame")
})
