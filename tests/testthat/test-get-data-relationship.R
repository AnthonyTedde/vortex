# Used data:
data("mtcars")

test_that("Data is bound by row", {
  d1 <- head(mtcars)
  d2 <- tail(mtcars)
  arguments <- list(data1 = d1,
                    data2 = d2)
  returned <- vortex::get_data_relationship(data)
  expect_equal(returned, rbind(d1, d2))
})


test_that("Data is bound by column (based on id)", {
  # Create the subset data.frame
  d1 <- head(mtcars)[, 1:2]
  d2 <- head(mtcars, 8)[, 3:4]
  # Add unique identifier for each row of each data.frame
  d1["id"] <- 1:nrow(d1)
  d2["id"] <- 1:nrow(d2)
  # Create arguments list
  arguments <- list(data1 = d1,
                    data2 = d2)
  returned <- vortex::get_data_relationship(data, id = "id")
  d1 <- tibble::rownames_to_column(d1)
  d2 <- tibble::rownames_to_column(d2)
  expected <- dplyr::inner_join(d1, d2, by = "id")
  expected <- tibble::column_to_rownames(expected)
  expect_equal(returned, expected)
})

test_that("Data is bound by column (based on rowname)", {
  # Create the subset data.frame
  d1 <- head(mtcars)[, 1:2]
  d2 <- head(mtcars, 8)[, 3:4]
  # Create arguments list
  arguments <- list(data1 = d1,
                    data2 = d2)
  returned <- vortex::get_data_relationship(data)
  d1 <- tibble::rownames_to_column(d1)
  d2 <- tibble::rownames_to_column(d2)
  expected <- dplyr::inner_join(d1, d2, by = "rowname")
  expected <- tibble::column_to_rownames(expected)
  expect_equal(returned, expected)
})

test_that("Data is bound by columns and rows accordingly", {
  expect_true(F)
})
