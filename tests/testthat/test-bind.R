# Used data:
data("mtcars")

test_that("Data is bound by column (based on id)", {
  # Create the subset data.frame
  d1 <- head(mtcars)[, 1:2]
  d2 <- head(mtcars, 8)[, 3:4]
  d3 <- head(mtcars, 20)[, 5:7]
  # Add unique identifier for each row of each data.frame
  d1["id"] <- 1:nrow(d1)
  d2["id"] <- 1:nrow(d2)
  d3["id"] <- 1:nrow(d3)
  # Create arguments list
  arguments <- list(data1 = d1,
                    data2 = d2,
                    data3 = d3)
  returned <- vortex::crossbind(arguments, by = "id")

  d1 <- tibble::rownames_to_column(d1, var = "data1.rowname")
  d2 <- tibble::rownames_to_column(d2, var = "data2.rowname")
  d3 <- tibble::rownames_to_column(d3, var = "data3.rowname")
  expected <- dplyr::inner_join(d1, d2, by = "id") %>%
    dplyr::inner_join(d3, by = "id")
  expect_equal(returned, expected)
})

test_that("Data is bound by column (based on rowname)", {
  # Create the subset data.frame
  d1 <- head(mtcars)[, 1:2]
  d2 <- head(mtcars, 8)[, 3:4]
  # Create arguments list
  arguments <- list(data1 = d1,
                    data2 = d2)
  returned <- vortex::crossbind(arguments)
  d1 <- tibble::rownames_to_column(d1)
  d2 <- tibble::rownames_to_column(d2)
  expected <- dplyr::inner_join(d1, d2, by = "rowname")
  expected <- tibble::column_to_rownames(expected)
  expect_equal(returned, expected)
})
