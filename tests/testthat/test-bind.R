# Used data:
data("mtcars")
data("airquality")
data("CO2")

# Global variables
cb_d1 <- head(mtcars)[, 1:2]
cb_d2 <- head(mtcars, 8)[, 3:4]
cb_d3 <- head(mtcars, 20)[, 5:7]
rb_d1 <- head(airquality)
rb_d2 <- tail(airquality)
rb_d3 <- head(CO2)
rb_d4 <- tail(CO2)


test_that("Data is crossbound (based on id)", {
  # Add unique identifier for each row of each data.frame
  cb_d1["id"] <- 1:nrow(cb_d1)
  cb_d2["id"] <- 1:nrow(cb_d2)
  cb_d3["id"] <- 1:nrow(cb_d3)
  # Create arguments list
  arguments <- list(data1 = cb_d1,
                    data2 = cb_d2,
                    data3 = cb_d3)
  returned <- vortex::crossbind(arguments, by = "id")

  cb_d1 <- tibble::rownames_to_column(cb_d1, var = "data1.rowname")
  cb_d2 <- tibble::rownames_to_column(cb_d2, var = "data2.rowname")
  cb_d3 <- tibble::rownames_to_column(cb_d3, var = "data3.rowname")
  expected <- dplyr::inner_join(cb_d1, cb_d2, by = "id") %>%
    dplyr::inner_join(cb_d3, by = "id")
  expect_equal(returned, expected)
})


test_that("Data is crossbound (based on rowname)", {
  # Create arguments list
  arguments <- list(data1 = cb_d1,
                    data2 = cb_d2)
  returned <- vortex::crossbind(arguments)
  cb_d1 <- tibble::rownames_to_column(cb_d1)
  cb_d2 <- tibble::rownames_to_column(cb_d2)
  expected <- dplyr::inner_join(cb_d1, cb_d2, by = "rowname")
  expected <- tibble::column_to_rownames(expected)
  expect_equal(returned, expected)
})


test_that("Data is rowbound (with named list of data.frames)", {
  argument <- list(rbd1 = rb_d1,
                   rbd2 = rb_d2,
                   rbd3 = rb_d3,
                   rbd4 = rb_d4)
  returned <- vortex::rowbind(data = argument)

  # At least the four original dataset are returned
  expect_true(all(argument %in% returned))

  col <- intersect(names(rb_d1), names(rb_d2))
  expected1 <- rbind(rb_d1[, col],
                     rb_d2[, col])
  col <- intersect(names(rb_d3), names(rb_d4))
  expected2 <- rbind(rb_d3[, col],
                     rb_d4[, col])
  expected <- list(rbd1_rbd2 = expected1,
                   rbd3_rbd4 = expected2)

  # The cross ones also are returned
  expect_true(all(expected %in% returned))

  # Check the name
  expect_true(all(
    c(names(expected), names(argument)) %in%
      names(returned)
  ))

})


test_that("Data is rowbound (with unnamed list of data.frames)",{
  argument <- list(rb_d1,
                   rb_d2,
                   rb_d3,
                   rb_d4)
  returned <- vortex::rowbind(data = argument)
  # Check the name
  # At least the four original dataset are returned
  expect_true(all(argument %in% returned))

  col <- intersect(names(rb_d1), names(rb_d2))
  expected1 <- rbind(rb_d1[, col],
                     rb_d2[, col])
  col <- intersect(names(rb_d3), names(rb_d4))
  expected2 <- rbind(rb_d3[, col],
                     rb_d4[, col])
  expected <- list(df1_df2 = expected1,
                   df3_df4 = expected2)

  # The cross ones also are returned
  expect_true(all(expected %in% returned))

  # Check the name
  expect_true(all(
    c(names(expected), c("df1", "df2", "df3", "df4")) %in%
      names(returned)
  ))
})
