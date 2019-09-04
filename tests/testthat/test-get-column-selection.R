test_that("Column returned are correct (unnamed list)", {
  data("mtcars")
  data("airquality")

  subsetcar1 <- 1:4; subsetcar2 <- 2:6
  subsetair1 <- 1:4; subsetair2 <- 2:6
  dfcar1 <- mtcars[1:10, subsetcar1]
  dfcar2 <- mtcars[1:10, subsetcar2]
  dfair1 <- airquality[1:10, subsetair1]
  dfair2 <- airquality[1:10, subsetair2]

  expectedcar <- names(mtcars)[intersect(subsetcar1, subsetcar2)]
  expectedair <- names(airquality)[intersect(subsetair1, subsetair2)]
  returned <- vortex::get_column_selection(list(dfcar1, dfcar2,
                                                dfair1, dfair2))

  expect_equal(returned[[1]]$columns, expectedcar)
  expect_equal(returned[[1]]$data.frame, c("df1", "df2"))
  expect_equal(returned[[2]]$columns, expectedair)
  expect_equal(returned[[2]]$data.frame, c("df3", "df4"))

  expect_equal(returned[[3]]$columns, names(dfcar1))
  expect_equal(returned[[3]]$data.frame, c("df1"))
  expect_equal(returned[[4]]$columns, names(dfcar2))
  expect_equal(returned[[4]]$data.frame, c("df2"))

  expect_equal(returned[[5]]$columns, names(dfair1))
  expect_equal(returned[[5]]$data.frame, c("df3"))
  expect_equal(returned[[6]]$columns, names(dfair2))
  expect_equal(returned[[6]]$data.frame, c("df4"))
})


test_that("Column returned are correct (named list)", {
  data("mtcars")
  data("airquality")

  subsetcar1 <- 1:4; subsetcar2 <- 2:6
  subsetair1 <- 1:4; subsetair2 <- 2:6
  dfcar1 <- mtcars[1:10, subsetcar1]
  dfcar2 <- mtcars[1:10, subsetcar2]
  dfair1 <- airquality[1:10, subsetair1]
  dfair2 <- airquality[1:10, subsetair2]

  named_list <- list(dfc1 = dfcar1,
                     dfc2 =dfcar2,
                     dfc3 = dfair1,
                     dfc4 = dfair2)

  expectedcar <- names(mtcars)[intersect(subsetcar1, subsetcar2)]
  expectedair <- names(airquality)[intersect(subsetair1, subsetair2)]
  returned <- vortex::get_column_selection(named_list)

  expect_equal(returned[[1]]$columns, expectedcar)
  expect_equal(returned[[1]]$data.frame, c("dfc1", "dfc2"))
  expect_equal(returned[[2]]$columns, expectedair)
  expect_equal(returned[[2]]$data.frame, c("dfc3", "dfc4"))

  expect_equal(returned[[3]]$columns, names(dfcar1))
  expect_equal(returned[[3]]$data.frame, c("dfc1"))
  expect_equal(returned[[4]]$columns, names(dfcar2))
  expect_equal(returned[[4]]$data.frame, c("dfc2"))

  expect_equal(returned[[5]]$columns, names(dfair1))
  expect_equal(returned[[5]]$data.frame, c("dfc3"))
  expect_equal(returned[[6]]$columns, names(dfair2))
  expect_equal(returned[[6]]$data.frame, c("dfc4"))
})
