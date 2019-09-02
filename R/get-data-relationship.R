get_data_relationship <- function(data, ...){
  if(is.null(names(data))) names(data) <- 1:length(data)

  fun <- get("map2", asNamespace("purrr"))
  data <- list(.x = data,
               .y = names(data),
               .f = ~tibble::rownames_to_column(
                 .x,
                 var = paste(.y, "rowname", sep = "."))) %>%
    do.call(what = fun, .)

  Reduce(function(x, y) dplyr::inner_join(x, y, ...),
         data)
}
