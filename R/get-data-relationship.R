crossbind <- function(data, ...){
  if(is.null(names(data))) names(data) <- 1:length(data)

  `%!in%` <- purrr::negate(`%in%`)
  if('by' %!in% names(list(...))){
    data <- data %>% purrr::map(tibble::rownames_to_column)
    # current <- match.call()[[1]] %>% deparse() %>% get()
    return(vortex::crossbind(data, by = "rowname"))
  }


  if(list(...)$by != "rowname"){
    fun <- get("map2", asNamespace("purrr"))
    data <- list(.x = data,
                 .y = names(data),
                 .f = ~tibble::rownames_to_column(
                   .x,
                   var = paste(.y, "rowname", sep = "."))) %>%
      do.call(what = fun, .)
  }

  bound_df <- Reduce(function(x, y) dplyr::inner_join(x, y, ...),
                     data)

  tryCatch(return(bound_df %>% tibble::column_to_rownames()),
    error = function(cond){
      return(bound_df)}
  )
}

rowbind <- function(data, ...){
  columns <- vortex::get_column_selection(data)

  newdataset_name <- purrr::map(columns, ~paste(.x$data.frame, collapse = "_"))

  purrr::map(columns, function(x){
    data[x$data.frame] %>% purrr::map(~.x[, x$columns]) %>%
      purrr::reduce(rbind)
  })%>%
      structure(names = newdataset_name)
}
