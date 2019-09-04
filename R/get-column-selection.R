get_column_selection <- function(data){
  columns <- structure(purrr::map(data, names),
                       names = paste0("l", 1:length(data)))

  all_columns <- Reduce(dplyr::union, columns)
  sparse <- columns %>%
    purrr::map(~`%in%`(all_columns, .)) %>%
    Reduce(rbind, .) %>%
    structure(dimnames = list(names(columns), all_columns))

  cluster <- hclust(dist(sparse), method = "complete")

  purrr::map(sort(cluster$height, decreasing = T), function(x){
    cut <- cutree(cluster, h = x)
    purrr::map(unique(cut), function(x){
      names(cut[cut == x])
    })
  }) %>%
    purrr::flatten() %>% dplyr::union(names(columns)) %>%
    purrr::map(~structure(
      list(.x, Reduce(dplyr::intersect, columns[.x])),
      names = c("data.frame", "columns")
    ))

}
