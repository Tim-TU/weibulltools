library(dplyr)
library(tibble)
library(visNetwork)

merge_reference <- function(reference) {
  out <- list()
  for (i in seq_len(length(reference) / 2)) {
    out[[i]] <- c(reference[[2*i-1]], reference[[2*i]])
  }
  out
}

reference_to_tbl <- function(reference) {
  reference <- merge_reference(reference)

  purrr::map_dfr(reference, function(group) {
    tibble::tibble(
      name = group$contents,
      group = group$title
    )
  })
}

get_function_nodes <- function() {
  x <- pkgdown:::as_pkgdown()
  reference <- reference_to_tbl(x$meta$reference)
  topics <- x$topics
  tbl <- inner_join(reference, topics, by = "name") %>%
    select(id = name, title, group = group)
  tbl
}

flow_chart <- function() {
  nodes <- get_function_nodes()
  nodes$label <- nodes$id

  visNetwork(nodes)
}

nodes_to_cluster <- function(nodes, group) {
  nodes <- paste0("\"", nodes, "\"", collapse = "\n")
  paste0(
    "subgraph \"cluster", group[1], "\" {\n", nodes, "\n}"
  )
}

