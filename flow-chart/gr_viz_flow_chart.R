library(DiagrammeR)
library(purrr)
library(htmltools)
source("flow-chart/flow_chart.R")

graph_start <- "strict digraph weibulltools {"
graph_end <- "}"

nodes <- get_function_nodes()
graph_nodes <- purrr::pmap_chr(nodes, function(id, title, group) {
  label <- paste0(id, "()")
  paste0(
    "\"", id, "\" [label =<", label, ">]"
  )
})
graph_nodes <- c("node [shape = plaintext]", graph_nodes)
graph_nodes <- paste0(graph_nodes, collapse = "\n")

graph_edges <- c(
"reliability_data -> estimate_cdf",
"reliability_data -> ml_estimation",
"reliability_data -> mixmod_em",
"estimate_cdf -> plot_prob",
"estimate_cdf -> rank_regression",
"estimate_cdf -> mixmod_regression",
"ml_estimation -> confint_fisher",
"ml_estimation -> plot_mod",
"rank_regression -> confint_betabinom",
"rank_regression -> plot_mod",
"mixmod_em -> plot_mod",
"mixmod_em -> plot_prob",
"mixmod_regression -> plot_mod",
"mixmod_regression -> plot_prob",
"confint_betabinom -> plot_conf",
"confint_fisher -> plot_conf"
)

graph_edges <- paste0(graph_edges, collapse = "\n")

cluster <- nodes %>%
  group_by(group) %>%
  summarise(cluster = nodes_to_cluster(id, group)) %>%
  `[[`("cluster")
graph_cluster <- paste0(cluster, collapse = "\n")


graph <- paste(
  graph_start,
  graph_nodes,
  graph_edges,
  graph_cluster,
  graph_end,
  sep = "\n"
)

grViz(graph)

grViz("
strict digraph weibulltools {
  node [
    shape = circle
  ]
  reliability_data [label = '@@1']
  estimate_cdf [label = '@@2-1']
  plot_prob [label = '@@2-2']
  ml_estimation [label = '@@3-1']
  loglik_function [label = '@@3-2']
  loglik_profiling [label = '@@3-3']
  rank_regression [label = '@@3-4']
  confint_betabinom [label = '@@4-1']
  confint_fisher [label = '@@4-2']
  delta_method [label = '@@4-3']
  plot_conf [label = '@@4-4']

  reliability_data -> estimate_cdf
  reliability_data -> ml_estimation
  estimate_cdf -> plot_prob
  estimate_cdf -> rank_regression
  ml_estimation -> confint_fisher
  rank_regression -> confint_betabinom
  confint_betabinom -> plot_conf
  confint_fisher -> plot_conf

  subgraph cluster_reliability_data {
    label = 'Reliability data'
    rank = same
    reliability_data
  }

  subgraph cluster_failure_probabilities {
    label = 'Non-Parametric Failure Probabilities'
    rank = same
    estimate_cdf
    plot_prob
  }

  subgraph cluster_parametric_models {
    label = 'Parametric Models'
    rank = same
    ml_estimation
    loglik_function
    loglik_profiling
    rank_regression
  }

  subgraph cluster_confidence_intervals {
    label = 'Confidence Intervals'
    rank = same
    confint_betabinom
    confint_fisher
    delta_method
    plot_conf
  }
}

[1]: 'reliability_data()'
[2]: c('estimate_cdf()', 'plot_prob()')
[3]: c('ml_estimation()', 'loglik_function()', 'loglik_profiling()', 'rank_regression()')
[4]: c('confint_betabinom()', 'confint_fisher()', 'delta_method()', 'plot_conf()')
")
