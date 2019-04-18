#' Graph Visualization for Twitter Mention Network
#'
#' @param user A string specifying the username to analyze
#' @param min.mention A number specifying minimal mentions to considered as a circle
#' @return Graph visualization using \code{ggplot}
#' @examples
#' mention_circle("twitter")
#' @importFrom rtweet get_timeline get_timelines lookup_users get_token create_token
#' @importFrom igraph graph_from_data_frame
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidyr unnest
#' @importFrom tidygraph centrality_degree
#' @import dplyr
#' @import stringr
#' @import ggraph
#' @import ggplot2
#' @export
mention_circle <- function(user, min.mention = 2, token=get_token()){

  timeline <- get_timeline(user, token=token)

  if(nrow(timeline)==0)
    stop("User account's are protected")

  mentions <- timeline %>%
    mutate(
      users = str_extract_all(text, "@[A-Za-z0-9_]+")
    )

  mentions <- timeline %>%
    mutate(
      users = str_extract_all(text, "@[A-Za-z0-9_]*")
    ) %>%
    select(text, users)

  close.friends <- unlist(mentions$users) %>%
    table() %>%
    as.data.frame() %>%
    arrange(-Freq) %>%
    filter(Freq > min.mention, . != "@")

  names(close.friends) <- c("mention_to","n")
  close.friends <- close.friends %>%
    mutate(mention_to = str_remove(mention_to, "@"))

  friends.timeline <- get_timelines(as.character(close.friends$mention_to))

  mentions.circle <- friends.timeline %>%
    select(screen_name, text) %>%
    mutate(
      mention_to = str_extract_all(text, "@[A-Za-z0-9_]*")
    ) %>%
    select(screen_name, mention_to) %>%
    unnest(mention_to) %>%
    filter(mention_to != "@") %>%
    count(screen_name, mention_to) %>%
    mutate(mention_to = str_remove(mention_to, "@")) %>%
    arrange(screen_name,-n) %>%
    filter(n > min.mention)

  nodes_df <- lookup_users(unique(c(mentions.circle$screen_name, mentions.circle$mention_to))) %>%
    select(screen_name, followers_count) %>%
    mutate(followers_count = log(followers_count))

  mention_edges <- close.friends %>%
    mutate(screen_name = user) %>%
    select(screen_name, mention_to, n) %>%
    rbind(mentions.circle) %>%
    filter(n > min.mention, screen_name %in% nodes_df$screen_name, mention_to %in% nodes_df$screen_name) %>%
    distinct()

  graph <- graph_from_data_frame(d = mention_edges, vertices = nodes_df) %>%
    as_tbl_graph()

  graph %>%
    mutate(community = as.factor(tidygraph::group_walktrap(weight = n))) %>%
    mutate(centrality = tidygraph::centrality_degree(weight = n)) %>%
    ggraph(layout = 'kk') +
    geom_edge_fan(alpha = 0.5) +
    geom_node_point(ggplot2::aes(color = followers_count, shape = community), show.legend = F, size = 3) +
    geom_node_text(ggplot2::aes(label = name), repel = TRUE, color = "black",
                   segment.colour = 'slateblue', size=4) +
    theme_graph() +
    ggplot2::guides(alpha = FALSE, colour = FALSE, shape = FALSE) +
    ggplot2::theme(legend.position = 'bottom') +
    ggplot2::labs(title = paste0('Circle Analysis @', user),
         subtitle = 'Top Mentioned Users between Circles')
}
