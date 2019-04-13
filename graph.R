library(rtweet)
library(tidyverse)
library(tidygraph)
library(igraph)


get_circle <- function(user, min.mention){
  
  timeline <- get_timeline(user)
  
  if(nrow(timeline)==0){
    return(NULL)
  }
  
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
    arrange(screen_name,-n)
  
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
    mutate(community = as.factor(group_walktrap(weight = n))) %>% 
    mutate(centrality = centrality_degree(weight = n)) %>% 
    ggraph(layout = 'kk') +
    geom_edge_fan(alpha = 0.5) +
    geom_node_point(aes(color = followers_count, shape = community), show.legend = F, size = 3) +
    geom_node_text(aes(label = name), repel = TRUE, color = "black",
                   segment.colour = 'slateblue', size=4) +
    theme_graph() +
    guides(alpha = FALSE, colour = FALSE, shape = FALSE) +
    theme(legend.position = 'bottom') +
    labs(title = paste0('Circle Analysis @', user),
         subtitle = 'Top Mentioned Users between Circles')
}


