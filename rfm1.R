# Author: Allyson de Lima Medeiros
# https://www.linkedin.com/in/allysonlm/

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(ggraph)
library(igraph)

# link
# http://dados.gov.br/dataset/solicitacao-de-material-de-registro-de-preco

# load data set
df_data <- fread('req-material-registro-preco.csv')

# select columns and drop na
df_data <- df_data %>%
  select(unidade_requisitante, data_cadastro, valor_solicitacao, denominacao_grupo_material) %>%
  drop_na()

# transform date value
df_data <- df_data %>% 
  mutate(data_cadastro = as.Date(data_cadastro, '%Y-%m-%d'))

# select 200 top greatest values
tb_df <- data.frame(table(df_data$unidade_requisitante))
tb_df <- tb_df[order(tb_df$Freq, decreasing = TRUE),c(1,2)]
tb_df <- tb_df[which(tb_df[,2]>200),]
df_data <- subset(df_data, unidade_requisitante %in% tb_df$Var1)

# create rfm
df_RFM <- df_data %>% 
  group_by(unidade_requisitante) %>% 
  summarise(recency   = as.numeric(as.Date('2019-05-03') - max(data_cadastro)),
            frequenci = n_distinct(denominacao_grupo_material), 
            monitery  = sum(valor_solicitacao)/n_distinct(denominacao_grupo_material))

# log value
df_RFM$monitery <- log(df_RFM$monitery)

# adjust rfm
df_RFM2 <- df_RFM
row.names(df_RFM2) <- df_RFM2$unidade_requisitante
df_RFM2$unidade_requisitante <- NULL
df_RFM2 <- scale(df_RFM2)

# create hclust
hc <- hclust(dist(df_RFM2), method = 'ward.D2')

# graphics
hierarchy <- as.dendrogram(hc)
igraf <- den_to_igraph(hierarchy)
from <- match(flare$imports$from, flare$vertices$name)
to <- match(flare$imports$to, flare$vertices$name)

# basic dendogram
ggraph(hierarchy, layout = 'dendrogram') +
  geom_edge_elbow2(aes(colour = ..index..)) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))

# dendogram circular
ggraph(igraf, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_link(size=0.4, alpha=0.1) +
  geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.1, colour="#373ae8") + 
  geom_node_text(aes(x = x*1.01, y=y*1.01, filter = leaf, label=name), size=2, alpha=1) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))

# dendogram style
ggraph(igraf, 'igraph', algorithm = 'tree', circular = TRUE) + 
  geom_edge_link(size=0.4, alpha=0.1) +
  geom_conn_bundle(aes(colour = ..index..), data = get_con(from, to), edge_alpha = 0.25, colour="#238423") +
  coord_fixed() + 
  geom_node_text(aes(x = x*1.01, y=y*1.01, filter = leaf, label=name), size=2, alpha=1) +
  geom_node_point(aes(filter = degree(igraf, mode = 'out') == 0), color = '#e0577c', size = 1) +
  ggforce::theme_no_axes()

# cut
members <- cutree(hc, k = 8)

# members
table(members)

# final rfm
aggregate(df_RFM[,2:4], by=list(members), mean)

  


