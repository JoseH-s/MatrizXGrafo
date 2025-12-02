dados <- read.csv(file.choose(), header = TRUE, sep = ",")


#Matriz incidencia
matriz_inc <- xtabs(weight ~ from + to, data = dados)
class(matriz_inc)
matriz_inc<-unclass(matriz_inc)
class(matriz_inc)
print(matriz_inc)

#Matriz similaridade
similaridade <- matriz_inc %*% t(matriz_inc) 
similaridade
class(similaridade)
diag(similaridade)<-0 #diagonal igual a zero
similaridade

#Matriz coocorrencia
coocorrencia <- t(matriz_inc) %*% matriz_inc 
coocorrencia
diag(coocorrencia)<-0
coocorrencia


#Grafos
install.packages('igraph')
library('igraph')


grafo_inc<-graph_from_biadjacency_matrix(matriz_inc, directed = T, mode="out", weighted = T) 

grafo_sim <- graph_from_adjacency_matrix(similaridade, weighted = T, mode = "undirected")

grafo_co <- graph_from_adjacency_matrix(coocorrencia, weighted = T, mode = "undirected")


#Plots
#plot desenha o grafo
#E(...)$weight sacessa os pesos das arestas
#edge.arrow.size=0.1 define o tamanho das setas
plot(grafo_inc, edge.width=E(grafo_inc)$weight, edge.arrow.size=0.1)

plot(grafo_sim, edge.width=E(grafo_sim)$weight)

plot(grafo_co, edge.width=E(grafo_co)$weight)


install.packages('visNetwork')
library(visNetwork)

#Convertendo para VIS
vis_inc <- toVisNetworkData(grafo_inc) #grafo da matriz de incidência

vis_sim <- toVisNetworkData(grafo_sim) #grafo da matriz de similaridade

vis_co <- toVisNetworkData(grafo_co) #grafo da matriz de coocorrência

#Construindo Grafos

# 1º - identificações dos nós
node_inc<-data.frame("id"=vis_inc$nodes$id, "label"=vis_inc$nodes$label)

node_sim<-data.frame("id"=vis_sim$nodes$id, "label"=vis_sim$nodes$label)

node_co<-data.frame("id"=vis_co$nodes$id, "label"=vis_co$nodes$label)

# 2º identificando ligações

#ligações FROM -> TO - matriz de incidencia
links_inc<-as.data.frame(vis_inc$edges) #Converte as arestas (edges) dos grafos em data frames

#ligações matriz de adjacencia e coocorrência
links_sim<-as.data.frame(vis_sim$edges) 

links_co<-as.data.frame(vis_co$edges) 

#plot Matriz Incidencia
visNetwork(node_inc, links_inc) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEdges(arrows = "to") %>%  #somente se for direcionado!
  visIgraphLayout(layout ="layout_with_fr")

#plot Matriz Similaridade
visNetwork(node_sim, links_sim) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout ="layout_with_fr")

#plot Matriz Coocorrencia
visNetwork(node_co, links_co) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout ="layout_with_fr")

#Metricas topologicas

# Grafo de Incidência
cat("\n1. GRAFO DE INCIDÊNCIA:")
cat("\n   Vértices:", length(V(grafo_inc)))
cat("\n   Arestas:", ecount(grafo_inc))
cat("\n   Grau médio:", mean(degree(grafo_inc, mode = "out")))
cat("\n   Densidade:", edge_density(grafo_inc, loops = FALSE))

# Grafo de Similaridade
cat("\n\n2. GRAFO DE SIMILARIDADE:")
cat("\n   Vértices:", length(V(grafo_sim)))
cat("\n   Arestas:", ecount(grafo_sim))
cat("\n   Grau médio:", mean(degree(grafo_sim)))
cat("\n   Peso médio:", mean(E(grafo_sim)$weight))
# Adicione a densidade:
cat("\n   Densidade:", edge_density(grafo_sim, loops = FALSE))

# Grafo de Coocorrência
cat("\n\n3. GRAFO DE COOCORRÊNCIA:")
cat("\n   Vértices:", length(V(grafo_co)))
cat("\n   Arestas:", ecount(grafo_co))
cat("\n   Grau médio:", mean(degree(grafo_co)))
cat("\n   Peso médio:", mean(E(grafo_co)$weight))
cat("\n   Densidade:", edge_density(grafo_co, loops = FALSE))

