plot.network <-
function(ttable, gs, eps = 0.25, similarity_threshold = 1/3, manipulation = FALSE, autoResize = TRUE, ...) {
similarity_matrix <- as.matrix(geneSets.sim(gs[rownames(ttable)], eps = eps))
colnames(similarity_matrix) <- rownames(similarity_matrix) <- rownames(ttable)
diag(similarity_matrix) <- 1
similarity_matrix <- similarity_matrix > similarity_threshold
###
min_node_size <- 10
ssize <- ttable$actualSize
ssize <- sapply(ssize, function(x) max(10, x) - 10)
ssize <- floor(sapply(ssize, function(x) min_node_size + 50*(2*(pnorm(x, sd = 500) -0.5))))
ccolor <- 1+round(99*pnorm(ttable$logit2NES, sd = 0.3))
ccolor <- colorRampPalette(c("green2", "red"))(100)[ccolor]
nodes <- data.frame(id = 1:nrow(ttable), label = rownames(ttable), size = ssize, color =ccolor)
###
K <- nrow(ttable)
edges <- data.frame(from = rep(2:K, (K-1):1), to = rep(1:(K-1), (K-1):1))
for(k in 2:K) edges[which(edges[, "to"] == k-1), "from"] <- k:K
edges <- edges[which(unclass(as.dist(similarity_matrix)) > 0),]
###
ans <- visNetwork(nodes, edges, width = "100%")%>% 
  visOptions(manipulation = manipulation, autoResize = autoResize) %>%
  visIgraphLayout() 
return(ans)
}
