#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("ggnetwork")
#install.packages("intergraph")
#install.packages("ggrepel")

library(jsonlite)      # read in the JSON data from the API
library(dplyr)         # data munging
library(igraph)        # work with graphs in R
library(ggnetwork)     # devtools::install_github("briatte/ggnetwork")
library(intergraph)    # ggnetwork needs this to wield igraph things
library(ggrepel)       # fancy, non-ovelapping labels
library(readxl)

Emails_network <- read_excel("C:/Users/Onontsatsal/Dropbox/UIC/20181Spring/IDS564/Project/Emails_network.xlsx")
View(Emails_network)

emails <- data.frame(Emails_network)
emails <- na.omit(emails) 
g_emails <- graph.data.frame(emails, directed = TRUE, vertices= NULL)
g_emails_un <- graph.data.frame(emails, directed = FALSE, vertices= NULL)

# Edges
ecount(g_emails)
## Vertices
vcount(g_emails)
is.weighted(g_emails)

V(g_emails)$name
is.simple(g_emails)
is.connected(g_emails)
is.weighted(g_emails)
E(g_emails)$weight 

#create a simple graph
g_emails_simpl<-simplify(g_emails)
g_emails_simpl_un <- simplify(g_emails_un)
is.simple(g_emails_simpl)
is.connected(g_emails_simpl)
is.weighted(g_emails_simpl)

#create a weighted graph
wg_emails<-g_emails_simpl
E(wg_emails)$weight <- runif(ecount(wg_emails))
is.weighted(wg_emails)

#degree measures
degree(wg_emails)
plot(degree(wg_emails), main = "Degree distribution")

#connected
is.connected(wg_emails)
is.connected(wg_emails, mode="strong")
is.connected(wg_emails, mode="weak")

#summary
summary(g_emails_simpl)
summary(g_emails_simpl_un)

#data overview
colors <- c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 
emailto <- table(emails$MetadataTo)
pie(emailto[emailto>20], main = "Most emails sent to")
barplot(sort(emailto[emailto>20]), col = rainbow(10), las = 1, horiz=T, main = "Most emails sent to", 
        xlab ="Email sent to")

emailfrom <- table(emails$MetadataFrom)
pie(emailfrom[emailfrom>20], main = "Most emails sent from")
barplot(sort(emailfrom[emailfrom>20]), col = colors, las = 1, horiz=T, main = "Most emails sent from", 
        xlab ="Email sent from")

#initial plots of simple graphs - compare directed and undirected
#igraph.options(vertex.size=3, edge.arrow.size=0.15, vertex.label=NA)
plot(g_emails_simpl, layout=layout.circle, vertex.size=3, edge.arrow.size=0.15, vertex.label=NA)
plot(g_emails_simpl, layout=layout.kamada.kawai, vertex.size=3, edge.arrow.size=0.15, vertex.label=NA)
plot(g_emails_simpl, layout=layout.reingold.tilford, vertex.size=3, edge.arrow.size=0.15, vertex.label=NA)
plot(g_emails_simpl, layout=layout.reingold.tilford(wg_emails, circular = T), vertex.size=3, edge.arrow.size=0.15, vertex.label=NA)

plot(g_emails_simpl_un, layout=layout.circle)
plot(g_emails_simpl_un, layout=layout.kamada.kawai)
plot(g_emails_simpl_un, layout=layout.reingold.tilford)
plot(g_emails_simpl_un, layout=layout.reingold.tilford(wg_emails, circular = T))

#network measures (degree, path length, transitivity)
degree(g_emails_simpl)
degree(g_emails_simpl_un)

plot(degree(g_emails_simpl), main = "Simple email network degree")
plot(degree(g_emails_simpl_un), main = "Simple undirected email network degree")

table(degree(g_emails_simpl))
table(degree(g_emails_simpl_un))

deg <- degree(g_emails_simpl)
pie(deg[deg>5], main = "Directed email graph degrees")

deg_un <- degree(g_emails_simpl_un)
pie(deg_un[deg_un>5], main = "Undirected email graph degrees") 

#closeness, betweenness and transitivity
#between <- order(betweenness(g_emails_simpl), decreasing = TRUE) - can we get betweenness in order?
closeness(g_emails_simpl)
closeness(g_emails_simpl_un)

betweenness(g_emails_simpl)
betweenness(g_emails_simpl_un)

bt<-betweenness(g_emails_simpl)
barplot(bt[bt>20], main = "Betweenness centrality", horiz = TRUE, las = 1, col = colors)

transitivity(g_emails_simpl)
transitivity(g_emails_simpl_un)

#average path length and diameter
average.path.length(g_emails_simpl)
diameter(g_emails_simpl)

average.path.length(g_emails_simpl_un)
diameter(g_emails_simpl_un)

#histograms
hist(degree(g_emails_simpl), col = "lightblue", xlab = "Vertex Degree", ylab = "Frequency")
hist(graph.strength(g_emails_simpl), col = "pink", xlab = "Vertex Strength", ylab = "Frequency")

#plot degree
d.hillary <- degree(wg_emails)
hist(d.hillary, col = "lightblue", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution")

#plot log-log degree
dd.hillary <- degree.distribution(wg_emails)
d <- 1:max(d.hillary)-1
ind <- (dd.hillary != 0)
plot(d[ind],dd.hillary[ind], log="xy", col = "black", xlab = c("Log-Degree"), ylab = c("Log-Intensity"), main = "Log-Log Degree Distribution")

#plot hubs and aurthorities
a <- layout.kamada.kawai(g_emails_simpl)*.2
plot(g_emails_simpl, layout = a, main = "Hubs", vertex.size = 10*sqrt(hub.score(g_emails_simpl)$vector), rescale = FALSE)
plot(g_emails_simpl, layout = a, main = "Authorities", vertex.size = 10*sqrt(authority.score(g_emails_simpl)$vector), rescale = FALSE)

V(g_emails_simpl)["Abedin, Huma"]$label <- "Huma"
V(g_emails_simpl)["Sullivan, Jacob J"]$label <- "Jacob S"
V(g_emails_simpl)["Mills, Cheryl D"]$label <- "Cheryl"
V(g_emails_simpl)["Slaughter, Anne-Marie"]$label <- "Anne-Marie"
V(g_emails_simpl)["Jiloty, Lauren C"]$label <- "Lauren"
V(g_emails_simpl)["Valmoro, Lona J"]$label <- "Lona"
V(g_emails_simpl)["Russo, Robert V"]$label <- "Robert"
V(g_emails_simpl)["Lew, Jacob J"]$label <- "Jacob L"
V(g_emails_simpl)["Talbott, Strobe"]$label <- "Strobe"
V(g_emails_simpl)["Reines, Philippe I"]$label <- "Phil"
V(g_emails_simpl)["McHale, Judith A"]$label <- "Judith"
V(g_emails_simpl)["Verma, Richard R"]$label <- "Richard"

plot(g_emails_simpl, main = "Authorities", vertex.size = 10*sqrt(authority.score(g_emails_simpl)$vector), rescale = TRUE, vertex.label = NA)

#cliques
table(sapply(cliques(g_emails_simpl), length))
cliques(g_emails_simpl)[sapply(cliques(g_emails_simpl), length) > 4]
clique_members <- cliques(g_emails_simpl)[sapply(cliques(g_emails_simpl), length) > 4]

cliqs <- cliques(g_emails_simpl)

#just largest cliques and induced graph
lcliqs<-largest.cliques(g_emails_simpl_un) #unlist(lcliqs, recursive = FALSE)
cliques_subgraph <- induced.subgraph(g_emails_simpl_un, unique(unlist(lcliqs)))

V(cliques_subgraph)["H"]$label <- "Hillary"
V(cliques_subgraph)["Abedin, Huma"]$label <- "Huma"
V(cliques_subgraph)["Sullivan, Jacob J"]$label <- "Jacob S"
V(cliques_subgraph)["Mills, Cheryl D"]$label <- "Cheryl"
V(cliques_subgraph)["Slaughter, Anne-Marie"]$label <- "Anne-Marie"
V(cliques_subgraph)["Jiloty, Lauren C"]$label <- "Lauren"
V(cliques_subgraph)["Valmoro, Lona J"]$label <- "Lona"
V(cliques_subgraph)["Russo, Robert V"]$label <- "Robert"
V(cliques_subgraph)["Lew, Jacob J"]$label <- "Jacob L"
V(cliques_subgraph)["Talbott, Strobe"]$label <- "Strobe"
V(cliques_subgraph)["Reines, Philippe I"]$label <- "Phil"
V(cliques_subgraph)["McHale, Judith A"]$label <- "Judith"
V(cliques_subgraph)["Verma, Richard R"]$label <- "Richard"

V(cliques_subgraph)$size <- 5*sqrt(graph.strength(wg_emails))
V(cliques_subgraph)$color <- V(cliques_subgraph)
plot(cliques_subgraph, layout = layout.kamada.kawai, vertex.label.dist = 0)

#community detection
rec.name <- get.vertex.attribute(g_emails_simpl_un, "MetadataTo")
send.name<- get.vertex.attribute(g_emails_simpl_un, "MetadataFrom")

comm_hillary_fast <- fastgreedy.community(g_emails_simpl_un, weights=E(g_emails_simpl_un)$weight)
plot(comm_hillary_fast,g_emails_simpl_un, vertex.label= NA, vertex.size=2)

###community detection after removing degree 1 nodes
deg1 <- V(g_emails_simpl_un)[degree(g_emails_simpl_un)==1]
g_emails_simpl_un_woDeg1<- delete.vertices(g_emails_simpl_un, deg1)

comm_hillary_fastDeg1 <- fastgreedy.community(g_emails_simpl_un_woDeg1, 
      weights=E(g_emails_simpl_un_woDeg1)$weight)
c.mDeg1 <- membership(comm_hillary_fastDeg1)

plot(comm_hillary_fastDeg1,g_emails_simpl_un_woDeg1, vertex.label= NA, vertex.size=2, 
     main = "Community detection without degree 1 nodes")

#community 1
First.com <- V(g_emails_simpl_un_woDeg1)[c.mDeg1==1]
First.graph <- induced.subgraph(g_emails_simpl_un_woDeg1, First.com)
plot(First.graph, main = "Community 1 (after removing degree 1)")

#community 2
second.com <- V(g_emails_simpl_un_woDeg1)[c.mDeg1==2]
second.graph <- induced.subgraph(g_emails_simpl_un_woDeg1, second.com)
plot(second.graph, main = "Community 2 (after removing degree 1)")

#community 3
third.com <- V(g_emails_simpl_un_woDeg1)[c.mDeg1==3]
third.graph <- induced.subgraph(g_emails_simpl_un_woDeg1, third.com)
plot(third.graph, main = "Community 3 (after removing degree 1)")

#community 4
fourth.com <- V(g_emails_simpl_un_woDeg1)[c.mDeg1==4]
fourth.graph <- induced.subgraph(g_emails_simpl_un_woDeg1, fourth.com)
plot(fourth.graph, main = "Community 4 (after removing degree 1)")

#community 5
fifth.com <- V(g_emails_simpl_un_woDeg1)[c.mDeg1==5]
fifth.graph <- induced.subgraph(g_emails_simpl_un_woDeg1, fifth.com)
plot(fifth.graph, main = "Community 5 (after removing degree 1)")

#community significance test function from professor
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

community.significance.test(g_emails_simpl_un_woDeg1, First.com) #p-value = 0.0007193
community.significance.test(g_emails_simpl_un_woDeg1, second.com) #W = 101.5, p-value = 0.8749
community.significance.test(g_emails_simpl_un_woDeg1, third.com) #W = 63.5, p-value = 0.6127
community.significance.test(g_emails_simpl_un_woDeg1, fourth.com) #W = 88.5, p-value = 0.8414
community.significance.test(g_emails_simpl_un_woDeg1, fifth.com) #W = 65, p-value = 0.7506

#induced graph with the nodes of communities 1 to 5
com15 <- V(g_emails_simpl_un_woDeg1)[c.mDeg1 %in% c(1, 2, 3, 4, 5)]
com15.graph <- induced.subgraph(g_emails_simpl_un_woDeg1, unique(com15))
plot(com15.graph, main = "Communities 1-5")
#####

#graph plots
#igraph.options(vertex.size=3, edge.arrow.size=0.15, vertex.label=NA)
V(wg_emails)$color <- "lightblue"
V(wg_emails)["H"]$color <- "pink"
V(wg_emails)$size <- sqrt(graph.strength(wg_emails))
V(wg_emails)$size2 <- V(wg_emails)$size * .25

E(wg_emails)$width <- (E(wg_emails)$weight)

plot(wg_emails, layout=layout.circle)
plot(wg_emails, layout=layout.kamada.kawai)
plot(wg_emails, layout=layout.reingold.tilford)
plot(wg_emails, layout=layout.reingold.tilford(wg_emails, circular = T))

#playing with plots and layouts
V(wg_emails)["H"]$label <- "Hillary"
V(wg_emails)["Abedin, Huma"]$label <- "Huma"
V(wg_emails)["Sullivan, Jacob J"]$label <- "Jacob"
V(wg_emails)["Mills, Cheryl D"]$label <- "Cheryl"
V(wg_emails)["Slaughter, Anne-Marie"]$label <- "Anne-Marie"
V(wg_emails)["Jiloty, Lauren c"]$label <- "Lauren"
V(wg_emails)["Valmoro, Lona J"]$label <- "Lona"
V(wg_emails)["Russo, Robert V"]$label <- "Robert"

coords <- layout.fruchterman.reingold(wg_emails)*.1
coords2 <- layout.fruchterman.reingold(wg_emails)*.075
plot(wg_emails, layout=coords, rescale=FALSE)
plot(wg_emails, layout=coords2, rescale=FALSE)

E(wg_emails)$width <- 1*(E(wg_emails)$weight)

plot(wg_emails, layout=coords2, rescale=FALSE)

coords3 <- layout.circle(wg_emails)*1
plot(wg_emails, layout=coords3, rescale=FALSE)
plot(wg_emails, layout=layout.kamada.kawai, rescale=FALSE)

plot(wg_emails, layout=layout.drl)
