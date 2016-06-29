edges=read.csv("edges.csv")
str(edges)

users=read.csv("users.csv")
str(users)

#install igraphic packages
install.packages("igraph")
library("igraph")


#plot the vertex and nodes
 g = graph.data.frame(edges, FALSE, users)
 plot(g, vertex.size=5, vertex.label=NA)


#make the nodes larger if it has more freedom eg. a user with a larger network of facebook friends

V(g)$size = degree(g)/2+2

plot(g, vertex.label=NA)


#update plot variables with difference color 
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"


#3D plot
rglplot(g, vertex.label=NA)