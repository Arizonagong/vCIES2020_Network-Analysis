# vCIES2020
## This script is created by Byoung-gyu Gong (bgong7@asu.edu), 
## for vCIES2020 Workshop for Network data analysis and visualization 

# Install packages
install.packages(c("igraph","readr","tidyr","RColorBrewer"))

#==================================================================#
#====================== Data Pre-processing =======================#
#==================================================================#
# In this section, you will retrieve csv format network data file  #
# to the R studio as a dataframe and convert it into the igraph    #
# object.                                                          #
#==================================================================#

#1. Read data from the Github repository csv files
library (readr)
urlfile1="https://raw.githubusercontent.com/Arizonagong/vCIES2020/master/igraph/primaryschool.csv?token=AHJ7KCMUBY3HF2EOHMF6YVK6VCAYK"
urlfile2="https://raw.githubusercontent.com/Arizonagong/vCIES2020/master/igraph/metadata_primaryschool.csv?token=AHJ7KCMZSBTXXUR5TLWEISK6VCA2C"
D<-read_csv(url(urlfile1))
D_meta<-read_csv(url(urlfile2))

#2. Manage dataset
B<-as.data.frame(table(D)) # Create an edge weight column named "Freq"
B1<-subset(B,Freq>0) # Delete all the edges having weight equal to 0

#3. Create an igraph object from the dataframes
library(igraph)
Stucont<-graph_from_data_frame(B1, directed = FALSE, vertices = D_meta)
E(Stucont)$weight<-E(Stucont)$Freq # Assigning edge attribute to each edge
Stucont

#==================================================================#
#=================== Explore Your igraph Data =====================#
#==================================================================#
# In this section, you will explore the igraph object named        #
# "Stucont". You will see the summary of the igraph object with    #
# its data structure.                                              #
#==================================================================#

#1. igraph summary
Stucont
gsize(Stucont)
gorder(Stucont)

#2. Nodelist
V(Stucont)

#3. Edgelist
E(Stucont)

#4. Attributes
V(Stucont)$Gender
V(Stucont)$Gender[V(Stucont)$Gender=='Unknown'] <- NA
V(Stucont)$Class

#5. Adjacency matrix
Stucont[c(1:10),c(1:10)]

#==================================================================#
#===================== Measuring Centrality =======================#
#==================================================================#
# In this section, you will measure the centrality of the igraph   #
# object, "Stucont". You will be able to see how the theoretical   #
# concept of each centrality such as degree, eigenvector, and      #
# betweenness centrality is measured by the igraph.                #
#==================================================================#

#1. Degree centrality
Stucont_deg<-degree(Stucont,mode=c("All"))
V(Stucont)$degree<-Stucont_deg
V(Stucont)$degree
which.max(Stucont_ind)

#2. Eigenvector centrality

#3. Betweenness centrality
Stucont_bw<- betweenness(Stucont, directed = FALSE)
V(Stucont)$betweenness<-Stucont_bw
V(Stucont)$betweenness
which.max(Stucont_bw)
DF<-as_long_data_frame(Stucont)

#==================================================================#
#================== Measuring Network Structure ===================#
#==================================================================#
# In this section, you will measure the indicators of the network  #
# structure such as network density, assortativity.                #
#==================================================================#

#1. Network Density
edge_density(Stucont) # Global density
A1<-induced_subgraph(Stucont, V(Stucont)[Class=="1A"], impl=c("auto")) # Subgraphing into each class
edge_density(A1) # Class level density

#2. Assortativity
values <- as.numeric(factor(V(Stucont)$Grade))
assortativity_nominal(Stucont, types=values)
library(tidyr)
D_meta<-separate(D_meta, Class, into=c("Grade","Class"), sep = 1)

#3. Calculate the observed assortativity
observed.assortativity <- assortativity_nominal(Stucont, types=as.numeric(factor(V(Stucont)$Grade)))
results <- vector('list', 1000)
for(i in 1:1000){results[[i]] <- assortativity_nominal(Stucont, sample(values))}

# Plot the distribution of assortativity values and add a red vertical line at the original observed value
hist(unlist(results), xlim = c(0,0.4))
abline(v = observed.assortativity,col = "red", lty = 3, lwd=2)                                                             

#==================================================================#
#===================== Network Visualization ======================#
#==================================================================#
# In this section, you will retrieve csv format network data file  #
# to the R studio as a dataframe and convert it into the igraph    #
# object.                                                          #
#==================================================================#

#1. Plotting a network with the degree centrality

library(RColorBrewer) # This is the color library
pal<-brewer.pal(length(unique(V(Stucont)$Class)), "Set3") # Vertex color assigned per each class number
plot(Stucont,edge.color = 'black',vertex.label.cex =0.5, 
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_ind)/3, edge.width=sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

#2. Plotting a network with the degree centrality

plot(Stucont,edge.color = 'black',vertex.label.cex =0.5, 
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_bw)/3, edge.width=sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

#3. Plotting a scatter plot to see the correlation 
# between degree and betweenness centrality

plot(V(Stucont)$degree, V(Stucont)$betweenness)

#==================================================================#
#====================== Community Detection =======================#
#==================================================================#
# In this section, you will retrieve csv format network data file  #
# to the R studio as a dataframe and convert it into the igraph    #
# object.                                                          #
#==================================================================#

#1. Louvain clustering
lc <- cluster_louvain(Stucont) # Create a cluster based on the Louvain method
communities(lc) # You can check which vertices belongs to which clusters.

#2. Plotting the Betweenness Centrality network with the community detection

set.seed(1001) # To duplicate the computer process and create exactly the same network repetitively you should set the seed.
plot(lc, Stucont, edge.color = 'black',vertex.label.cex =0.5, 
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_bw)/3, edge.width=sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

     