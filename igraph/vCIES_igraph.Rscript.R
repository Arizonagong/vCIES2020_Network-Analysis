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
## Warning: There can be an error to get access to the urls at bellow.
## Although you get the error message and fail to create data frames,
## you should try it multiple times until you succeed because the error occurs randomly depending on
## the server condition.
## If you fail to load the data through this method, then just download the file
## from the link uploaded at my github mainpage (readme), and manually load it.

urlfile1="https://raw.githubusercontent.com/Arizonagong/vCIES2020_Network-Analysis/master/igraph/primaryschool.csv"
urlfile2="https://raw.githubusercontent.com/Arizonagong/vCIES2020_Network-Analysis/master/igraph/metadata_primaryschool.csv"
D<-read_csv(url(urlfile1))
D_meta<-read_csv(url(urlfile2))

#2. Manage dataset
B<-as.data.frame(table(D)) # Create an edge weight column named "Freq"
B1<-subset(B,Freq>0) # Delete all the edges having weight equal to 0

#3. Create an igraph object from the dataframes
library(igraph)
# Unless your edgelist in B1 is recognized as 'factor' it will produce an error
######
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
which.max(Stucont_deg)

#2. Eigenvector centrality
Stucont_eig <- evcent(Stucont)$vector
V(Stucont)$Eigen<-Stucont_eig
V(Stucont)$Eigen
which.max(Stucont_eig)

#3. Betweenness centrality
Stucont_bw<-betweenness(Stucont, directed = FALSE)
V(Stucont)$betweenness<-Stucont_bw
V(Stucont)$betweenness
which.max(Stucont_bw)

DF<-as_long_data_frame(Stucont)
Stucont

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
values <- as.numeric(factor(V(Stucont)$Class))
assortativity_nominal(Stucont, types=values)

#2.1. Calculate the observed assortativity
observed.assortativity <- assortativity_nominal(Stucont, types=values)
results <- vector('list', 1000)
for(i in 1:1000){results[[i]] <- assortativity_nominal(Stucont, sample(values))}

#2.2.  Plot the distribution of assortativity values and add a red vertical line at the original observed value
hist(unlist(results), xlim = c(0,0.4))
abline(v = observed.assortativity,col = "red", lty = 3, lwd=2)

#==================================================================#
#===================== Network Visualization ======================#
#==================================================================#

#1. Plotting a network with the degree centrality

set.seed(1001)
library(RColorBrewer) # This is the color library
pal<-brewer.pal(length(unique(V(Stucont)$Class)), "Set3") # Vertex color assigned per each class number
plot(Stucont,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_deg)/3, edge.width=sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

#1. Plotting a network with the eigenvector centrality

set.seed(1001)
plot(Stucont,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_eig)*10, edge.width=sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

#2. Plotting a network with the betweenness centrality

set.seed(1001)
plot(Stucont,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_bw)/3, edge.width=sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

#3. Plotting a scatter plot to see the correlation

#3.1. between degree and betweenness centrality

plot(V(Stucont)$degree, V(Stucont)$betweenness)

#3.2. between degree and eigenvector centrality

plot(V(Stucont)$degree, V(Stucont)$Eigen)

#==================================================================#
#====================== Community Detection =======================#
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

