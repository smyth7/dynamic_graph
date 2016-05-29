# Plotting a dynamic graph

# http://estebanmoro.org/2015/12/temporal-networks-with-r-and-igraph-updated/

require(igraph)
par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,0))
g = watts.strogatz.game(1,20,3,0.4)
layout.old = layout_with_fr(g)
for(i in 1:4){
  layout.new = layout_with_fr(g,niter=10,coords=layout.old,
                              start.temp=0.05,grid="nogrid")
  plot(g,layout=layout.new)
  layout.old = layout.new
}


#this version of the script has been tested on igraph 1.0.1
#load libraries
library(RcolorBrewer)

#load the edges with time stamp
#there are three columns in edges: id1,id2,time
edges <- read.table("edges.csv",header=T)

#generate the full graph
g <- graph.data.frame(edges,directed=F)

#generate a cool palette for the graph (darker colors = older nodes)
YlOrBr.pal <- colorRampPalette(brewer.pal(8,"YlOrRd"))
#colors for the nodes are chosen from the very beginning
V(g)$color <- rev(YlOrBr.pal(vcount(g)))[as.numeric(V(g)$name)]

#time in the edges goes from 1 to 300. We kick off at time 3
ti <- 3
#remove edges which are not present
gt <- delete_edges(g,which(E(g)$time > ti))
#generate first layout using graphopt with normalized coordinates. This places the initially connected set of nodes in the middle. If you use fruchterman.reingold it will place that initial set in the outer ring.
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)

#total time of the dynamics
total_time <- max(E(g)$time)
#This is the time interval for the animation. In this case is taken to be 1/10
#of the time (i.e. 10 snapshots) between adding two consecutive nodes
dt <- 0.1
#Output for each frame will be a png with HD size 1600x900 <img draggable="false" class="emoji" alt="🙂" src="https://s.w.org/images/core/emoji/72x72/1f642.png">
png(file="animation/example%03d.png", width=1600,height=900)
#Time loop starts
for(time in seq(3,total_time,dt)){
  #remove edges which are not present
  gt <- delete_edges(g,which(E(g)$time > time))
  #with the new graph, we update the layout a little bit
  layout.new <- layout_with_fr(gt,coords=layout.old,niter=10,start.temp=0.05,grid="nogrid")
  #plot the new graph
  plot(gt,layout=layout.new,vertex.label="",vertex.size=1+2*log(degree(gt)),vertex.frame.color=V(g)$color,edge.width=1.5,asp=9/16,margin=-0.15)
  #use the new layout in the next round
  layout.old <- layout.new
}
dev.off()


# some tests

g <- make_ring(10) %>% delete_edges(seq(1, 9, by = 2))
g
g$layout <- layout_in_circle
plot(g)
tkplot(g)
rglplot(g)
plot(g)

g <- make_ring(10)
g$layout <- layout_in_circle
plot(g)
