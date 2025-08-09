library(ggplot2)
library(ggraph)
library(sf)
library(igraph)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggrepel)
library(data.table)
setwd("D:/AFRE/My Research/HPCC/Price/Data")


# Load country and state data
world <- ne_countries(scale = "medium", returnclass = "sf")
nigeria_states <- ne_states(country = "Nigeria", returnclass = "sf")

# Load node and edge data
nodes <- fread('market_nodes.csv')
edges <- fread('market_edges.csv')
nodes<-nodes[Urban==1,Type:="Urban"][Urban==0,Type:="Rural"]
nodes<-nodes[Zone=="South East"|Zone=="South South"|Zone=="South West",Region:="South"][Zone=="North East"|Zone=="North West"|Zone=="North Central",Region:="North"]
nodes<-nodes[Id!=121,]
nodes<-nodes[Id!=67,]

nodes_urban<-nodes[Urban==1,]
nodes_rural<-nodes[Urban==0,]

edges_urbanturban<-edges[Source%in%nodes_urban$Id&Target%in%nodes_urban$Id,]
edges_urbantrural<-edges[(Source%in%nodes_urban$Id&Target%in%nodes_rural$Id)|(Source%in%nodes_rural$Id&Target%in%nodes_urban$Id),]
edges_ruraltrural<-edges[(Source%in%nodes_rural$Id&Target%in%nodes_rural$Id),]


# Create an igraph object
graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Extracting the coordinates for the nodes
coords <- data.frame(id = nodes$Id, Longitude = nodes$Longitude, Latitude = nodes$Latitude, size = nodes$size, Type=as.factor(nodes$Region))

# Define larger cities and their coordinates
larger_cities <- data.frame(
  city = c("Kano", "Bauchi", "Gombe", "Sokoto", "Jimeta", "Enugu", "Ilorin", "Port Harcourt"),
  Longitude = c(8.5919, 9.8442, 11.1670, 5.2476, 12.4584, 7.5086, 4.5421, 7.0498),
  Latitude = c(12.0022, 10.3157, 10.2897, 13.0059, 9.2795, 6.5244, 8.4966, 4.8156)
)

# Base plot with Nigeria map and state boundaries
g <- ggplot() +
  geom_sf(data = nigeria_states, fill = "#f5f5f5", color = "#aaaaaa") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank())

# Adding curved edges with adjusted transparency and smaller arrow
for(edge in E(graph)) {
  start_node <- ends(graph, edge, names = TRUE)[1]
  end_node <- ends(graph, edge, names = TRUE)[2]
  
  if (start_node != end_node) {
    start_coord <- coords[which(coords$id == start_node), ]
    end_coord <- coords[which(coords$id == end_node), ]
    
    # Check for NA values and closely located nodes
    if (!is.na(start_coord$Longitude) && !is.na(end_coord$Longitude) &&
        !(start_coord$Longitude == end_coord$Longitude && start_coord$Latitude == end_coord$Latitude)) {
      
      edge_alpha <- ifelse(degree(graph, mode = 'out')[start_node] > 7, 0.2, 0.5) # More transparent
      
      g <- g + geom_curve(data = data.frame(x = start_coord$Longitude, y = start_coord$Latitude, 
                                            xend = end_coord$Longitude, yend = end_coord$Latitude), 
                          aes(x = x, y = y, xend = xend, yend = yend), 
                          color = "#1f78b4", size = 1, alpha = edge_alpha, curvature = 0.2,
                          arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "last")) # Smaller arrow
    }
  }
}

# Adding nodes
g <- g + geom_point(data = coords, aes(x = Longitude, y = Latitude, size = size, color = Type), alpha = 0.8) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(values = c('red','blue'))

# Adding labels for larger cities
g <- g + geom_point(data = larger_cities, aes(x = Longitude, y = Latitude), color = 'white', size = 0.05) +
  geom_text_repel(data = larger_cities, aes(x = Longitude, y = Latitude, label = city), 
                  size = 6, box.padding = 0.5, point.padding = 0.5, color = "#2e2e2e")+
  theme(
    legend.position = "right",            # Position the legend at the bottom
    legend.title = element_blank(),        # Remove the legend title
    legend.text = element_text(size = 14), # Increase the text size of the legend
    legend.key.size = unit(1.5, "lines")   # Increase the size of the legend keys (symbols)
  ) +
  guides(color = guide_legend(override.aes = list(size = 6))) # Adjust the size of the legend symbols

g

# Save the plot to a PDF file
ggsave("nigeria_market_network_northSouth.pdf", plot = g, width = 11, height = 8, units = "in")


g<-graph_from_data_frame(d=edges,vertices = nodes, directed = T)

nodes$degree_centrality<-degree(g)

