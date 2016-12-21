#### Similarity Analysis

## load necessary libraries
require(FactoMineR)
require(vegan)
require(car)
require(ape)
require(sparcl)
require(ggdendro)
require(ggplot2)
require(gridExtra)

## Load data for single sample analysis
count <- read.table('Zurro2015data.txt', header = T, row.names =1)

## Define sample names
sample_names <- c('B1', 'B13', 'B3', 'B7', 'B6', 'B12', 'B10', 'B9', 'B8', 
        'B11', 'B5', 'B2', 'B4', 'M1', 'M2', 'M3', 'M4', 'M5', 'M6', 'M7', 
        'M8', 'M9', 'M10', 'M11', 'M12', 'M13','M14', 'M15', 'M16', 'M17', 
        'M18', 'M19', 'M20', 'M21', 'M22', 'M23')

## Subset data for analysis of single samples
subset_indices <- matrix(c(1,8, 9, 16, 17, 24, 25, 32, 33, 40, 41, 48, 49, 
            60, 61, 72, 73, 80, 81, 86, 87, 94, 95, 102, 103, 110, 111, 118, 
            119, 130, 131, 142, 143, 154, 155, 166, 167, 178, 179, 186, 187, 
            194, 195, 202, 203, 210, 211, 218, 219, 226, 227, 238, 239, 246, 
            247, 254, 255, 261, 262, 269, 270, 277, 278, 285, 286, 297, 298, 
            305, 306, 313, 314, 321), nrow=36, byrow=T)
subset_list <- apply(subset_indices, 1, function(x) subset(count[x[1]:x[2],]))

## Perform similarity analysis (they are able to handle unknown and variable 
# sample size) Chao (takes into account unseen species)
dist_list <- Map(function(...) vegdist(..., method='chao', binary=F, diag=F, 
            upper=F, na.rm=F), subset_list)

## Create dendrograms for the analyses per sample
plot_list <- list()
for (i in 1:length(dist_list)) {
    hc <- hclust(dist(dist_list[[i]]))
    plot_list[[i]] <- ggdendrogram(hc, rotate = T, size = 4, theme_dendro = F) + 
        labs(title = sample_names[i], y = 'dissimilarity (Chao index)', x='') + 
        theme(panel.background = element_blank(), axis.ticks = element_blank(), 
                axis.title.x = element_text(size=8, vjust=0.1))
}

## Remove unnecessary plots
plot_list <- plot_list[-c(6, 10, 27)]

## Export plots in grids
postscript('grid_plots1.eps', width=14, height=10, horizontal=F)
    do.call('grid.arrange', c(plot_list[1:12], ncol=3)); dev.off()
postscript('grid_plots2.eps', width=14, height=10, horizontal=F)
    do.call('grid.arrange', c(plot_list[13:24], ncol=3)); dev.off()
postscript('grid_plots3.eps', width=10, height=10, horizontal=F)
    do.call('grid.arrange', c(plot_list[25:33], ncol=3)); dev.off()

