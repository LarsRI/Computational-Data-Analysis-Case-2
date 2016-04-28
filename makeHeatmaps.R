library(ggplot2)
library(reshape2)
library(ggdendro)
library(gridExtra)
library(grid)
library(gtable)

# Read and format the data
options("datatable.fread.dec.locale" = "fr_FR.UTF-8")
dat <- fread("dtu data_final.csv", encoding = "Latin-1", dec = ',')

dat[, RX:=RX - ADHD - Diabetes - Astma]
dat[, OTC:=OTC - Rygestop]
dat <- dat[RX > 0, ]

# Load archetypes
load("archetypes.RData")

x <- aa7$alphas
colnames(x) <- paste("Archetype", 1:7)

# Get row and col order
dd.row <- as.dendrogram(hclust(dist(x)))
row.ord <- order.dendrogram(dd.row)

dd.col <- as.dendrogram(hclust(dist(t(x))))
col.ord <- order.dendrogram(dd.col)

# Reorder the data, and prepare dataframe for plotting
xx <- x[row.ord, col.ord]
df <- as.data.frame(xx)

df$id <- seq_len(nrow(df))
df$Habitat <- dat[row.ord, Habitat]
df$Region <- dat[row.ord, Region]
df$Chain <- dat[row.ord, Chain]

mdf <- melt(df, id.vars=c("id", "Habitat", "Region", "Chain"))

# Define plotting function
plotFn <- function(p1)
{
    theme_none <- theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
    )
    
    ### Get dendrogram data and create plots
    ddata_y <- dendro_data(dd.row)
    ddata_x <- dendro_data(dd.col)
    
    ### Extract-the-legend-function:
    g_legend<-function(a.gplot){ 
        tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
        legend <- tmp$grobs[[leg]] 
        return(legend)} 
    
    ### Create plot components ###    
    # Heatmap
    leg <- g_legend(p1)
    p1 <- p1 + theme(legend.position="none")
    
    # Dendrogram 1
    p2 <- ggplot(segment(ddata_x)) + 
        geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
        scale_x_continuous(expand = c(0,0), limits = range(segment(ddata_x)$x) + c(-0.5, 0.5)) +
        theme_none + theme(axis.title.x=element_blank()) + theme(panel.margin = unit(c(0,0,0,0), "cm"), plot.margin = unit(c(0,0,0,0), "cm")) + labs(x=NULL, y=NULL)
    
    # Dendrogram 2
    p3 <- ggplot(segment(ddata_y)) + 
        geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
        coord_flip() + scale_x_continuous(expand = c(0,0), limits = range(segment(ddata_y)$x) + c(-0.5, 0.5)) + theme_none + theme(panel.margin = unit(c(0,0,0,0), "cm"), plot.margin = unit(c(0,0,0,0), "cm")) + labs(x=NULL, y=NULL)
    
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))
    g3 <- ggplot_gtable(ggplot_build(p3))
    
    maxWidth <- unit.pmax(g1$widths, g2$widths)
    g1$widths <- maxWidth
    g2$widths <- maxWidth
    
    maxHeight <- unit.pmax(g1$heights, g3$heights)
    g1$heights <- maxHeight
    g3$heights <- maxHeight
    
    
    gl <- list(g1, g2, g3, leg)
    
    
    ### Draw graphic ###
    gt <- gtable(widths=unit(rep(1,11), "null"),
                 heights=unit(rep(1,10), "null"))
    
    gt2 <- gtable_add_grob(gt, gl, 
                           l=c(1,1,8,10),
                           r=c(7,7,9,11),
                           t=c(2,1,2,2),
                           b=c(10,1,10,10))
    gt2
}

p1 <- ggplot(mdf, aes(x=variable, y=id, alpha = value, fill = Habitat)) + theme_bw(base_size = 15) +
    geom_tile() + theme(panel.margin = unit(c(0,0,0,0), "cm"), plot.margin = unit(c(0,0,1,0), "lines")) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) + 
    scale_alpha(guide = FALSE, range = c(0, 1)) +
    labs(x=NULL, y=NULL) +
    theme(panel.grid = element_blank())

pHabitat <- plotFn(p1)
grid.newpage()
grid.draw(pHabitat)
ggsave(plot = pHabitat, filename = "colourByHabitat.png", width = 30, height = 20, units = 'cm')



p2 <- ggplot(mdf, aes(x=variable, y=id, alpha = value, fill = Region)) + theme_bw() +
    geom_tile() + theme(panel.margin = unit(c(0,0,0,0), "cm"), plot.margin = unit(c(0,0,1,0), "lines")) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) + 
    scale_alpha(guide = FALSE, range = c(0, 1)) +
    labs(x=NULL, y=NULL) +
    theme(panel.grid = element_blank())

pRegion <- plotFn(p2)
grid.newpage()
grid.draw(pRegion)
ggsave(plot = pRegion, filename = "colourByRegion.png", width = 30, height = 20, units = 'cm')



p3 <- ggplot(mdf, aes(x=variable, y=id, alpha = value, fill = Chain)) + theme_bw() +
    geom_tile() + theme(panel.margin = unit(c(0,0,0,0), "cm"), plot.margin = unit(c(0,0,1,0), "lines")) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) + 
    scale_alpha(guide = FALSE, range = c(0, 1)) +
    labs(x=NULL, y=NULL) +
    theme(panel.grid = element_blank())

pChain <- plotFn(p3)
grid.newpage()
grid.draw(pChain)
ggsave(plot = pChain, filename = "colourByChain.png", width = 30, height = 20, units = 'cm')
