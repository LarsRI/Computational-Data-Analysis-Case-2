library(ggplot2)
library(reshape2)
library(ggdendro)
library(gridExtra)
library(grid)
library(gtable)
library(data.table)
library(archetypes)
setwd("~/DTU/Computational Data Analysis/Case 2 - DLi-MI/")

# Read and format the data
options("datatable.fread.dec.locale" = "fr_FR.UTF-8")
dat <- fread("dtu data_final.csv", encoding = "Latin-1", dec = ',')

dat[, RX:=RX - ADHD - Diabetes - Astma]
dat[, OTC:=OTC - Rygestop]
dat <- dat[RX > 0, ]

# Load archetypes
load("archetypes.RData")

x <- aa7$alphas
colnames(x) <- paste("A", 1:7)

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
setwd("figures7/")
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



myTheme <- theme_minimal(base_size = 30) +
  theme(#axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 90),
        panel.grid = element_blank(),
        legend.text = element_text(size = 15))


pD <- melt(aa7$archetypes)
aTypes <- paste("A", 1:7)

pD$Var1 <- factor(rep(aTypes, 7))#, levels = aTypes[col.ord])
pD$value <- pD$value * 100

barPlot <- ggplot(pD, aes(x = Var1, fill = Var2, y = value)) + geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Set3", name = "Sale") + 
    scale_x_discrete(name = element_blank()) +
    scale_y_discrete(name = "Percentage", expand = c(0,0), limits = seq(0, 100, length.out = 11)) +
    myTheme

ggsave(barPlot, filename = "archetypes.png", width = 30, height = 20, units = 'cm')

tmp <- data.table(aa7$alphas)
setnames(tmp, paste("Archetype", 1:7))
pD2 <- melt(cbind(tmp, dat[, list(Habitat, Region, Chain)]), id.vars = c('Habitat', 'Region', 'Chain'))
pD2$value <- pD2$value
pD2[, Habitat:=gsub(" ", "\n", Habitat)]
pD2[, Habitat:=gsub("Metropolomegn", "Metropol-\nomegn", Habitat)]


dataHabitat <- pD2[, list(value = sum(value)), by = c('Habitat', 'variable')]
dataHabitat[, value2:=(value/sum(value))*100, by = Habitat]

barPlot2 <- ggplot(dataHabitat, aes(fill = variable, x = Habitat, y = value)) + geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Set3", name = element_blank()) + 
    scale_x_discrete(name = element_blank()) +
    scale_y_discrete(name = "Observations", expand = c(0,0), limits = seq(0, 150, length.out = 16)) +
    myTheme

barPlot3 <- ggplot(dataHabitat, aes(fill = variable, x = Habitat, y = value2)) + geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Set3", name = element_blank()) + 
    scale_x_discrete(name = element_blank()) +
    scale_y_discrete(name = "Percentage", expand = c(0,0), limits = seq(0, 100, length.out = 11)) +
    myTheme

ggsave(barPlot2, filename = "habitat_1.png", width = 30, height = 20, units = 'cm')
ggsave(barPlot3, filename = "habitat_2.png", width = 30, height = 20, units = 'cm')




dataRegion <- pD2[, list(value = sum(value)), by = c('Region', 'variable')]
dataRegion[, value2:=(value/sum(value))*100, by = Region]

barPlot4 <- ggplot(dataRegion, aes(fill = variable, x = Region, y = value)) + geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Set3", name = element_blank()) + 
    scale_x_discrete(name = element_blank()) +
    scale_y_discrete(name = "Observations", expand = c(0,0), limits = seq(0, 130, length.out = 14)) +
    myTheme

barPlot5 <- ggplot(dataRegion, aes(fill = variable, x = Region, y = value2)) + geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Set3", name = element_blank()) + 
    scale_x_discrete(name = element_blank()) +
    scale_y_discrete(name = "Percentage", expand = c(0,0), limits = seq(0, 100, length.out = 11)) +
    myTheme

ggsave(barPlot4, filename = "region_1.png", width = 30, height = 20, units = 'cm')
ggsave(barPlot5, filename = "region_2.png", width = 30, height = 20, units = 'cm')





dataChain <- pD2[, list(value = sum(value)), by = c('Chain', 'variable')]
dataChain[, value2:=(value/sum(value))*100, by = Chain]

barPlot6 <- ggplot(dataChain, aes(fill = variable, x = Chain, y = value)) + geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Set3", name = element_blank()) + 
    scale_x_discrete(name = element_blank()) +
    scale_y_discrete(name = "Observations", expand = c(0,0), limits = seq(0, 160, length.out = 17)) +
    myTheme

barPlot7 <- ggplot(dataChain, aes(fill = variable, x = Chain, y = value2)) + geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Set3", name = element_blank()) + 
    scale_x_discrete(name = element_blank()) +
    scale_y_discrete(name = "Percentage", expand = c(0,0), limits = seq(0, 100, length.out = 11)) +
    myTheme

ggsave(barPlot6, filename = "chain_1.png", width = 30, height = 20, units = 'cm')
ggsave(barPlot7, filename = "chain_2.png", width = 30, height = 20, units = 'cm')




befolkDat <- data.table(income = rowSums(dat[, list(OTC, Branded, RX)]))
befolkDat[, grp:=factor(max.col(coef(aa7)))]
befolkDat[, mean(income), by = grp]



total <- fread("../total.csv")
total <- total[dat$Modul, -1, with = FALSE]

pls <- plsr(coef(aa7) ~ as.matrix(total))

xShuf <- apply(as.matrix(total), 2, function(x)x[sample(length(x))])
plsShuf <- plsr(coef(aa7) ~ xShuf)
cutOff <- max(explvar(plsShuf))

nComp <- max(which(explvar(pls) > cutOff))
pls2 <- plsr(coef(aa7) ~ as.matrix(total), ncomp = nComp)


pls2$loadings

pD3 <- data.table(as.matrix(pls2$scores[]))
setnames(pD3, c("Comp1", "Comp2"))
pD3[, atype:=factor(max.col(coef(aa7)))]
pD3 <- cbind(pD3, dat)


ggplot(pD3, aes(Comp1, Comp2, colour = atype)) + geom_point(position = position_jitter(width = 500, height = 500)) + scale_color_brewer(palette = "Set2")


ggplot(pD3, aes(Comp1, Comp2, colour = Habitat)) + geom_point(position = position_jitter(width = 500, height = 500)) + scale_color_brewer(palette = "Set2")
ggplot(pD3, aes(Comp1, Comp2, colour = Chain)) + geom_point(position = position_jitter(width = 500, height = 500)) + scale_color_brewer(palette = "Set2")
pD3[, selected:=ifelse(atype %in% c('3', '4'), 'Yes', 'No')]
pD3[, shapeD:=selected]
pD3[atype == '3', shapeD:="t1"]
pD3[atype == '4', shapeD:="t2"]
pD3[, atype:=paste("Archetype", atype)]

p1 <- ggplot(pD3, aes(Comp1, Comp2, colour = Region)) + 
  geom_point(position = position_jitter(width = 1000, height = 1000), size = 3) + 
  scale_color_brewer(palette = "Set2", name = element_blank()) +
  theme_bw(base_size = 25) +
  scale_x_continuous(name = element_blank()) +
  scale_y_continuous(name = element_blank()) +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 15))

  
p2 <- ggplot(pD3[atype %in% c('Archetype 3', 'Archetype 4')], aes(Comp1, Comp2, colour = atype)) + 
  geom_point(position = position_jitter(width = 500, height = 500), size = 3) + 
  scale_color_brewer(palette = "Set1", name = element_blank()) +
  scale_x_continuous(name = element_blank()) +
  scale_y_continuous(name = element_blank()) +
  theme_bw(base_size = 25) +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 15))

ggsave(p1, filename = "pls_1.png", width = 20, height = 20, units = 'cm')
ggsave(p2, filename = "pls_2.png", width = 20, height = 20, units = 'cm')
