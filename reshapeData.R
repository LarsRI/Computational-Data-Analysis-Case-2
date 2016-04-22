setwd("~/DTU/Computational Data Analysis/Case 2 - DLi-MI/")
library(data.table)
library(archetypes)
library(reshape2)

options("datatable.fread.dec.locale" = "fr_FR.UTF-8")

befolk <- fread("befolk_16.csv", encoding = "Latin-1", dec = ',')
dat <- fread("dtu data_final.csv", encoding = "Latin-1", dec = ',')

males <- dcast(befolk[, list(ALDER, Modul, M)], Modul ~ ALDER, value.var = 'M')[, -1]
females <- dcast(befolk[, list(ALDER, Modul, K)], Modul ~ ALDER, value.var = 'K')[, -1]
total <- dcast(befolk[, list(ALDER, Modul, TOTAL)], Modul ~ ALDER, value.var = 'TOTAL')[, -1]

d <- dist(total)
hc <- hclust(d, method = "ward.D2")
plot(hc)
