library(data.table)
library(archetypes)
library(reshape2)

options("datatable.fread.dec.locale" = "fr_FR.UTF-8")

befolk <- fread("befolk_16.csv", encoding = "Latin-1", dec = ',')
dat <- fread("dtu data_final.csv", encoding = "Latin-1", dec = ',')

males <- dcast(befolk[, list(ALDER, Modul, M)], Modul ~ ALDER, value.var = 'M')[, -1]
females <- dcast(befolk[, list(ALDER, Modul, K)], Modul ~ ALDER, value.var = 'K')[, -1]
total <- dcast(befolk[, list(ALDER, Modul, TOTAL)], Modul ~ ALDER, value.var = 'TOTAL')[, -1]

write.csv(males, file = "males.csv")
write.csv(females, file = "females.csv")
write.csv(total, file = "total.csv")