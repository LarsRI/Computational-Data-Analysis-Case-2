library(data.table)
library(archetypes)
library(reshape2)

dat <- fread("befolkDat.csv")

males <- dcast(befolk[, list(ALDER, Modul, M)], Modul ~ ALDER, value.var = 'M')[, -1]
females <- dcast(befolk[, list(ALDER, Modul, K)], Modul ~ ALDER, value.var = 'K')[, -1]
total <- dcast(befolk[, list(ALDER, Modul, TOTAL)], Modul ~ ALDER, value.var = 'TOTAL')[, -1]

write.csv(males, file = "males.csv")
write.csv(females, file = "females.csv")
write.csv(total, file = "total.csv")