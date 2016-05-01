library(data.table)
library(NMF)
library(archetypes)
library(ica)
befolkDat <- fread("total.csv")
befolkDat[, V1:=NULL]

### NMF
nmfResult <- nmf(t(befolkDat), rank = 1:10)
plot(nmfResult)

nmfResultRand <- nmf(randomize(t(befolkDat)), rank = 1:10)
plot(nmfResultRand)

nmfSelected <- nmf(befolkDat, rank = 3, seed = "nndsvd")
fit(nmfSelected)
summary(nmfSelected)
# get matrix W
w <- basis(nmfSelected)
dim(w)

# get matrix H
h <- coef(nmfSelected)
dim(h)

extractFeatures(nmfSelected)

### Archetypical Analysis 
options("datatable.fread.dec.locale" = "fr_FR.UTF-8")
dat <- fread("dtu data_final.csv", encoding = "Latin-1", dec = ',')

dat[, RX:=RX - ADHD - Diabetes - Astma]
dat[, OTC:=OTC - Rygestop]
dat <- dat[RX > 0, ]

befolkDat <- dat[, list(ADHD, Diabetes, OTC, Branded, Rygestop, Astma, RX)]

befolkDat <- befolkDat / rowSums(befolkDat)
archTest <- stepArchetypes(befolkDat, k = 1:15, nrep = 5)
screeplot(archTest)

aa7 <- bestModel(archTest[[7]])
aa7$archetypes
save(archTest, aa7, file = "archetypes.RData")
write.csv(aa7$archetypes, file = "arcehtypes.csv")
write.csv(aa7$alphas, file = "arcehtypesScores.csv")
write.csv(befolkDat, file = "befolkDat.csv")

d <- dist(aa7$alphas)
hcl <- hclust(d, method = "ward.D2")
plot(hcl)


ir.pca <- prcomp(aa7$alphas,
                 center = TRUE,
                 scale. = TRUE) 
biplot(ir.pca)
ggbiplot(ir.pca)







### Archetypical Analysis2 
options("datatable.fread.dec.locale" = "fr_FR.UTF-8")
dat <- fread("dtu data_final.csv", encoding = "Latin-1", dec = ',')

#dat[, RX:=RX - ADHD - Diabetes - Astma]
#dat[, OTC:=OTC - Rygestop]
#dat <- dat[RX > 0, ]

befolkDat <- dat[, list(OTC, Branded, RX)]

befolkDat <- befolkDat / rowSums(befolkDat)
archTest <- stepArchetypes(befolkDat, k = 3, nrep = 25)
screeplot(archTest)

aa3 <- bestModel(archTest[[1]])
aa3$archetypes
save(archTest, aa3, file = "archetypes3.RData")
write.csv(aa3$archetypes, file = "arcehtypes2.csv")
write.csv(aa3$alphas, file = "arcehtypesScores2.csv")
write.csv(befolkDat, file = "befolkDat2.csv")

d <- dist(aa3$alphas)
hcl <- hclust(d, method = "ward.D2")
plot(hcl)











### ICA
ica <- icafast(befolkDat, nc = 2)


