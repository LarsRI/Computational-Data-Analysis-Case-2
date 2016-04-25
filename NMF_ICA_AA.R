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

nmfSelected <- nmf(t(befolkDat), rank = 3, seed = "nndsvd")
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
archTest <- stepArchetypes(befolkDat, k = 1:15, nrep = 5)
screeplot(archTest)


### ICA
ica <- icafast(befolkDat, nc = 10)
