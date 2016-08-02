

library(deal)
library(stringr)
#source("ade-tbl8.R")

pv<-data.frame(heart_diseases = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2), 
               obstruction = c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2), 
               stenosis = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2), 
               thrombus = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2), 
               autonomic_neuropathy = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                        1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
               medication = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
               n.tot = c(11, 40, 38, 6, 15, 13, 8, 32, 25, 22, 21, 7, 5, 9, 2, 1, 45, 29, 36, 17, 16, 24, 2, 1, 34, 35, 27, 1, 28, 46, 2, 1, 33, 30, 44, 7, 18, 20, 2, 2, 19, 26, 3, 1, 12, 31, 1, 1, 37, 42, 17, 32, 26, 41, 23, 1, 39, 4, 14, 1, 43, 10, 1, 1), 
               n.ade = c(17, 21, 25, 8, 13, 16, 14, 14, 15, 24, 23, 1, 19, 32, 14, 1, 36, 37, 14, 14, 22, 32, 14, 1, 35, 11, 1, 1, 6, 18, 1, 1, 33, 39, 3, 14, 5, 9, 2, 1, 30, 31, 28, 1, 38, 12, 1, 1, 34, 7, 20, 20, 4, 29, 1, 1, 10, 26, 2, 1, 27, 14, 1, 1))



factor.nc <- ncol(pv)-3
print(factor.nc)
confoundingAndMedNames <- names(pv[,1:factor.nc])
print(confoundingAndMedNames)
for (pv.df.i in 1:factor.nc) {
  print(pv.df.i)
  pv[,pv.df.i] <- as.factor(pv[,pv.df.i])
  print(confoundingAndMedNames[pv.df.i])
  fieldName <-  substr(x = confoundingAndMedNames[pv.df.i], start = 7, stop = nchar(confoundingAndMedNames[pv.df.i]))
  print(fieldName)
  names(pv[,pv.df.i]) <- fieldName 
}
pv.nw <- network(pv)
pv.prior <- jointprior(pv.nw)
#blist <- matrix(c(seq(1:(ncol(pv)-1)), rep((ncol(pv)-1), (ncol(pv)-1))), ncol=2)
#blist
#banlist(pv.nw) <- blist
#blist <- matrix(c(c(seq(1:(ncol(pv)-1)), seq(1:(ncol(pv)))),
#                c(rep((ncol(pv)), (ncol(pv)-1)), rep((ncol(pv-2)), (ncol(pv))))), ncol=2)
#fromArray <- c(seq(1:(ncol(pv)-1)), seq(1:(ncol(pv))))
#toArray <- c(rep((ncol(pv)-1), (ncol(pv)-1)), rep((ncol(pv)-2), (ncol(pv))))
#blist <- matrix(c(fromArray, toArray), ncol=2)
banlist(pv.nw) <- blist
pv.nw.v2 <- learn(pv.nw, pv, pv.prior)$nw
# perform structural search
pv.search <- autosearch(pv.nw.v2, pv, pv.prior, trace=TRUE)
pv.heuristic <- heuristic(pv.search$nw, pv, pv.prior, restart = 2, degree = 10, trace = TRUE, trylist = pv.search$trylist)
best.pv <- pv.heuristic$nw

