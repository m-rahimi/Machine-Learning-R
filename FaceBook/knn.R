closeAllConnections()
rm(list = ls())

library(data.table)
library(doParallel)

setwd('/work/Rproject/FB')
#setwd('C:/Users/Amin/Documents/R/FB')
train <- fread("subset/subtrain.csv", integer64 = "character")
test <- fread("subset/subtest.csv", integer64 = "character")

test$row_id <- seq(1:nrow(test))-1
print(object.size(train), units = "Mb")
print(object.size(test), units = "Mb")

#Scale 
scaley = 2
train[, y := 2*y]
test[, y := 2*y]

#Parameters
rad0 = 0.02
minfeq = 2 #minimum frequency in a domain
knn = 35


output <- matrix(data = NA, nrow = 0, ncol = 4, byrow = FALSE, dimnames = NULL)

#make first grid
binx = 0.4; biny = 0.4
nbinx = 10/binx; nbiny = (10*scaley)/biny; nbinxy = nbinx*nbiny
train[, nx := as.integer(floor(x/binx))]
train[nx == nbinx, nx := nbinx - 1]
train[, ny := as.integer(floor(y/biny))]
train[ny == nbiny, ny := nbiny - 1]
train[, nxy := ny*nbinx + nx]

test[, nx := as.integer(floor(x/binx))]
test[nx == nbinx, nx := nbinx - 1]
test[, ny := as.integer(floor(y/biny))]
test[ny == nbiny, ny := nbiny - 1]
test[, nxy := ny*nbinx + nx]

#second grid
binx2 = 0.05; biny2 = 0.05
nbinx2 = 10/binx2; nbiny2 = 10/biny2; nbinxy2 = nbinx2*nbiny2
train[, nx2 := as.integer(floor(x/binx2))]
train[nx2 == nbinx2, nx2 := nbinx2 - 1]
train[, ny2 := as.integer(floor(y/biny2))]
train[ny2 == nbiny2, ny2 := nbiny2 - 1]
train[, nxy2 := ny2*nbinx2 + nx2]

test[, nx2 := as.integer(floor(x/binx2))]
test[nx2 == nbinx2, nx2 := nbinx2 - 1]
test[, ny2 := as.integer(floor(y/biny2))]
test[ny2 == nbiny2, ny2 := nbiny2 - 1]
test[, nxy2 := ny2*nbinx2 + nx2]

nnbinx = binx/binx2; nnbiny = biny/biny2
bintable <- train[,.N, by=.(nx,ny,nxy)][order(nxy)]

################################
ii = 50
output <- matrix(data = NA, nrow = 0, ncol = 4, byrow = FALSE, dimnames = NULL)
gc()
cat("bin number ", ii ," from ", nbinxy, "\n")

dtest  <- test[nxy == ii, .(row_id,x,y,accuracy,time,nx2,ny2, place_id)]

xborder_r = bintable[nxy == ii, nx]*nnbinx + nnbinx
xborder_l = bintable[nxy == ii, nx]*nnbinx - 1
yborder_u = bintable[nxy == ii, ny]*nnbiny + nnbiny
yborder_d = bintable[nxy == ii, ny]*nnbiny - 1

dtrain <- train[nx2 <= xborder_r & nx2 >= xborder_l &
                  ny2 <= yborder_u & ny2 >= yborder_d
                , .(x,y,accuracy,time,place_id, nx2, ny2)]

dtrain_place_N <- dtrain[, .(nfeq=.N), by=(place_id)][order(-nfeq)][nfeq > 10]
dtrain <- dtrain[place_id %in% dtrain_place_N$place_id]

iterations <- nrow(dtest)
pb <- txtProgressBar(min = 1, max = iterations, style = 3)

cl <- makePSOCKcluster(6, type = "FORK", outfile="")
registerDoParallel(cl)
system.time(
  result <- foreach(jj=1:iterations, .combine = rbind, .packages='data.table') %dopar%
  {
#    cat(jj, "\n")
    xi = as.numeric(dtest[jj,x]); yi = as.numeric(dtest[jj,y])
    nxi = as.integer(dtest[jj,nx2]); nyi = as.integer(dtest[jj,ny2])
    ti = as.integer(dtest[jj,time])
    aci = as.numeric(dtest[jj,accuracy])

    radx = rad0; rady = rad0
    
    dbin = 1
    repeat {
      domain <- dtrain[ abs(nx2-nxi) <= dbin & abs(ny2-nyi) <= dbin]
      if (nrow(domain) < 200) {
        dbin = dbin + 1
      } else break
    }
    
    knn0 = knn
    repeat {
      repeat {
        neighbor <- domain[, dis := ((x-xi)/radx)^2 + ((y-yi)/rady)^2][dis <= 1][, dis1 := (1-dis)]
        if (dim(neighbor)[1] < knn0) {
          radx = radx * 1.1
          rady = rady * 1.1
        } else break()
      }
      neighbor <- neighbor[order(dis)][1:knn0]
      prob <- neighbor[, .(nfeq=.N), by=(place_id)][order(-nfeq)] #[nfeq > minfeq]
      if (nrow(prob) < 3) {
        knn0 = knn0 + 5
      } else break()
    }
      
    #calculate probability
    sum_nfeq <- sum(prob$nfeq)
    prob[, Pn := nfeq / sum_nfeq]
      
    prob <- prob[, Pall := Pn ][order(-Pall)][1:3, place_id]
    setTxtProgressBar(pb, jj)
    gc()
    return(prob)
  })
close(pb)
stopCluster(cl)


result <- cbind(result, dtest$row_id)
output <- rbind(output, result)

outdata <- as.data.table(output)
colnames(outdata) <- c("A1","A2","A3","ID")
outdata$ID <- as.integer(outdata$ID)
outdata <- outdata[order(ID)]

scores <- rep(0, nrow(outdata))
kk = 3
for (ii in 1:nrow(outdata)){
  pred <- outdata[ii,c(A1,A2,A3)]
  actul <- dtest[ii,place_id]
  for (jj in 1:kk){
    if (actul == pred[jj] & scores[ii] == 0){
      scores[ii] = 1/jj
    }
  }
}
sum(scores)
sum(scores)/nrow(dtest)
head(scores)


