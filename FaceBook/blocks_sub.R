closeAllConnections()
rm(list = ls())

library(data.table)
library(doParallel)
cl <- makePSOCKcluster(12, type = "FORK", outfile="")
registerDoParallel(cl)

#Functions
nint <- function(x){
  ifelse(x >= 0, as.integer(x+0.5), as.integer(x-0.5))
}

shift <- function(dx, size){
  dx - nint(dx / size) * size
}

# This is the main function
onecell <- function(icell, icell_radx, icell_rady, icell_knn, minfeq){
  hour = 60
  day = 24*60
  week = 7*24*60
  month = 30*24*60
  year = 365*24*60

  dtest  <- test[nxy == icell, .(row_id,x,y,accuracy,time,nx2,ny2)]
  
  xborder_r = bintable[nxy == icell, nx]*nnbinx + nnbinx
  xborder_l = bintable[nxy == icell, nx]*nnbinx - 1
  yborder_u = bintable[nxy == icell, ny]*nnbiny + nnbiny
  yborder_d = bintable[nxy == icell, ny]*nnbiny - 1
  
  dtrain <- train[nx2 <= xborder_r & nx2 >= xborder_l &
                    ny2 <= yborder_u & ny2 >= yborder_d
                  , .(x,y,accuracy,time,place_id, nx2, ny2)]
  
  dtrain_place_N <- dtrain[, .(nfeq=.N), by=(place_id)][order(-nfeq)][nfeq > 10]
  dtrain <- dtrain[place_id %in% dtrain_place_N$place_id]
  
  iterations <- nrow(dtest)
  
  result <- foreach(jj=1:iterations, .combine = rbind, .packages='data.table') %dopar%
  {
    xi = as.numeric(dtest[jj,x]); yi = as.numeric(dtest[jj,y])
    nxi = as.integer(dtest[jj,nx2]); nyi = as.integer(dtest[jj,ny2])
    ti = as.integer(dtest[jj,time])
    aci = as.numeric(dtest[jj,accuracy])
    
    radx = icell_radx; rady = icell_rady 
    knn = icell_knn #- as.integer(2*aci)

    domain <- dtrain[ abs(nx2-nxi) <= 1 & abs(ny2-nyi) <= 1]
      
    nstep = 1
    repeat {
      neighbors <- domain[, dis := ((x-xi)/radx)^2 + ((y-yi)/rady)^2][dis <= 1][, dis1 := (1-dis)^0.2][, acc := accuracy*(1-dis)^0.2]
      prob <- neighbors[, .(nfeq=.N, dsum=sum(dis1), asum=sum(acc)), by=.(place_id)][order(-nfeq)][nfeq > minfeq]
        
      if ((nrow(neighbors) < knn | nrow(prob) < 3) & (nstep < 5)) {
          radx = radx * 1.2
          rady = rady * 1.2
      } else break
    }

    if (nrow(prob) < 3) {
      prob <- neighbors[, .(nfeq=.N, dsum=sum(dis1)), by=.(place_id)][order(-nfeq)][nfeq > 1]
    }
    neighbors <- neighbors[place_id %in% prob$place_id]

    #calculate time difference
    neighbors[,dt := time-ti]
    neighbors[,dH := (shift(dt, day) / hour)^2][, dH := dH/max(dH)][, dH := (1-dH)^2.0]

    probH <- neighbors[, .(nfeq=.N, dHsum=sum(dH)), by=.(place_id)]

    setkey(prob,place_id)
    setkey(probH,place_id)
    prob <- prob[probH, nomatch=0]
      
    #calculate probability
    sum_nfeq <- sum(prob$nfeq)
    prob[, Pn := dsum / sum_nfeq]

    sum_acc <- sum(prob$asum)
    prob[, Pa := asum / sum_acc]

    sum_dH <- sum(prob$dHsum)
    prob[, Ph := dHsum / sum_dH]
    
    prob <- prob[, Pall := Pn * Pa * Ph][order(-Pall)][1:3, place_id]
    prob
    return(prob)
  }
  
  result <- cbind(result, dtest$row_id)
  output <<- rbind(output, result)
}

#######################################################################################################################################
#setwd('/work/Rproject/FB')
#setwd('C:/Users/Amin/Documents/R/FB')
train <- fread("../../../data/train.csv", integer64 = "character")
test <- fread("../../../data/test.csv", integer64 = "character")

test$row_id <- seq(1:nrow(test))-1
print(object.size(train), units = "Mb")
print(object.size(test), units = "Mb")

#convert accuracy log10
train$accuracy <- log10(train$accuracy)
test$accuracy <- log10(test$accuracy)

#make first grid
binx = 1.; biny = 1.
nbinx = 10/binx; nbiny = 10/biny; nbinxy = nbinx*nbiny
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


#############################################################################################################
#Parameters
output <- matrix(data = NA, nrow = 0, ncol = 4, byrow = FALSE, dimnames = NULL)
radx0 = 0.010 
rady0 = 0.005
knn = 60
minfeq = 3 #minimum frequency in a domain

# Loop over all cells

for (ii in 0:(nbinxy-1)){
  cat("bin number ", ii ," from ", nbinxy, "\n")
  onecell(ii, radx0, rady0, knn, minfeq)
}


#Write output
outdata <- as.data.table(output)
colnames(outdata) <- c("A1","A2","A3","ID")
outdata$ID <- as.integer(outdata$ID)
outdata <- outdata[order(ID)]

outdata$place_id <- apply(outdata[, 1:3, with=FALSE], 1, paste, collapse=" ") 
outdata <- outdata[,.(ID,place_id)]
names(outdata) <- c("row_id","place_id")
write.table(outdata,"submission.csv",sep=",",row.names=FALSE,quote=FALSE)

