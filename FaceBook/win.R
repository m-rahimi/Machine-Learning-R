closeAllConnections()
rm(list = ls())

library(data.table)
#prepare foreach for parallel
library(doParallel)
cl <- makePSOCKcluster(12, outfile="")
registerDoParallel(cl)

#Functions
nint <- function(x){
  ifelse(x >= 0, as.integer(x+0.5), as.integer(x-0.5))
}

shift <- function(dx, size){
  dx - nint(dx / size) * size
}

#setwd('/work/Rproject/FB')
#setwd('C:/Users/Amin/Documents/R/FB')
train <- fread("../../subset/subtrain.csv", integer64 = "character")
test <- fread("../../subset/subtest.csv", integer64 = "character")
test$row_id <- seq(0:(nrow(test)-1))
print(object.size(train), units = "Mb")
print(object.size(test), units = "Mb")

radx0 = 0.0006
rady0 = 0.0002

hour = 60
day = 24*60
week = 7*24*60
month = 30*24*60
year = 365*24*60

minfeq = 3 #minimum frequency in a domain
Hbin0 = 2.5
Wbin0 = 2
Mbin0 = 1
Ybin0 = 1

output <- matrix(data = NA, nrow = 0, ncol = 4, byrow = FALSE, dimnames = NULL)

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
binx2 = 0.02; biny2 = 0.02
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


system.time(
  for (ii in 0:(nbinxy-1)){
    cat("bin number ", ii ," from ", nbinxy, "\n")
    gc()
    
    dtest  <- test[nxy == ii, .(row_id,x,y,accuracy,time,nx2,ny2)]
    
    xborder_r = bintable[nxy == ii, nx]*nnbinx + nnbinx
    xborder_l = bintable[nxy == ii, nx]*nnbinx - 1
    yborder_u = bintable[nxy == ii, ny]*nnbiny + nnbiny
    yborder_d = bintable[nxy == ii, ny]*nnbiny - 1
    
    dtrain <- train[nx2 <= xborder_r & nx2 >= xborder_l &
                      ny2 <= yborder_u & ny2 >= yborder_d
                    , .(x,y,accuracy,time,place_id, nx2, ny2)]
    
    iterations <- nrow(dtest)
    pb <- txtProgressBar(min = 1, max = iterations, style = 3)
    
    result <- foreach(jj=1:iterations, .combine = rbind, .packages='data.table') %dopar%
    {
      xi = as.numeric(dtest[jj,x]); yi = as.numeric(dtest[jj,y])
      nxi = as.integer(dtest[jj,nx2]); nyi = as.integer(dtest[jj,ny2])
      ti = as.integer(dtest[jj,time])
      
      Hbin = Hbin0; Wbin = Wbin0; Mbin = Mbin0; Ybin = Ybin0
      radx = radx0; rady = rady0

      dbin = 1
      repeat {
        #select the geometrical domain
        repeat {
          domain <- dtrain[ abs(nx2-nxi) <= dbin & abs(ny2-nyi) <= dbin]
          if (nrow(domain) < 1000) {
            dbin = dbin + 1
          } else break
        }

        repeat {
          temp <- domain[, dis := ((x-xi)^2)/radx + ((y-yi)^2)/rady][dis <= 1]
          if (nrow(temp) < 150) {
            radx = radx * 1.1
            rady = rady * 1.1
          } else break
        }
        domain <- temp

        #calculate number of each business in the domain
        prob <- domain[, .(nfeq=.N), by=(place_id)][order(-nfeq)][nfeq > minfeq]

        #keep the observation with frequency > minfeq; reduce the size of domain might improve speed
        domain <- domain[place_id %in% prob$place_id]
        if (nrow(domain) < 100 | nrow(prob) < 3) {
          radx = radx * 1.1
          rady = rady * 1.1
        } else break
      }

      #calculate time difference
      domain[,dt := time-ti]
      domain[,dH := (shift(dt, day) / hour)^2]
      domain[,dW := (shift(dt, week) / day)^2]
#      domain[,dM := (shift(dt, month) / day)]
#      domain[,dY := (dt) / year]

      #calculate the time probability
      nfeqtime = 25
      repeat{
        #Hour
        repeat{
          probH <- domain[dH < Hbin][, .(nfeqH=.N), by=.(place_id)]
          if (probH[,sum(nfeqH)]<nfeqtime | nrow(probH)<3) {
            Hbin = Hbin + 0.1
          } else break
        }

        #Week day
        repeat{
          probW <- domain[dW < Wbin][, .(nfeqW=.N), by=.(place_id)]
          if (probW[,sum(nfeqW)]<nfeqtime | nrow(probW)<3){
            Wbin = Wbin + 0.1
          } else break
        }

        setkey(probH,place_id)
        setkey(probW,place_id)
        probT <- probH[probW, nomatch=0]

        if (nrow(probT)<3){
          nfeqtime = nfeqtime + 5
        } else break
      }

      setkey(prob,place_id)
      setkey(probT,place_id)
      prob <- prob[probT, nomatch=0]

      #calculate probability
      sum_nfeq <- sum(prob$nfeq)
      prob[, Pn := nfeq / sum_nfeq]
      prob[, Ph := nfeqH / nfeq]
      prob[, Pw := nfeqW / nfeq]

      prob <- prob[, Pall := Pn * Ph * Pw][order(-Pall)][1:3, place_id]
      setTxtProgressBar(pb, jj)
      gc()
      return(prob)
    }
    close(pb)
    
    result <- cbind(result, dtest$row_id)
    output <- rbind(output, result)
  }
)


outdata <- as.data.table(output)
colnames(outdata) <- c("A1","A2","A3","ID")
outdata$ID <- as.integer(outdata$ID)
outdata <- outdata[order(ID)]

scores <- rep(0, nrow(outdata))
kk = 3
for (ii in 1:nrow(outdata)){
  pred <- outdata[ii,c(A1,A2,A3)]
  actul <- test[ii,place_id]
  for (jj in 1:kk){
    if (actul == pred[jj] & scores[ii] == 0){
      scores[ii] = 1/jj
    }
  }
}
sum(scores)

#outdata$place_id <- apply(outdata[, 1:3, with=FALSE], 1, paste, collapse=" ") 
#outdata <- outdata[,.(ID,place_id)]
#names(outdata) <- c("row_id","place_id")
#write.table(result,"submission.csv",sep=",",row.names=FALSE,quote=FALSE)
