#CHALLENGE Q1

closeAllConnections()
rm(list = ls())

# save keypad
keypad <- matrix(c(1,2,3,4,5,6,7,8,9,-1,0,-1), nrow = 4, ncol = 3, byrow = TRUE)
# all allowable moves for a chess knight
all_moves <- matrix(c(2,1,2,-1,-2,1,-2,-1,1,2,1,-2,-1,2,-1,-2), nrow = 8, ncol = 2, byrow = TRUE)

# This function return all allowable moves for a chess knigth on the keypad
# input is the key on which the knigth lands
moves <- function(key){
  max_row = 4
  max_col = 3
  for (ii in 1:max_row){
    for (jj in 1:max_col){
      if (keypad[ii,jj] == key){
        row = ii
        col = jj
      }
    }
  }
  
  new_key <- c()
  for (ii in 1:8){
    new_row = row + all_moves[ii,1]
    new_col = col + all_moves[ii,2]
    if ((new_col>0 & new_col<=max_col) & (new_row>0 & new_row<=max_row)) {
      new_key <- c(new_key,keypad[new_row,new_col])
    }
  }
  new_key[!new_key %in% c(-1)]
}

# to obtain a good statistic I need to calculate the modules over many samples and get average over all samples
set.seed(123)
# q1, q2 and q5
nsample = 1000000       # number of samples
Sum <- c()              # accumulate S
T = 10                 # number of moves
for (nn in 1:nsample){
  start = 0            # position of knight on keypad
  S = 0                # sum 
  for (ii in 1:T){
    allowable_moves <- moves(start)
    accept = sample(1:length(allowable_moves),1) # select one motion randomly
    start = allowable_moves[accept]
    S = S + start
  }
  Sum <- c(Sum, S)
}

Smod10 <- Sum%%10 
              
mean(Smod10)  #4.58653
sd(Smod10)    #2.859632

length(Sum[Sum%%5==0 & Sum%%7==0])/nsample # 0.026388 probibility of S is divisible by 5 and 7


# q3, q4 and q6
nsample = 100000
Sum <- c()
T = 1024
for (nn in 1:nsample){
  start = 0
  S = 0
  for (ii in 1:T){
    allowable_moves <- moves(start)
    accept = sample(1:length(allowable_moves),1)
    start = allowable_moves[accept]
    S = S + start
  }
  Sum <- c(Sum, S)
}

Smod1024 <- Sum%%1024

mean(Smod1024)  # 511.1166
sd(Smod1024)    # 70.25343

length(Sum[Sum%%23==0 & Sum%%29==0])/nsample # 0.00403 probibility of S is divisible by 5 and 7

