# https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-intro.html
DT = data.table(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c = 13:18)
DT
##DT[i, j, by]
##   R:      i                 j        by
## SQL:  where   select | update  group by

# subset rows in i
DT[ID=="b" & c==13]
DT[1:2]
DT[order(ID,-c)]

# select columns in j
DT[,b]
DT[,.(b)]
DT[,.(newb = b, newc = c)]

#compute on j
DT[,sum(b)]
DT[,sum(b<10)]
DT[1:2,sum(b)]

DT[ID=="b" & c>13, .N] # .N count the number of line

#refer to the columns by name
DT[,c("b","c"), with=FALSE]
DT[,-c("b","c"), with=FALSE]
DT[,b:c, with=FALSE]       #between b and c

#group by
DT[,.N,by=.(ID)]
DT[,.(mb=mean(b)),by=.(ID)] #keep the order
DT[,.(mb=mean(b)),keyby=.(ID)] #order ID columns
DT[,.N,by=.(ID,c)]

DT[,.N,by=.(ID)][order(-ID)] #use experesion one after another

#experesion by
DT[,.N,by=.(b>10, c>13)]

#multiple columns in j - .SD
# .SD = Subset of Data = holds the data for current group defined using by
DT[, print(.SD), by = ID]
DT[, lapply(.SD, mean), by = ID]
DT[, lapply(.SD, mean), by = ID, .SDcol = c("a","b")] # select groups in SD

#concerete two columns
DT[, .(val = c(a,b)), by = ID]
DT[, .(val = list(c(a,b))), by = ID]  #as a list
DT[, print(c(a,b)), by = ID]
DT[, print(list(c(a,b))), by = ID]

#https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-reference-semantics.html
DT[,"d" := list()]
DT[,"d" := a]
DT[, d := NULL]

DT[, `:=`(sum = a + b, min = a - b)]
DT[a == 1, a:=0] #replace
DT[,sort(unique(ID))]
DT[,max := max(a), by=.(ID)]

#Multiple columns 
in_cols  = c("a", "b")
out_cols = c("max_a", "max_b")
DT[, c(out_cols) := lapply(.SD, max), by = ID, .SDcols = in_cols]

#function
foo <- function(dt) {
  dt[, speed := a / b]
  dt[, .(max_speed = max(speed)), by = ID]
}
ans = foo(DT)

#function
foo <- function(dt) {
  dt <- copy(DT)  # deep copy; keep DT 
  dt[, speed := a / b]
  dt[, .(max_speed = max(speed)), by = ID]
}
ans = foo(DT)

#set key
setkey(DT, ID)
setkey(DT,"ID")
DT[.("a")] #subset all rows where the ID matches "a"
DT[c("b","c")]
key(DT)
setkey(DT, ID, c)
key(DT)
DT[.("a",16)] #subset all rows where the ID matches "a" and c matches 16
DT[.(unique(ID),16)] #subset all rows where the second columns is 16

#keys with by
setkey(DT, ID, c)
key(DT)
DT[.("a"),.(c)]
DT[.("a"),.(c)][order(-c)]

#compute or do in j 
setkey(DT, ID, c)
key(DT)
DT[.("a"), max(c)]

#subassign
DT[,sort(unique(c))]
setkey(DT,c)
key(DT)
DT[.(17), c:=0]
DT
key(DT)

#Aggregation using by
setkey(DT,ID)
DT["b", max(c), keyby=b]

# mult and nomatch
setkey(DT,ID)
DT["b", mult = "first"]
DT["b", mult = "last"]

setkey(DT, ID, a)
DT[.(c("a","b"),2), nomatch=0] #skip when there is no matches

print(object.size(DT), units = "Mb")

#binary search vs vector scan
DT[ID == "a"] #binary search
DT[.("a")] #vector scan which is much faster
