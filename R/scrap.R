library(data.table)

rm(r,x,y,myPal,grid)
names(erss)
erss[, lapply(.SD[, .(femheadcount:litheadpop)], sum, na.rm=T)]
erss[, lapply(.SD, sum, na.rm=T), .SDcols=femheadcount:litheadpop]
erss[, lapply(femheadcount:litheadpop, sum, na.rm=T)]

DT = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
sapply(DT, class)
# x           y           v
# "character"   "numeric"   "integer"

DT[, lapply(.SD, sum, na.rm=T), by=x:y, .SDcols=y:v]
#    x  y  v
# 1: b 10  6
# 2: a 10 15
# 3: c 10 24

DT[, lapply(.SD, sum, na.rm=T), by=x:y, .SDcols=names(DT) %like% "v"]


DT <- data.table(a=1:5, b=6:10, c=c(12:1))
DT
#    a  b c
# 1: 1  6 5
# 2: 2  7 4
# 3: 3  8 3
# 4: 4  9 2
# 5: 5 10 1
DT[between(b, 7, 9)] # same as above
DT[between(c, a, b)] # same as above
#    a b c
# 1: 1 6 5
# 2: 2 7 4
# 3: 3 8 3
DT[inrange(c, a, b)]
#    a  b c
# 1: 1  6 5
# 2: 2  7 4
# 3: 3  8 3
# 4: 4  9 2
# 5: 5 10 1

shift(DT[["b"]], n=1, fill=NA, give.names=T)
DT[, shift(b), by=a]

DT = data.table(grp=rep(c("A", "B", "C", "A", "B"), c(2,2,3,1,2)), value=1:10)
rleid(DT$grp) # get run-length ids
rleidv(DT, "grp") # same as above
DT[, sum(value), by=.(grp, rleid(grp))]

setkey(DT, grp)
unique(DT, by=key(DT))

