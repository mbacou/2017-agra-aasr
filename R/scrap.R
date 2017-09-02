#####################################################################################
# 2017.07.12 Reformat results from EPAR
#####################################################################################
library(foreign)
library(data.table)
library(survey)
library(tables)
library(stringr)

setwd("~/Projects/2017-agra-aasr")
load("./tmp/2017-agra-aasr.RData")

# Copy tables from `./out/EPAR/EPAR_UW_356_Categorizing Smallholder Farmers in ETH and TZ_7.11.17.xlsx`
epar1 <- fread("./out/EPAR/EPAR_UW_356_Categorizing Smallholder Farmers in ETH and TZ_7.11.17_Table1.csv")
epar2 <- fread("./out/EPAR/EPAR_UW_356_Categorizing Smallholder Farmers in ETH and TZ_7.11.17_Table2.csv")
epar3 <- fread("./out/EPAR/EPAR_UW_356_Categorizing Smallholder Farmers in ETH and TZ_7.11.17_Table3.csv")

#####################################################################################
# 2017.07.23 Export Estimates to Peter's Table Template
#####################################################################################
# Export into Peter's table template
# 0) `class5`
tmp <- 100*svymean(~class5, gha.svy.shf[["gha6"]], na.rm=T)
html(t(tmp), file="./out/MB/gha-class5glues.html", rmarkdown=T, rownames=T)

# 1) `seg_quad_4`
tmp <- data.frame(100*svymean(~seg_quad_4, gha.svy.shf[["gha6"]], na.rm=T))
html(tmp, file="./out/MB/gha-class5glues.html", rmarkdown=T, rownames=T, append=T)

tmp <- svytotal(~I(interaction(seg_quad_4, class5)), gha.svy.shf[["gha6"]], na.rm=T)
tmp <- as.data.table(tmp)
tmp[, `:=`(
  seg_quad_4 = factor(rep(levels(gha$seg_quad_4), each=1, 5),
    levels=levels(gha$seg_quad_4)),
  class5 = factor(rep(levels(gha$class5), each=4, 1),
    levels=levels(gha$class5))
)]
tmp[, pct := 100*total/sum(total, na.rm=T), by=class5]
html(tabular(Heading("Development Domain (GAEZ)")*(seg_quad_4+1)~Heading("Farm Type")*(class5)*Heading()*(pct*sum)*Format(digits=0, nsmall=1, big.mark=",", scientific=F),
  data=tmp), rmarkdown=T, file="./out/MB/gha-class5glues.html", append=T)

tmp <- 100*svymean(~I(interaction(seg_quad_4, class5)), gha.svy.shf[["gha6"]], na.rm=T)
tmp <- as.data.table(tmp)
tmp[, `:=`(
  seg_quad_4 = factor(rep(levels(gha$seg_quad_4), each=1, 5),
    levels=levels(gha$seg_quad_4)),
  class5 = factor(rep(levels(gha$class5), each=4, 1),
    levels=levels(gha$class5))
)]
html(tabular(Heading("Development Domain (GAEZ)")*(seg_quad_4+1)~Heading("Farm Type")*(class5+1)*Heading()*(mean*sum)*Format(digits=0, nsmall=1, big.mark=",", scientific=F),
  data=tmp), rmarkdown=T, file="./out/MB/gha-class5glues.html", append=T)

###############################################
# 2) `seg_quad_glues_4`
tmp <- 100*svymean(~seg_quad_glues_4, gha.svy.shf[["gha6"]], na.rm=T)
html(t(tmp), file="./out/MB/gha-class5glues.html", rmarkdown=T, rownames=T, append=T)

tmp <- svytotal(~I(interaction(seg_quad_glues_4, class5)), gha.svy.shf[["gha6"]], na.rm=T)
tmp <- as.data.table(tmp)
tmp[, `:=`(
  seg_quad_glues_4 = factor(rep(levels(gha$seg_quad_glues_4), each=1, 5),
    levels=levels(gha$seg_quad_glues_4)),
  class5 = factor(rep(levels(gha$class5), each=4, 1),
    levels=levels(gha$class5))
)]
tmp[, pct := 100*total/sum(total, na.rm=T), by=class5]
html(tabular(Heading("Development Domain (GLUES)")*(seg_quad_glues_4+1)~Heading("Farm Type")*(class5)*Heading()*(pct*sum)*Format(digits=0, nsmall=1, big.mark=",", scientific=F),
  data=tmp), rmarkdown=T, file="./out/MB/gha-class5glues.html", append=T)

tmp <- 100*svymean(~I(interaction(seg_quad_glues_4, class5)), gha.svy.shf[["gha6"]], na.rm=T)
tmp <- as.data.table(tmp)
tmp[, `:=`(
  seg_quad_glues_4 = factor(rep(levels(gha$seg_quad_glues_4), each=1, 5),
    levels=levels(gha$seg_quad_glues_4)),
  class5 = factor(rep(levels(gha$class5), each=4, 1),
    levels=levels(gha$class5))
)]

html(tabular(Heading("Development Domain (GLUES)")*(seg_quad_glues_4+1)~Heading("Farm Type")*(class5+1)*Heading()*(mean*sum)*Format(digits=0, nsmall=1, big.mark=",", scientific=F),
  data=tmp), rmarkdown=T, file="./out/MB/gha-class5glues.html", append=T)

tmp <- svymean(~
    I(100*naggross_sh) +
    I(100*cropsales_sh) +
    totgross +
    agehead +
    I(100*femhead) +
    hhsize_imp +
    hhlabor +
    landown +
    croparea_imp +
    educhead +
    educhigh +
    I(100*cellphone) + I(100*electricity) + distroad +
    I(100*seeds) + I(100*fert_inorg) + I(100*hired_labor), gha.svy.shf[["gha6"]], na.rm=T)
html(tmp, rmarkdown=T, file="./out/MB/gha-class5glues.html", append=T)

tmp <- svyquantile(~totgross, gha.svy.shf[["gha6"]], .5, na.rm=T)
html(tmp, rmarkdown=T, file="./out/MB/gha-class5glues.html", append=T)

tmp <- svyCrossTab(list(
  ~I(100*naggross_sh),
  ~I(100*cropsales_sh),
  ~totgross,
  ~agehead,
  ~I(100*femhead),
  ~hhsize_imp,
  ~hhlabor,
  ~landown,
  ~croparea_imp,
  ~educhead,
  ~educhigh,
  ~I(100*cellphone), ~I(100*electricity), ~distroad,
  ~I(100*seeds), ~I(100*fert_inorg), ~I(100*hired_labor)
), ~class5, gha.svy.shf[["gha6"]], quantiles=c(.25,.5,.75))

html(tabular(Variable*Mean~Heading()*By*Heading()*Stat*Heading()*identity*Format(digits=0, nsmall=1, big.mark=",", scientific=F),
  data=tmp), rmarkdown=T, file="./out/MB/gha-class5glues.html", append=T)

tmp <- svyby(~totgross, ~class5, gha.svy.shf[["gha6"]], svyquantile, .5, ci=T, na.rm=T)
html(tmp, rmarkdown=T, file="./out/MB/gha-class5glues.html", append=T)

#####################################################################################

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

p <- viridis_pal()(20)
p <- viridis_pal(option="A")(20)
p <- viridis_pal(option="B")(20)
p <- viridis_pal(option="C")(20)

p <- col2rgb(p, alpha=T)
tmp <- paste0(seq(1,100,5)/100, ";", p[1,], ",", p[2,], ",", p[3,], ",", p[4,], ",", collapse=":")
cat(tmp, sep="\n")

# Variogram
library(sp)
library(gstat)

data(meuse)
coordinates(meuse) <- ~x+y
spplot(meuse, "zinc")
spplot(meuse, "soil")

vAll <- variogram(zinc~soil, meuse)
plot(vAll)

vWithinSoils <- variogram(zinc~soil, meuse, dX=0.5)
plot(vWithinSoils)


#####################################################################################
#
#####################################################################################
library(ggplot2)
library(ggthemes)

dataset <- expand.grid(
  x = seq(0, 1, length = 41),
  y = seq(0, 1, length = 41),
  z =factor( c("A", "B"))
)
dataset$fit <- with(dataset,
  ifelse(
    z == "A",
    x - 2 * x^2 + 0.5 * x * y + 3 * y - y ^ 2,
    -x - 1 * x^2 - 2 * x * y - 3 * y + y ^ 2
  )
)
ggplot(dataset, aes(x = x, y = y, fill = fit)) +
  geom_raster() +
  facet_wrap(~ z) +
  scale_fill_gradient2() +
  theme_par()
ggplot(dataset, aes(x = x, y = y, z = fit)) +
  geom_contour(aes(colour = ..level..), binwidth = 0.25) +
  facet_wrap(~ z) +
  scale_colour_gradient2()
ggplot(dataset, aes(x = x, y = y)) +
  geom_raster(aes(fill = fit)) +
  geom_contour(aes(z = fit), binwidth = 0.25) +
  facet_wrap(~ z) +
  scale_fill_gradient(low = "black", high = "white")

