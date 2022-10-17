# Process condom intervention output.

library(EpiModelHPC)
setwd("/homes/dth2/kenyaTM/KTMint")


library(kableExtra)
library(knitr)
library(EpiModel)
library(EpiModelHPC)
library(parallel)
library(ggplot2)
library(plyr)
library(xlsx)
library(xlsxjars)




# Set-up data
## Load simulation results

noint_256_002 <- merge_simfiles(1000, indir = "data/" ,ftype="min")
noint_256_006 <- merge_simfiles(1001, indir = "data/" ,ftype="min")
noint_256_009 <- merge_simfiles(1002, indir = "data/" ,ftype="min")

int_256_002 <- merge_simfiles(1012, indir = "data/" ,ftype="min")
int_256_006 <- merge_simfiles(1013, indir = "data/" ,ftype="min")
int_256_009 <- merge_simfiles(1014, indir = "data/" ,ftype="min")

int_45_002 <- merge_simfiles(1015, indir = "data/" ,ftype="min")
int_45_006 <- merge_simfiles(1016, indir = "data/" ,ftype="min")
int_45_009 <- merge_simfiles(1017, indir = "data/" ,ftype="min")

int_949_002 <- merge_simfiles(1018, indir = "data/" ,ftype="min")
int_949_006 <- merge_simfiles(1019, indir = "data/" ,ftype="min")
int_949_009 <- merge_simfiles(1020, indir = "data/" ,ftype="min")

int_100_002 <- merge_simfiles(1021, indir = "data/" ,ftype="min")
int_100_006 <- merge_simfiles(1022, indir = "data/" ,ftype="min")
int_100_009 <- merge_simfiles(1023, indir = "data/" ,ftype="min")

int_45_002_ab <- merge_simfiles(1024, indir = "data/" ,ftype="min")
int_45_006_ab <- merge_simfiles(1025, indir = "data/" ,ftype="min")
int_45_009_ab <- merge_simfiles(1026, indir = "data/" ,ftype="min")

int_949_002_ab <- merge_simfiles(1027, indir = "data/" ,ftype="min")
int_949_006_ab <- merge_simfiles(1028, indir = "data/" ,ftype="min")
int_949_009_ab <- merge_simfiles(1029, indir = "data/" ,ftype="min")

int_100_002_ab <- merge_simfiles(1030, indir = "data/" ,ftype="min")
int_100_006_ab <- merge_simfiles(1031, indir = "data/" ,ftype="min")
int_100_009_ab <- merge_simfiles(1032, indir = "data/" ,ftype="min")

noint_256_006_l <- merge_simfiles(2001, indir = "data/" ,ftype="min")
int_256_006_l <- merge_simfiles(2013, indir = "data/" ,ftype="min")
int_45_006_l <- merge_simfiles(2016, indir = "data/" ,ftype="min")
int_949_006_l <- merge_simfiles(2019, indir = "data/" ,ftype="min")
int_100_006_l <- merge_simfiles(2022, indir = "data/" ,ftype="min")

scenarios <- c(
  "noint_256_002", 
  "noint_256_006", 
  "noint_256_009",
  "int_256_002", 
  "int_256_006",
  "int_256_009", 
  "int_45_002",
  "int_45_006",
  "int_45_009",
  "int_949_002",
  "int_949_006",
  "int_949_009",
  "int_100_002",
  "int_100_006",
  "int_100_009",
  "int_45_002_ab",
  "int_45_006_ab",
  "int_45_009_ab",
  "int_949_002_ab",
  "int_949_006_ab",
  "int_949_009_ab",
  "int_100_002_ab",
  "int_100_006_ab",
  "int_100_009_ab",
  "noint_256_006_l",
  "int_256_006_l",
  "int_45_006_l",
  "int_949_006_l",
  "int_100_006_l")


level <- c("","_hi","_low")

trim <- ((22 * 52) * 10)

##Prevalence
for(i in 1:length(scenarios)){
  
  x<-get(scenarios[i])
  nsims<-300
  steps<-x$control$nsteps - trim
  
  for(j in 1:length(level)){
    
    fn <- paste0(scenarios[i], ".prev", level[j])
    assign(fn,rep(NA,steps))
    

  }}

hi.cut <- round(.95*nsims)
low.cut <- round(.05*nsims)


Mod.noint_256_002 <- truncate_sim(noint_256_002, at = trim)
Mod.noint_256_006 <- truncate_sim(noint_256_006, at = trim)
Mod.noint_256_009 <- truncate_sim(noint_256_009, at = trim)

Mod.int_256_002 <- truncate_sim(int_256_002, at = trim)
Mod.int_256_006 <- truncate_sim(int_256_006, at = trim)
Mod.int_256_009 <- truncate_sim(int_256_009, at = trim)

Mod.int_45_002 <- truncate_sim(int_45_002, at = trim)
Mod.int_45_006 <- truncate_sim(int_45_006, at = trim)
Mod.int_45_009 <- truncate_sim(int_45_009, at = trim)

Mod.int_949_002 <- truncate_sim(int_949_002, at = trim)
Mod.int_949_006 <- truncate_sim(int_949_006, at = trim)
Mod.int_949_009 <- truncate_sim(int_949_009, at = trim)

Mod.int_100_002 <- truncate_sim(int_100_002, at = trim)
Mod.int_100_006 <- truncate_sim(int_100_006, at = trim)
Mod.int_100_009 <- truncate_sim(int_100_009, at = trim)

Mod.int_45_002_ab <- truncate_sim(int_45_002_ab, at = trim)
Mod.int_45_006_ab <- truncate_sim(int_45_006_ab, at = trim)
Mod.int_45_009_ab <- truncate_sim(int_45_009_ab, at = trim)

Mod.int_949_002_ab <- truncate_sim(int_949_002_ab, at = trim)
Mod.int_949_006_ab <- truncate_sim(int_949_006_ab, at = trim)
Mod.int_949_009_ab <- truncate_sim(int_949_009_ab, at = trim)

Mod.int_100_002_ab <- truncate_sim(int_100_002_ab, at = trim)
Mod.int_100_006_ab <- truncate_sim(int_100_006_ab, at = trim)
Mod.int_100_009_ab <- truncate_sim(int_100_009_ab, at = trim)

Mod.noint_256_006_l <- truncate_sim(noint_256_006_l, at = trim)
Mod.int_256_006_l <- truncate_sim(int_256_006_l, at = trim)
Mod.int_45_006_l <- truncate_sim(int_45_006_l, at = trim)
Mod.int_949_006_l <- truncate_sim(int_949_006_l, at = trim)
Mod.int_100_006_l <- truncate_sim(int_100_006_l, at = trim)


Mod.noint_256_002$trans.el <- NULL
Mod.noint_256_006$trans.el <- NULL
Mod.noint_256_009$trans.el <- NULL

Mod.int_256_002$trans.el <- NULL
Mod.int_256_006$trans.el <- NULL
Mod.int_256_009$trans.el <- NULL

Mod.int_45_002$trans.el <- NULL
Mod.int_45_006$trans.el <- NULL
Mod.int_45_009$trans.el <- NULL

Mod.int_949_002$trans.el <- NULL
Mod.int_949_006$trans.el <- NULL
Mod.int_949_009$trans.el <- NULL

Mod.int_100_002$trans.el <- NULL
Mod.int_100_006$trans.el <- NULL
Mod.int_100_009$trans.el <- NULL

Mod.int_45_002_ab$trans.el <- NULL
Mod.int_45_006_ab$trans.el <- NULL
Mod.int_45_009_ab$trans.el <- NULL

Mod.int_949_002_ab$trans.el <- NULL
Mod.int_949_006_ab$trans.el <- NULL
Mod.int_949_009_ab$trans.el <- NULL

Mod.int_100_002_ab$trans.el <- NULL
Mod.int_100_006_ab$trans.el <- NULL
Mod.int_100_009_ab$trans.el <- NULL

Mod.noint_256_006_l$trans.el <- NULL
Mod.int_256_006_l$trans.el <- NULL
Mod.int_45_006_l$trans.el <- NULL
Mod.int_949_006_l$trans.el <- NULL
Mod.int_100_006_l$trans.el <- NULL


for (k in seq_along(1:steps)) {
  

 #no int 
  x<-sort(as.numeric(Mod.noint_256_002$epi$prev.poi[k,1:nsims]))
  noint_256_002.prev[k]<-mean(x) * 100
  noint_256_002.prev_hi[k]<-x[hi.cut] * 100
  noint_256_002.prev_low[k]<-x[low.cut] * 100
  
  #"noint_256_006", 
  x<-sort(as.numeric(Mod.noint_256_006$epi$prev.poi[k,1:nsims]))
  noint_256_006.prev[k]<-mean(x) * 100
  noint_256_006.prev_hi[k]<-x[hi.cut] * 100
  noint_256_006.prev_low[k]<-x[low.cut] * 100
  
  #"noint_256_009", 
  x<-sort(as.numeric(Mod.noint_256_009$epi$prev.poi[k,1:nsims]))
  noint_256_009.prev[k]<-mean(x) * 100
  noint_256_009.prev_hi[k]<-x[hi.cut] * 100
  noint_256_009.prev_low[k]<-x[low.cut] * 100
  
  #"int_256_002", 
  x<-sort(as.numeric(Mod.int_256_002$epi$prev.poi[k,1:nsims]))
  int_256_002.prev[k]<-mean(x) * 100
  int_256_002.prev_hi[k]<-x[hi.cut] * 100
  int_256_002.prev_low[k]<-x[low.cut] * 100
  
  #"int_256_006", 
  x<-sort(as.numeric(Mod.int_256_006$epi$prev.poi[k,1:nsims]))
  int_256_006.prev[k]<-mean(x) * 100
  int_256_006.prev_hi[k]<-x[hi.cut] * 100
  int_256_006.prev_low[k]<-x[low.cut] * 100
  

  #int_256_009
  x<-sort(as.numeric(Mod.int_256_009$epi$prev.poi[k,1:nsims]))
  int_256_009.prev[k]<-mean(x) * 100
  int_256_009.prev_hi[k]<-x[hi.cut] * 100
  int_256_009.prev_low[k]<-x[low.cut] * 100
  
  #int_45_002
  x<-sort(as.numeric(Mod.int_45_002$epi$prev.poi[k,1:nsims]))
  int_45_002.prev[k]<-mean(x) * 100
  int_45_002.prev_hi[k]<-x[hi.cut] * 100
  int_45_002.prev_low[k]<-x[low.cut] * 100
  
  #int_45_006
  x<-sort(as.numeric(Mod.int_45_006$epi$prev.poi[k,1:nsims]))
  int_45_006.prev[k]<-mean(x) * 100
  int_45_006.prev_hi[k]<-x[hi.cut] * 100
  int_45_006.prev_low[k]<-x[low.cut] * 100
  
  #int_45_009
  x<-sort(as.numeric(Mod.int_45_009$epi$prev.poi[k,1:nsims]))
  int_45_009.prev[k]<-mean(x) * 100
  int_45_009.prev_hi[k]<-x[hi.cut] * 100
  int_45_009.prev_low[k]<-x[low.cut] * 100
  
  #int_949_002
  x<-sort(as.numeric(Mod.int_949_002$epi$prev.poi[k,1:nsims]))
  int_949_002.prev[k]<-mean(x) * 100
  int_949_002.prev_hi[k]<-x[hi.cut] * 100
  int_949_002.prev_low[k]<-x[low.cut] * 100
  
  #int_949_006
  x<-sort(as.numeric(Mod.int_949_006$epi$prev.poi[k,1:nsims]))
  int_949_006.prev[k]<-mean(x) * 100
  int_949_006.prev_hi[k]<-x[hi.cut] * 100
  int_949_006.prev_low[k]<-x[low.cut] * 100
  
  #int_949_009
  x<-sort(as.numeric(Mod.int_949_009$epi$prev.poi[k,1:nsims]))
  int_949_009.prev[k]<-mean(x) * 100
  int_949_009.prev_hi[k]<-x[hi.cut] * 100
  int_949_009.prev_low[k]<-x[low.cut] * 100
  
  #int_100_002
  x<-sort(as.numeric(Mod.int_100_002$epi$prev.poi[k,1:nsims]))
  int_100_002.prev[k]<-mean(x) * 100
  int_100_002.prev_hi[k]<-x[hi.cut] * 100
  int_100_002.prev_low[k]<-x[low.cut] * 100
  
  #int_100_006
  x<-sort(as.numeric(Mod.int_100_006$epi$prev.poi[k,1:nsims]))
  int_100_006.prev[k]<-mean(x) * 100
  int_100_006.prev_hi[k]<-x[hi.cut] * 100
  int_100_006.prev_low[k]<-x[low.cut] * 100
  
  #int_100_009
  x<-sort(as.numeric(Mod.int_100_009$epi$prev.poi[k,1:nsims]))
  int_100_009.prev[k]<-mean(x) * 100
  int_100_009.prev_hi[k]<-x[hi.cut] * 100
  int_100_009.prev_low[k]<-x[low.cut] * 100
  
  ##AB ONLY
  
  #int_45_002_ab
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$prev.poi[k,1:nsims]))
  int_45_002_ab.prev[k]<-mean(x) * 100
  int_45_002_ab.prev_hi[k]<-x[hi.cut] * 100
  int_45_002_ab.prev_low[k]<-x[low.cut] * 100
  
  #int_45_006
  x<-sort(as.numeric(Mod.int_45_006_ab$epi$prev.poi[k,1:nsims]))
  int_45_006_ab.prev[k]<-mean(x) * 100
  int_45_006_ab.prev_hi[k]<-x[hi.cut] * 100
  int_45_006_ab.prev_low[k]<-x[low.cut] * 100
  
  #int_45_009
  x<-sort(as.numeric(Mod.int_45_009_ab$epi$prev.poi[k,1:nsims]))
  int_45_009_ab.prev[k]<-mean(x) * 100
  int_45_009_ab.prev_hi[k]<-x[hi.cut] * 100
  int_45_009_ab.prev_low[k]<-x[low.cut] * 100
  
  #int_949_002
  x<-sort(as.numeric(Mod.int_949_002_ab$epi$prev.poi[k,1:nsims]))
  int_949_002_ab.prev[k]<-mean(x) * 100
  int_949_002_ab.prev_hi[k]<-x[hi.cut] * 100
  int_949_002_ab.prev_low[k]<-x[low.cut] * 100
  
  #int_949_006
  x<-sort(as.numeric(Mod.int_949_006_ab$epi$prev.poi[k,1:nsims]))
  int_949_006_ab.prev[k]<-mean(x) * 100
  int_949_006_ab.prev_hi[k]<-x[hi.cut] * 100
  int_949_006_ab.prev_low[k]<-x[low.cut] * 100
  
  #int_949_009
  x<-sort(as.numeric(Mod.int_949_009_ab$epi$prev.poi[k,1:nsims]))
  int_949_009_ab.prev[k]<-mean(x) * 100
  int_949_009_ab.prev_hi[k]<-x[hi.cut] * 100
  int_949_009_ab.prev_low[k]<-x[low.cut] * 100
  
  #int_100_002
  x<-sort(as.numeric(Mod.int_100_002_ab$epi$prev.poi[k,1:nsims]))
  int_100_002_ab.prev[k]<-mean(x) * 100
  int_100_002_ab.prev_hi[k]<-x[hi.cut] * 100
  int_100_002_ab.prev_low[k]<-x[low.cut] * 100
  
  #int_100_006
  x<-sort(as.numeric(Mod.int_100_006_ab$epi$prev.poi[k,1:nsims]))
  int_100_006_ab.prev[k]<-mean(x) * 100
  int_100_006_ab.prev_hi[k]<-x[hi.cut] * 100
  int_100_006_ab.prev_low[k]<-x[low.cut] * 100
  
  #int_100_009
  x<-sort(as.numeric(Mod.int_100_009_ab$epi$prev.poi[k,1:nsims]))
  int_100_009_ab.prev[k]<-mean(x) * 100
  int_100_009_ab.prev_hi[k]<-x[hi.cut] * 100
  int_100_009_ab.prev_low[k]<-x[low.cut] * 100
  
  #LOWER AHI SEEKING PROB
  #int_100_009
  
  x<-sort(as.numeric(Mod.noint_256_006_l$epi$prev.poi[k,1:nsims]))
  noint_256_006_l.prev[k]<-mean(x) * 100
  noint_256_006_l.prev_hi[k]<-x[hi.cut] * 100
  noint_256_006_l.prev_low[k]<-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006_l$epi$prev.poi[k,1:nsims]))
  int_256_006_l.prev[k]<-mean(x) * 100
  int_256_006_l.prev_hi[k]<-x[hi.cut] * 100
  int_256_006_l.prev_low[k]<-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_l$epi$prev.poi[k,1:nsims]))
  int_45_006_l.prev[k]<-mean(x) * 100
  int_45_006_l.prev_hi[k]<-x[hi.cut] * 100
  int_45_006_l.prev_low[k]<-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_l$epi$prev.poi[k,1:nsims]))
  int_949_006_l.prev[k]<-mean(x) * 100
  int_949_006_l.prev_hi[k]<-x[hi.cut] * 100
  int_949_006_l.prev_low[k]<-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_l$epi$prev.poi[k,1:nsims]))
  int_100_006_l.prev[k]<-mean(x) * 100
  int_100_006_l.prev_hi[k]<-x[hi.cut] * 100
  int_100_006_l.prev_low[k]<-x[low.cut] * 100
  
  
  }


for(i in 1:length(scenarios)){
  
  x<-get(scenarios[i])
  nsims<-300
  steps<-x$control$nsteps - trim
  
  for(j in 1:length(level)){
    
    fn <- paste0(scenarios[i], ".ir100", level[j])
    assign(fn,rep(NA,steps))
    
    
  }}


for (k in seq_along(1:steps)) {
  
  
  #no int 
  x<-sort(as.numeric(Mod.noint_256_002$epi$ir100[k,1:nsims]))
  noint_256_002.ir100[k]<-mean(x)
  noint_256_002.ir100_hi[k]<-x[hi.cut]
  noint_256_002.ir100_low[k]<-x[low.cut]
  
  #"noint_256_006", 
  x<-sort(as.numeric(Mod.noint_256_006$epi$ir100[k,1:nsims]))
  noint_256_006.ir100[k]<-mean(x)
  noint_256_006.ir100_hi[k]<-x[hi.cut]
  noint_256_006.ir100_low[k]<-x[low.cut]
  
  #"noint_256_009", 
  x<-sort(as.numeric(Mod.noint_256_009$epi$ir100[k,1:nsims]))
  noint_256_009.ir100[k]<-mean(x)
  noint_256_009.ir100_hi[k]<-x[hi.cut]
  noint_256_009.ir100_low[k]<-x[low.cut]
  
  #"int_256_002", 
  x<-sort(as.numeric(Mod.int_256_002$epi$ir100[k,1:nsims]))
  int_256_002.ir100[k]<-mean(x)
  int_256_002.ir100_hi[k]<-x[hi.cut]
  int_256_002.ir100_low[k]<-x[low.cut]
  
  #"int_256_006", 
  x<-sort(as.numeric(Mod.int_256_006$epi$ir100[k,1:nsims]))
  int_256_006.ir100[k]<-mean(x)
  int_256_006.ir100_hi[k]<-x[hi.cut]
  int_256_006.ir100_low[k]<-x[low.cut]
  
  
  #int_256_009
  x<-sort(as.numeric(Mod.int_256_009$epi$ir100[k,1:nsims]))
  int_256_009.ir100[k]<-mean(x) 
  int_256_009.ir100_hi[k]<-x[hi.cut]
  int_256_009.ir100_low[k]<-x[low.cut]
  
  #int_45_002
  x<-sort(as.numeric(Mod.int_45_002$epi$ir100[k,1:nsims]))
  int_45_002.ir100[k]<-mean(x)
  int_45_002.ir100_hi[k]<-x[hi.cut]
  int_45_002.ir100_low[k]<-x[low.cut]
  
  #int_45_006
  x<-sort(as.numeric(Mod.int_45_006$epi$ir100[k,1:nsims]))
  int_45_006.ir100[k]<-mean(x)
  int_45_006.ir100_hi[k]<-x[hi.cut]
  int_45_006.ir100_low[k]<-x[low.cut]
  
  #int_45_009
  x<-sort(as.numeric(Mod.int_45_009$epi$ir100[k,1:nsims]))
  int_45_009.ir100[k]<-mean(x)
  int_45_009.ir100_hi[k]<-x[hi.cut]
  int_45_009.ir100_low[k]<-x[low.cut]
  
  #int_949_002
  x<-sort(as.numeric(Mod.int_949_002$epi$ir100[k,1:nsims]))
  int_949_002.ir100[k]<-mean(x)
  int_949_002.ir100_hi[k]<-x[hi.cut]
  int_949_002.ir100_low[k]<-x[low.cut]
  
  #int_949_006
  x<-sort(as.numeric(Mod.int_949_006$epi$ir100[k,1:nsims]))
  int_949_006.ir100[k]<-mean(x)
  int_949_006.ir100_hi[k]<-x[hi.cut]
  int_949_006.ir100_low[k]<-x[low.cut]
  
  #int_949_009
  x<-sort(as.numeric(Mod.int_949_009$epi$ir100[k,1:nsims]))
  int_949_009.ir100[k]<-mean(x)
  int_949_009.ir100_hi[k]<-x[hi.cut]
  int_949_009.ir100_low[k]<-x[low.cut]
  
  #int_100_002
  x<-sort(as.numeric(Mod.int_100_002$epi$ir100[k,1:nsims]))
  int_100_002.ir100[k]<-mean(x)
  int_100_002.ir100_hi[k]<-x[hi.cut]
  int_100_002.ir100_low[k]<-x[low.cut]
  
  #int_100_006
  x<-sort(as.numeric(Mod.int_100_006$epi$ir100[k,1:nsims]))
  int_100_006.ir100[k]<-mean(x)
  int_100_006.ir100_hi[k]<-x[hi.cut]
  int_100_006.ir100_low[k]<-x[low.cut]
  
  #int_100_009
  x<-sort(as.numeric(Mod.int_100_009$epi$ir100[k,1:nsims]))
  int_100_009.ir100[k]<-mean(x)
  int_100_009.ir100_hi[k]<-x[hi.cut]
  int_100_009.ir100_low[k]<-x[low.cut]
  
  ##AB ONLY
  
  #int_45_002_ab
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$ir100[k,1:nsims]))
  int_45_002_ab.ir100[k]<-mean(x)
  int_45_002_ab.ir100_hi[k]<-x[hi.cut]
  int_45_002_ab.ir100_low[k]<-x[low.cut]
  
  #int_45_006
  x<-sort(as.numeric(Mod.int_45_006_ab$epi$ir100[k,1:nsims]))
  int_45_006_ab.ir100[k]<-mean(x)
  int_45_006_ab.ir100_hi[k]<-x[hi.cut]
  int_45_006_ab.ir100_low[k]<-x[low.cut]
  
  #int_45_009
  x<-sort(as.numeric(Mod.int_45_009_ab$epi$ir100[k,1:nsims]))
  int_45_009_ab.ir100[k]<-mean(x)
  int_45_009_ab.ir100_hi[k]<-x[hi.cut]
  int_45_009_ab.ir100_low[k]<-x[low.cut]
  
  #int_949_002
  x<-sort(as.numeric(Mod.int_949_002_ab$epi$ir100[k,1:nsims]))
  int_949_002_ab.ir100[k]<-mean(x)
  int_949_002_ab.ir100_hi[k]<-x[hi.cut]
  int_949_002_ab.ir100_low[k]<-x[low.cut]
  
  #int_949_006
  x<-sort(as.numeric(Mod.int_949_006_ab$epi$ir100[k,1:nsims]))
  int_949_006_ab.ir100[k]<-mean(x)
  int_949_006_ab.ir100_hi[k]<-x[hi.cut]
  int_949_006_ab.ir100_low[k]<-x[low.cut]
  
  #int_949_009
  x<-sort(as.numeric(Mod.int_949_009_ab$epi$ir100[k,1:nsims]))
  int_949_009_ab.ir100[k]<-mean(x)
  int_949_009_ab.ir100_hi[k]<-x[hi.cut]
  int_949_009_ab.ir100_low[k]<-x[low.cut]
  
  #int_100_002
  x<-sort(as.numeric(Mod.int_100_002_ab$epi$ir100[k,1:nsims]))
  int_100_002_ab.ir100[k]<-mean(x)
  int_100_002_ab.ir100_hi[k]<-x[hi.cut]
  int_100_002_ab.ir100_low[k]<-x[low.cut]
  
  #int_100_006
  x<-sort(as.numeric(Mod.int_100_006_ab$epi$ir100[k,1:nsims]))
  int_100_006_ab.ir100[k]<-mean(x)
  int_100_006_ab.ir100_hi[k]<-x[hi.cut]
  int_100_006_ab.ir100_low[k]<-x[low.cut]
  
  #int_100_009
  x<-sort(as.numeric(Mod.int_100_009_ab$epi$ir100[k,1:nsims]))
  int_100_009_ab.ir100[k]<-mean(x)
  int_100_009_ab.ir100_hi[k]<-x[hi.cut]
  int_100_009_ab.ir100_low[k]<-x[low.cut]
  
  #LOW AHI SEEKING RATE
  x<-sort(as.numeric(Mod.noint_256_006_l$epi$ir100[k,1:nsims]))
  noint_256_006_l.ir100[k]<-mean(x)
  noint_256_006_l.ir100_hi[k]<-x[hi.cut]
  noint_256_006_l.ir100_low[k]<-x[low.cut]
  
  x<-sort(as.numeric(Mod.int_256_006_l$epi$ir100[k,1:nsims]))
  int_256_006_l.ir100[k]<-mean(x)
  int_256_006_l.ir100_hi[k]<-x[hi.cut]
  int_256_006_l.ir100_low[k]<-x[low.cut]
  
  x<-sort(as.numeric(Mod.int_45_006_l$epi$ir100[k,1:nsims]))
  int_45_006_l.ir100[k]<-mean(x)
  int_45_006_l.ir100_hi[k]<-x[hi.cut]
  int_45_006_l.ir100_low[k]<-x[low.cut]
  
  x<-sort(as.numeric(Mod.int_949_006_l$epi$ir100[k,1:nsims]))
  int_949_006_l.ir100[k]<-mean(x)
  int_949_006_l.ir100_hi[k]<-x[hi.cut]
  int_949_006_l.ir100_low[k]<-x[low.cut]
  
  x<-sort(as.numeric(Mod.int_100_006_l$epi$ir100[k,1:nsims]))
  int_100_006_l.ir100[k]<-mean(x)
  int_100_006_l.ir100_hi[k]<-x[hi.cut]
  int_100_006_l.ir100_low[k]<-x[low.cut]
}


##Summary measures
measure <- c("cum.incid", "cum.ptar", "cum.ptinf", "cum.rna.tests", "cum.ab.tests", "cum.tests", "cum.ab.tests", "cum.diag", "cum.diag.p",
             "cum.diag.a", "cum.partners", "cum.partners.f", "cum.n.tests.tst.ps" , "NIA.100K.list", "PIA.list", "DPTST.list" , "DPTM.list", "cum.bg.tests",
             "cum.n.tests", "cum.n.false.pos", "cum.n.false.neg", "cum.n.true.pos", "cum.n.true.neg", "cum.n.presented", "cum.n.presented.pos", "cum.n.presented.neg",
             "cum.n.missed.pos")



for(i in 1:length(scenarios)){
  
  x<-get(scenarios[i])
  for(k in 1:length(measure)){

    fn <- paste0(scenarios[i], ".",measure[k])
    assign(fn,rep(NA,nsims))
    

  }}

##Cumulative incidence
for (i in 1:nsims){

noint_256_002.cum.incid[i] <- sum(as.numeric(Mod.noint_256_002$epi$incid.poi[,i]))
noint_256_002.cum.ptar[i] <- sum(Mod.noint_256_002$epi$num.poi[,i])-sum(Mod.noint_256_002$epi$i.num.poi[,i])
noint_256_002.cum.ptinf[i] <- sum(Mod.noint_256_002$epi$i.num.poi[,i])
noint_256_002.cum.rna.tests[i] <- sum(Mod.noint_256_002$epi$n.tests.rna[,i])
noint_256_002.cum.ab.tests[i] <- sum(Mod.noint_256_002$epi$n.tests.ab[,i])
noint_256_002.cum.tests[i] <- sum(Mod.noint_256_002$epi$n.tests[,i])
noint_256_002.cum.diag[i] <- sum(Mod.noint_256_002$epi$diag.prevalent[,i]) + sum(Mod.noint_256_002$epi$diag.acute[,i])
noint_256_002.cum.diag.p[i] <- sum(Mod.noint_256_002$epi$diag.prevalent[,i])
noint_256_002.cum.diag.a[i] <- sum(Mod.noint_256_002$epi$diag.acute[,i])
noint_256_002.cum.partners[i] <- sum(Mod.noint_256_002$epi$partners.sought.new[,i])
noint_256_002.cum.partners.f[i] <- sum(Mod.noint_256_002$epi$partners.found[,i])
noint_256_002.cum.n.tests.tst.ps[i] <- sum(Mod.noint_256_002$epi$n.tests.tst.psositive[,i])
noint_256_002.NIA.100K.list[i]<-NA
noint_256_002.PIA.list[i]<-NA
noint_256_002.DPTST.list[i]<-noint_256_002.cum.diag[i]/(noint_256_002.cum.tests[i] / 1000)
noint_256_002.DPTM.list[i]<- (noint_256_002.cum.diag[i]/noint_256_002.cum.ptinf[i])*52*100
noint_256_002.cum.bg.tests[i] <- sum(Mod.noint_256_002$epi$n.tests.bg[,i])

noint_256_002.cum.n.false.pos[i] <- sum(as.numeric(Mod.noint_256_002$epi$n.false.pos[,i]),na.rm=TRUE)
noint_256_002.cum.n.false.neg[i] <- sum(as.numeric(Mod.noint_256_002$epi$n.false.neg[,i]),na.rm=TRUE)
noint_256_002.cum.n.true.pos[i] <- sum(as.numeric(Mod.noint_256_002$epi$n.true.pos[,i]),na.rm=TRUE)
noint_256_002.cum.n.true.neg[i] <- sum(as.numeric(Mod.noint_256_002$epi$n.true.neg[,i]),na.rm=TRUE)
noint_256_002.cum.n.presented[i] <- sum(as.numeric(Mod.noint_256_002$epi$n.presented[,i]), na.rm=TRUE)
noint_256_002.cum.n.presented.pos[i] <- sum(as.numeric(Mod.noint_256_002$epi$n.presented.pos[,i]), na.rm=TRUE)
noint_256_002.cum.n.presented.neg[i] <- sum(as.numeric(Mod.noint_256_002$epi$n.presented.neg[,i]), na.rm=TRUE)
noint_256_002.cum.n.missed.pos[i] <- sum(as.numeric(Mod.noint_256_002$epi$missed.pos[,i]), na.rm=TRUE)


#noint_256_006
noint_256_006.cum.incid[i] <- sum(as.numeric(Mod.noint_256_006$epi$incid.poi[,i]))
noint_256_006.cum.ptar[i] <- sum(Mod.noint_256_006$epi$num.poi[,i])-sum(Mod.noint_256_006$epi$i.num.poi[,i])
noint_256_006.cum.ptinf[i] <- sum(Mod.noint_256_006$epi$i.num.poi[,i])
noint_256_006.cum.rna.tests[i] <- sum(Mod.noint_256_006$epi$n.tests.rna[,i])
noint_256_006.cum.ab.tests[i] <- sum(Mod.noint_256_006$epi$n.tests.ab[,i])
noint_256_006.cum.tests[i] <- sum(Mod.noint_256_006$epi$n.tests[,i])
noint_256_006.cum.diag[i] <- sum(Mod.noint_256_006$epi$diag.prevalent[,i]) + sum(Mod.noint_256_006$epi$diag.acute[,i])
noint_256_006.cum.diag.p[i] <- sum(Mod.noint_256_006$epi$diag.prevalent[,i])
noint_256_006.cum.diag.a[i] <- sum(Mod.noint_256_006$epi$diag.acute[,i])
noint_256_006.cum.partners[i] <- sum(Mod.noint_256_006$epi$partners.sought.new[,i])
noint_256_006.cum.partners.f[i] <- sum(Mod.noint_256_006$epi$partners.found[,i])
noint_256_006.cum.n.tests.tst.ps[i] <- sum(Mod.noint_256_006$epi$n.tests.tst.psositive[,i])
noint_256_006.NIA.100K.list[i] <- NA
noint_256_006.PIA.list[i]<- NA
noint_256_006.DPTST.list[i]<-noint_256_006.cum.diag[i]/(noint_256_006.cum.tests[i] / 1000)
noint_256_006.DPTM.list[i]<- (noint_256_006.cum.diag[i]/noint_256_006.cum.ptinf[i])*52*100
noint_256_006.cum.bg.tests[i] <- sum(Mod.noint_256_006$epi$n.tests.bg[,i])

noint_256_006.cum.n.false.pos[i] <- sum(as.numeric(Mod.noint_256_006$epi$n.false.pos[,i]),na.rm=TRUE)
noint_256_006.cum.n.false.neg[i] <- sum(as.numeric(Mod.noint_256_006$epi$n.false.neg[,i]),na.rm=TRUE)
noint_256_006.cum.n.true.pos[i] <- sum(as.numeric(Mod.noint_256_006$epi$n.true.pos[,i]),na.rm=TRUE)
noint_256_006.cum.n.true.neg[i] <- sum(as.numeric(Mod.noint_256_006$epi$n.true.neg[,i]),na.rm=TRUE)
noint_256_006.cum.n.presented[i] <- sum(as.numeric(Mod.noint_256_006$epi$n.presented[,i]), na.rm=TRUE)
noint_256_006.cum.n.presented.pos[i] <- sum(as.numeric(Mod.noint_256_006$epi$n.presented.pos[,i]), na.rm=TRUE)
noint_256_006.cum.n.presented.neg[i] <- sum(as.numeric(Mod.noint_256_006$epi$n.presented.neg[,i]), na.rm=TRUE)
noint_256_006.cum.n.missed.pos[i] <- sum(as.numeric(Mod.noint_256_006$epi$missed.pos[,i]), na.rm=TRUE)

#noint_256_009 
noint_256_009.cum.incid[i] <- sum(as.numeric(Mod.noint_256_009$epi$incid.poi[,i]))
noint_256_009.cum.ptar[i] <- sum(Mod.noint_256_009$epi$num.poi[,i])-sum(Mod.noint_256_009$epi$i.num.poi[,i])
noint_256_009.cum.ptinf[i] <- sum(Mod.noint_256_009$epi$i.num.poi[,i])
noint_256_009.cum.rna.tests[i] <- sum(Mod.noint_256_009$epi$n.tests.rna[,i])
noint_256_009.cum.ab.tests[i] <- sum(Mod.noint_256_009$epi$n.tests.ab[,i])
noint_256_009.cum.tests[i] <- sum(Mod.noint_256_009$epi$n.tests[,i])
noint_256_009.cum.diag[i] <- sum(Mod.noint_256_009$epi$diag.prevalent[,i]) + sum(Mod.noint_256_009$epi$diag.acute[,i])
noint_256_009.cum.diag.p[i] <- sum(Mod.noint_256_009$epi$diag.prevalent[,i])
noint_256_009.cum.diag.a[i] <- sum(Mod.noint_256_009$epi$diag.acute[,i])
noint_256_009.cum.partners[i] <- sum(Mod.noint_256_009$epi$partners.sought.new[,i])
noint_256_009.cum.partners.f[i] <- sum(Mod.noint_256_009$epi$partners.found[,i])
noint_256_009.cum.n.tests.tst.ps[i] <- sum(Mod.noint_256_009$epi$n.tests.tst.psositive[,i])
noint_256_009.NIA.100K.list[i] <- NA
noint_256_009.PIA.list[i] <- NA
noint_256_009.DPTST.list[i]<-noint_256_009.cum.diag[i]/(noint_256_009.cum.tests[i] / 1000)
noint_256_009.DPTM.list[i]<- (noint_256_009.cum.diag[i]/noint_256_009.cum.ptinf[i])*52*100
noint_256_009.cum.bg.tests[i] <- sum(Mod.noint_256_009$epi$n.tests.bg[,i])

noint_256_009.cum.n.false.pos[i] <- sum(as.numeric(Mod.noint_256_009$epi$n.false.pos[,i]),na.rm=TRUE)
noint_256_009.cum.n.false.neg[i] <- sum(as.numeric(Mod.noint_256_009$epi$n.false.neg[,i]),na.rm=TRUE)
noint_256_009.cum.n.true.pos[i] <- sum(as.numeric(Mod.noint_256_009$epi$n.true.pos[,i]),na.rm=TRUE)
noint_256_009.cum.n.true.neg[i] <- sum(as.numeric(Mod.noint_256_009$epi$n.true.neg[,i]),na.rm=TRUE)
noint_256_009.cum.n.presented[i] <- sum(as.numeric(Mod.noint_256_009$epi$n.presented[,i]), na.rm=TRUE)
noint_256_009.cum.n.presented.pos[i] <- sum(as.numeric(Mod.noint_256_009$epi$n.presented.pos[,i]), na.rm=TRUE)
noint_256_009.cum.n.presented.neg[i] <- sum(as.numeric(Mod.noint_256_009$epi$n.presented.neg[,i]), na.rm=TRUE)
noint_256_009.cum.n.missed.pos[i] <- sum(as.numeric(Mod.noint_256_009$epi$missed.pos[,i]), na.rm=TRUE)


#int_256_002 
int_256_002.cum.incid[i] <- sum(as.numeric(Mod.int_256_002$epi$incid.poi[,i]))
int_256_002.cum.ptar[i] <- sum(Mod.int_256_002$epi$num.poi[,i])-sum(Mod.int_256_002$epi$i.num.poi[,i])
int_256_002.cum.ptinf[i] <- sum(Mod.int_256_002$epi$i.num.poi[,i])
int_256_002.cum.rna.tests[i] <- sum(Mod.int_256_002$epi$n.tests.rna[,i])
int_256_002.cum.ab.tests[i] <- sum(Mod.int_256_002$epi$n.tests.ab[,i])
int_256_002.cum.tests[i] <- sum(Mod.int_256_002$epi$n.tests[,i])
int_256_002.cum.diag[i] <- sum(Mod.int_256_002$epi$diag.prevalent[,i]) + sum(Mod.int_256_002$epi$diag.acute[,i])
int_256_002.cum.diag.p[i] <- sum(Mod.int_256_002$epi$diag.prevalent[,i])
int_256_002.cum.diag.a[i] <- sum(Mod.int_256_002$epi$diag.acute[,i])
int_256_002.cum.partners[i] <- sum(Mod.int_256_002$epi$partners.sought.new[,i])
int_256_002.cum.partners.f[i] <- sum(Mod.int_256_002$epi$partners.found[,i])
int_256_002.cum.n.tests.tst.ps[i] <- sum(Mod.int_256_002$epi$n.tests.tst.psositive[,i])
int_256_002.NIA.100K.list[i]<-((noint_256_002.cum.incid[i]-int_256_002.cum.incid[i])/int_256_002.cum.ptar[i])*52*100000
int_256_002.PIA.list[i]<-(noint_256_002.cum.incid[i]-int_256_002.cum.incid[i])/noint_256_002.cum.incid[i] * 100
int_256_002.DPTST.list[i]<-int_256_002.cum.diag[i]/(int_256_002.cum.tests[i] / 1000)
int_256_002.DPTM.list[i]<- (int_256_002.cum.diag[i]/int_256_002.cum.ptinf[i])*52*100
int_256_002.cum.bg.tests[i] <- sum(Mod.int_256_002$epi$n.tests.bg[,i])

int_256_002.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_256_002$epi$n.false.pos[,i]),na.rm=TRUE)
int_256_002.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_256_002$epi$n.false.neg[,i]),na.rm=TRUE)
int_256_002.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_256_002$epi$n.true.pos[,i]),na.rm=TRUE)
int_256_002.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_256_002$epi$n.true.neg[,i]),na.rm=TRUE)
int_256_002.cum.n.presented[i] <- sum(as.numeric(Mod.int_256_002$epi$n.presented[,i]), na.rm=TRUE)
int_256_002.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_256_002$epi$n.presented.pos[,i]), na.rm=TRUE)
int_256_002.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_256_002$epi$n.presented.neg[,i]), na.rm=TRUE)
int_256_002.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_256_002$epi$missed.pos[,i]), na.rm=TRUE)

#int_256_006
int_256_006.cum.incid[i] <- sum(as.numeric(Mod.int_256_006$epi$incid.poi[,i]))
int_256_006.cum.ptar[i] <- sum(Mod.int_256_006$epi$num.poi[,i])-sum(Mod.int_256_006$epi$i.num.poi[,i])
int_256_006.cum.ptinf[i] <- sum(Mod.int_256_006$epi$i.num.poi[,i])
int_256_006.cum.rna.tests[i] <- sum(Mod.int_256_006$epi$n.tests.rna[,i])
int_256_006.cum.ab.tests[i] <- sum(Mod.int_256_006$epi$n.tests.ab[,i])
int_256_006.cum.tests[i] <- sum(Mod.int_256_006$epi$n.tests[,i])
int_256_006.cum.diag[i] <- sum(Mod.int_256_006$epi$diag.prevalent[,i]) + sum(Mod.int_256_006$epi$diag.acute[,i])
int_256_006.cum.diag.p[i] <- sum(Mod.int_256_006$epi$diag.prevalent[,i])
int_256_006.cum.diag.a[i] <- sum(Mod.int_256_006$epi$diag.acute[,i])
int_256_006.cum.partners[i] <- sum(Mod.int_256_006$epi$partners.sought.new[,i])
int_256_006.cum.partners.f[i] <- sum(Mod.int_256_006$epi$partners.found[,i])
int_256_006.cum.n.tests.tst.ps[i] <- sum(Mod.int_256_006$epi$n.tests.tst.psositive[,i])
int_256_006.NIA.100K.list[i]<-((noint_256_006.cum.incid[i]-int_256_006.cum.incid[i])/int_256_006.cum.ptar[i])*52*100000
int_256_006.PIA.list[i]<-(noint_256_006.cum.incid[i]-int_256_006.cum.incid[i])/noint_256_006.cum.incid[i] * 100
int_256_006.DPTST.list[i]<-int_256_006.cum.diag[i]/(int_256_006.cum.tests[i] / 1000)
int_256_006.DPTM.list[i]<- (int_256_006.cum.diag[i]/int_256_006.cum.ptinf[i])*52*100
int_256_006.cum.bg.tests[i] <- sum(Mod.int_256_006$epi$n.tests.bg[,i])

int_256_006.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_256_006$epi$n.false.pos[,i]),na.rm=TRUE)
int_256_006.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_256_006$epi$n.false.neg[,i]),na.rm=TRUE)
int_256_006.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_256_006$epi$n.true.pos[,i]),na.rm=TRUE)
int_256_006.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_256_006$epi$n.true.neg[,i]),na.rm=TRUE)
int_256_006.cum.n.presented[i] <- sum(as.numeric(Mod.int_256_006$epi$n.presented[,i]), na.rm=TRUE)
int_256_006.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_256_006$epi$n.presented.pos[,i]), na.rm=TRUE)
int_256_006.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_256_006$epi$n.presented.neg[,i]), na.rm=TRUE)
int_256_006.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_256_006$epi$missed.pos[,i]), na.rm=TRUE)

#int_256_009
int_256_009.cum.incid[i] <- sum(as.numeric(Mod.int_256_009$epi$incid.poi[,i]))
int_256_009.cum.ptar[i] <- sum(Mod.int_256_009$epi$num.poi[,i])-sum(Mod.int_256_009$epi$i.num.poi[,i])
int_256_009.cum.ptinf[i] <- sum(Mod.int_256_009$epi$i.num.poi[,i])
int_256_009.cum.rna.tests[i] <- sum(Mod.int_256_009$epi$n.tests.rna[,i])
int_256_009.cum.ab.tests[i] <- sum(Mod.int_256_009$epi$n.tests.ab[,i])
int_256_009.cum.tests[i] <- sum(Mod.int_256_009$epi$n.tests[,i])
int_256_009.cum.diag[i] <- sum(Mod.int_256_009$epi$diag.prevalent[,i]) + sum(Mod.int_256_009$epi$diag.acute[,i])
int_256_009.cum.diag.p[i] <- sum(Mod.int_256_009$epi$diag.prevalent[,i])
int_256_009.cum.diag.a[i] <- sum(Mod.int_256_009$epi$diag.acute[,i])
int_256_009.cum.partners[i] <- sum(Mod.int_256_009$epi$partners.sought.new[,i])
int_256_009.cum.partners.f[i] <- sum(Mod.int_256_009$epi$partners.found[,i])
int_256_009.cum.n.tests.tst.ps[i] <- sum(Mod.int_256_009$epi$n.tests.tst.psositive[,i])
int_256_009.NIA.100K.list[i]<-((noint_256_009.cum.incid[i]-int_256_009.cum.incid[i])/int_256_009.cum.ptar[i])*52*100000
int_256_009.PIA.list[i]<-(noint_256_009.cum.incid[i]-int_256_009.cum.incid[i])/noint_256_009.cum.incid[i] * 100
int_256_009.DPTST.list[i]<-int_256_009.cum.diag[i]/(int_256_009.cum.tests[i] / 1000)
int_256_009.DPTM.list[i]<- (int_256_009.cum.diag[i]/int_256_009.cum.ptinf[i])*52*100
int_256_009.cum.bg.tests[i] <- sum(Mod.int_256_009$epi$n.tests.bg[,i])

int_256_009.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_256_009$epi$n.false.pos[,i]),na.rm=TRUE)
int_256_009.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_256_009$epi$n.false.neg[,i]),na.rm=TRUE)
int_256_009.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_256_009$epi$n.true.pos[,i]),na.rm=TRUE)
int_256_009.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_256_009$epi$n.true.neg[,i]),na.rm=TRUE)
int_256_009.cum.n.presented[i] <- sum(as.numeric(Mod.int_256_009$epi$n.presented[,i]), na.rm=TRUE)
int_256_009.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_256_009$epi$n.presented.pos[,i]), na.rm=TRUE)
int_256_009.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_256_009$epi$n.presented.neg[,i]), na.rm=TRUE)
int_256_009.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_256_009$epi$missed.pos[,i]), na.rm=TRUE)


#int_45_002
int_45_002.cum.incid[i] <- sum(as.numeric(Mod.int_45_002$epi$incid.poi[,i]))
int_45_002.cum.ptar[i] <- sum(Mod.int_45_002$epi$num.poi[,i])-sum(Mod.int_45_002$epi$i.num.poi[,i])
int_45_002.cum.ptinf[i] <- sum(Mod.int_45_002$epi$i.num.poi[,i])
int_45_002.cum.rna.tests[i] <- sum(Mod.int_45_002$epi$n.tests.rna[,i])
int_45_002.cum.ab.tests[i] <- sum(Mod.int_45_002$epi$n.tests.ab[,i])
int_45_002.cum.tests[i] <- sum(Mod.int_45_002$epi$n.tests[,i])
int_45_002.cum.diag[i] <- sum(Mod.int_45_002$epi$diag.prevalent[,i]) + sum(Mod.int_45_002$epi$diag.acute[,i])
int_45_002.cum.diag.p[i] <- sum(Mod.int_45_002$epi$diag.prevalent[,i])
int_45_002.cum.diag.a[i] <- sum(Mod.int_45_002$epi$diag.acute[,i])
int_45_002.cum.partners[i] <- sum(Mod.int_45_002$epi$partners.sought.new[,i])
int_45_002.cum.partners.f[i] <- sum(Mod.int_45_002$epi$partners.found[,i])
int_45_002.cum.n.tests.tst.ps[i] <- sum(Mod.int_45_002$epi$n.tests.tst.psositive[,i])
int_45_002.NIA.100K.list[i]<-((noint_256_002.cum.incid[i]-int_45_002.cum.incid[i])/int_45_002.cum.ptar[i])*52*100000
int_45_002.PIA.list[i]<-(noint_256_002.cum.incid[i]-int_45_002.cum.incid[i])/noint_256_002.cum.incid[i] * 100
int_45_002.DPTST.list[i]<-int_45_002.cum.diag[i]/(int_45_002.cum.tests[i] / 1000)
int_45_002.DPTM.list[i]<- (int_45_002.cum.diag[i]/int_45_002.cum.ptinf[i])*52*100
int_45_002.cum.bg.tests[i] <- sum(Mod.int_45_002$epi$n.tests.bg[,i])

int_45_002.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_45_002$epi$n.false.pos[,i]),na.rm=TRUE)
int_45_002.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_45_002$epi$n.false.neg[,i]),na.rm=TRUE)
int_45_002.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_45_002$epi$n.true.pos[,i]),na.rm=TRUE)
int_45_002.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_45_002$epi$n.true.neg[,i]),na.rm=TRUE)
int_45_002.cum.n.presented[i] <- sum(as.numeric(Mod.int_45_002$epi$n.presented[,i]), na.rm=TRUE)
int_45_002.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_45_002$epi$n.presented.pos[,i]), na.rm=TRUE)
int_45_002.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_45_002$epi$n.presented.neg[,i]), na.rm=TRUE)
int_45_002.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_45_002$epi$missed.pos[,i]), na.rm=TRUE)


#int_45_006
int_45_006.cum.incid[i] <- sum(as.numeric(Mod.int_45_006$epi$incid.poi[,i]))
int_45_006.cum.ptar[i] <- sum(Mod.int_45_006$epi$num.poi[,i])-sum(Mod.int_45_006$epi$i.num.poi[,i])
int_45_006.cum.ptinf[i] <- sum(Mod.int_45_006$epi$i.num.poi[,i])
int_45_006.cum.rna.tests[i] <- sum(Mod.int_45_006$epi$n.tests.rna[,i])
int_45_006.cum.ab.tests[i] <- sum(Mod.int_45_006$epi$n.tests.ab[,i])
int_45_006.cum.tests[i] <- sum(Mod.int_45_006$epi$n.tests[,i])
int_45_006.cum.diag[i] <- sum(Mod.int_45_006$epi$diag.prevalent[,i]) + sum(Mod.int_45_006$epi$diag.acute[,i])
int_45_006.cum.diag.p[i] <- sum(Mod.int_45_006$epi$diag.prevalent[,i])
int_45_006.cum.diag.a[i] <- sum(Mod.int_45_006$epi$diag.acute[,i])
int_45_006.cum.partners[i] <- sum(Mod.int_45_006$epi$partners.sought.new[,i])
int_45_006.cum.partners.f[i] <- sum(Mod.int_45_006$epi$partners.found[,i])
int_45_006.cum.n.tests.tst.ps[i] <- sum(Mod.int_45_006$epi$n.tests.tst.psositive[,i])
int_45_006.NIA.100K.list[i]<-((noint_256_006.cum.incid[i]-int_45_006.cum.incid[i])/int_45_006.cum.ptar[i])*52*100000
int_45_006.PIA.list[i]<-(noint_256_006.cum.incid[i]-int_45_006.cum.incid[i])/noint_256_006.cum.incid[i] * 100
int_45_006.DPTST.list[i]<-int_45_006.cum.diag[i]/(int_45_006.cum.tests[i] / 1000)
int_45_006.DPTM.list[i]<- (int_45_006.cum.diag[i]/int_45_006.cum.ptinf[i])*52*100
int_45_006.cum.bg.tests[i] <- sum(Mod.int_45_006$epi$n.tests.bg[,i])

int_45_006.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_45_006$epi$n.false.pos[,i]),na.rm=TRUE)
int_45_006.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_45_006$epi$n.false.neg[,i]),na.rm=TRUE)
int_45_006.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_45_006$epi$n.true.pos[,i]),na.rm=TRUE)
int_45_006.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_45_006$epi$n.true.neg[,i]),na.rm=TRUE)
int_45_006.cum.n.presented[i] <- sum(as.numeric(Mod.int_45_006$epi$n.presented[,i]), na.rm=TRUE)
int_45_006.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_45_006$epi$n.presented.pos[,i]), na.rm=TRUE)
int_45_006.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_45_006$epi$n.presented.neg[,i]), na.rm=TRUE)
int_45_006.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_45_006$epi$missed.pos[,i]), na.rm=TRUE)

#int_45_009
int_45_009.cum.incid[i] <- sum(as.numeric(Mod.int_45_009$epi$incid.poi[,i]))
int_45_009.cum.ptar[i] <- sum(Mod.int_45_009$epi$num.poi[,i])-sum(Mod.int_45_009$epi$i.num.poi[,i])
int_45_009.cum.ptinf[i] <- sum(Mod.int_45_009$epi$i.num.poi[,i])
int_45_009.cum.rna.tests[i] <- sum(Mod.int_45_009$epi$n.tests.rna[,i])
int_45_009.cum.ab.tests[i] <- sum(Mod.int_45_009$epi$n.tests.ab[,i])
int_45_009.cum.tests[i] <- sum(Mod.int_45_009$epi$n.tests[,i])
int_45_009.cum.diag[i] <- sum(Mod.int_45_009$epi$diag.prevalent[,i]) + sum(Mod.int_45_009$epi$diag.acute[,i])
int_45_009.cum.diag.p[i] <- sum(Mod.int_45_009$epi$diag.prevalent[,i])
int_45_009.cum.diag.a[i] <- sum(Mod.int_45_009$epi$diag.acute[,i])
int_45_009.cum.partners[i] <- sum(Mod.int_45_009$epi$partners.sought.new[,i])
int_45_009.cum.partners.f[i] <- sum(Mod.int_45_009$epi$partners.found[,i])
int_45_009.cum.n.tests.tst.ps[i] <- sum(Mod.int_45_009$epi$n.tests.tst.psositive[,i])
int_45_009.NIA.100K.list[i]<-((noint_256_009.cum.incid[i]-int_45_009.cum.incid[i])/int_45_009.cum.ptar[i])*52*100000
int_45_009.PIA.list[i]<-(noint_256_009.cum.incid[i]-int_45_009.cum.incid[i])/noint_256_009.cum.incid[i] * 100
int_45_009.DPTST.list[i]<-int_45_009.cum.diag[i]/(int_45_009.cum.tests[i] / 1000)
int_45_009.DPTM.list[i]<- (int_45_009.cum.diag[i]/int_45_009.cum.ptinf[i])*52*100
int_45_009.cum.bg.tests[i] <- sum(Mod.int_45_009$epi$n.tests.bg[,i])

int_45_009.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_45_009$epi$n.false.pos[,i]),na.rm=TRUE)
int_45_009.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_45_009$epi$n.false.neg[,i]),na.rm=TRUE)
int_45_009.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_45_009$epi$n.true.pos[,i]),na.rm=TRUE)
int_45_009.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_45_009$epi$n.true.neg[,i]),na.rm=TRUE)
int_45_009.cum.n.presented[i] <- sum(as.numeric(Mod.int_45_009$epi$n.presented[,i]), na.rm=TRUE)
int_45_009.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_45_009$epi$n.presented.pos[,i]), na.rm=TRUE)
int_45_009.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_45_009$epi$n.presented.neg[,i]), na.rm=TRUE)
int_45_009.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_45_009$epi$missed.pos[,i]), na.rm=TRUE)

#int_949_002
int_949_002.cum.incid[i] <- sum(as.numeric(Mod.int_949_002$epi$incid.poi[,i]))
int_949_002.cum.ptar[i] <- sum(Mod.int_949_002$epi$num.poi[,i])-sum(Mod.int_949_002$epi$i.num.poi[,i])
int_949_002.cum.ptinf[i] <- sum(Mod.int_949_002$epi$i.num.poi[,i])
int_949_002.cum.rna.tests[i] <- sum(Mod.int_949_002$epi$n.tests.rna[,i])
int_949_002.cum.ab.tests[i] <- sum(Mod.int_949_002$epi$n.tests.ab[,i])
int_949_002.cum.tests[i] <- sum(Mod.int_949_002$epi$n.tests[,i])
int_949_002.cum.diag[i] <- sum(Mod.int_949_002$epi$diag.prevalent[,i]) + sum(Mod.int_949_002$epi$diag.acute[,i])
int_949_002.cum.diag.p[i] <- sum(Mod.int_949_002$epi$diag.prevalent[,i])
int_949_002.cum.diag.a[i] <- sum(Mod.int_949_002$epi$diag.acute[,i])
int_949_002.cum.partners[i] <- sum(Mod.int_949_002$epi$partners.sought.new[,i])
int_949_002.cum.partners.f[i] <- sum(Mod.int_949_002$epi$partners.found[,i])
int_949_002.cum.n.tests.tst.ps[i] <- sum(Mod.int_949_002$epi$n.tests.tst.psositive[,i])
int_949_002.NIA.100K.list[i]<-((noint_256_002.cum.incid[i]-int_949_002.cum.incid[i])/int_949_002.cum.ptar[i])*52*100000
int_949_002.PIA.list[i]<-(noint_256_002.cum.incid[i]-int_949_002.cum.incid[i])/noint_256_002.cum.incid[i] * 100
int_949_002.DPTST.list[i]<-int_949_002.cum.diag[i]/(int_949_002.cum.tests[i] / 1000)
int_949_002.DPTM.list[i]<- (int_949_002.cum.diag[i]/int_949_002.cum.ptinf[i])*52*100
int_949_002.cum.bg.tests[i] <- sum(Mod.int_949_002$epi$n.tests.bg[,i])

int_949_002.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_949_002$epi$n.false.pos[,i]),na.rm=TRUE)
int_949_002.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_949_002$epi$n.false.neg[,i]),na.rm=TRUE)
int_949_002.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_949_002$epi$n.true.pos[,i]),na.rm=TRUE)
int_949_002.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_949_002$epi$n.true.neg[,i]),na.rm=TRUE)
int_949_002.cum.n.presented[i] <- sum(as.numeric(Mod.int_949_002$epi$n.presented[,i]), na.rm=TRUE)
int_949_002.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_949_002$epi$n.presented.pos[,i]), na.rm=TRUE)
int_949_002.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_949_002$epi$n.presented.neg[,i]), na.rm=TRUE)
int_949_002.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_949_002$epi$missed.pos[,i]), na.rm=TRUE)

#int_949_006
int_949_006.cum.incid[i] <- sum(as.numeric(Mod.int_949_006$epi$incid.poi[,i]))
int_949_006.cum.ptar[i] <- sum(Mod.int_949_006$epi$num.poi[,i])-sum(Mod.int_949_006$epi$i.num.poi[,i])
int_949_006.cum.ptinf[i] <- sum(Mod.int_949_006$epi$i.num.poi[,i])
int_949_006.cum.rna.tests[i] <- sum(Mod.int_949_006$epi$n.tests.rna[,i])
int_949_006.cum.ab.tests[i] <- sum(Mod.int_949_006$epi$n.tests.ab[,i])
int_949_006.cum.tests[i] <- sum(Mod.int_949_006$epi$n.tests[,i])
int_949_006.cum.diag[i] <- sum(Mod.int_949_006$epi$diag.prevalent[,i]) + sum(Mod.int_949_006$epi$diag.acute[,i])
int_949_006.cum.diag.p[i] <- sum(Mod.int_949_006$epi$diag.prevalent[,i])
int_949_006.cum.diag.a[i] <- sum(Mod.int_949_006$epi$diag.acute[,i])
int_949_006.cum.partners[i] <- sum(Mod.int_949_006$epi$partners.sought.new[,i])
int_949_006.cum.partners.f[i] <- sum(Mod.int_949_006$epi$partners.found[,i])
int_949_006.cum.n.tests.tst.ps[i] <- sum(Mod.int_949_006$epi$n.tests.tst.psositive[,i])
int_949_006.NIA.100K.list[i]<-((noint_256_006.cum.incid[i]-int_949_006.cum.incid[i])/int_949_006.cum.ptar[i])*52*100000
int_949_006.PIA.list[i]<-(noint_256_006.cum.incid[i]-int_949_006.cum.incid[i])/noint_256_006.cum.incid[i] * 100
int_949_006.DPTST.list[i]<-int_949_006.cum.diag[i]/(int_949_006.cum.tests[i] / 1000)
int_949_006.DPTM.list[i]<- (int_949_006.cum.diag[i]/int_949_006.cum.ptinf[i])*52*100
int_949_006.cum.bg.tests[i] <- sum(Mod.int_949_006$epi$n.tests.bg[,i])

int_949_006.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_949_006$epi$n.false.pos[,i]),na.rm=TRUE)
int_949_006.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_949_006$epi$n.false.neg[,i]),na.rm=TRUE)
int_949_006.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_949_006$epi$n.true.pos[,i]),na.rm=TRUE)
int_949_006.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_949_006$epi$n.true.neg[,i]),na.rm=TRUE)
int_949_006.cum.n.presented[i] <- sum(as.numeric(Mod.int_949_006$epi$n.presented[,i]), na.rm=TRUE)
int_949_006.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_949_006$epi$n.presented.pos[,i]), na.rm=TRUE)
int_949_006.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_949_006$epi$n.presented.neg[,i]), na.rm=TRUE)
int_949_006.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_949_006$epi$missed.pos[,i]), na.rm=TRUE)


#int_949_009
int_949_009.cum.incid[i] <- sum(as.numeric(Mod.int_949_009$epi$incid.poi[,i]))
int_949_009.cum.ptar[i] <- sum(Mod.int_949_009$epi$num.poi[,i])-sum(Mod.int_949_009$epi$i.num.poi[,i])
int_949_009.cum.ptinf[i] <- sum(Mod.int_949_009$epi$i.num.poi[,i])
int_949_009.cum.rna.tests[i] <- sum(Mod.int_949_009$epi$n.tests.rna[,i])
int_949_009.cum.ab.tests[i] <- sum(Mod.int_949_009$epi$n.tests.ab[,i])
int_949_009.cum.tests[i] <- sum(Mod.int_949_009$epi$n.tests[,i])
int_949_009.cum.diag[i] <- sum(Mod.int_949_009$epi$diag.prevalent[,i]) + sum(Mod.int_949_009$epi$diag.acute[,i])
int_949_009.cum.diag.p[i] <- sum(Mod.int_949_009$epi$diag.prevalent[,i])
int_949_009.cum.diag.a[i] <- sum(Mod.int_949_009$epi$diag.acute[,i])
int_949_009.cum.partners[i] <- sum(Mod.int_949_009$epi$partners.sought.new[,i])
int_949_009.cum.partners.f[i] <- sum(Mod.int_949_009$epi$partners.found[,i])
int_949_009.cum.n.tests.tst.ps[i] <- sum(Mod.int_949_009$epi$n.tests.tst.psositive[,i])
int_949_009.NIA.100K.list[i]<-((noint_256_009.cum.incid[i]-int_949_009.cum.incid[i])/int_949_009.cum.ptar[i])*52*100000
int_949_009.PIA.list[i]<-(noint_256_009.cum.incid[i]-int_949_009.cum.incid[i])/noint_256_009.cum.incid[i] * 100
int_949_009.DPTST.list[i]<-int_949_009.cum.diag[i]/(int_949_009.cum.tests[i] / 1000)
int_949_009.DPTM.list[i]<- (int_949_009.cum.diag[i]/int_949_009.cum.ptinf[i])*52*100
int_949_009.cum.bg.tests[i] <- sum(Mod.int_949_009$epi$n.tests.bg[,i])

int_949_009.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_949_009$epi$n.false.pos[,i]),na.rm=TRUE)
int_949_009.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_949_009$epi$n.false.neg[,i]),na.rm=TRUE)
int_949_009.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_949_009$epi$n.true.pos[,i]),na.rm=TRUE)
int_949_009.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_949_009$epi$n.true.neg[,i]),na.rm=TRUE)
int_949_009.cum.n.presented[i] <- sum(as.numeric(Mod.int_949_009$epi$n.presented[,i]), na.rm=TRUE)
int_949_009.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_949_009$epi$n.presented.pos[,i]), na.rm=TRUE)
int_949_009.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_949_009$epi$n.presented.neg[,i]), na.rm=TRUE)
int_949_009.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_949_009$epi$missed.pos[,i]), na.rm=TRUE)

#int_100_002
int_100_002.cum.incid[i] <- sum(as.numeric(Mod.int_100_002$epi$incid.poi[,i]))
int_100_002.cum.ptar[i] <- sum(Mod.int_100_002$epi$num.poi[,i])-sum(Mod.int_100_002$epi$i.num.poi[,i])
int_100_002.cum.ptinf[i] <- sum(Mod.int_100_002$epi$i.num.poi[,i])
int_100_002.cum.rna.tests[i] <- sum(Mod.int_100_002$epi$n.tests.rna[,i])
int_100_002.cum.ab.tests[i] <- sum(Mod.int_100_002$epi$n.tests.ab[,i])
int_100_002.cum.tests[i] <- sum(Mod.int_100_002$epi$n.tests[,i])
int_100_002.cum.diag[i] <- sum(Mod.int_100_002$epi$diag.prevalent[,i]) + sum(Mod.int_100_002$epi$diag.acute[,i])
int_100_002.cum.diag.p[i] <- sum(Mod.int_100_002$epi$diag.prevalent[,i])
int_100_002.cum.diag.a[i] <- sum(Mod.int_100_002$epi$diag.acute[,i])
int_100_002.cum.partners[i] <- sum(Mod.int_100_002$epi$partners.sought.new[,i])
int_100_002.cum.partners.f[i] <- sum(Mod.int_100_002$epi$partners.found[,i])
int_100_002.cum.n.tests.tst.ps[i] <- sum(Mod.int_100_002$epi$n.tests.tst.psositive[,i])
int_100_002.NIA.100K.list[i]<-((noint_256_002.cum.incid[i]-int_100_002.cum.incid[i])/int_100_002.cum.ptar[i])*52*100000
int_100_002.PIA.list[i]<-(noint_256_002.cum.incid[i]-int_100_002.cum.incid[i])/noint_256_002.cum.incid[i] * 100
int_100_002.DPTST.list[i]<-int_100_002.cum.diag[i]/(int_100_002.cum.tests[i] / 1000)
int_100_002.DPTM.list[i]<- (int_100_002.cum.diag[i]/int_100_002.cum.ptinf[i])*52*100
int_100_002.cum.bg.tests[i] <- sum(Mod.int_100_002$epi$n.tests.bg[,i])

int_100_002.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_100_002$epi$n.false.pos[,i]),na.rm=TRUE)
int_100_002.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_100_002$epi$n.false.neg[,i]),na.rm=TRUE)
int_100_002.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_100_002$epi$n.true.pos[,i]),na.rm=TRUE)
int_100_002.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_100_002$epi$n.true.neg[,i]),na.rm=TRUE)
int_100_002.cum.n.presented[i] <- sum(as.numeric(Mod.int_100_002$epi$n.presented[,i]), na.rm=TRUE)
int_100_002.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_100_002$epi$n.presented.pos[,i]), na.rm=TRUE)
int_100_002.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_100_002$epi$n.presented.neg[,i]), na.rm=TRUE)
int_100_002.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_100_002$epi$missed.pos[,i]), na.rm=TRUE)


#int_100_006
int_100_006.cum.incid[i] <- sum(as.numeric(Mod.int_100_006$epi$incid.poi[,i]))
int_100_006.cum.ptar[i] <- sum(Mod.int_100_006$epi$num.poi[,i])-sum(Mod.int_100_006$epi$i.num.poi[,i])
int_100_006.cum.ptinf[i] <- sum(Mod.int_100_006$epi$i.num.poi[,i])
int_100_006.cum.rna.tests[i] <- sum(Mod.int_100_006$epi$n.tests.rna[,i])
int_100_006.cum.ab.tests[i] <- sum(Mod.int_100_006$epi$n.tests.ab[,i])
int_100_006.cum.tests[i] <- sum(Mod.int_100_006$epi$n.tests[,i])
int_100_006.cum.diag[i] <- sum(Mod.int_100_006$epi$diag.prevalent[,i]) + sum(Mod.int_100_006$epi$diag.acute[,i])
int_100_006.cum.diag.p[i] <- sum(Mod.int_100_006$epi$diag.prevalent[,i])
int_100_006.cum.diag.a[i] <- sum(Mod.int_100_006$epi$diag.acute[,i])
int_100_006.cum.partners[i] <- sum(Mod.int_100_006$epi$partners.sought.new[,i])
int_100_006.cum.partners.f[i] <- sum(Mod.int_100_006$epi$partners.found[,i])
int_100_006.cum.n.tests.tst.ps[i] <- sum(Mod.int_100_006$epi$n.tests.tst.psositive[,i])
int_100_006.NIA.100K.list[i]<-((noint_256_006.cum.incid[i]-int_100_006.cum.incid[i])/int_100_006.cum.ptar[i])*52*100000
int_100_006.PIA.list[i]<-(noint_256_006.cum.incid[i]-int_100_006.cum.incid[i])/noint_256_006.cum.incid[i] * 100
int_100_006.DPTST.list[i]<-int_100_006.cum.diag[i]/(int_100_006.cum.tests[i] / 1000)
int_100_006.DPTM.list[i]<- (int_100_006.cum.diag[i]/int_100_006.cum.ptinf[i])*52*100
int_100_006.cum.bg.tests[i] <- sum(Mod.int_100_006$epi$n.tests.bg[,i])

int_100_006.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_100_006$epi$n.false.pos[,i]),na.rm=TRUE)
int_100_006.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_100_006$epi$n.false.neg[,i]),na.rm=TRUE)
int_100_006.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_100_006$epi$n.true.pos[,i]),na.rm=TRUE)
int_100_006.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_100_006$epi$n.true.neg[,i]),na.rm=TRUE)
int_100_006.cum.n.presented[i] <- sum(as.numeric(Mod.int_100_006$epi$n.presented[,i]), na.rm=TRUE)
int_100_006.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_100_006$epi$n.presented.pos[,i]), na.rm=TRUE)
int_100_006.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_100_006$epi$n.presented.neg[,i]), na.rm=TRUE)
int_100_006.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_100_006$epi$missed.pos[,i]), na.rm=TRUE)


#int_100_009
int_100_009.cum.incid[i] <- sum(as.numeric(Mod.int_100_009$epi$incid.poi[,i]))
int_100_009.cum.ptar[i] <- sum(Mod.int_100_009$epi$num.poi[,i])-sum(Mod.int_100_009$epi$i.num.poi[,i])
int_100_009.cum.ptinf[i] <- sum(Mod.int_100_009$epi$i.num.poi[,i])
int_100_009.cum.rna.tests[i] <- sum(Mod.int_100_009$epi$n.tests.rna[,i])
int_100_009.cum.ab.tests[i] <- sum(Mod.int_100_009$epi$n.tests.ab[,i])
int_100_009.cum.tests[i] <- sum(Mod.int_100_009$epi$n.tests[,i])
int_100_009.cum.diag[i] <- sum(Mod.int_100_009$epi$diag.prevalent[,i]) + sum(Mod.int_100_009$epi$diag.acute[,i])
int_100_009.cum.diag.p[i] <- sum(Mod.int_100_009$epi$diag.prevalent[,i])
int_100_009.cum.diag.a[i] <- sum(Mod.int_100_009$epi$diag.acute[,i])
int_100_009.cum.partners[i] <- sum(Mod.int_100_009$epi$partners.sought.new[,i])
int_100_009.cum.partners.f[i] <- sum(Mod.int_100_009$epi$partners.found[,i])
int_100_009.cum.n.tests.tst.ps[i] <- sum(Mod.int_100_009$epi$n.tests.tst.psositive[,i])
int_100_009.NIA.100K.list[i]<-((noint_256_009.cum.incid[i]-int_100_009.cum.incid[i])/int_100_009.cum.ptar[i])*52*100000
int_100_009.PIA.list[i]<-(noint_256_009.cum.incid[i]-int_100_009.cum.incid[i])/noint_256_009.cum.incid[i] * 100
int_100_009.DPTST.list[i]<-int_100_009.cum.diag[i]/(int_100_009.cum.tests[i] / 1000)
int_100_009.DPTM.list[i]<- (int_100_009.cum.diag[i]/int_100_009.cum.ptinf[i])*52*100
int_100_009.cum.bg.tests[i] <- sum(Mod.int_100_009$epi$n.tests.bg[,i])

int_100_009.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_100_009$epi$n.false.pos[,i]),na.rm=TRUE)
int_100_009.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_100_009$epi$n.false.neg[,i]),na.rm=TRUE)
int_100_009.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_100_009$epi$n.true.pos[,i]),na.rm=TRUE)
int_100_009.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_100_009$epi$n.true.neg[,i]),na.rm=TRUE)
int_100_009.cum.n.presented[i] <- sum(as.numeric(Mod.int_100_009$epi$n.presented[,i]), na.rm=TRUE)
int_100_009.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_100_009$epi$n.presented.pos[,i]), na.rm=TRUE)
int_100_009.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_100_009$epi$n.presented.neg[,i]), na.rm=TRUE)
int_100_009.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_100_009$epi$missed.pos[,i]), na.rm=TRUE)


##AB only
#int_45_002
int_45_002_ab.cum.incid[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$incid.poi[,i]))
int_45_002_ab.cum.ptar[i] <- sum(Mod.int_45_002_ab$epi$num.poi[,i])-sum(Mod.int_45_002_ab$epi$i.num.poi[,i])
int_45_002_ab.cum.ptinf[i] <- sum(Mod.int_45_002_ab$epi$i.num.poi[,i])
int_45_002_ab.cum.rna.tests[i] <- sum(Mod.int_45_002_ab$epi$n.tests.rna[,i])
int_45_002_ab.cum.ab.tests[i] <- sum(Mod.int_45_002_ab$epi$n.tests.ab[,i])
int_45_002_ab.cum.tests[i] <- sum(Mod.int_45_002_ab$epi$n.tests[,i])
int_45_002_ab.cum.diag[i] <- sum(Mod.int_45_002_ab$epi$diag.prevalent[,i]) + sum(Mod.int_45_002_ab$epi$diag.acute[,i])
int_45_002_ab.cum.diag.p[i] <- sum(Mod.int_45_002_ab$epi$diag.prevalent[,i])
int_45_002_ab.cum.diag.a[i] <- sum(Mod.int_45_002_ab$epi$diag.acute[,i])
int_45_002_ab.cum.partners[i] <- sum(Mod.int_45_002_ab$epi$partners.sought.new[,i])
int_45_002_ab.cum.partners.f[i] <- sum(Mod.int_45_002_ab$epi$partners.found[,i])
int_45_002_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_45_002_ab$epi$n.tests.tst.psositive[,i])
int_45_002_ab.NIA.100K.list[i]<-((noint_256_002.cum.incid[i]-int_45_002_ab.cum.incid[i])/int_45_002_ab.cum.ptar[i])*52*100000
int_45_002_ab.PIA.list[i]<-(noint_256_002.cum.incid[i]-int_45_002_ab.cum.incid[i])/noint_256_002.cum.incid[i] * 100
int_45_002_ab.DPTST.list[i]<-int_45_002_ab.cum.diag[i]/(int_45_002_ab.cum.tests[i] / 1000)
int_45_002_ab.DPTM.list[i]<- (int_45_002_ab.cum.diag[i]/int_45_002_ab.cum.ptinf[i])*52*100
int_45_002_ab.cum.bg.tests[i] <- sum(Mod.int_45_002_ab$epi$n.tests.bg[,i])

int_45_002_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_45_002_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_45_002_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_45_002_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_45_002_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$n.presented[,i]), na.rm=TRUE)
int_45_002_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_45_002_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_45_002_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_45_002_ab$epi$missed.pos[,i]), na.rm=TRUE)


#int_45_006
int_45_006_ab.cum.incid[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$incid.poi[,i]))
int_45_006_ab.cum.ptar[i] <- sum(Mod.int_45_006_ab$epi$num.poi[,i])-sum(Mod.int_45_006_ab$epi$i.num.poi[,i])
int_45_006_ab.cum.ptinf[i] <- sum(Mod.int_45_006_ab$epi$i.num.poi[,i])
int_45_006_ab.cum.rna.tests[i] <- sum(Mod.int_45_006_ab$epi$n.tests.rna[,i])
int_45_006_ab.cum.ab.tests[i] <- sum(Mod.int_45_006_ab$epi$n.tests.ab[,i])
int_45_006_ab.cum.tests[i] <- sum(Mod.int_45_006_ab$epi$n.tests[,i])
int_45_006_ab.cum.diag[i] <- sum(Mod.int_45_006_ab$epi$diag.prevalent[,i]) + sum(Mod.int_45_006_ab$epi$diag.acute[,i])
int_45_006_ab.cum.diag.p[i] <- sum(Mod.int_45_006_ab$epi$diag.prevalent[,i])
int_45_006_ab.cum.diag.a[i] <- sum(Mod.int_45_006_ab$epi$diag.acute[,i])
int_45_006_ab.cum.partners[i] <- sum(Mod.int_45_006_ab$epi$partners.sought.new[,i])
int_45_006_ab.cum.partners.f[i] <- sum(Mod.int_45_006_ab$epi$partners.found[,i])
int_45_006_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_45_006_ab$epi$n.tests.tst.psositive[,i])
int_45_006_ab.NIA.100K.list[i]<-((noint_256_006.cum.incid[i]-int_45_006_ab.cum.incid[i])/int_45_006_ab.cum.ptar[i])*52*100000
int_45_006_ab.PIA.list[i]<-(noint_256_006.cum.incid[i]-int_45_006_ab.cum.incid[i])/noint_256_006.cum.incid[i] * 100
int_45_006_ab.DPTST.list[i]<-int_45_006_ab.cum.diag[i]/(int_45_006_ab.cum.tests[i] / 1000)
int_45_006_ab.DPTM.list[i]<- (int_45_006_ab.cum.diag[i]/int_45_006_ab.cum.ptinf[i])*52*100
int_45_006_ab.cum.bg.tests[i] <- sum(Mod.int_45_006_ab$epi$n.tests.bg[,i])

int_45_006_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_45_006_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_45_006_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_45_006_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_45_006_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$n.presented[,i]), na.rm=TRUE)
int_45_006_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_45_006_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_45_006_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_45_006_ab$epi$missed.pos[,i]), na.rm=TRUE)


#int_45_009
int_45_009_ab.cum.incid[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$incid.poi[,i]))
int_45_009_ab.cum.ptar[i] <- sum(Mod.int_45_009_ab$epi$num.poi[,i])-sum(Mod.int_45_009_ab$epi$i.num.poi[,i])
int_45_009_ab.cum.ptinf[i] <- sum(Mod.int_45_009_ab$epi$i.num.poi[,i])
int_45_009_ab.cum.rna.tests[i] <- sum(Mod.int_45_009_ab$epi$n.tests.rna[,i])
int_45_009_ab.cum.ab.tests[i] <- sum(Mod.int_45_009_ab$epi$n.tests.ab[,i])
int_45_009_ab.cum.tests[i] <- sum(Mod.int_45_009_ab$epi$n.tests[,i])
int_45_009_ab.cum.diag[i] <- sum(Mod.int_45_009_ab$epi$diag.prevalent[,i]) + sum(Mod.int_45_009_ab$epi$diag.acute[,i])
int_45_009_ab.cum.diag.p[i] <- sum(Mod.int_45_009_ab$epi$diag.prevalent[,i])
int_45_009_ab.cum.diag.a[i] <- sum(Mod.int_45_009_ab$epi$diag.acute[,i])
int_45_009_ab.cum.partners[i] <- sum(Mod.int_45_009_ab$epi$partners.sought.new[,i])
int_45_009_ab.cum.partners.f[i] <- sum(Mod.int_45_009_ab$epi$partners.found[,i])
int_45_009_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_45_009_ab$epi$n.tests.tst.psositive[,i])
int_45_009_ab.NIA.100K.list[i]<-((noint_256_009.cum.incid[i]-int_45_009_ab.cum.incid[i])/int_45_009_ab.cum.ptar[i])*52*100000
int_45_009_ab.PIA.list[i]<-(noint_256_009.cum.incid[i]-int_45_009_ab.cum.incid[i])/noint_256_009.cum.incid[i] * 100
int_45_009_ab.DPTST.list[i]<-int_45_009_ab.cum.diag[i]/(int_45_009_ab.cum.tests[i] / 1000)
int_45_009_ab.DPTM.list[i]<- (int_45_009_ab.cum.diag[i]/int_45_009_ab.cum.ptinf[i])*52*100
int_45_009_ab.cum.bg.tests[i] <- sum(Mod.int_45_009_ab$epi$n.tests.bg[,i])

int_45_009_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_45_009_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_45_009_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_45_009_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_45_009_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$n.presented[,i]), na.rm=TRUE)
int_45_009_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_45_009_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_45_009_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_45_009_ab$epi$missed.pos[,i]), na.rm=TRUE)


#int_949_002
int_949_002_ab.cum.incid[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$incid.poi[,i]))
int_949_002_ab.cum.ptar[i] <- sum(Mod.int_949_002_ab$epi$num.poi[,i])-sum(Mod.int_949_002_ab$epi$i.num.poi[,i])
int_949_002_ab.cum.ptinf[i] <- sum(Mod.int_949_002_ab$epi$i.num.poi[,i])
int_949_002_ab.cum.rna.tests[i] <- sum(Mod.int_949_002_ab$epi$n.tests.rna[,i])
int_949_002_ab.cum.ab.tests[i] <- sum(Mod.int_949_002_ab$epi$n.tests.ab[,i])
int_949_002_ab.cum.tests[i] <- sum(Mod.int_949_002_ab$epi$n.tests[,i])
int_949_002_ab.cum.diag[i] <- sum(Mod.int_949_002_ab$epi$diag.prevalent[,i]) + sum(Mod.int_949_002_ab$epi$diag.acute[,i])
int_949_002_ab.cum.diag.p[i] <- sum(Mod.int_949_002_ab$epi$diag.prevalent[,i])
int_949_002_ab.cum.diag.a[i] <- sum(Mod.int_949_002_ab$epi$diag.acute[,i])
int_949_002_ab.cum.partners[i] <- sum(Mod.int_949_002_ab$epi$partners.sought.new[,i])
int_949_002_ab.cum.partners.f[i] <- sum(Mod.int_949_002_ab$epi$partners.found[,i])
int_949_002_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_949_002_ab$epi$n.tests.tst.psositive[,i])
int_949_002_ab.NIA.100K.list[i]<-((noint_256_002.cum.incid[i]-int_949_002_ab.cum.incid[i])/int_949_002_ab.cum.ptar[i])*52*100000
int_949_002_ab.PIA.list[i]<-(noint_256_002.cum.incid[i]-int_949_002_ab.cum.incid[i])/noint_256_002.cum.incid[i] * 100
int_949_002_ab.DPTST.list[i]<-int_949_002_ab.cum.diag[i]/(int_949_002_ab.cum.tests[i] / 1000)
int_949_002_ab.DPTM.list[i]<- (int_949_002_ab.cum.diag[i]/int_949_002_ab.cum.ptinf[i])*52*100
int_949_002_ab.cum.bg.tests[i] <- sum(Mod.int_949_002_ab$epi$n.tests.bg[,i])

int_949_002_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_949_002_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_949_002_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_949_002_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_949_002_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$n.presented[,i]), na.rm=TRUE)
int_949_002_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_949_002_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_949_002_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_949_002_ab$epi$missed.pos[,i]), na.rm=TRUE)


#int_949_006
int_949_006_ab.cum.incid[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$incid.poi[,i]))
int_949_006_ab.cum.ptar[i] <- sum(Mod.int_949_006_ab$epi$num.poi[,i])-sum(Mod.int_949_006_ab$epi$i.num.poi[,i])
int_949_006_ab.cum.ptinf[i] <- sum(Mod.int_949_006_ab$epi$i.num.poi[,i])
int_949_006_ab.cum.rna.tests[i] <- sum(Mod.int_949_006_ab$epi$n.tests.rna[,i])
int_949_006_ab.cum.ab.tests[i] <- sum(Mod.int_949_006_ab$epi$n.tests.ab[,i])
int_949_006_ab.cum.tests[i] <- sum(Mod.int_949_006_ab$epi$n.tests[,i])
int_949_006_ab.cum.diag[i] <- sum(Mod.int_949_006_ab$epi$diag.prevalent[,i]) + sum(Mod.int_949_006_ab$epi$diag.acute[,i])
int_949_006_ab.cum.diag.p[i] <- sum(Mod.int_949_006_ab$epi$diag.prevalent[,i])
int_949_006_ab.cum.diag.a[i] <- sum(Mod.int_949_006_ab$epi$diag.acute[,i])
int_949_006_ab.cum.partners[i] <- sum(Mod.int_949_006_ab$epi$partners.sought.new[,i])
int_949_006_ab.cum.partners.f[i] <- sum(Mod.int_949_006_ab$epi$partners.found[,i])
int_949_006_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_949_006_ab$epi$n.tests.tst.psositive[,i])
int_949_006_ab.NIA.100K.list[i]<-((noint_256_006.cum.incid[i]-int_949_006_ab.cum.incid[i])/int_949_006_ab.cum.ptar[i])*52*100000
int_949_006_ab.PIA.list[i]<-(noint_256_006.cum.incid[i]-int_949_006_ab.cum.incid[i])/noint_256_006.cum.incid[i] * 100
int_949_006_ab.DPTST.list[i]<-int_949_006_ab.cum.diag[i]/(int_949_006_ab.cum.tests[i] / 1000)
int_949_006_ab.DPTM.list[i]<- (int_949_006_ab.cum.diag[i]/int_949_006_ab.cum.ptinf[i])*52*100
int_949_006_ab.cum.bg.tests[i] <- sum(Mod.int_949_006_ab$epi$n.tests.bg[,i])

int_949_006_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_949_006_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_949_006_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_949_006_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_949_006_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$n.presented[,i]), na.rm=TRUE)
int_949_006_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_949_006_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_949_006_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_949_006_ab$epi$missed.pos[,i]), na.rm=TRUE)



#int_949_009
int_949_009_ab.cum.incid[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$incid.poi[,i]))
int_949_009_ab.cum.ptar[i] <- sum(Mod.int_949_009_ab$epi$num.poi[,i])-sum(Mod.int_949_009_ab$epi$i.num.poi[,i])
int_949_009_ab.cum.ptinf[i] <- sum(Mod.int_949_009_ab$epi$i.num.poi[,i])
int_949_009_ab.cum.rna.tests[i] <- sum(Mod.int_949_009_ab$epi$n.tests.rna[,i])
int_949_009_ab.cum.ab.tests[i] <- sum(Mod.int_949_009_ab$epi$n.tests.ab[,i])
int_949_009_ab.cum.tests[i] <- sum(Mod.int_949_009_ab$epi$n.tests[,i])
int_949_009_ab.cum.diag[i] <- sum(Mod.int_949_009_ab$epi$diag.prevalent[,i]) + sum(Mod.int_949_009_ab$epi$diag.acute[,i])
int_949_009_ab.cum.diag.p[i] <- sum(Mod.int_949_009_ab$epi$diag.prevalent[,i])
int_949_009_ab.cum.diag.a[i] <- sum(Mod.int_949_009_ab$epi$diag.acute[,i])
int_949_009_ab.cum.partners[i] <- sum(Mod.int_949_009_ab$epi$partners.sought.new[,i])
int_949_009_ab.cum.partners.f[i] <- sum(Mod.int_949_009_ab$epi$partners.found[,i])
int_949_009_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_949_009_ab$epi$n.tests.tst.psositive[,i])
int_949_009_ab.NIA.100K.list[i]<-((noint_256_009.cum.incid[i]-int_949_009_ab.cum.incid[i])/int_949_009_ab.cum.ptar[i])*52*100000
int_949_009_ab.PIA.list[i]<-(noint_256_009.cum.incid[i]-int_949_009_ab.cum.incid[i])/noint_256_009.cum.incid[i] * 100
int_949_009_ab.DPTST.list[i]<-int_949_009_ab.cum.diag[i]/(int_949_009_ab.cum.tests[i] / 1000)
int_949_009_ab.DPTM.list[i]<- (int_949_009_ab.cum.diag[i]/int_949_009_ab.cum.ptinf[i])*52*100
int_949_009_ab.cum.bg.tests[i] <- sum(Mod.int_949_009_ab$epi$n.tests.bg[,i])

int_949_009_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_949_009_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_949_009_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_949_009_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_949_009_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$n.presented[,i]), na.rm=TRUE)
int_949_009_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_949_009_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_949_009_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_949_009_ab$epi$missed.pos[,i]), na.rm=TRUE)

#int_100_002
int_100_002_ab.cum.incid[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$incid.poi[,i]))
int_100_002_ab.cum.ptar[i] <- sum(Mod.int_100_002_ab$epi$num.poi[,i])-sum(Mod.int_100_002_ab$epi$i.num.poi[,i])
int_100_002_ab.cum.ptinf[i] <- sum(Mod.int_100_002_ab$epi$i.num.poi[,i])
int_100_002_ab.cum.rna.tests[i] <- sum(Mod.int_100_002_ab$epi$n.tests.rna[,i])
int_100_002_ab.cum.ab.tests[i] <- sum(Mod.int_100_002_ab$epi$n.tests.ab[,i])
int_100_002_ab.cum.tests[i] <- sum(Mod.int_100_002_ab$epi$n.tests[,i])
int_100_002_ab.cum.diag[i] <- sum(Mod.int_100_002_ab$epi$diag.prevalent[,i]) + sum(Mod.int_100_002_ab$epi$diag.acute[,i])
int_100_002_ab.cum.diag.p[i] <- sum(Mod.int_100_002_ab$epi$diag.prevalent[,i])
int_100_002_ab.cum.diag.a[i] <- sum(Mod.int_100_002_ab$epi$diag.acute[,i])
int_100_002_ab.cum.partners[i] <- sum(Mod.int_100_002_ab$epi$partners.sought.new[,i])
int_100_002_ab.cum.partners.f[i] <- sum(Mod.int_100_002_ab$epi$partners.found[,i])
int_100_002_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_100_002_ab$epi$n.tests.tst.psositive[,i])
int_100_002_ab.NIA.100K.list[i]<-((noint_256_002.cum.incid[i]-int_100_002_ab.cum.incid[i])/int_100_002_ab.cum.ptar[i])*52*100000
int_100_002_ab.PIA.list[i]<-(noint_256_002.cum.incid[i]-int_100_002_ab.cum.incid[i])/noint_256_002.cum.incid[i] * 100
int_100_002_ab.DPTST.list[i]<-int_100_002_ab.cum.diag[i]/(int_100_002_ab.cum.tests[i] / 1000)
int_100_002_ab.DPTM.list[i]<- (int_100_002_ab.cum.diag[i]/int_100_002_ab.cum.ptinf[i])*52*100
int_100_002_ab.cum.bg.tests[i] <- sum(Mod.int_100_002_ab$epi$n.tests.bg[,i])

int_100_002_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_100_002_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_100_002_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_100_002_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_100_002_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$n.presented[,i]), na.rm=TRUE)
int_100_002_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_100_002_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_100_002_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_100_002_ab$epi$missed.pos[,i]), na.rm=TRUE)

#int_100_006
int_100_006_ab.cum.incid[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$incid.poi[,i]))
int_100_006_ab.cum.ptar[i] <- sum(Mod.int_100_006_ab$epi$num.poi[,i])-sum(Mod.int_100_006_ab$epi$i.num.poi[,i])
int_100_006_ab.cum.ptinf[i] <- sum(Mod.int_100_006_ab$epi$i.num.poi[,i])
int_100_006_ab.cum.rna.tests[i] <- sum(Mod.int_100_006_ab$epi$n.tests.rna[,i])
int_100_006_ab.cum.ab.tests[i] <- sum(Mod.int_100_006_ab$epi$n.tests.ab[,i])
int_100_006_ab.cum.tests[i] <- sum(Mod.int_100_006_ab$epi$n.tests[,i])
int_100_006_ab.cum.diag[i] <- sum(Mod.int_100_006_ab$epi$diag.prevalent[,i]) + sum(Mod.int_100_006_ab$epi$diag.acute[,i])
int_100_006_ab.cum.diag.p[i] <- sum(Mod.int_100_006_ab$epi$diag.prevalent[,i])
int_100_006_ab.cum.diag.a[i] <- sum(Mod.int_100_006_ab$epi$diag.acute[,i])
int_100_006_ab.cum.partners[i] <- sum(Mod.int_100_006_ab$epi$partners.sought.new[,i])
int_100_006_ab.cum.partners.f[i] <- sum(Mod.int_100_006_ab$epi$partners.found[,i])
int_100_006_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_100_006_ab$epi$n.tests.tst.psositive[,i])
int_100_006_ab.NIA.100K.list[i]<-((noint_256_006.cum.incid[i]-int_100_006_ab.cum.incid[i])/int_100_006_ab.cum.ptar[i])*52*100000
int_100_006_ab.PIA.list[i]<-(noint_256_006.cum.incid[i]-int_100_006_ab.cum.incid[i])/noint_256_006.cum.incid[i] * 100
int_100_006_ab.DPTST.list[i]<-int_100_006_ab.cum.diag[i]/(int_100_006_ab.cum.tests[i] / 1000)
int_100_006_ab.DPTM.list[i]<- (int_100_006_ab.cum.diag[i]/int_100_006_ab.cum.ptinf[i])*52*100
int_100_006_ab.cum.bg.tests[i] <- sum(Mod.int_100_006_ab$epi$n.tests.bg[,i])

int_100_006_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_100_006_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_100_006_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_100_006_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_100_006_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$n.presented[,i]), na.rm=TRUE)
int_100_006_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_100_006_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_100_006_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_100_006_ab$epi$missed.pos[,i]), na.rm=TRUE)

#int_100_009
int_100_009_ab.cum.incid[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$incid.poi[,i]))
int_100_009_ab.cum.ptar[i] <- sum(Mod.int_100_009_ab$epi$num.poi[,i])-sum(Mod.int_100_009_ab$epi$i.num.poi[,i])
int_100_009_ab.cum.ptinf[i] <- sum(Mod.int_100_009_ab$epi$i.num.poi[,i])
int_100_009_ab.cum.rna.tests[i] <- sum(Mod.int_100_009_ab$epi$n.tests.rna[,i])
int_100_009_ab.cum.ab.tests[i] <- sum(Mod.int_100_009_ab$epi$n.tests.ab[,i])
int_100_009_ab.cum.tests[i] <- sum(Mod.int_100_009_ab$epi$n.tests[,i])
int_100_009_ab.cum.diag[i] <- sum(Mod.int_100_009_ab$epi$diag.prevalent[,i]) + sum(Mod.int_100_009_ab$epi$diag.acute[,i])
int_100_009_ab.cum.diag.p[i] <- sum(Mod.int_100_009_ab$epi$diag.prevalent[,i])
int_100_009_ab.cum.diag.a[i] <- sum(Mod.int_100_009_ab$epi$diag.acute[,i])
int_100_009_ab.cum.partners[i] <- sum(Mod.int_100_009_ab$epi$partners.sought.new[,i])
int_100_009_ab.cum.partners.f[i] <- sum(Mod.int_100_009_ab$epi$partners.found[,i])
int_100_009_ab.cum.n.tests.tst.ps[i] <- sum(Mod.int_100_009_ab$epi$n.tests.tst.psositive[,i])
int_100_009_ab.NIA.100K.list[i]<-((noint_256_009.cum.incid[i]-int_100_009_ab.cum.incid[i])/int_100_009_ab.cum.ptar[i])*52*100000
int_100_009_ab.PIA.list[i]<-(noint_256_009.cum.incid[i]-int_100_009_ab.cum.incid[i])/noint_256_009.cum.incid[i] * 100
int_100_009_ab.DPTST.list[i]<-int_100_009_ab.cum.diag[i]/(int_100_009_ab.cum.tests[i] / 1000)
int_100_009_ab.DPTM.list[i]<- (int_100_009_ab.cum.diag[i]/int_100_009_ab.cum.ptinf[i])*52*100
int_100_009_ab.cum.bg.tests[i] <- sum(Mod.int_100_009_ab$epi$n.tests.bg[,i])

int_100_009_ab.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$n.false.pos[,i]),na.rm=TRUE)
int_100_009_ab.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$n.false.neg[,i]),na.rm=TRUE)
int_100_009_ab.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$n.true.pos[,i]),na.rm=TRUE)
int_100_009_ab.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$n.true.neg[,i]),na.rm=TRUE)
int_100_009_ab.cum.n.presented[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$n.presented[,i]), na.rm=TRUE)
int_100_009_ab.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$n.presented.pos[,i]), na.rm=TRUE)
int_100_009_ab.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$n.presented.neg[,i]), na.rm=TRUE)
int_100_009_ab.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_100_009_ab$epi$missed.pos[,i]), na.rm=TRUE)


##LOWER AHI SEEKING PROBABILITY

noint_256_006_l.cum.incid[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$incid.poi[,i]))
noint_256_006_l.cum.ptar[i] <- sum(Mod.noint_256_006_l$epi$num.poi[,i])-sum(Mod.noint_256_006_l$epi$i.num.poi[,i])
noint_256_006_l.cum.ptinf[i] <- sum(Mod.noint_256_006_l$epi$i.num.poi[,i])
noint_256_006_l.cum.rna.tests[i] <- sum(Mod.noint_256_006_l$epi$n.tests.rna[,i])
noint_256_006_l.cum.ab.tests[i] <- sum(Mod.noint_256_006_l$epi$n.tests.ab[,i])
noint_256_006_l.cum.tests[i] <- sum(Mod.noint_256_006_l$epi$n.tests[,i])
noint_256_006_l.cum.diag[i] <- sum(Mod.noint_256_006_l$epi$diag.prevalent[,i]) + sum(Mod.noint_256_006_l$epi$diag.acute[,i])
noint_256_006_l.cum.diag.p[i] <- sum(Mod.noint_256_006_l$epi$diag.prevalent[,i])
noint_256_006_l.cum.diag.a[i] <- sum(Mod.noint_256_006_l$epi$diag.acute[,i])
noint_256_006_l.cum.partners[i] <- sum(Mod.noint_256_006_l$epi$partners.sought.new[,i])
noint_256_006_l.cum.partners.f[i] <- sum(Mod.noint_256_006_l$epi$partners.found[,i])
noint_256_006_l.cum.n.tests.tst.ps[i] <- sum(Mod.noint_256_006_l$epi$n.tests.tst.psositive[,i])
noint_256_006_l.NIA.100K.list[i] <- NA
noint_256_006_l.PIA.list[i]<- NA
noint_256_006_l.DPTST.list[i]<-noint_256_006_l.cum.diag[i]/(noint_256_006_l.cum.tests[i] / 1000)
noint_256_006_l.DPTM.list[i]<- (noint_256_006_l.cum.diag[i]/noint_256_006_l.cum.ptinf[i])*52*100
noint_256_006_l.cum.bg.tests[i] <- sum(Mod.noint_256_006_l$epi$n.tests.bg[,i])

noint_256_006_l.cum.n.false.pos[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$n.false.pos[,i]),na.rm=TRUE)
noint_256_006_l.cum.n.false.neg[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$n.false.neg[,i]),na.rm=TRUE)
noint_256_006_l.cum.n.true.pos[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$n.true.pos[,i]),na.rm=TRUE)
noint_256_006_l.cum.n.true.neg[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$n.true.neg[,i]),na.rm=TRUE)
noint_256_006_l.cum.n.presented[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$n.presented[,i]), na.rm=TRUE)
noint_256_006_l.cum.n.presented.pos[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$n.presented.pos[,i]), na.rm=TRUE)
noint_256_006_l.cum.n.presented.neg[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$n.presented.neg[,i]), na.rm=TRUE)
noint_256_006_l.cum.n.missed.pos[i] <- sum(as.numeric(Mod.noint_256_006_l$epi$missed.pos[,i]), na.rm=TRUE)


int_256_006_l.cum.incid[i] <- sum(as.numeric(Mod.int_256_006_l$epi$incid.poi[,i]))
int_256_006_l.cum.ptar[i] <- sum(Mod.int_256_006_l$epi$num.poi[,i])-sum(Mod.int_256_006_l$epi$i.num.poi[,i])
int_256_006_l.cum.ptinf[i] <- sum(Mod.int_256_006_l$epi$i.num.poi[,i])
int_256_006_l.cum.rna.tests[i] <- sum(Mod.int_256_006_l$epi$n.tests.rna[,i])
int_256_006_l.cum.ab.tests[i] <- sum(Mod.int_256_006_l$epi$n.tests.ab[,i])
int_256_006_l.cum.tests[i] <- sum(Mod.int_256_006_l$epi$n.tests[,i])
int_256_006_l.cum.diag[i] <- sum(Mod.int_256_006_l$epi$diag.prevalent[,i]) + sum(Mod.int_256_006_l$epi$diag.acute[,i])
int_256_006_l.cum.diag.p[i] <- sum(Mod.int_256_006_l$epi$diag.prevalent[,i])
int_256_006_l.cum.diag.a[i] <- sum(Mod.int_256_006_l$epi$diag.acute[,i])
int_256_006_l.cum.partners[i] <- sum(Mod.int_256_006_l$epi$partners.sought.new[,i])
int_256_006_l.cum.partners.f[i] <- sum(Mod.int_256_006_l$epi$partners.found[,i])
int_256_006_l.cum.n.tests.tst.ps[i] <- sum(Mod.int_256_006_l$epi$n.tests.tst.psositive[,i])
int_256_006_l.NIA.100K.list[i]<-((noint_256_006_l.cum.incid[i]-int_256_006_l.cum.incid[i])/int_256_006_l.cum.ptar[i])*52*100000
int_256_006_l.PIA.list[i]<-(noint_256_006_l.cum.incid[i]-int_256_006_l.cum.incid[i])/noint_256_006_l.cum.incid[i] * 100
int_256_006_l.DPTST.list[i]<-int_256_006_l.cum.diag[i]/(int_256_006_l.cum.tests[i] / 1000)
int_256_006_l.DPTM.list[i]<- (int_256_006_l.cum.diag[i]/int_256_006_l.cum.ptinf[i])*52*100
int_256_006_l.cum.bg.tests[i] <- sum(Mod.int_256_006_l$epi$n.tests.bg[,i])

int_256_006_l.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_256_006_l$epi$n.false.pos[,i]),na.rm=TRUE)
int_256_006_l.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_256_006_l$epi$n.false.neg[,i]),na.rm=TRUE)
int_256_006_l.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_256_006_l$epi$n.true.pos[,i]),na.rm=TRUE)
int_256_006_l.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_256_006_l$epi$n.true.neg[,i]),na.rm=TRUE)
int_256_006_l.cum.n.presented[i] <- sum(as.numeric(Mod.int_256_006_l$epi$n.presented[,i]), na.rm=TRUE)
int_256_006_l.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_256_006_l$epi$n.presented.pos[,i]), na.rm=TRUE)
int_256_006_l.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_256_006_l$epi$n.presented.neg[,i]), na.rm=TRUE)
int_256_006_l.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_256_006_l$epi$missed.pos[,i]), na.rm=TRUE)


int_45_006_l.cum.incid[i] <- sum(as.numeric(Mod.int_45_006_l$epi$incid.poi[,i]))
int_45_006_l.cum.ptar[i] <- sum(Mod.int_45_006_l$epi$num.poi[,i])-sum(Mod.int_45_006_l$epi$i.num.poi[,i])
int_45_006_l.cum.ptinf[i] <- sum(Mod.int_45_006_l$epi$i.num.poi[,i])
int_45_006_l.cum.rna.tests[i] <- sum(Mod.int_45_006_l$epi$n.tests.rna[,i])
int_45_006_l.cum.ab.tests[i] <- sum(Mod.int_45_006_l$epi$n.tests.ab[,i])
int_45_006_l.cum.tests[i] <- sum(Mod.int_45_006_l$epi$n.tests[,i])
int_45_006_l.cum.diag[i] <- sum(Mod.int_45_006_l$epi$diag.prevalent[,i]) + sum(Mod.int_45_006_l$epi$diag.acute[,i])
int_45_006_l.cum.diag.p[i] <- sum(Mod.int_45_006_l$epi$diag.prevalent[,i])
int_45_006_l.cum.diag.a[i] <- sum(Mod.int_45_006_l$epi$diag.acute[,i])
int_45_006_l.cum.partners[i] <- sum(Mod.int_45_006_l$epi$partners.sought.new[,i])
int_45_006_l.cum.partners.f[i] <- sum(Mod.int_45_006_l$epi$partners.found[,i])
int_45_006_l.cum.n.tests.tst.ps[i] <- sum(Mod.int_45_006_l$epi$n.tests.tst.psositive[,i])
int_45_006_l.NIA.100K.list[i]<-((noint_256_006_l.cum.incid[i]-int_45_006_l.cum.incid[i])/int_45_006_l.cum.ptar[i])*52*100000
int_45_006_l.PIA.list[i]<-(noint_256_006_l.cum.incid[i]-int_45_006_l.cum.incid[i])/noint_256_006_l.cum.incid[i] * 100
int_45_006_l.DPTST.list[i]<-int_45_006_l.cum.diag[i]/(int_45_006_l.cum.tests[i] / 1000)
int_45_006_l.DPTM.list[i]<- (int_45_006_l.cum.diag[i]/int_45_006_l.cum.ptinf[i])*52*100
int_45_006_l.cum.bg.tests[i] <- sum(Mod.int_45_006_l$epi$n.tests.bg[,i])

int_45_006_l.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_45_006_l$epi$n.false.pos[,i]),na.rm=TRUE)
int_45_006_l.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_45_006_l$epi$n.false.neg[,i]),na.rm=TRUE)
int_45_006_l.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_45_006_l$epi$n.true.pos[,i]),na.rm=TRUE)
int_45_006_l.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_45_006_l$epi$n.true.neg[,i]),na.rm=TRUE)
int_45_006_l.cum.n.presented[i] <- sum(as.numeric(Mod.int_45_006_l$epi$n.presented[,i]), na.rm=TRUE)
int_45_006_l.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_45_006_l$epi$n.presented.pos[,i]), na.rm=TRUE)
int_45_006_l.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_45_006_l$epi$n.presented.neg[,i]), na.rm=TRUE)
int_45_006_l.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_45_006_l$epi$missed.pos[,i]), na.rm=TRUE)


int_949_006_l.cum.incid[i] <- sum(as.numeric(Mod.int_949_006_l$epi$incid.poi[,i]))
int_949_006_l.cum.ptar[i] <- sum(Mod.int_949_006_l$epi$num.poi[,i])-sum(Mod.int_949_006_l$epi$i.num.poi[,i])
int_949_006_l.cum.ptinf[i] <- sum(Mod.int_949_006_l$epi$i.num.poi[,i])
int_949_006_l.cum.rna.tests[i] <- sum(Mod.int_949_006_l$epi$n.tests.rna[,i])
int_949_006_l.cum.ab.tests[i] <- sum(Mod.int_949_006_l$epi$n.tests.ab[,i])
int_949_006_l.cum.tests[i] <- sum(Mod.int_949_006_l$epi$n.tests[,i])
int_949_006_l.cum.diag[i] <- sum(Mod.int_949_006_l$epi$diag.prevalent[,i]) + sum(Mod.int_949_006_l$epi$diag.acute[,i])
int_949_006_l.cum.diag.p[i] <- sum(Mod.int_949_006_l$epi$diag.prevalent[,i])
int_949_006_l.cum.diag.a[i] <- sum(Mod.int_949_006_l$epi$diag.acute[,i])
int_949_006_l.cum.partners[i] <- sum(Mod.int_949_006_l$epi$partners.sought.new[,i])
int_949_006_l.cum.partners.f[i] <- sum(Mod.int_949_006_l$epi$partners.found[,i])
int_949_006_l.cum.n.tests.tst.ps[i] <- sum(Mod.int_949_006_l$epi$n.tests.tst.psositive[,i])
int_949_006_l.NIA.100K.list[i]<-((noint_256_006_l.cum.incid[i]-int_949_006_l.cum.incid[i])/int_949_006_l.cum.ptar[i])*52*100000
int_949_006_l.PIA.list[i]<-(noint_256_006_l.cum.incid[i]-int_949_006_l.cum.incid[i])/noint_256_006_l.cum.incid[i] * 100
int_949_006_l.DPTST.list[i]<-int_949_006_l.cum.diag[i]/(int_949_006_l.cum.tests[i] / 1000)
int_949_006_l.DPTM.list[i]<- (int_949_006_l.cum.diag[i]/int_949_006_l.cum.ptinf[i])*52*100
int_949_006_l.cum.bg.tests[i] <- sum(Mod.int_949_006_l$epi$n.tests.bg[,i])

int_949_006_l.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_949_006_l$epi$n.false.pos[,i]),na.rm=TRUE)
int_949_006_l.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_949_006_l$epi$n.false.neg[,i]),na.rm=TRUE)
int_949_006_l.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_949_006_l$epi$n.true.pos[,i]),na.rm=TRUE)
int_949_006_l.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_949_006_l$epi$n.true.neg[,i]),na.rm=TRUE)
int_949_006_l.cum.n.presented[i] <- sum(as.numeric(Mod.int_949_006_l$epi$n.presented[,i]), na.rm=TRUE)
int_949_006_l.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_949_006_l$epi$n.presented.pos[,i]), na.rm=TRUE)
int_949_006_l.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_949_006_l$epi$n.presented.neg[,i]), na.rm=TRUE)
int_949_006_l.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_949_006_l$epi$missed.pos[,i]), na.rm=TRUE)


int_100_006_l.cum.incid[i] <- sum(as.numeric(Mod.int_100_006_l$epi$incid.poi[,i]))
int_100_006_l.cum.ptar[i] <- sum(Mod.int_100_006_l$epi$num.poi[,i])-sum(Mod.int_100_006_l$epi$i.num.poi[,i])
int_100_006_l.cum.ptinf[i] <- sum(Mod.int_100_006_l$epi$i.num.poi[,i])
int_100_006_l.cum.rna.tests[i] <- sum(Mod.int_100_006_l$epi$n.tests.rna[,i])
int_100_006_l.cum.ab.tests[i] <- sum(Mod.int_100_006_l$epi$n.tests.ab[,i])
int_100_006_l.cum.tests[i] <- sum(Mod.int_100_006_l$epi$n.tests[,i])
int_100_006_l.cum.diag[i] <- sum(Mod.int_100_006_l$epi$diag.prevalent[,i]) + sum(Mod.int_100_006_l$epi$diag.acute[,i])
int_100_006_l.cum.diag.p[i] <- sum(Mod.int_100_006_l$epi$diag.prevalent[,i])
int_100_006_l.cum.diag.a[i] <- sum(Mod.int_100_006_l$epi$diag.acute[,i])
int_100_006_l.cum.partners[i] <- sum(Mod.int_100_006_l$epi$partners.sought.new[,i])
int_100_006_l.cum.partners.f[i] <- sum(Mod.int_100_006_l$epi$partners.found[,i])
int_100_006_l.cum.n.tests.tst.ps[i] <- sum(Mod.int_100_006_l$epi$n.tests.tst.psositive[,i])
int_100_006_l.NIA.100K.list[i]<-((noint_256_006_l.cum.incid[i]-int_100_006_l.cum.incid[i])/int_100_006_l.cum.ptar[i])*52*100000
int_100_006_l.PIA.list[i]<-(noint_256_006_l.cum.incid[i]-int_100_006_l.cum.incid[i])/noint_256_006_l.cum.incid[i] * 100
int_100_006_l.DPTST.list[i]<-int_100_006_l.cum.diag[i]/(int_100_006_l.cum.tests[i] / 1000)
int_100_006_l.DPTM.list[i]<- (int_100_006_l.cum.diag[i]/int_100_006_l.cum.ptinf[i])*52*100
int_100_006_l.cum.bg.tests[i] <- sum(Mod.int_100_006_l$epi$n.tests.bg[,i])

int_100_006_l.cum.n.false.pos[i] <- sum(as.numeric(Mod.int_100_006_l$epi$n.false.pos[,i]),na.rm=TRUE)
int_100_006_l.cum.n.false.neg[i] <- sum(as.numeric(Mod.int_100_006_l$epi$n.false.neg[,i]),na.rm=TRUE)
int_100_006_l.cum.n.true.pos[i] <- sum(as.numeric(Mod.int_100_006_l$epi$n.true.pos[,i]),na.rm=TRUE)
int_100_006_l.cum.n.true.neg[i] <- sum(as.numeric(Mod.int_100_006_l$epi$n.true.neg[,i]),na.rm=TRUE)
int_100_006_l.cum.n.presented[i] <- sum(as.numeric(Mod.int_100_006_l$epi$n.presented[,i]), na.rm=TRUE)
int_100_006_l.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int_100_006_l$epi$n.presented.pos[,i]), na.rm=TRUE)
int_100_006_l.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int_100_006_l$epi$n.presented.neg[,i]), na.rm=TRUE)
int_100_006_l.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int_100_006_l$epi$missed.pos[,i]), na.rm=TRUE)

}



for (i in 1:length(scenarios)){

  #CUM INCID
    fn <- paste0(scenarios[i], ".cum.incid")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.incid", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.incid", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.incid", ".mean_low")
    assign(fn,x[low.cut])
    
    
    fn <- paste0(scenarios[i], ".cum.ptar")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.ptar", ".mean")
    assign(fn,mean(x))
  
    #cum.rna.tests
    fn <- paste0(scenarios[i], ".cum.rna.tests")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.rna.tests", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.rna.tests", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.rna.tests", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.ab.tests
    fn <- paste0(scenarios[i], ".cum.ab.tests")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.ab.tests", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.ab.tests", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.ab.tests", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.tests
    fn <- paste0(scenarios[i], ".cum.tests")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.tests", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.tests", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.tests", ".mean_low")
    assign(fn,x[low.cut])
    
#cum.diag
    fn <- paste0(scenarios[i], ".cum.diag")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.diag", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.diag", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.diag", ".mean_low")
    assign(fn,x[low.cut])
    
#cum.diag.p
    fn <- paste0(scenarios[i], ".cum.diag.p")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.diag.p", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.diag.p", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.diag.p", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.diag.a
    fn <- paste0(scenarios[i], ".cum.diag.a")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.diag.a", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.diag.a", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.diag.a", ".mean_low")
    assign(fn,x[low.cut])
    
#cum.partners
    fn <- paste0(scenarios[i], ".cum.partners")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.partners", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.partners", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.partners", ".mean_low")
    assign(fn,x[low.cut])
    
#cum.partners.f
    fn <- paste0(scenarios[i], ".cum.partners.f")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.partners.f", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.partners.f", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.partners.f", ".mean_low")
    assign(fn,x[low.cut])
   
#cum.n.tests.tst.ps
    fn <- paste0(scenarios[i], ".cum.n.tests.tst.ps")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.tests.tst.ps", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.tests.tst.ps", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.tests.tst.ps", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.bg.tests
    fn <- paste0(scenarios[i], ".cum.bg.tests")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.bg.tests", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.bg.tests", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.bg.tests", ".mean_low")
    assign(fn,x[low.cut])
    
##########################################
    #cum.n.false.pos
    fn <- paste0(scenarios[i], ".cum.n.false.pos")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.false.pos", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.false.pos", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.false.pos", ".mean_low")
    assign(fn,x[low.cut])
    
    
    #cum.n.false.neg
    fn <- paste0(scenarios[i], ".cum.n.false.neg")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.false.neg", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.false.neg", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.false.neg", ".mean_low")
    assign(fn,x[low.cut])
    
    
    #cum.n.true.pos
    fn <- paste0(scenarios[i], ".cum.n.true.pos")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.true.pos", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.true.pos", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.true.pos", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.n.true.neg
    fn <- paste0(scenarios[i], ".cum.n.true.neg")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.true.neg", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.true.neg", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.true.neg", ".mean_low")
    assign(fn,x[low.cut])
    
    
    #cum.n.presented
    fn <- paste0(scenarios[i], ".cum.n.presented")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.presented", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.presented", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.presented", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.n.presented.pos
    fn <- paste0(scenarios[i], ".cum.n.presented.pos")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.presented.pos", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.presented.pos", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.presented.pos", ".mean_low")
    assign(fn,x[low.cut])
    

    #cum.n.presented.neg
    fn <- paste0(scenarios[i], ".cum.n.presented.neg")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.presented.neg", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.presented.neg", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.presented.neg", ".mean_low")
    assign(fn,x[low.cut])
    
    
    #cum.n.missed.pos
    fn <- paste0(scenarios[i], ".cum.n.missed.pos")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(scenarios[i], ".cum.n.missed.pos", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(scenarios[i], ".cum.n.missed.pos", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(scenarios[i], ".cum.n.missed.pos", ".mean_low")
    assign(fn,x[low.cut])
    

}

#MAKE NIA / 10Kpt and PIA

for (i in 1:length(scenarios)){
  

  ##NIA
  fn <- paste0(scenarios[i], ".NIA.100K")
  fn2 <- paste0(scenarios[i], ".NIA.100K.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,mean(x))
  
  fn <- paste0(scenarios[i], ".NIA.100K_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(scenarios[i], ".NIA.100K_low")
  assign(fn,x[low.cut])
  
  ##PIA
  fn <- paste0(scenarios[i], ".PIA")
  fn2 <- paste0(scenarios[i], ".PIA.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,mean(x))
  
  fn <- paste0(scenarios[i], ".PIA_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(scenarios[i], ".PIA_low")
  assign(fn,x[low.cut])
  
  ##DPTST
  fn <- paste0(scenarios[i], ".DPTST")
  fn2 <- paste0(scenarios[i], ".DPTST.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,mean(x))
  
  fn <- paste0(scenarios[i], ".DPTST_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(scenarios[i], ".DPTST_low")
  assign(fn,x[low.cut])
  
  ##DPTM
  fn <- paste0(scenarios[i], ".DPTM")
  fn2 <- paste0(scenarios[i], ".DPTM.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,mean(x))
  
  fn <- paste0(scenarios[i], ".DPTM_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(scenarios[i], ".DPTM_low")
  assign(fn,x[low.cut])
}


#TABLE of point estimates

incid.mean <- incid.mean.hi <- incid.mean.low  <-ptar.mean <- 
rna.tests.mean <- rna.tests.mean.hi <- rna.tests.mean.low <- ab.tests.mean <- ab.tests.mean.hi <- ab.tests.mean.low <-
tests.mean <- tests.mean.hi <- tests.mean.low <-
diag.mean <- diag.mean.hi <- diag.mean.low <- diag.p.mean <- diag.p.mean.hi <- diag.p.mean.low <-
diag.a.mean <- diag.a.mean.hi <- diag.a.mean.low <-
partners.mean <- partners.mean.hi <- partners.mean.low <-
partners.f.mean <- partners.f.mean.hi <- partners.f.mean.low <-
n.tests.tst.ps.mean <- n.tests.tst.ps.mean.hi <- n.tests.tst.ps.mean.low <-
NIA  <-NIA.low  <-NIA.hi <- 
PIA <- PIA.low <- PIA.hi <-
DPTST <- DPTST.low <- DPTST.hi <-
DPTM <- DPTM.low <- DPTM.hi <-
Prev <- Prev.low <- Prev.hi <-rep(NA,length(scenarios))
ir100 <- ir100.low <- ir100.hi <-rep(NA,length(scenarios))
bg.tests.mean <- bg.tests.mean.hi <- bg.tests.mean.low <-
n.false.pos.mean <-n.false.pos.mean.low <-n.false.pos.mean.hi <-
n.false.neg.mean <- n.false.neg.mean.low <- n.false.neg.mean.hi <-
n.true.pos.mean <- n.true.pos.mean.low <- n.true.pos.mean.hi <-
n.true.neg.mean <- n.true.neg.mean.low <- n.true.neg.mean.hi <-
n.presented.mean <- n.presented.mean.low <- n.presented.mean.hi <-
n.presented.pos.mean <- n.presented.pos.mean.low <- n.presented.pos.mean.hi <-
n.presented.neg.mean <- n.presented.neg.mean.low <- n.presented.neg.mean.hi <- rep(NA,length(scenarios))
n.missed.pos.mean <- n.missed.pos.mean.low <- n.missed.pos.mean.hi <- rep(NA,length(scenarios))

for (i in 1:length(scenarios)){
  
 
  fn <- paste0(scenarios[i], ".cum.incid.mean")
  incid.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.incid.mean_hi")
  incid.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.incid.mean_low")
  incid.mean.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.ptar.mean")
  ptar.mean[i] <- get(fn)
  

  fn <- paste0(scenarios[i], ".cum.rna.tests.mean")
  rna.tests.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.rna.tests.mean_hi")
  rna.tests.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.rna.tests.mean_low")
  rna.tests.mean.low[i] <- get(fn)
  
 
   fn <- paste0(scenarios[i], ".cum.ab.tests.mean")
  ab.tests.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.ab.tests.mean_hi")
  ab.tests.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.ab.tests.mean_low")
  ab.tests.mean.low[i] <- get(fn)
  
  
  fn <- paste0(scenarios[i], ".cum.tests.mean")
  tests.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.tests.mean_hi")
  tests.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.tests.mean_low")
  tests.mean.low[i] <- get(fn)
  
  
  fn <- paste0(scenarios[i], ".cum.diag.mean")
  diag.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.diag.mean_hi")
  diag.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.diag.mean_low")
  diag.mean.low[i] <- get(fn)
  
  
  fn <- paste0(scenarios[i], ".cum.diag.p.mean")
  diag.p.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.diag.p.mean_hi")
  diag.p.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.diag.p.mean_low")
  diag.p.mean.low[i] <- get(fn)

  
  fn <- paste0(scenarios[i], ".cum.diag.a.mean")
  diag.a.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.diag.a.mean_hi")
  diag.a.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.diag.a.mean_low")
  diag.a.mean.low[i] <- get(fn)
  
  
  fn <- paste0(scenarios[i], ".cum.partners.mean")
  partners.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.partners.mean_hi")
  partners.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.partners.mean_low")
  partners.mean.low[i] <- get(fn)
  
  
  fn <- paste0(scenarios[i], ".cum.partners.f.mean")
  partners.f.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.partners.f.mean_hi")
  partners.f.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.partners.f.mean_low")
  partners.f.mean.low[i] <- get(fn)

  
  fn <- paste0(scenarios[i], ".cum.n.tests.tst.ps.mean")
  n.tests.tst.ps.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.tests.tst.ps.mean_hi")
  n.tests.tst.ps.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.tests.tst.ps.mean_low")
  n.tests.tst.ps.mean.low[i] <- get(fn)
    

  fn <- paste0(scenarios[i], ".NIA.100K")
  NIA[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".NIA.100K_low")
  NIA.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".NIA.100K_hi")
  NIA.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".PIA")
  PIA[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".PIA_low")
  PIA.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".PIA_hi")
  PIA.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".DPTST")
  DPTST[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".DPTST_low")
  DPTST.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".DPTST_hi")
  DPTST.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".DPTM")
  DPTM[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".DPTM_low")
  DPTM.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".DPTM_hi")
  DPTM.hi[i] <- get(fn)
  
  
  fn <- paste0(scenarios[i], ".prev")
  x  <- get(fn) 
  Prev[i] <- tail(x,1)
  
  fn <- paste0(scenarios[i], ".prev_low")
  x  <- get(fn) 
  Prev.low[i] <- tail(x,1)
  
  fn <- paste0(scenarios[i], ".prev_hi")
  x  <- get(fn) 
  Prev.hi[i] <- tail(x,1)
  
  fn <- paste0(scenarios[i], ".ir100")
  x  <- get(fn) 
  ir100[i] <- tail(x,1)
  
  fn <- paste0(scenarios[i], ".ir100_low")
  x  <- get(fn) 
  ir100.low[i] <- tail(x,1)
  
  fn <- paste0(scenarios[i], ".ir100_hi")
  x  <- get(fn) 
  ir100.hi[i] <- tail(x,1)
  
  fn <- paste0(scenarios[i], ".cum.bg.tests.mean")
  bg.tests.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.bg.tests.mean_hi")
  bg.tests.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.bg.tests.mean_low")
  bg.tests.mean.low[i] <- get(fn)
  
 
  fn <- paste0(scenarios[i], ".cum.n.false.pos.mean")
  n.false.pos.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.false.pos.mean_hi")
  n.false.pos.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.false.pos.mean_low")
  n.false.pos.mean.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.false.neg.mean")
  n.false.neg.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.false.neg.mean_hi")
  n.false.neg.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.false.neg.mean_low")
  n.false.neg.mean.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.true.pos.mean")
  n.true.pos.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.true.pos.mean_hi")
  n.true.pos.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.true.pos.mean_low")
  n.true.pos.mean.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.true.neg.mean")
  n.true.neg.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.true.neg.mean_hi")
  n.true.neg.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.true.neg.mean_low")
  n.true.neg.mean.low[i] <- get(fn)
  
  
  fn <- paste0(scenarios[i], ".cum.n.presented.mean")
  n.presented.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.presented.mean_hi")
  n.presented.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.presented.mean_low")
  n.presented.mean.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.presented.pos.mean")
  n.presented.pos.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.presented.pos.mean_hi")
  n.presented.pos.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.presented.pos.mean_low")
  n.presented.pos.mean.low[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.presented.neg.mean")
  n.presented.neg.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.presented.neg.mean_hi")
  n.presented.neg.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.presented.neg.mean_low")
  n.presented.neg.mean.low[i] <- get(fn)
  
  
  fn <- paste0(scenarios[i], ".cum.n.missed.pos.mean")
  n.missed.pos.mean[i]  <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.missed.pos.mean_hi")
  n.missed.pos.mean.hi[i] <- get(fn)
  
  fn <- paste0(scenarios[i], ".cum.n.missed.pos.mean_low")
  n.missed.pos.mean.low[i] <- get(fn)
  
}
  


table<-cbind(scenarios,incid.mean, incid.mean.hi, incid.mean.low, ptar.mean, rna.tests.mean, rna.tests.mean.hi, rna.tests.mean.low,
             ab.tests.mean, ab.tests.mean.hi, ab.tests.mean.low, tests.mean, tests.mean.hi, tests.mean.low,
             diag.mean, diag.mean.hi, diag.mean.low, diag.p.mean, diag.p.mean.hi, diag.p.mean.low, diag.a.mean, diag.a.mean.hi, diag.a.mean.low,
             partners.mean, partners.mean.hi, partners.mean.low, partners.f.mean, partners.f.mean.hi, partners.f.mean.low,
             n.tests.tst.ps.mean, n.tests.tst.ps.mean.hi, n.tests.tst.ps.mean.low,
             NIA, NIA.low, NIA.hi, PIA, PIA.low, PIA.hi, 
             DPTST, DPTST.low, DPTST.hi, DPTM, DPTM.low, DPTM.hi,
             Prev, Prev.low, Prev.hi,
             ir100, ir100.low, ir100.hi,
             bg.tests.mean, bg.tests.mean.hi, bg.tests.mean.low,
             n.false.pos.mean, n.false.pos.mean.low, n.false.pos.mean.hi,
             n.false.neg.mean, n.false.neg.mean.low, n.false.neg.mean.hi,
             n.true.pos.mean, n.true.pos.mean.low, n.true.pos.mean.hi,
             n.true.neg.mean, n.true.neg.mean.low, n.true.neg.mean.hi,
             n.presented.mean, n.presented.mean.low, n.presented.mean.hi,
             n.presented.pos.mean, n.presented.pos.mean.low, n.presented.pos.mean.hi,
             n.presented.neg.mean, n.presented.neg.mean.low, n.presented.neg.mean.hi,
             n.missed.pos.mean, n.missed.pos.mean.low, n.missed.pos.mean.hi)
table

library(xlsx) #load the package
write.xlsx(x = table, file = "out/KTM.int.results.xlsx",
           sheetName = "KTM intervention", row.names = FALSE)


##95% outcomes at the end of the simulation



for(i in 1:length(scenarios)){
  
  x<-get(scenarios[i])
  nsims<-300
  steps<-x$control$nsteps - trim
  
  for(j in 1:length(level)){
    
    fn <- paste0(scenarios[i], ".pct.diag", level[j])
    assign(fn,rep(NA,1))
  
    
    fn <- paste0(scenarios[i], ".pct.txt", level[j])
    assign(fn,rep(NA,1))
    
    fn <- paste0(scenarios[i], ".pct.vsup", level[j])
    assign(fn,rep(NA,1))
    
    fn <- paste0(scenarios[i], ".fprev", level[j])
    assign(fn,rep(NA,1))
    
    fn <- paste0(scenarios[i], ".time.inf.diag", level[j])
    assign(fn,rep(NA,1))
    
    
  }}

hi.cut <- round(.95*nsims)
low.cut <- round(.05*nsims)
  #no int 
  x<-sort(as.numeric(Mod.noint_256_002$epi$pct.diag[521,1:nsims]))
  noint_256_002.pct.diag.list <- x * 100
  noint_256_002.pct.diag <-mean(x) * 100
  noint_256_002.pct.diag_hi <-x[hi.cut] * 100
  noint_256_002.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_002$epi$pct.txt[521,1:nsims]))
  noint_256_002.pct.txt <-mean(x) * 100
  noint_256_002.pct.txt_hi <-x[hi.cut] * 100
  noint_256_002.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_002$epi$pct.vsup[521,1:nsims]))
  noint_256_002.pct.vsup <-mean(x) * 100
  noint_256_002.pct.vsup_hi <-x[hi.cut] * 100
  noint_256_002.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_002$epi$pct.test.ly[521,1:nsims]))
  noint_256_002.pct.test.ly <-mean(x) * 100
  noint_256_002.pct.test.ly_hi <-x[hi.cut] * 100
  noint_256_002.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_002$epi$time.inf.diag[521,1:nsims]))
  noint_256_002.time.inf.diag <-mean(x)
  noint_256_002.time.inf.diag_hi <-x[hi.cut]
  noint_256_002.time.inf.diag_low <-x[low.cut]
  
  #"noint_256_006", 
  x<-sort(as.numeric(Mod.noint_256_006$epi$pct.diag[521,1:nsims]))
  noint_256_006.pct.diag.list <- x * 100
  noint_256_006.pct.diag <-mean(x) * 100
  noint_256_006.pct.diag_hi <-x[hi.cut] * 100
  noint_256_006.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_006$epi$pct.txt[521,1:nsims]))
  noint_256_006.pct.txt.list <- x *100
  noint_256_006.pct.txt <-mean(x) * 100
  noint_256_006.pct.txt_hi <-x[hi.cut] * 100
  noint_256_006.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_006$epi$pct.vsup[521,1:nsims]))
  noint_256_006.pct.vsup.list <- x * 100
  noint_256_006.pct.vsup <-mean(x) * 100
  noint_256_006.pct.vsup_hi <-x[hi.cut] * 100
  noint_256_006.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_006$epi$pct.test.ly[521,1:nsims]))
  noint_256_006.pct.test.ly <-mean(x) * 100
  noint_256_006.pct.test.ly_hi <-x[hi.cut] * 100
  noint_256_006.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_006$epi$time.inf.diag[521,1:nsims]))
  noint_256_006.time.inf.diag <-mean(x)
  noint_256_006.time.inf.diag_hi <-x[hi.cut]
  noint_256_006.time.inf.diag_low <-x[low.cut]
  
  #"noint_256_009", 
  x<-sort(as.numeric(Mod.noint_256_009$epi$pct.diag[521,1:nsims]))
  noint_256_009.pct.diag <-mean(x) * 100
  noint_256_009.pct.diag.list <- x * 100
  noint_256_009.pct.diag_hi <-x[hi.cut] * 100
  noint_256_009.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_009$epi$pct.txt[521,1:nsims]))
  noint_256_009.pct.txt <-mean(x) * 100
  noint_256_009.pct.txt_hi <-x[hi.cut] * 100
  noint_256_009.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_009$epi$pct.vsup[521,1:nsims]))
  noint_256_009.pct.vsup <-mean(x) * 100
  noint_256_009.pct.vsup_hi <-x[hi.cut] * 100
  noint_256_009.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_009$epi$pct.test.ly[521,1:nsims]))
  noint_256_009.pct.test.ly <-mean(x) * 100
  noint_256_009.pct.test.ly_hi <-x[hi.cut] * 100
  noint_256_009.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_009$epi$time.inf.diag[521,1:nsims]))
  noint_256_009.time.inf.diag <-mean(x)
  noint_256_009.time.inf.diag_hi <-x[hi.cut]
  noint_256_009.time.inf.diag_low <-x[low.cut]
  
  #"int_256_002", 
  x<-sort(as.numeric(Mod.int_256_002$epi$pct.diag[521,1:nsims]))
  int_256_002.pct.diag.list <- x * 100
  int_256_002.pct.diag <-mean(x) * 100
  int_256_002.pct.diag_hi <-x[hi.cut] * 100
  int_256_002.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_002$epi$pct.txt[521,1:nsims]))
  int_256_002.pct.txt <-mean(x) * 100
  int_256_002.pct.txt_hi <-x[hi.cut] * 100
  int_256_002.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_002$epi$pct.vsup[521,1:nsims]))
  int_256_002.pct.vsup <-mean(x) * 100
  int_256_002.pct.vsup_hi <-x[hi.cut] * 100
  int_256_002.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_002$epi$pct.test.ly[521,1:nsims]))
  int_256_002.pct.test.ly <-mean(x) * 100
  int_256_002.pct.test.ly_hi <-x[hi.cut] * 100
  int_256_002.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_002$epi$time.inf.diag[521,1:nsims]))
  int_256_002.time.inf.diag <-mean(x)
  int_256_002.time.inf.diag_hi <-x[hi.cut]
  int_256_002.time.inf.diag_low <-x[low.cut]
  
  #"int_256_006", 
  x<-sort(as.numeric(Mod.int_256_006$epi$pct.diag[521,1:nsims]))
  int_256_006.pct.diag.list <- x * 100
  int_256_006.pct.diag <-mean(x) * 100
  int_256_006.pct.diag_hi <-x[hi.cut] * 100
  int_256_006.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006$epi$pct.txt[521,1:nsims]))
  int_256_006.pct.txt.list <- x * 100
  int_256_006.pct.txt <-mean(x) * 100
  int_256_006.pct.txt_hi <-x[hi.cut] * 100
  int_256_006.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006$epi$pct.vsup[521,1:nsims]))
  int_256_006.pct.vsup.list <-x * 100
  int_256_006.pct.vsup <-mean(x) * 100
  int_256_006.pct.vsup_hi <-x[hi.cut] * 100
  int_256_006.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006$epi$pct.test.ly[521,1:nsims]))
  int_256_006.pct.test.ly <-mean(x) * 100
  int_256_006.pct.test.ly_hi <-x[hi.cut] * 100
  int_256_006.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006$epi$time.inf.diag[521,1:nsims]))
  int_256_006.time.inf.diag <-mean(x)
  int_256_006.time.inf.diag_hi <-x[hi.cut]
  int_256_006.time.inf.diag_low <-x[low.cut]
  
  #int_256_009
  x<-sort(as.numeric(Mod.int_256_009$epi$pct.diag[521,1:nsims]))
  int_256_009.pct.diag.list <- x * 100
  int_256_009.pct.diag <-mean(x) * 100
  int_256_009.pct.diag_hi <-x[hi.cut] * 100
  int_256_009.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_009$epi$pct.txt[521,1:nsims]))
  int_256_009.pct.txt <-mean(x) * 100
  int_256_009.pct.txt_hi <-x[hi.cut] * 100
  int_256_009.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_009$epi$pct.vsup[521,1:nsims]))
  int_256_009.pct.vsup <-mean(x) * 100
  int_256_009.pct.vsup_hi <-x[hi.cut] * 100
  int_256_009.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_009$epi$pct.test.ly[521,1:nsims]))
  int_256_009.pct.test.ly <-mean(x) * 100
  int_256_009.pct.test.ly_hi <-x[hi.cut] * 100
  int_256_009.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_009$epi$time.inf.diag[521,1:nsims]))
  int_256_009.time.inf.diag <-mean(x)
  int_256_009.time.inf.diag_hi <-x[hi.cut]
  int_256_009.time.inf.diag_low <-x[low.cut]
  
  #int_45_002
  x<-sort(as.numeric(Mod.int_45_002$epi$pct.diag[521,1:nsims]))
  int_45_002.pct.diag.list <- x * 100
  int_45_002.pct.diag <-mean(x) * 100
  int_45_002.pct.diag_hi <-x[hi.cut] * 100
  int_45_002.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002$epi$pct.txt[521,1:nsims]))
  int_45_002.pct.txt <-mean(x) * 100
  int_45_002.pct.txt_hi <-x[hi.cut] * 100
  int_45_002.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002$epi$pct.vsup[521,1:nsims]))
  int_45_002.pct.vsup <-mean(x) * 100
  int_45_002.pct.vsup_hi <-x[hi.cut] * 100
  int_45_002.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002$epi$pct.test.ly[521,1:nsims]))
  int_45_002.pct.test.ly <-mean(x) * 100
  int_45_002.pct.test.ly_hi <-x[hi.cut] * 100
  int_45_002.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002$epi$time.inf.diag[521,1:nsims]))
  int_45_002.time.inf.diag <-mean(x)
  int_45_002.time.inf.diag_hi <-x[hi.cut]
  int_45_002.time.inf.diag_low <-x[low.cut]
  
  #int_45_006
  x<-sort(as.numeric(Mod.int_45_006$epi$pct.diag[521,1:nsims]))
  int_45_006.pct.diag.list <-x * 100
  int_45_006.pct.diag <-mean(x) * 100
  int_45_006.pct.diag_hi <-x[hi.cut] * 100
  int_45_006.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006$epi$pct.txt[521,1:nsims]))
  int_45_006.pct.txt.list <-x * 100
  int_45_006.pct.txt <-mean(x) * 100
  int_45_006.pct.txt_hi <-x[hi.cut] * 100
  int_45_006.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006$epi$pct.vsup[521,1:nsims]))
  int_45_006.pct.vsup.list <- x * 100
  int_45_006.pct.vsup <-mean(x) * 100
  int_45_006.pct.vsup_hi <-x[hi.cut] * 100
  int_45_006.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006$epi$pct.test.ly[521,1:nsims]))
  int_45_006.pct.test.ly <-mean(x) * 100
  int_45_006.pct.test.ly_hi <-x[hi.cut] * 100
  int_45_006.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006$epi$time.inf.diag[521,1:nsims]))
  int_45_006.time.inf.diag <-mean(x)
  int_45_006.time.inf.diag_hi <-x[hi.cut]
  int_45_006.time.inf.diag_low <-x[low.cut]
  
  #int_45_009
  x<-sort(as.numeric(Mod.int_45_009$epi$pct.diag[521,1:nsims]))
  int_45_009.pct.diag.list <- x * 100
  int_45_009.pct.diag <-mean(x) * 100
  int_45_009.pct.diag_hi <-x[hi.cut] * 100
  int_45_009.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_009$epi$pct.txt[521,1:nsims]))
  int_45_009.pct.txt <-mean(x) * 100
  int_45_009.pct.txt_hi <-x[hi.cut] * 100
  int_45_009.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_009$epi$pct.vsup[521,1:nsims]))
  int_45_009.pct.vsup <-mean(x) * 100
  int_45_009.pct.vsup_hi <-x[hi.cut] * 100
  int_45_009.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_009$epi$pct.test.ly[521,1:nsims]))
  int_45_009.pct.test.ly <-mean(x) * 100
  int_45_009.pct.test.ly_hi <-x[hi.cut] * 100
  int_45_009.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_009$epi$time.inf.diag[521,1:nsims]))
  int_45_009.time.inf.diag <-mean(x)
  int_45_009.time.inf.diag_hi <-x[hi.cut]
  int_45_009.time.inf.diag_low <-x[low.cut]
  
  #int_949_002
  x<-sort(as.numeric(Mod.int_949_002$epi$pct.diag[521,1:nsims]))
  int_949_002.pct.diag.list <- x * 100
  int_949_002.pct.diag <-mean(x) * 100
  int_949_002.pct.diag_hi <-x[hi.cut] * 100
  int_949_002.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_002$epi$pct.txt[521,1:nsims]))
  int_949_002.pct.txt <-mean(x) * 100
  int_949_002.pct.txt_hi <-x[hi.cut] * 100
  int_949_002.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_002$epi$pct.vsup[521,1:nsims]))
  int_949_002.pct.vsup <-mean(x) * 100
  int_949_002.pct.vsup_hi <-x[hi.cut] * 100
  int_949_002.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_002$epi$pct.test.ly[521,1:nsims]))
  int_949_002.pct.test.ly <-mean(x) * 100
  int_949_002.pct.test.ly_hi <-x[hi.cut] * 100
  int_949_002.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_002$epi$time.inf.diag[521,1:nsims]))
  int_949_002.time.inf.diag <-mean(x)
  int_949_002.time.inf.diag_hi <-x[hi.cut]
  int_949_002.time.inf.diag_low <-x[low.cut]

  
  #int_949_006
  x<-sort(as.numeric(Mod.int_949_006$epi$pct.diag[521,1:nsims]))
  int_949_006.pct.diag.list <- x * 100
  int_949_006.pct.diag <-mean(x) * 100
  int_949_006.pct.diag_hi <-x[hi.cut] * 100
  int_949_006.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006$epi$pct.txt[521,1:nsims]))
  int_949_006.pct.txt.list <- x * 100
  int_949_006.pct.txt <-mean(x) * 100
  int_949_006.pct.txt_hi <-x[hi.cut] * 100
  int_949_006.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006$epi$pct.vsup[521,1:nsims]))
  int_949_006.pct.vsup.list <- x * 100
  int_949_006.pct.vsup <-mean(x) * 100
  int_949_006.pct.vsup_hi <-x[hi.cut] * 100
  int_949_006.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006$epi$pct.test.ly[521,1:nsims]))
  int_949_006.pct.test.ly <-mean(x) * 100
  int_949_006.pct.test.ly_hi <-x[hi.cut] * 100
  int_949_006.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006$epi$time.inf.diag[521,1:nsims]))
  int_949_006.time.inf.diag <-mean(x)
  int_949_006.time.inf.diag_hi <-x[hi.cut]
  int_949_006.time.inf.diag_low <-x[low.cut]
  
  #int_949_009
  x<-sort(as.numeric(Mod.int_949_009$epi$pct.diag[521,1:nsims]))
  int_949_009.pct.diag.list <- x * 100
  int_949_009.pct.diag <-mean(x) * 100
  int_949_009.pct.diag_hi <-x[hi.cut] * 100
  int_949_009.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_009$epi$pct.txt[521,1:nsims]))
  int_949_009.pct.txt <-mean(x) * 100
  int_949_009.pct.txt_hi <-x[hi.cut] * 100
  int_949_009.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_009$epi$pct.vsup[521,1:nsims]))
  int_949_009.pct.vsup <-mean(x) * 100
  int_949_009.pct.vsup_hi <-x[hi.cut] * 100
  int_949_009.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_009$epi$pct.test.ly[521,1:nsims]))
  int_949_009.pct.test.ly <-mean(x) * 100
  int_949_009.pct.test.ly_hi <-x[hi.cut] * 100
  int_949_009.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_009$epi$time.inf.diag[521,1:nsims]))
  int_949_009.time.inf.diag <-mean(x)
  int_949_009.time.inf.diag_hi <-x[hi.cut]
  int_949_009.time.inf.diag_low <-x[low.cut]
  
  #int_100_002
  x<-sort(as.numeric(Mod.int_100_002$epi$pct.diag[521,1:nsims]))
  int_100_002.pct.diag.list <- x * 100
  int_100_002.pct.diag <-mean(x) * 100
  int_100_002.pct.diag_hi <-x[hi.cut] * 100
  int_100_002.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_002$epi$pct.txt[521,1:nsims]))
  int_100_002.pct.txt <-mean(x) * 100
  int_100_002.pct.txt_hi <-x[hi.cut] * 100
  int_100_002.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_002$epi$pct.vsup[521,1:nsims]))
  int_100_002.pct.vsup <-mean(x) * 100
  int_100_002.pct.vsup_hi <-x[hi.cut] * 100
  int_100_002.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_002$epi$pct.test.ly[521,1:nsims]))
  int_100_002.pct.test.ly <-mean(x) * 100
  int_100_002.pct.test.ly_hi <-x[hi.cut] * 100
  int_100_002.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_002$epi$time.inf.diag[521,1:nsims]))
  int_100_002.time.inf.diag <-mean(x)
  int_100_002.time.inf.diag_hi <-x[hi.cut]
  int_100_002.time.inf.diag_low <-x[low.cut]
  
  #int_100_006
  x<-sort(as.numeric(Mod.int_100_006$epi$pct.diag[521,1:nsims]))
  int_100_006.pct.diag.list <- x * 100
  int_100_006.pct.diag <-mean(x) * 100
  int_100_006.pct.diag_hi <-x[hi.cut] * 100
  int_100_006.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006$epi$pct.txt[521,1:nsims]))
  int_100_006.pct.txt.list <- x * 100
  int_100_006.pct.txt <-mean(x) * 100
  int_100_006.pct.txt_hi <-x[hi.cut] * 100
  int_100_006.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006$epi$pct.vsup[521,1:nsims]))
  int_100_006.pct.vsup.list <- x * 100
  int_100_006.pct.vsup <-mean(x) * 100
  int_100_006.pct.vsup_hi <-x[hi.cut] * 100
  int_100_006.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006$epi$pct.test.ly[521,1:nsims]))
  int_100_006.pct.test.ly <-mean(x) * 100
  int_100_006.pct.test.ly_hi <-x[hi.cut] * 100
  int_100_006.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006$epi$time.inf.diag[521,1:nsims]))
  int_100_006.time.inf.diag <-mean(x)
  int_100_006.time.inf.diag_hi <-x[hi.cut]
  int_100_006.time.inf.diag_low <-x[low.cut]
  
  #int_100_009
  x<-sort(as.numeric(Mod.int_100_009$epi$pct.diag[521,1:nsims]))
  int_100_009.pct.diag.list <- x * 100
  int_100_009.pct.diag <-mean(x) * 100
  int_100_009.pct.diag_hi <-x[hi.cut] * 100
  int_100_009.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_009$epi$pct.txt[521,1:nsims]))
  int_100_009.pct.txt <-mean(x) * 100
  int_100_009.pct.txt_hi <-x[hi.cut] * 100
  int_100_009.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_009$epi$pct.vsup[521,1:nsims]))
  int_100_009.pct.vsup <-mean(x) * 100
  int_100_009.pct.vsup_hi <-x[hi.cut] * 100
  int_100_009.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_009$epi$pct.test.ly[521,1:nsims]))
  int_100_009.pct.test.ly <-mean(x) * 100
  int_100_009.pct.test.ly_hi <-x[hi.cut] * 100
  int_100_009.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_009$epi$time.inf.diag[521,1:nsims]))
  int_100_009.time.inf.diag <-mean(x)
  int_100_009.time.inf.diag_hi <-x[hi.cut]
  int_100_009.time.inf.diag_low <-x[low.cut]
  
  ##AB ONLY
  
  #int_45_002_ab
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$pct.diag[521,1:nsims]))
  int_45_002_ab.pct.diag.list <- x * 100
  int_45_002_ab.pct.diag <-mean(x) * 100
  int_45_002_ab.pct.diag_hi <-x[hi.cut] * 100
  int_45_002_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$pct.txt[521,1:nsims]))
  int_45_002_ab.pct.txt <-mean(x) * 100
  int_45_002_ab.pct.txt_hi <-x[hi.cut] * 100
  int_45_002_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$pct.vsup[521,1:nsims]))
  int_45_002_ab.pct.vsup <-mean(x) * 100
  int_45_002_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_45_002_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$pct.test.ly[521,1:nsims]))
  int_45_002_ab.pct.test.ly <-mean(x) * 100
  int_45_002_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_45_002_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$time.inf.diag[521,1:nsims]))
  int_45_002_ab.time.inf.diag <-mean(x)
  int_45_002_ab.time.inf.diag_hi <-x[hi.cut]
  int_45_002_ab.time.inf.diag_low <-x[low.cut]

  #int_45_006
  x<-sort(as.numeric(Mod.int_45_006_ab$epi$pct.diag[521,1:nsims]))
  int_45_006_ab.pct.diag.list <- x * 100
  int_45_006_ab.pct.diag <-mean(x) * 100
  int_45_006_ab.pct.diag_hi <-x[hi.cut] * 100
  int_45_006_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_ab$epi$pct.txt[521,1:nsims]))
  int_45_006_ab.pct.txt <-mean(x) * 100
  int_45_006_ab.pct.txt_hi <-x[hi.cut] * 100
  int_45_006_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_ab$epi$pct.vsup[521,1:nsims]))
  int_45_006_ab.pct.vsup <-mean(x) * 100
  int_45_006_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_45_006_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_ab$epi$pct.test.ly[521,1:nsims]))
  int_45_006_ab.pct.test.ly <-mean(x) * 100
  int_45_006_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_45_006_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_ab$epi$time.inf.diag[521,1:nsims]))
  int_45_006_ab.time.inf.diag <-mean(x)
  int_45_006_ab.time.inf.diag_hi <-x[hi.cut]
  int_45_006_ab.time.inf.diag_low <-x[low.cut]
  
  #int_45_009
  x<-sort(as.numeric(Mod.int_45_009_ab$epi$pct.diag[521,1:nsims]))
  int_45_009_ab.pct.diag.list <- x * 100
  int_45_009_ab.pct.diag <-mean(x) * 100
  int_45_009_ab.pct.diag_hi <-x[hi.cut] * 100
  int_45_009_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_009_ab$epi$pct.txt[521,1:nsims]))
  int_45_009_ab.pct.txt <-mean(x) * 100
  int_45_009_ab.pct.txt_hi <-x[hi.cut] * 100
  int_45_009_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$pct.vsup[521,1:nsims]))
  int_45_009_ab.pct.vsup <-mean(x) * 100
  int_45_009_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_45_009_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_009_ab$epi$pct.test.ly[521,1:nsims]))
  int_45_009_ab.pct.test.ly <-mean(x) * 100
  int_45_009_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_45_009_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_009_ab$epi$time.inf.diag[521,1:nsims]))
  int_45_009_ab.time.inf.diag <-mean(x)
  int_45_009_ab.time.inf.diag_hi <-x[hi.cut]
  int_45_009_ab.time.inf.diag_low <-x[low.cut]
  
  #int_949_002
  x<-sort(as.numeric(Mod.int_949_002_ab$epi$pct.diag[521,1:nsims]))
  int_949_002_ab.pct.diag.list <- x * 100
  int_949_002_ab.pct.diag <-mean(x) * 100
  int_949_002_ab.pct.diag_hi <-x[hi.cut] * 100
  int_949_002_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_002_ab$epi$pct.txt[521,1:nsims]))
  int_949_002_ab.pct.txt <-mean(x) * 100
  int_949_002_ab.pct.txt_hi <-x[hi.cut] * 100
  int_949_002_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_002_ab$epi$pct.vsup[521,1:nsims]))
  int_949_002_ab.pct.vsup <-mean(x) * 100
  int_949_002_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_949_002_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_002_ab$epi$pct.test.ly[521,1:nsims]))
  int_949_002_ab.pct.test.ly <-mean(x) * 100
  int_949_002_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_949_002_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_002_ab$epi$time.inf.diag[521,1:nsims]))
  int_949_002_ab.time.inf.diag <-mean(x)
  int_949_002_ab.time.inf.diag_hi <-x[hi.cut]
  int_949_002_ab.time.inf.diag_low <-x[low.cut]
  
  #int_949_006
  x<-sort(as.numeric(Mod.int_949_006_ab$epi$pct.diag[521,1:nsims]))
  int_949_006_ab.pct.diag.list <- x * 100
  int_949_006_ab.pct.diag <-mean(x) * 100
  int_949_006_ab.pct.diag_hi <-x[hi.cut] * 100
  int_949_006_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_ab$epi$pct.txt[521,1:nsims]))
  int_949_006_ab.pct.txt <-mean(x) * 100
  int_949_006_ab.pct.txt_hi <-x[hi.cut] * 100
  int_949_006_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_ab$epi$pct.vsup[521,1:nsims]))
  int_949_006_ab.pct.vsup <-mean(x) * 100
  int_949_006_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_949_006_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_ab$epi$pct.test.ly[521,1:nsims]))
  int_949_006_ab.pct.test.ly <-mean(x) * 100
  int_949_006_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_949_006_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_ab$epi$time.inf.diag[521,1:nsims]))
  int_949_006_ab.time.inf.diag <-mean(x)
  int_949_006_ab.time.inf.diag_hi <-x[hi.cut]
  int_949_006_ab.time.inf.diag_low <-x[low.cut]
  
  #int_949_009
  x<-sort(as.numeric(Mod.int_949_009_ab$epi$pct.diag[521,1:nsims]))
  int_949_009_ab.pct.diag.list <- x * 100
  int_949_009_ab.pct.diag <-mean(x) * 100
  int_949_009_ab.pct.diag_hi <-x[hi.cut] * 100
  int_949_009_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_009_ab$epi$pct.txt[521,1:nsims]))
  int_949_009_ab.pct.txt <-mean(x) * 100
  int_949_009_ab.pct.txt_hi <-x[hi.cut] * 100
  int_949_009_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_009_ab$epi$pct.vsup[521,1:nsims]))
  int_949_009_ab.pct.vsup <-mean(x) * 100
  int_949_009_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_949_009_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_009_ab$epi$pct.test.ly[521,1:nsims]))
  int_949_009_ab.pct.test.ly <-mean(x) * 100
  int_949_009_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_949_009_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_009_ab$epi$time.inf.diag[521,1:nsims]))
  int_949_009_ab.time.inf.diag <-mean(x)
  int_949_009_ab.time.inf.diag_hi <-x[hi.cut]
  int_949_009_ab.time.inf.diag_low <-x[low.cut]
  
  #int_100_002
  x<-sort(as.numeric(Mod.int_100_002_ab$epi$pct.diag[521,1:nsims]))
  int_100_002_ab.pct.diag.list <- x * 100
  int_100_002_ab.pct.diag <-mean(x) * 100
  int_100_002_ab.pct.diag_hi <-x[hi.cut] * 100
  int_100_002_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_002_ab$epi$pct.txt[521,1:nsims]))
  int_100_002_ab.pct.txt <-mean(x) * 100
  int_100_002_ab.pct.txt_hi <-x[hi.cut] * 100
  int_100_002_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_002_ab$epi$pct.vsup[521,1:nsims]))
  int_100_002_ab.pct.vsup <-mean(x) * 100
  int_100_002_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_100_002_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_002_ab$epi$pct.test.ly[521,1:nsims]))
  int_100_002_ab.pct.test.ly <-mean(x) * 100
  int_100_002_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_100_002_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_002_ab$epi$time.inf.diag[521,1:nsims]))
  int_100_002_ab.time.inf.diag <-mean(x)
  int_100_002_ab.time.inf.diag_hi <-x[hi.cut]
  int_100_002_ab.time.inf.diag_low <-x[low.cut]
  
  #int_100_006
  x<-sort(as.numeric(Mod.int_100_006_ab$epi$pct.diag[521,1:nsims]))
  int_100_006_ab.pct.diag.list <- x * 100
  int_100_006_ab.pct.diag <-mean(x) * 100
  int_100_006_ab.pct.diag_hi <-x[hi.cut] * 100
  int_100_006_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_ab$epi$pct.txt[521,1:nsims]))
  int_100_006_ab.pct.txt <-mean(x) * 100
  int_100_006_ab.pct.txt_hi <-x[hi.cut] * 100
  int_100_006_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_ab$epi$pct.vsup[521,1:nsims]))
  int_100_006_ab.pct.vsup <-mean(x) * 100
  int_100_006_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_100_006_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_ab$epi$pct.test.ly[521,1:nsims]))
  int_100_006_ab.pct.test.ly <-mean(x) * 100
  int_100_006_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_100_006_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_ab$epi$time.inf.diag[521,1:nsims]))
  int_100_006_ab.time.inf.diag <-mean(x)
  int_100_006_ab.time.inf.diag_hi <-x[hi.cut]
  int_100_006_ab.time.inf.diag_low <-x[low.cut]
  
  #int_100_009
  x<-sort(as.numeric(Mod.int_100_009_ab$epi$pct.diag[521,1:nsims]))
  int_100_009_ab.pct.diag.list <- x * 100
  int_100_009_ab.pct.diag <-mean(x) * 100
  int_100_009_ab.pct.diag_hi <-x[hi.cut] * 100
  int_100_009_ab.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_009_ab$epi$pct.txt[521,1:nsims]))
  int_100_009_ab.pct.txt <-mean(x) * 100
  int_100_009_ab.pct.txt_hi <-x[hi.cut] * 100
  int_100_009_ab.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_009_ab$epi$pct.vsup[521,1:nsims]))
  int_100_009_ab.pct.vsup <-mean(x) * 100
  int_100_009_ab.pct.vsup_hi <-x[hi.cut] * 100
  int_100_009_ab.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_009_ab$epi$pct.test.ly[521,1:nsims]))
  int_100_009_ab.pct.test.ly <-mean(x) * 100
  int_100_009_ab.pct.test.ly_hi <-x[hi.cut] * 100
  int_100_009_ab.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_009_ab$epi$time.inf.diag[521,1:nsims]))
  int_100_009_ab.time.inf.diag <-mean(x)
  int_100_009_ab.time.inf.diag_hi <-x[hi.cut]
  int_100_009_ab.time.inf.diag_low <-x[low.cut]
  
  ##LOWER AHI SEEKKING PROBABILITY
  
  x<-sort(as.numeric(Mod.noint_256_006_l$epi$pct.diag[521,1:nsims]))
  noint_256_006_l.pct.diag.list <-x * 100
  noint_256_006_l.pct.diag <-mean(x) * 100
  noint_256_006_l.pct.diag_hi <-x[hi.cut] * 100
  noint_256_006_l.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_006_l$epi$pct.txt[521,1:nsims]))
  noint_256_006_l.pct.txt <-mean(x) * 100
  noint_256_006_l.pct.txt_hi <-x[hi.cut] * 100
  noint_256_006_l.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_006_l$epi$pct.vsup[521,1:nsims]))
  noint_256_006_l.pct.vsup <-mean(x) * 100
  noint_256_006_l.pct.vsup_hi <-x[hi.cut] * 100
  noint_256_006_l.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_006_l$epi$pct.test.ly[521,1:nsims]))
  noint_256_006_l.pct.test.ly <-mean(x) * 100
  noint_256_006_l.pct.test.ly_hi <-x[hi.cut] * 100
  noint_256_006_l.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint_256_006_l$epi$time.inf.diag[521,1:nsims]))
  noint_256_006_l.time.inf.diag <-mean(x)
  noint_256_006_l.time.inf.diag_hi <-x[hi.cut]
  noint_256_006_l.time.inf.diag_low <-x[low.cut]
  
  x<-sort(as.numeric(Mod.int_256_006_l$epi$pct.diag[521,1:nsims]))
  int_256_006_l.pct.diag.list <- x * 100
  int_256_006_l.pct.diag <-mean(x) * 100
  int_256_006_l.pct.diag_hi <-x[hi.cut] * 100
  int_256_006_l.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006_l$epi$pct.txt[521,1:nsims]))
  int_256_006_l.pct.txt <-mean(x) * 100
  int_256_006_l.pct.txt_hi <-x[hi.cut] * 100
  int_256_006_l.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006_l$epi$pct.vsup[521,1:nsims]))
  int_256_006_l.pct.vsup <-mean(x) * 100
  int_256_006_l.pct.vsup_hi <-x[hi.cut] * 100
  int_256_006_l.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006_l$epi$pct.test.ly[521,1:nsims]))
  int_256_006_l.pct.test.ly <-mean(x) * 100
  int_256_006_l.pct.test.ly_hi <-x[hi.cut] * 100
  int_256_006_l.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006_l$epi$time.inf.diag[521,1:nsims]))
  int_256_006_l.time.inf.diag <-mean(x)
  int_256_006_l.time.inf.diag_hi <-x[hi.cut]
  int_256_006_l.time.inf.diag_low <-x[low.cut]
  

  x<-sort(as.numeric(Mod.int_45_006_l$epi$pct.diag[521,1:nsims]))
  int_45_006_l.pct.diag.list <- x * 100
  int_45_006_l.pct.diag <-mean(x) * 100
  int_45_006_l.pct.diag_hi <-x[hi.cut] * 100
  int_45_006_l.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_l$epi$pct.txt[521,1:nsims]))
  int_45_006_l.pct.txt <-mean(x) * 100
  int_45_006_l.pct.txt_hi <-x[hi.cut] * 100
  int_45_006_l.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_l$epi$pct.vsup[521,1:nsims]))
  int_45_006_l.pct.vsup <-mean(x) * 100
  int_45_006_l.pct.vsup_hi <-x[hi.cut] * 100
  int_45_006_l.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_l$epi$pct.test.ly[521,1:nsims]))
  int_45_006_l.pct.test.ly <-mean(x) * 100
  int_45_006_l.pct.test.ly_hi <-x[hi.cut] * 100
  int_45_006_l.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_l$epi$time.inf.diag[521,1:nsims]))
  int_45_006_l.time.inf.diag <-mean(x)
  int_45_006_l.time.inf.diag_hi <-x[hi.cut]
  int_45_006_l.time.inf.diag_low <-x[low.cut]

  x<-sort(as.numeric(Mod.int_949_006_l$epi$pct.diag[521,1:nsims]))
  int_949_006_l.pct.diag.list <- x * 100
  int_949_006_l.pct.diag <-mean(x) * 100
  int_949_006_l.pct.diag_hi <-x[hi.cut] * 100
  int_949_006_l.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_l$epi$pct.txt[521,1:nsims]))
  int_949_006_l.pct.txt <-mean(x) * 100
  int_949_006_l.pct.txt_hi <-x[hi.cut] * 100
  int_949_006_l.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_l$epi$pct.vsup[521,1:nsims]))
  int_949_006_l.pct.vsup <-mean(x) * 100
  int_949_006_l.pct.vsup_hi <-x[hi.cut] * 100
  int_949_006_l.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_l$epi$pct.test.ly[521,1:nsims]))
  int_949_006_l.pct.test.ly <-mean(x) * 100
  int_949_006_l.pct.test.ly_hi <-x[hi.cut] * 100
  int_949_006_l.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_l$epi$time.inf.diag[521,1:nsims]))
  int_949_006_l.time.inf.diag <-mean(x)
  int_949_006_l.time.inf.diag_hi <-x[hi.cut]
  int_949_006_l.time.inf.diag_low <-x[low.cut]
  
  x<-sort(as.numeric(Mod.int_100_006_l$epi$pct.diag[521,1:nsims]))
  int_100_006_l.pct.diag.list <- x * 100
  int_100_006_l.pct.diag <-mean(x) * 100
  int_100_006_l.pct.diag_hi <-x[hi.cut] * 100
  int_100_006_l.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_l$epi$pct.txt[521,1:nsims]))
  int_100_006_l.pct.txt <-mean(x) * 100
  int_100_006_l.pct.txt_hi <-x[hi.cut] * 100
  int_100_006_l.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_l$epi$pct.vsup[521,1:nsims]))
  int_100_006_l.pct.vsup <-mean(x) * 100
  int_100_006_l.pct.vsup_hi <-x[hi.cut] * 100
  int_100_006_l.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_l$epi$pct.test.ly[521,1:nsims]))
  int_100_006_l.pct.test.ly <-mean(x) * 100
  int_100_006_l.pct.test.ly_hi <-x[hi.cut] * 100
  int_100_006_l.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_l$epi$time.inf.diag[521,1:nsims]))
  int_100_006_l.time.inf.diag <-mean(x)
  int_100_006_l.time.inf.diag_hi <-x[hi.cut]
  int_100_006_l.time.inf.diag_low <-x[low.cut]
  
  ##FINAL prevalence
  

  
  
##MAKE AGE AT INFECTION
  for(i in 1:length(scenarios)){
    
    x<-get(scenarios[i])
    nsims<-300
    steps<-x$control$nsteps - trim
    
    for(j in 1:length(level)){
      
      fn <- paste0(scenarios[i], ".age.inf", level[j])
      assign(fn,rep(NA,nsims))
      
    }}
  
  for (i in 1:nsims){
  #no int 
    noint_256_002.age.inf[i] <- sum((Mod.noint_256_002$epi$incid.poi[,i] * Mod.noint_256_002$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.noint_256_002$epi$incid.poi[,i])
    noint_256_006.age.inf[i] <- sum((Mod.noint_256_006$epi$incid.poi[,i] * Mod.noint_256_006$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.noint_256_006$epi$incid.poi[,i])
    noint_256_009.age.inf[i] <- sum((Mod.noint_256_009$epi$incid.poi[,i] * Mod.noint_256_009$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.noint_256_009$epi$incid.poi[,i])
    
    int_256_002.age.inf[i] <- sum((Mod.int_256_002$epi$incid.poi[,i] * Mod.int_256_002$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_256_002$epi$incid.poi[,i])
    int_256_006.age.inf[i] <- sum((Mod.int_256_006$epi$incid.poi[,i] * Mod.int_256_006$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_256_006$epi$incid.poi[,i])
    int_256_009.age.inf[i] <- sum((Mod.int_256_009$epi$incid.poi[,i] * Mod.int_256_009$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_256_009$epi$incid.poi[,i])
    
    int_45_002.age.inf[i] <- sum((Mod.int_45_002$epi$incid.poi[,i] * Mod.int_45_002$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_45_002$epi$incid.poi[,i])
    int_45_006.age.inf[i] <- sum((Mod.int_45_006$epi$incid.poi[,i] * Mod.int_45_006$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_45_006$epi$incid.poi[,i])
    int_45_009.age.inf[i] <- sum((Mod.int_45_009$epi$incid.poi[,i] * Mod.int_45_009$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_45_009$epi$incid.poi[,i])
    
    int_949_002.age.inf[i] <- sum((Mod.int_949_002$epi$incid.poi[,i] * Mod.int_949_002$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_949_002$epi$incid.poi[,i])
    int_949_006.age.inf[i] <- sum((Mod.int_949_006$epi$incid.poi[,i] * Mod.int_949_006$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_949_006$epi$incid.poi[,i])
    int_949_009.age.inf[i] <- sum((Mod.int_949_009$epi$incid.poi[,i] * Mod.int_949_009$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_949_009$epi$incid.poi[,i])
    
    int_100_002.age.inf[i] <- sum((Mod.int_100_002$epi$incid.poi[,i] * Mod.int_100_002$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_100_002$epi$incid.poi[,i])
    int_100_006.age.inf[i] <- sum((Mod.int_100_006$epi$incid.poi[,i] * Mod.int_100_006$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_100_006$epi$incid.poi[,i])
    int_100_009.age.inf[i] <- sum((Mod.int_100_009$epi$incid.poi[,i] * Mod.int_100_009$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_100_009$epi$incid.poi[,i])
    
    
    int_45_002_ab.age.inf[i] <- sum((Mod.int_45_002_ab$epi$incid.poi[,i] * Mod.int_45_002_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_45_002_ab$epi$incid.poi[,i])
    int_45_006_ab.age.inf[i] <- sum((Mod.int_45_006_ab$epi$incid.poi[,i] * Mod.int_45_006_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_45_006_ab$epi$incid.poi[,i])
    int_45_009_ab.age.inf[i] <- sum((Mod.int_45_009_ab$epi$incid.poi[,i] * Mod.int_45_009_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_45_009_ab$epi$incid.poi[,i])
    
    int_949_002_ab.age.inf[i] <- sum((Mod.int_949_002_ab$epi$incid.poi[,i] * Mod.int_949_002_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_949_002_ab$epi$incid.poi[,i])
    int_949_006_ab.age.inf[i] <- sum((Mod.int_949_006_ab$epi$incid.poi[,i] * Mod.int_949_006_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_949_006_ab$epi$incid.poi[,i])
    int_949_009_ab.age.inf[i] <- sum((Mod.int_949_009_ab$epi$incid.poi[,i] * Mod.int_949_009_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_949_009_ab$epi$incid.poi[,i])
    
    int_100_002_ab.age.inf[i] <- sum((Mod.int_100_002_ab$epi$incid.poi[,i] * Mod.int_100_002_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_100_002_ab$epi$incid.poi[,i])
    int_100_006_ab.age.inf[i] <- sum((Mod.int_100_006_ab$epi$incid.poi[,i] * Mod.int_100_006_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_100_006_ab$epi$incid.poi[,i])
    int_100_009_ab.age.inf[i] <- sum((Mod.int_100_009_ab$epi$incid.poi[,i] * Mod.int_100_009_ab$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_100_009_ab$epi$incid.poi[,i])
   
    noint_256_006_l.age.inf[i] <- sum((Mod.noint_256_006_l$epi$incid.poi[,i] * Mod.noint_256_006_l$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.noint_256_006_l$epi$incid.poi[,i]) 
    int_256_006_l.age.inf[i] <- sum((Mod.int_256_006_l$epi$incid.poi[,i] * Mod.int_256_006_l$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_256_006_l$epi$incid.poi[,i]) 
    int_45_006_l.age.inf[i] <- sum((Mod.int_45_006_l$epi$incid.poi[,i] * Mod.int_45_006_l$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_45_006_l$epi$incid.poi[,i]) 
    int_949_006_l.age.inf[i] <- sum((Mod.int_949_006_l$epi$incid.poi[,i] * Mod.int_949_006_l$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_949_006_l$epi$incid.poi[,i]) 
    int_100_006_l.age.inf[i] <- sum((Mod.int_100_006_l$epi$incid.poi[,i] * Mod.int_100_006_l$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int_100_006_l$epi$incid.poi[,i]) 
  }

  
  x<-sort(noint_256_002.age.inf)
  noint_256_002.age.inf_hi <-x[hi.cut]
  noint_256_002.age.inf_low <-x[low.cut]
  noint_256_002.age.inf <- mean(noint_256_002.age.inf)
  
  x<-sort(noint_256_006.age.inf)
  noint_256_006.age.inf_hi <-x[hi.cut]
  noint_256_006.age.inf_low <-x[low.cut]
  noint_256_006.age.inf <- mean(noint_256_006.age.inf)
  
  x<-sort(noint_256_009.age.inf)
  noint_256_009.age.inf_hi <-x[hi.cut]
  noint_256_009.age.inf_low <-x[low.cut]
  noint_256_009.age.inf <- mean(noint_256_009.age.inf)
  
  ## int 25.6
  x<-sort(int_256_002.age.inf)
  int_256_002.age.inf_hi <-x[hi.cut]
  int_256_002.age.inf_low <-x[low.cut]
  int_256_002.age.inf <- mean(int_256_002.age.inf)
  
  x<-sort(int_256_006.age.inf)
  int_256_006.age.inf_hi <-x[hi.cut]
  int_256_006.age.inf_low <-x[low.cut]
  int_256_006.age.inf <- mean(int_256_006.age.inf)
  
  x<-sort(int_256_009.age.inf)
  int_256_009.age.inf_hi <-x[hi.cut]
  int_256_009.age.inf_low <-x[low.cut]
  int_256_009.age.inf <- mean(int_256_009.age.inf)
  
  #Int 45
  x<-sort(int_45_002.age.inf)
  int_45_002.age.inf_hi <-x[hi.cut]
  int_45_002.age.inf_low <-x[low.cut]
  int_45_002.age.inf <- mean(int_45_002.age.inf)
  
  x<-sort(int_45_006.age.inf)
  int_45_006.age.inf_hi <-x[hi.cut]
  int_45_006.age.inf_low <-x[low.cut]
  int_45_006.age.inf <- mean(int_45_006.age.inf)
  
  x<-sort(int_45_009.age.inf)
  int_45_009.age.inf_hi <-x[hi.cut]
  int_45_009.age.inf_low <-x[low.cut]
  int_45_009.age.inf <- mean(int_45_009.age.inf)
  
  #INT 90
  x<-sort(int_949_002.age.inf)
  int_949_002.age.inf_hi <-x[hi.cut]
  int_949_002.age.inf_low <-x[low.cut]
  int_949_002.age.inf <- mean(int_949_002.age.inf)
  
  x<-sort(int_949_006.age.inf)
  int_949_006.age.inf_hi <-x[hi.cut]
  int_949_006.age.inf_low <-x[low.cut]
  int_949_006.age.inf <- mean(int_949_006.age.inf)
  
  x<-sort(int_949_009.age.inf)
  int_949_009.age.inf_hi <-x[hi.cut]
  int_949_009.age.inf_low <-x[low.cut]
  int_949_009.age.inf <- mean(int_949_009.age.inf)
  
  #INT 100
  x<-sort(int_100_002.age.inf)
  int_100_002.age.inf_hi <-x[hi.cut]
  int_100_002.age.inf_low <-x[low.cut]
  int_100_002.age.inf <- mean(int_100_002.age.inf)
  
  x<-sort(int_100_006.age.inf)
  int_100_006.age.inf_hi <-x[hi.cut]
  int_100_006.age.inf_low <-x[low.cut]
  int_100_006.age.inf <- mean(int_100_006.age.inf)
  
  x<-sort(int_100_009.age.inf)
  int_100_009.age.inf_hi <-x[hi.cut]
  int_100_009.age.inf_low <-x[low.cut]
  int_100_009.age.inf <- mean(int_100_009.age.inf)
  

  ##AB ONLY int
  #Int 45
  x<-sort(int_45_002_ab.age.inf)
  int_45_002_ab.age.inf_hi <-x[hi.cut]
  int_45_002_ab.age.inf_low <-x[low.cut]
  int_45_002_ab.age.inf <- mean(int_45_002_ab.age.inf)
  
  x<-sort(int_45_006_ab.age.inf)
  int_45_006_ab.age.inf_hi <-x[hi.cut]
  int_45_006_ab.age.inf_low <-x[low.cut]
  int_45_006_ab.age.inf <- mean(int_45_006_ab.age.inf)
  
  x<-sort(int_45_009_ab.age.inf)
  int_45_009_ab.age.inf_hi <-x[hi.cut]
  int_45_009_ab.age.inf_low <-x[low.cut]
  int_45_009_ab.age.inf <- mean(int_45_009_ab.age.inf)
  
  #INT 90
  x<-sort(int_949_002_ab.age.inf)
  int_949_002_ab.age.inf_hi <-x[hi.cut]
  int_949_002_ab.age.inf_low <-x[low.cut]
  int_949_002_ab.age.inf <- mean(int_949_002_ab.age.inf)
  
  x<-sort(int_949_006_ab.age.inf)
  int_949_006_ab.age.inf_hi <-x[hi.cut]
  int_949_006_ab.age.inf_low <-x[low.cut]
  int_949_006_ab.age.inf <- mean(int_949_006_ab.age.inf)
  
  x<-sort(int_949_009_ab.age.inf)
  int_949_009_ab.age.inf_hi <-x[hi.cut]
  int_949_009_ab.age.inf_low <-x[low.cut]
  int_949_009_ab.age.inf <- mean(int_949_009_ab.age.inf)
  
  #INT 100
  x<-sort(int_100_002_ab.age.inf)
  int_100_002_ab.age.inf_hi <-x[hi.cut]
  int_100_002_ab.age.inf_low <-x[low.cut]
  int_100_002_ab.age.inf <- mean(int_100_002_ab.age.inf)
  
  x<-sort(int_100_006_ab.age.inf)
  int_100_006_ab.age.inf_hi <-x[hi.cut]
  int_100_006_ab.age.inf_low <-x[low.cut]
  int_100_006_ab.age.inf <- mean(int_100_006_ab.age.inf)
  
  x<-sort(int_100_009_ab.age.inf)
  int_100_009_ab.age.inf_hi <-x[hi.cut]
  int_100_009_ab.age.inf_low <-x[low.cut]
  int_100_009_ab.age.inf <- mean(int_100_009_ab.age.inf)
  
  #LOWER AHI SEEKING PROB
  
  x<-sort(noint_256_006_l.age.inf)
  noint_256_006_l.age.inf_hi <-x[hi.cut]
  noint_256_006_l.age.inf_low <-x[low.cut]
  noint_256_006_l.age.inf <- mean(noint_256_006_l.age.inf)
  
  x<-sort(int_256_006_l.age.inf)
  int_256_006_l.age.inf_hi <-x[hi.cut]
  int_256_006_l.age.inf_low <-x[low.cut]
  int_256_006_l.age.inf <- mean(int_256_006_l.age.inf)
  
  x<-sort(int_45_006_l.age.inf)
  int_45_006_l.age.inf_hi <-x[hi.cut]
  int_45_006_l.age.inf_low <-x[low.cut]
  int_45_006_l.age.inf <- mean(int_45_006_l.age.inf)
  
  x<-sort(int_949_006_l.age.inf)
  int_949_006_l.age.inf_hi <-x[hi.cut]
  int_949_006_l.age.inf_low <-x[low.cut]
  int_949_006_l.age.inf <- mean(int_949_006_l.age.inf)
  
  x<-sort(int_100_006_l.age.inf)
  int_100_006_l.age.inf_hi <-x[hi.cut]
  int_100_006_l.age.inf_low <-x[low.cut]
  int_100_006_l.age.inf <- mean(int_100_006_l.age.inf)
  
  x<-sort(as.numeric(Mod.noint_256_002$epi$prev.poi[521,1:nsims]))
  noint_256_002.fprev.list <- x * 100
  noint_256_002.fprev <-mean(x) * 100
  noint_256_002.fprev_hi <-x[hi.cut] * 100
  noint_256_002.fprev_low <-x[low.cut] * 100
  
  #"noint_256_006", 
  x<-sort(as.numeric(Mod.noint_256_006$epi$prev.poi[521,1:nsims]))
  noint_256_006.fprev.list <- x * 100
  noint_256_006.fprev <-mean(x) * 100
  noint_256_006.fprev_hi <-x[hi.cut] * 100
  noint_256_006.fprev_low <-x[low.cut] * 100
  
  #"noint_256_009", 
  x<-sort(as.numeric(Mod.noint_256_009$epi$prev.poi[521,1:nsims]))
  noint_256_009.fprev <-mean(x) * 100
  noint_256_009.fprev.list <- x * 100
  noint_256_009.fprev_hi <-x[hi.cut] * 100
  noint_256_009.fprev_low <-x[low.cut] * 100
  
  #"int_256_002", 
  x<-sort(as.numeric(Mod.int_256_002$epi$prev.poi[521,1:nsims]))
  int_256_002.fprev.list <- x * 100
  int_256_002.fprev <-mean(x) * 100
  int_256_002.fprev_hi <-x[hi.cut] * 100
  int_256_002.fprev_low <-x[low.cut] * 100
  
  #"int_256_006", 
  x<-sort(as.numeric(Mod.int_256_006$epi$prev.poi[521,1:nsims]))
  int_256_006.fprev.list <- x * 100
  int_256_006.fprev <-mean(x) * 100
  int_256_006.fprev_hi <-x[hi.cut] * 100
  int_256_006.fprev_low <-x[low.cut] * 100
  
  #int_256_009
  x<-sort(as.numeric(Mod.int_256_009$epi$prev.poi[521,1:nsims]))
  int_256_009.fprev.list <- x * 100
  int_256_009.fprev <-mean(x) * 100
  int_256_009.fprev_hi <-x[hi.cut] * 100
  int_256_009.fprev_low <-x[low.cut] * 100
  
  #int_45_002
  x<-sort(as.numeric(Mod.int_45_002$epi$prev.poi[521,1:nsims]))
  int_45_002.fprev.list <- x * 100
  int_45_002.fprev <-mean(x) * 100
  int_45_002.fprev_hi <-x[hi.cut] * 100
  int_45_002.fprev_low <-x[low.cut] * 100
  
  #int_45_006
  x<-sort(as.numeric(Mod.int_45_006$epi$prev.poi[521,1:nsims]))
  int_45_006.fprev.list <-x * 100
  int_45_006.fprev <-mean(x) * 100
  int_45_006.fprev_hi <-x[hi.cut] * 100
  int_45_006.fprev_low <-x[low.cut] * 100
  
  #int_45_009
  x<-sort(as.numeric(Mod.int_45_009$epi$prev.poi[521,1:nsims]))
  int_45_009.fprev.list <- x * 100
  int_45_009.fprev <-mean(x) * 100
  int_45_009.fprev_hi <-x[hi.cut] * 100
  int_45_009.fprev_low <-x[low.cut] * 100
  
  #int_949_002
  x<-sort(as.numeric(Mod.int_949_002$epi$prev.poi[521,1:nsims]))
  int_949_002.fprev.list <- x * 100
  int_949_002.fprev <-mean(x) * 100
  int_949_002.fprev_hi <-x[hi.cut] * 100
  int_949_002.fprev_low <-x[low.cut] * 100
  
  #int_949_006
  x<-sort(as.numeric(Mod.int_949_006$epi$prev.poi[521,1:nsims]))
  int_949_006.fprev.list <- x * 100
  int_949_006.fprev <-mean(x) * 100
  int_949_006.fprev_hi <-x[hi.cut] * 100
  int_949_006.fprev_low <-x[low.cut] * 100
  
  #int_949_009
  x<-sort(as.numeric(Mod.int_949_009$epi$prev.poi[521,1:nsims]))
  int_949_009.fprev.list <- x * 100
  int_949_009.fprev <-mean(x) * 100
  int_949_009.fprev_hi <-x[hi.cut] * 100
  int_949_009.fprev_low <-x[low.cut] * 100
  
  #int_100_002
  x<-sort(as.numeric(Mod.int_100_002$epi$prev.poi[521,1:nsims]))
  int_100_002.fprev.list <- x * 100
  int_100_002.fprev <-mean(x) * 100
  int_100_002.fprev_hi <-x[hi.cut] * 100
  int_100_002.fprev_low <-x[low.cut] * 100
  
  #int_100_006
  x<-sort(as.numeric(Mod.int_100_006$epi$prev.poi[521,1:nsims]))
  int_100_006.fprev.list <- x * 100
  int_100_006.fprev <-mean(x) * 100
  int_100_006.fprev_hi <-x[hi.cut] * 100
  int_100_006.fprev_low <-x[low.cut] * 100
  
  #int_100_009
  x<-sort(as.numeric(Mod.int_100_009$epi$prev.poi[521,1:nsims]))
  int_100_009.fprev.list <- x * 100
  int_100_009.fprev <-mean(x) * 100
  int_100_009.fprev_hi <-x[hi.cut] * 100
  int_100_009.fprev_low <-x[low.cut] * 100
  
  ##AB ONLY
  
  #int_45_002_ab
  x<-sort(as.numeric(Mod.int_45_002_ab$epi$prev.poi[521,1:nsims]))
  int_45_002_ab.fprev.list <- x * 100
  int_45_002_ab.fprev <-mean(x) * 100
  int_45_002_ab.fprev_hi <-x[hi.cut] * 100
  int_45_002_ab.fprev_low <-x[low.cut] * 100
  
  #int_45_006
  x<-sort(as.numeric(Mod.int_45_006_ab$epi$prev.poi[521,1:nsims]))
  int_45_006_ab.fprev.list <- x * 100
  int_45_006_ab.fprev <-mean(x) * 100
  int_45_006_ab.fprev_hi <-x[hi.cut] * 100
  int_45_006_ab.fprev_low <-x[low.cut] * 100
  
  #int_45_009
  x<-sort(as.numeric(Mod.int_45_009_ab$epi$prev.poi[521,1:nsims]))
  int_45_009_ab.fprev.list <- x * 100
  int_45_009_ab.fprev <-mean(x) * 100
  int_45_009_ab.fprev_hi <-x[hi.cut] * 100
  int_45_009_ab.fprev_low <-x[low.cut] * 100
  
  #int_949_002
  x<-sort(as.numeric(Mod.int_949_002_ab$epi$prev.poi[521,1:nsims]))
  int_949_002_ab.fprev.list <- x * 100
  int_949_002_ab.fprev <-mean(x) * 100
  int_949_002_ab.fprev_hi <-x[hi.cut] * 100
  int_949_002_ab.fprev_low <-x[low.cut] * 100
  
  #int_949_006
  x<-sort(as.numeric(Mod.int_949_006_ab$epi$prev.poi[521,1:nsims]))
  int_949_006_ab.fprev.list <- x * 100
  int_949_006_ab.fprev <-mean(x) * 100
  int_949_006_ab.fprev_hi <-x[hi.cut] * 100
  int_949_006_ab.fprev_low <-x[low.cut] * 100
  
  
  #int_949_009
  x<-sort(as.numeric(Mod.int_949_009_ab$epi$prev.poi[521,1:nsims]))
  int_949_009_ab.fprev.list <- x * 100
  int_949_009_ab.fprev <-mean(x) * 100
  int_949_009_ab.fprev_hi <-x[hi.cut] * 100
  int_949_009_ab.fprev_low <-x[low.cut] * 100
  
  #int_100_002
  x<-sort(as.numeric(Mod.int_100_002_ab$epi$prev.poi[521,1:nsims]))
  int_100_002_ab.fprev.list <- x * 100
  int_100_002_ab.fprev <-mean(x) * 100
  int_100_002_ab.fprev_hi <-x[hi.cut] * 100
  int_100_002_ab.fprev_low <-x[low.cut] * 100
  
  #int_100_006
  x<-sort(as.numeric(Mod.int_100_006_ab$epi$prev.poi[521,1:nsims]))
  int_100_006_ab.fprev.list <- x * 100
  int_100_006_ab.fprev <-mean(x) * 100
  int_100_006_ab.fprev_hi <-x[hi.cut] * 100
  int_100_006_ab.fprev_low <-x[low.cut] * 100
  
  #int_100_009
  x<-sort(as.numeric(Mod.int_100_009_ab$epi$prev.poi[521,1:nsims]))
  int_100_009_ab.fprev.list <- x * 100
  int_100_009_ab.fprev <-mean(x) * 100
  int_100_009_ab.fprev_hi <-x[hi.cut] * 100
  int_100_009_ab.fprev_low <-x[low.cut] * 100
  
  ##LOWER AHI SEEKKING PROBABILITY
  
  x<-sort(as.numeric(Mod.noint_256_006_l$epi$prev.poi[521,1:nsims]))
  noint_256_006_l.fprev.list <-x * 100
  noint_256_006_l.fprev <-mean(x) * 100
  noint_256_006_l.fprev_hi <-x[hi.cut] * 100
  noint_256_006_l.fprev_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_256_006_l$epi$prev.poi[521,1:nsims]))
  int_256_006_l.fprev.list <- x * 100
  int_256_006_l.fprev <-mean(x) * 100
  int_256_006_l.fprev_hi <-x[hi.cut] * 100
  int_256_006_l.fprev_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_45_006_l$epi$prev.poi[521,1:nsims]))
  int_45_006_l.fprev.list <- x * 100
  int_45_006_l.fprev <-mean(x) * 100
  int_45_006_l.fprev_hi <-x[hi.cut] * 100
  int_45_006_l.fprev_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_949_006_l$epi$prev.poi[521,1:nsims]))
  int_949_006_l.fprev.list <- x * 100
  int_949_006_l.fprev <-mean(x) * 100
  int_949_006_l.fprev_hi <-x[hi.cut] * 100
  int_949_006_l.fprev_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int_100_006_l$epi$prev.poi[521,1:nsims]))
  int_100_006_l.fprev.list <- x * 100
  int_100_006_l.fprev <-mean(x) * 100
  int_100_006_l.fprev_hi <-x[hi.cut] * 100
  int_100_006_l.fprev_low <-x[low.cut] * 100
  
  
  
  #TABLE of point estimates
  

  pct.diag <- pct.diag.low <- pct.diag.hi <-rep(NA,length(scenarios))
  pct.txt <- pct.txt.low <- pct.txt.hi <-rep(NA,length(scenarios))
  pct.vsup <- pct.vsup.low <- pct.vsup.hi <-rep(NA,length(scenarios))
  pct.test.ly <- pct.test.ly.low <- pct.test.ly.hi <-rep(NA,length(scenarios))
  age.inf <- age.inf.low <- age.inf.hi <-rep(NA,length(scenarios))
  fprev <- fprev.low <- fprev.hi <-rep(NA,length(scenarios))
  time.inf.diag <- time.inf.diag.low <- time.inf.diag.hi <- rep(NA,length(scenarios))
  

  
  for (i in 1:length(scenarios)){
    
    
    fn <- paste0(scenarios[i], ".pct.diag")
    x  <- get(fn) 
    pct.diag[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.diag_low")
    x  <- get(fn) 
    pct.diag.low[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.diag_hi")
    x  <- get(fn) 
    pct.diag.hi[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.txt")
    x  <- get(fn) 
    pct.txt[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.txt_low")
    x  <- get(fn) 
    pct.txt.low[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.txt_hi")
    x  <- get(fn) 
    pct.txt.hi[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.vsup")
    x  <- get(fn) 
    pct.vsup[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.vsup_low")
    x  <- get(fn) 
    pct.vsup.low[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.vsup_hi")
    x  <- get(fn) 
    pct.vsup.hi[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.test.ly")
    x  <- get(fn) 
    pct.test.ly[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.test.ly_low")
    x  <- get(fn) 
    pct.test.ly.low[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".pct.test.ly_hi")
    x  <- get(fn) 
    pct.test.ly.hi[i] <- tail(x,1)

    
    fn <- paste0(scenarios[i], ".age.inf")
    x  <- get(fn) 
    age.inf[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".age.inf_low")
    x  <- get(fn) 
    age.inf.low[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".age.inf_hi")
    x  <- get(fn) 
    age.inf.hi[i] <- tail(x,1)
    
    
    fn <- paste0(scenarios[i], ".fprev")
    x  <- get(fn) 
    fprev[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".fprev_low")
    x  <- get(fn) 
    fprev.low[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".fprev_hi")
    x  <- get(fn) 
    fprev.hi[i] <- tail(x,1)
    
    
    fn <- paste0(scenarios[i], ".time.inf.diag")
    x  <- get(fn) 
    time.inf.diag[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".time.inf.diag_low")
    x  <- get(fn) 
    time.inf.diag.low[i] <- tail(x,1)
    
    fn <- paste0(scenarios[i], ".time.inf.diag_hi")
    x  <- get(fn) 
    time.inf.diag.hi[i] <- tail(x,1)
    
    
      }
  
  
  
  table2<-cbind(scenarios,
               pct.diag, pct.diag.low, pct.diag.hi,
               pct.txt, pct.txt.low, pct.txt.hi,
               pct.vsup, pct.vsup.low, pct.vsup.hi,
               pct.test.ly, pct.test.ly.low, pct.test.ly.hi,
               age.inf, age.inf.low, age.inf.hi,
               fprev, fprev.low, fprev.hi,
               time.inf.diag, time.inf.diag.low, time.inf.diag.hi)
  table2
  

  write.xlsx(x = table2, file = "out/KTM.int.results2.xlsx",
             sheetName = "KTM intervention", row.names = FALSE)

##Plots

  
#Prevalence
tiff(filename = "out/Figure 1 prevalence.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,5,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(5,7,.2)
plot(noint_256_006.prev, type = "n", ylim = c(5, 7), lwd = 3, col="black", axes=FALSE,
     xlab = "Years", ylab = "HIV prevalence",  cex.main=.8)
axis(1, at = xticks, labels = c("0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.8)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.8)

xx <- c(1:(length(noint_256_006.prev)), (length(noint_256_006.prev)):1)
yy <- c(noint_256_006.prev_low, rev(noint_256_006.prev_hi))
polygon(xx, yy, col = adjustcolor("red", alpha = 0.2), border = NA)
lines(noint_256_006.prev, lwd = 1, col = "red")

xx <- c(1:(length(int_256_006.prev)), (length(int_256_006.prev)):1)
yy <- c(int_256_006.prev_low, rev(int_256_006.prev_hi))
polygon(xx, yy, col = adjustcolor("blue", alpha = 0.2), border = NA)
lines(int_256_006.prev, lwd = 1, col = "blue")

xx <- c(1:(length(int_45_006.prev)), (length(int_45_006.prev)):1)
yy <- c(int_45_006.prev_low, rev(int_45_006.prev_hi))
polygon(xx, yy, col = adjustcolor("green", alpha = 0.2), border = NA)
lines(int_45_006.prev, lwd = 1, col = "green")

xx <- c(1:(length(int_949_006.prev)), (length(int_949_006.prev)):1)
yy <- c(int_949_006.prev_low, rev(int_949_006.prev_hi))
polygon(xx, yy, col = adjustcolor("yellow", alpha = 0.2), border = NA)
lines(int_949_006.prev, lwd = 1, col = "yellow")



legend("bottomleft", c("Standard PITC","+ RNA tests", "+ RNA tests & 45% tested","+ RNA tests & 94.9% tested"), 
       col = c("red", "blue" , "green" , "yellow"),lty=1, cex = .8)

dev.off()

##Incidence rate moving average


#####  MOVING AVERAGES NO SI   ####################

tiff(filename = "out/incidence moving average.tiff", height = 7, width = 7, units = "in", res = 250)

noint_256_006.ir100_ma <- rep(NA,468)
int_256_006.ir100_ma <- rep(NA,468)
int_45_006.ir100_ma <- rep(NA,468)
int_949_006.ir100_ma <- rep(NA,468)

for (i in 1:468){
  
  noint_256_006.ir100_ma[i] <-mean(noint_256_006.ir100[i+26:(i+52)],na.rm = TRUE)
  int_256_006.ir100_ma[i] <-mean(int_256_006.ir100[i+26:(i+52)],na.rm = TRUE)
  int_45_006.ir100_ma[i] <-mean(int_45_006.ir100[i+26:(i+52)],na.rm = TRUE)
  int_949_006.ir100_ma[i] <-mean(int_949_006.ir100[i+26:(i+52)],na.rm = TRUE)
}

par(mfrow = c(1,1), mar = c(5,5,3,3), mgp = c(2,1,0))
xticks <- seq(0, 468, 52)
yticks <- seq(.54,.7,.02)
plot(noint_256_006.ir100_ma, type = "n", ylim = c(.54, .7), lwd = 3, col="black", axes=FALSE,
     main = "HIV Incidence per 100 person years at risk (6 month rolling average)", xlab = "Years", ylab = " ",  cex.main=1)
axis(1, at = xticks, labels = c("1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=1)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=1)

lines(noint_256_006.ir100_ma, lwd = 1, col = "black")
lines(int_256_006.ir100_ma, lwd = 1, col = "blue")
lines(int_45_006.ir100_ma, lwd = 1, col = "green")
lines(int_949_006.ir100_ma, lwd = 1, col = "yellow")

legend("bottomleft", c("Standard PITC","+ RNA tests", "+ RNA tests & 45% tested","+ RNA tests & 94.9% tested"), 
       col = c("black", "blue" , "green" , "yellow"),lty=1, cex = .8)

dev.off()




####  NIA AND PIA PLOTS.

library(wesanderson)
pal <- wes_palette("Zissou")[c(1, 9)]


PIA<-cbind(int_256_006.PIA.list, int_45_006.PIA.list, int_949_006.PIA.list, int_100_006.PIA.list)

NIA<-cbind(int_256_006.NIA.100K.list, int_45_006.NIA.100K.list, int_949_006.NIA.100K.list, int_100_006.NIA.100K.list)

tiff(filename = "out/PIA_NIA.tiff", height = 4, width = 8.5, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)

boxplot(PIA, outline = FALSE, medlwd = 1.1, ylim = c(-30,40),
        col = c(rep(8, 2)),
        names=c("25.6%", "45%", "94.9%", "100%"),las=1,
        main = "Percent of infections averted",
        ylab = "Percent of infections averted",
        xlab = "Eligible patients receiving TMP")

boxplot(NIA, outline = FALSE, medlwd = 1.1, ylim = c(-150,250),
        col = c(rep(8, 2)),
        names=c("25.6%", "45%", "94.9%", "100%"),las=1,
        main = "Number of infections averted per 100K person years at risk",
        ylab = "Number of infections averted",
        xlab = "Eligible patients receiving TMP")
dev.off()

##95 95 95 fractions

diag.pct<-cbind(noint_256_006.pct.diag.list, int_256_006.pct.diag.list, int_45_006.pct.diag.list, int_949_006.pct.diag.list, int_100_006.pct.diag.list)
txt.pct<-cbind(noint_256_006.pct.txt.list, int_256_006.pct.txt.list, int_45_006.pct.txt.list, int_949_006.pct.txt.list, int_100_006.pct.txt.list)
vsup.pct<-cbind(noint_256_006.pct.vsup.list, int_256_006.pct.vsup.list, int_45_006.pct.vsup.list, int_949_006.pct.vsup.list, int_100_006.pct.vsup.list)

tiff(filename = "out/cascade.tiff", height = 4, width = 8.5, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)

boxplot(diag.pct, outline = FALSE, medlwd = 1.1, ylim = c(60,100),
        col = c(rep(8, 2)),
        names=c("0%", "25.6%", "45%", "94.9%", "100%"),las=.9,
        main = "Percent diagnosed",
        ylab = "Percent diagnosed",
        xlab = "Eligible patients receiving TMP")

boxplot(txt.pct, outline = FALSE, medlwd = 1.1, ylim = c(60,100),
        col = c(rep(8, 2)),
        names=c("0%", "25.6%", "45%", "94.9%", "100%"),las=.9,
        main = "Percent of diagnosed on treatment",
        ylab = "Percent on treatment",
        xlab = "Eligible patients receiving TMP")

#boxplot(vsup.pct, outline = FALSE, medlwd = 1.1, ylim = c(60,100),
#        col = c(rep(8, 2)),
#        names=c("0%", "25.6%", "45%", "94.9%", "100%"),las=.9,
#        main = "Percent virally suppressed",
#        ylab = "Percent suppressed",
#        xlab = "Eligible patients receiving TMP")
dev.off()


#Sensitivity analyses



##Figure 3 by age and duration for sexually experience as a heatmap.

# PIA<-cbind(int_256_009.52.PIA,int_45_002.52.PIA,int_45_006.52.PIA,int_45_009.52.PIA,int_949_002.52.PIA,int_949_006.52.PIA,
#            int_256_009.104.PIA,int_45_002.104.PIA,int_45_006.104.PIA,int_45_009.104.PIA,int_949_002.104.PIA,int_949_006.104.PIA,
#            int_256_009.156.PIA,int_45_002.156.PIA,int_45_006.156.PIA,int_45_009.156.PIA,int_949_002.156.PIA,int_949_006.156.PIA,
#            int_256_009.208.PIA,int_45_002.208.PIA,int_45_006.208.PIA,int_45_009.208.PIA,int_949_002.208.PIA,int_949_006.208.PIA,
#            int_256_009.260.PIA,int_45_002.260.PIA,int_45_006.260.PIA,int_45_009.260.PIA,int_949_002.260.PIA,int_949_006.260.PIA)
# 
# 
# ages <- c(13, 14, 15, 16, 17, 18,
#           13, 14, 15, 16, 17, 18,
#           13, 14, 15, 16, 17, 18,
#           13, 14, 15, 16, 17, 18,
#           13, 14, 15, 16, 17, 18)
# 
# durations <- c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6))
# 
# 
# library("plot3D")
# x <- ages 
# y <- durations 
# z <- as.vector(PIA)
# 
# x2 <- unique(ages) 
# y2 <- as.vector(unique(durations)) 
# z2 <- matrix(PIA, nrow=6, ncol=5)
# 
# data <- data.frame(cbind(x,y,z))
# 
# tiff(filename = "cond/out/Figure 3 PIA by age and duration.tiff", height = 4, width = 8, units = "in", res = 250)
# par(mar = c(3,3,2,1), mgp = c(2,1,0), mfrow = c(1,1))
# x2 <- unique(ages) 
# y2 <- as.vector(unique(durations)) 
# z2 <- matrix(PIA, nrow=6, ncol=5)
# image2D(z2, x2, y2, xlab = "Age at intervention delivery", ylab = "Duration of effect (years)", 
#         clab = "PIA",
#         rasterImage=TRUE)
# dev.off()
# 
# 
# 
# #scatter3D(x, y, z, phi = 20, theta = 30, bty = "g", type = "h", ticktype = "detailed", pch = 19, cex = 0.5, xlab = "Age", ylab = "Duration", 
# #          zlab = "Percent of infections averted")
# 
# 
# #hist3D(x2, y2, z2, phi = 60, theta = 40, bty = "g", type = "h", ticktype = "detailed", pch = 19, cex = 0.5, xlab = "Age", ylab = "Duration", 
# #          zlab = "Percent of infections averted")









