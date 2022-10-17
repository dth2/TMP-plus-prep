# Process KTM with PrEP intervention output.

library(EpiModelHPC)
setwd("/homes/dth2/kenyaTM/PREPint")


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


scenarios <- c(
  "noint", 
  "int_pst_.1_risk_w0",
  "int_pst_.2_risk_w0",
  "int_pst_.3_risk_w0",
  "int_pst_.4_risk_w0",
  "int_pst_.5_risk_w0",
  "int_pst_.6_risk_w0",
  "int_pst_.7_risk_w0",
  "int_pst_.8_risk_w0",
  "int_pst_.9_risk_w0",
  "int_pst_1_risk_w0",
  "int_pst_.1_risk_w12",
  "int_pst_.2_risk_w12",
  "int_pst_.3_risk_w12",
  "int_pst_.4_risk_w12",
  "int_pst_.5_risk_w12",
  "int_pst_.6_risk_w12",
  "int_pst_.7_risk_w12",
  "int_pst_.8_risk_w12",
  "int_pst_.9_risk_w12",
  "int_pst_1_risk_w12",
  "int_pst_.1_risk_w26",
  "int_pst_.2_risk_w26",
  "int_pst_.3_risk_w26",
  "int_pst_.4_risk_w26",
  "int_pst_.5_risk_w26",
  "int_pst_.6_risk_w26",
  "int_pst_.7_risk_w26",
  "int_pst_.8_risk_w26",
  "int_pst_.9_risk_w26",
  "int_pst_1_risk_w26",
  "int_pst_.1_risk_w52",
  "int_pst_.2_risk_w52",
  "int_pst_.3_risk_w52",
  "int_pst_.4_risk_w52",
  "int_pst_.5_risk_w52",
  "int_pst_.6_risk_w52",
  "int_pst_.7_risk_w52",
  "int_pst_.8_risk_w52",
  "int_pst_.9_risk_w52",
  "int_pst_1_risk_w52",
  "int_pst_.1_all",
  "int_pst_.2_all",
  "int_pst_.3_all",
  "int_pst_.4_all",
  "int_pst_.5_all",
  "int_pst_.6_all",
  "int_pst_.7_all",
  "int_pst_.8_all",
  "int_pst_.9_all",
  "int_pst_1_all",
  "int_pst_.1_disc",
  "int_pst_.2_disc",
  "int_pst_.3_disc",
  "int_pst_.4_disc",
  "int_pst_.5_disc",
  "int_pst_.6_disc",
  "int_pst_.7_disc",
  "int_pst_.8_disc",
  "int_pst_.9_disc",
  "int_pst_1_disc",
  "int_pst_.1_disc_risk_w0",
  "int_pst_.2_disc_risk_w0",
  "int_pst_.3_disc_risk_w0",
  "int_pst_.4_disc_risk_w0",
  "int_pst_.5_disc_risk_w0",
  "int_pst_.6_disc_risk_w0",
  "int_pst_.7_disc_risk_w0",
  "int_pst_.8_disc_risk_w0",
  "int_pst_.9_disc_risk_w0",
  "int_pst_1_disc_risk_w0",
  "int_pst_.1_disc_risk_w12",
  "int_pst_.2_disc_risk_w12",
  "int_pst_.3_disc_risk_w12",
  "int_pst_.4_disc_risk_w12",
  "int_pst_.5_disc_risk_w12",
  "int_pst_.6_disc_risk_w12",
  "int_pst_.7_disc_risk_w12",
  "int_pst_.8_disc_risk_w12",
  "int_pst_.9_disc_risk_w12",
  "int_pst_1_disc_risk_w12",
  "int_pst_.1_disc_risk_w26",
  "int_pst_.2_disc_risk_w26",
  "int_pst_.3_disc_risk_w26",
  "int_pst_.4_disc_risk_w26",
  "int_pst_.5_disc_risk_w26",
  "int_pst_.6_disc_risk_w26",
  "int_pst_.7_disc_risk_w26",
  "int_pst_.8_disc_risk_w26",
  "int_pst_.9_disc_risk_w26",
  "int_pst_1_disc_risk_w26",
  "int_pst_.1_disc_risk_w52",
  "int_pst_.2_disc_risk_w52",
  "int_pst_.3_disc_risk_w52",
  "int_pst_.4_disc_risk_w52",
  "int_pst_.5_disc_risk_w52",
  "int_pst_.6_disc_risk_w52",
  "int_pst_.7_disc_risk_w52",
  "int_pst_.8_disc_risk_w52",
  "int_pst_.9_disc_risk_w52",
  "int_pst_1_disc_risk_w52",
  "int_pst_.1_ps",
  "int_pst_.2_ps",
  "int_pst_.3_ps",
  "int_pst_.4_ps",
  "int_pst_.5_ps",
  "int_pst_.6_ps",
  "int_pst_.7_ps",
  "int_pst_.8_ps",
  "int_pst_.9_ps",
  "int_pst_1_ps",
  "int_pst_.1_ps_risk_w0",
  "int_pst_.2_ps_risk_w0",
  "int_pst_.3_ps_risk_w0",
  "int_pst_.4_ps_risk_w0",
  "int_pst_.5_ps_risk_w0",
  "int_pst_.6_ps_risk_w0",
  "int_pst_.7_ps_risk_w0",
  "int_pst_.8_ps_risk_w0",
  "int_pst_.9_ps_risk_w0",
  "int_pst_1_ps_risk_w0",
  "int_pst_.1_ps_risk_w12",
  "int_pst_.2_ps_risk_w12",
  "int_pst_.3_ps_risk_w12",
  "int_pst_.4_ps_risk_w12",
  "int_pst_.5_ps_risk_w12",
  "int_pst_.6_ps_risk_w12",
  "int_pst_.7_ps_risk_w12",
  "int_pst_.8_ps_risk_w12",
  "int_pst_.9_ps_risk_w12",
  "int_pst_1_ps_risk_w12",
  "int_pst_.1_ps_risk_w26",
  "int_pst_.2_ps_risk_w26",
  "int_pst_.3_ps_risk_w26",
  "int_pst_.4_ps_risk_w26",
  "int_pst_.5_ps_risk_w26",
  "int_pst_.6_ps_risk_w26",
  "int_pst_.7_ps_risk_w26",
  "int_pst_.8_ps_risk_w26",
  "int_pst_.9_ps_risk_w26",
  "int_pst_1_ps_risk_w26",
  "int_pst_.1_ps_risk_w52",
  "int_pst_.2_ps_risk_w52",
  "int_pst_.3_ps_risk_w52",
  "int_pst_.4_ps_risk_w52",
  "int_pst_.5_ps_risk_w52",
  "int_pst_.6_ps_risk_w52",
  "int_pst_.7_ps_risk_w52",
  "int_pst_.8_ps_risk_w52",
  "int_pst_.9_ps_risk_w52",
  "int_pst_1_ps_risk_w52",
  "int_pst_.1_ps_all",
  "int_pst_.2_ps_all",
  "int_pst_.3_ps_all",
  "int_pst_.4_ps_all",
  "int_pst_.5_ps_all",
  "int_pst_.6_ps_all",
  "int_pst_.7_ps_all",
  "int_pst_.8_ps_all",
  "int_pst_.9_ps_all",
  "int_pst_1_ps_all",
  "int_pst_.1_ps_disc",
  "int_pst_.2_ps_disc",
  "int_pst_.3_ps_disc",
  "int_pst_.4_ps_disc",
  "int_pst_.5_ps_disc",
  "int_pst_.6_ps_disc",
  "int_pst_.7_ps_disc",
  "int_pst_.8_ps_disc",
  "int_pst_.9_ps_disc",
  "int_pst_1_ps_disc",
  "int_pst_.1_ps_disc_risk_w0",
  "int_pst_.2_ps_disc_risk_w0",
  "int_pst_.3_ps_disc_risk_w0",
  "int_pst_.4_ps_disc_risk_w0",
  "int_pst_.5_ps_disc_risk_w0",
  "int_pst_.6_ps_disc_risk_w0",
  "int_pst_.7_ps_disc_risk_w0",
  "int_pst_.8_ps_disc_risk_w0",
  "int_pst_.9_ps_disc_risk_w0",
  "int_pst_1_ps_disc_risk_w0",
  "int_pst_.1_ps_disc_risk_w12",
  "int_pst_.2_ps_disc_risk_w12",
  "int_pst_.3_ps_disc_risk_w12",
  "int_pst_.4_ps_disc_risk_w12",
  "int_pst_.5_ps_disc_risk_w12",
  "int_pst_.6_ps_disc_risk_w12",
  "int_pst_.7_ps_disc_risk_w12",
  "int_pst_.8_ps_disc_risk_w12",
  "int_pst_.9_ps_disc_risk_w12",
  "int_pst_1_ps_disc_risk_w12",
  "int_pst_.1_ps_disc_risk_w26",
  "int_pst_.2_ps_disc_risk_w26",
  "int_pst_.3_ps_disc_risk_w26",
  "int_pst_.4_ps_disc_risk_w26",
  "int_pst_.5_ps_disc_risk_w26",
  "int_pst_.6_ps_disc_risk_w26",
  "int_pst_.7_ps_disc_risk_w26",
  "int_pst_.8_ps_disc_risk_w26",
  "int_pst_.9_ps_disc_risk_w26",
  "int_pst_1_ps_disc_risk_w26",
  "int_pst_.1_ps_disc_risk_w52",
  "int_pst_.2_ps_disc_risk_w52",
  "int_pst_.3_ps_disc_risk_w52",
  "int_pst_.4_ps_disc_risk_w52",
  "int_pst_.5_ps_disc_risk_w52",
  "int_pst_.6_ps_disc_risk_w52",
  "int_pst_.7_ps_disc_risk_w52",
  "int_pst_.8_ps_disc_risk_w52",
  "int_pst_.9_ps_disc_risk_w52",
  "int_pst_1_ps_disc_risk_w52",
  "int_pst_1_ps_1_disc_risk_w52"

)

simno <- c("1111",
           "1620", "1621", "1622", "1623", "1624", "1625", "1626", "1627", "1628", "1629",
           "1740", "1741", "1742", "1743", "1744", "1745", "1746", "1747", "1748", "1749",
           "1860", "1861", "1862", "1863", "1864", "1865", "1866", "1867", "1868", "1869",
           "2100", "2101", "2102", "2103", "2104", "2105", "2106", "2107", "2108", "2109",
           "2220", "2221", "2222", "2223", "2224", "2225", "2226", "2227", "2228", "2229", 
           "2820", "2821", "2822", "2823", "2824", "2825", "2826", "2827", "2828", "2829", 
           "3420", "3421", "3422", "3423", "3424", "3425", "3426", "3427", "3428", "3429",
           "3540", "3541", "3542", "3543", "3544", "3545", "3546", "3547", "3548", "3549",
           "3660", "3661", "3662", "3663", "3664", "3665", "3666", "3667", "3668", "3669", 
           "3900", "3901", "3902", "3903", "3904", "3905", "3906", "3907", "3908", "3909", 
           "4620", "4621", "4622", "4623", "4624", "4625", "4626", "4627", "4628", "4629", 
           "5220", "5221", "5222", "5223", "5224", "5225", "5226", "5227", "5228", "5229", 
           "5340", "5341", "5342", "5342", "5344", "5345", "5346", "5347", "5348", "5349", 
           "5460", "5461", "5462", "5463", "5464", "5465", "5466", "5467", "5468", "5469", 
           "5700", "5701", "5702", "5703", "5704", "5705", "5706", "5707", "5708", "5709", 
           "5820", "5821", "5822", "5823", "5824", "5825", "5826", "5827", "5828", "5829",
           "6420", "6421", "6422", "6423", "6424", "6425", "6426", "6427", "6428", "6429",
           "7020", "7021", "7022", "7023", "7024", "7025", "7026", "7027", "7028", "7029",
           "7140", "7141", "7142", "7143", "7144", "7145", "7146", "7147", "7148", "7149", 
           "7260", "7261", "7262", "7263", "7264", "7265", "7266", "7267", "7268", "7269", 
           "7500", "7501", "7502", "7503", "7504", "7505", "7506", "7507", "7508", "7509",
           "8888", "8889"
           )

names <- c("noint", "int")
#Start loop to table concatination
for(q in 2:length(scenarios)){

x <- paste0(simno[1])
y <- paste0(simno[q])

noint <- merge_simfiles(x, indir = "data/" ,ftype="min")
int <- merge_simfiles(y, indir = "data/" ,ftype="min")

nsims <- 200
level <- c("","_hi","_low")
trim <- ((22 * 52) * 10)
steps<-noint$control$nsteps - trim
hi.cut <- round(.9*nsims)
low.cut <- round(.1*nsims)


Mod.noint <- truncate_sim(noint, at = trim)
Mod.int <- truncate_sim(int, at = trim)



Mod.noint$trans.el <- NULL
Mod.int$trans.el <- NULL

  for(j in 1:length(level)){
    
    fn <- paste0("noint", ".prev", level[j])
    assign(fn,rep(NA,steps))
    
    fn <- paste0("int", ".prev", level[j])
    assign(fn,rep(NA,steps))
    
  }

for (k in seq_along(1:steps)) {
  
 #no int 
  x<-sort(as.numeric(Mod.noint$epi$prev.poi[k,1:nsims]))
  noint.prev[k]<-mean(x) * 100
  noint.prev_hi[k]<-x[hi.cut] * 100
  noint.prev_low[k]<-x[low.cut] * 100
  
  #"int", 
  x<-sort(as.numeric(Mod.int$epi$prev.poi[k,1:nsims]))
  int.prev[k]<-mean(x) * 100
  int.prev_hi[k]<-x[hi.cut] * 100
  int.prev_low[k]<-x[low.cut] * 100
    }



  
  for(j in 1:length(level)){
    
    fn <- paste0("noint", ".ir100", level[j])
    assign(fn,rep(NA,steps))
    
    fn <- paste0("int", ".ir100", level[j])
    assign(fn,rep(NA,steps)) 
  }


for (k in seq_along(1:steps)) {
  
  
  #no int 
  x<-sort(as.numeric(Mod.noint$epi$ir100[k,1:nsims]))
  noint.ir100[k]<-mean(x)
  noint.ir100_hi[k]<-x[hi.cut]
  noint.ir100_low[k]<-x[low.cut]
  
  #"int", 
  x<-sort(as.numeric(Mod.int$epi$ir100[k,1:nsims]))
  int.ir100[k]<-mean(x)
  int.ir100_hi[k]<-x[hi.cut]
  int.ir100_low[k]<-x[low.cut]
  
}


##Summary measures
measure <- c("cum.incid", "cum.ptar", "cum.ptinf", "cum.rna.tests", "cum.ab.tests", "cum.tests", "cum.ab.tests", "cum.diag", "cum.diag.p",
             "cum.diag.a", "cum.partners", "cum.partners.f", "cum.n.tests.tst.ps" , "NIA.100K.list", "PIA.list", "cum.prep.t", "NNT.list", "DPTST.list" , "DPTM.list", "cum.bg.tests",
             "cum.n.tests", "cum.n.false.pos", "cum.n.false.neg", "cum.n.true.pos", "cum.n.true.neg", "cum.n.presented", "cum.n.presented.pos", "cum.n.presented.neg",
             "cum.n.missed.pos")



  for(k in 1:length(measure)){

    fn <- paste0("noint", ".",measure[k])
    assign(fn,rep(NA,nsims))
    
    fn <- paste0("int", ".",measure[k])
    assign(fn,rep(NA,nsims))

  }

##Cumulative incidence
for (i in 1:nsims){

noint.cum.incid[i] <- sum(as.numeric(Mod.noint$epi$incid.poi[,i]))
noint.cum.ptar[i] <- sum(Mod.noint$epi$num.poi[,i])-sum(Mod.noint$epi$i.num.poi[,i])
noint.cum.ptinf[i] <- sum(Mod.noint$epi$i.num.poi[,i])
noint.cum.rna.tests[i] <- sum(Mod.noint$epi$n.tests.rna[,i])
noint.cum.ab.tests[i] <- sum(Mod.noint$epi$n.tests.ab[,i])
noint.cum.tests[i] <- sum(Mod.noint$epi$n.tests[,i])
noint.cum.diag[i] <- sum(Mod.noint$epi$diag.prevalent[,i]) + sum(Mod.noint$epi$diag.acute[,i])
noint.cum.diag.p[i] <- sum(Mod.noint$epi$diag.prevalent[,i])
noint.cum.diag.a[i] <- sum(Mod.noint$epi$diag.acute[,i])
noint.cum.partners[i] <- sum(Mod.noint$epi$partners.sought.new[,i])
noint.cum.partners.f[i] <- sum(Mod.noint$epi$partners.found[,i])
noint.cum.n.tests.tst.ps[i] <- sum(Mod.noint$epi$n.tests.tst.psositive[,i])
noint.NIA.100K.list[i]<-NA
noint.PIA.list[i]<-NA
noint.cum.prep.t[i]<-NA
noint.NNT.list[i]<-NA
noint.DPTST.list[i]<-noint.cum.diag[i]/(noint.cum.tests[i] / 1000)
noint.DPTM.list[i]<- (noint.cum.diag[i]/noint.cum.ptinf[i])*52*100
noint.cum.bg.tests[i] <- sum(Mod.noint$epi$n.tests.bg[,i])
noint.cum.n.false.pos[i] <- sum(as.numeric(Mod.noint$epi$n.false.pos[,i]),na.rm=TRUE)
noint.cum.n.false.neg[i] <- sum(as.numeric(Mod.noint$epi$n.false.neg[,i]),na.rm=TRUE)
noint.cum.n.true.pos[i] <- sum(as.numeric(Mod.noint$epi$n.true.pos[,i]),na.rm=TRUE)
noint.cum.n.true.neg[i] <- sum(as.numeric(Mod.noint$epi$n.true.neg[,i]),na.rm=TRUE)
noint.cum.n.presented[i] <- sum(as.numeric(Mod.noint$epi$n.presented[,i]), na.rm=TRUE)
noint.cum.n.presented.pos[i] <- sum(as.numeric(Mod.noint$epi$n.presented.pos[,i]), na.rm=TRUE)
noint.cum.n.presented.neg[i] <- sum(as.numeric(Mod.noint$epi$n.presented.neg[,i]), na.rm=TRUE)
noint.cum.n.missed.pos[i] <- sum(as.numeric(Mod.noint$epi$missed.pos[,i]), na.rm=TRUE)


#int 
int.cum.incid[i] <- sum(as.numeric(Mod.int$epi$incid.poi[,i]))
int.cum.prep.t
int.cum.ptar[i] <- sum(Mod.int$epi$num.poi[,i])-sum(Mod.int$epi$i.num.poi[,i])
int.cum.ptinf[i] <- sum(Mod.int$epi$i.num.poi[,i])
int.cum.rna.tests[i] <- sum(Mod.int$epi$n.tests.rna[,i])
int.cum.ab.tests[i] <- sum(Mod.int$epi$n.tests.ab[,i])
int.cum.tests[i] <- sum(Mod.int$epi$n.tests[,i])
int.cum.diag[i] <- sum(Mod.int$epi$diag.prevalent[,i]) + sum(Mod.int$epi$diag.acute[,i])
int.cum.diag.p[i] <- sum(Mod.int$epi$diag.prevalent[,i])
int.cum.diag.a[i] <- sum(Mod.int$epi$diag.acute[,i])
int.cum.partners[i] <- sum(Mod.int$epi$partners.sought.new[,i])
int.cum.partners.f[i] <- sum(Mod.int$epi$partners.found[,i])
int.cum.n.tests.tst.ps[i] <- sum(Mod.int$epi$n.tests.tst.psositive[,i])
int.NIA.100K.list[i]<-((noint.cum.incid[i]-int.cum.incid[i])/int.cum.ptar[i])*52*100000
int.PIA.list[i]<-(noint.cum.incid[i]-int.cum.incid[i])/noint.cum.incid[i] * 100
int.cum.prep.t[i] <- sum(as.numeric(int$epi$prepCurr[,i]))
int.NNT.list[i]<-(int.cum.prep.t[i]/52)/(noint.cum.incid[i]-int.cum.incid[i])


int.DPTST.list[i]<-int.cum.diag[i]/(int.cum.tests[i] / 1000)
int.DPTM.list[i]<- (int.cum.diag[i]/int.cum.ptinf[i])*52*100
int.cum.bg.tests[i] <- sum(Mod.int$epi$n.tests.bg[,i])

int.cum.n.false.pos[i] <- sum(as.numeric(Mod.int$epi$n.false.pos[,i]),na.rm=TRUE)
int.cum.n.false.neg[i] <- sum(as.numeric(Mod.int$epi$n.false.neg[,i]),na.rm=TRUE)
int.cum.n.true.pos[i] <- sum(as.numeric(Mod.int$epi$n.true.pos[,i]),na.rm=TRUE)
int.cum.n.true.neg[i] <- sum(as.numeric(Mod.int$epi$n.true.neg[,i]),na.rm=TRUE)
int.cum.n.presented[i] <- sum(as.numeric(Mod.int$epi$n.presented[,i]), na.rm=TRUE)
int.cum.n.presented.pos[i] <- sum(as.numeric(Mod.int$epi$n.presented.pos[,i]), na.rm=TRUE)
int.cum.n.presented.neg[i] <- sum(as.numeric(Mod.int$epi$n.presented.neg[,i]), na.rm=TRUE)
int.cum.n.missed.pos[i] <- sum(as.numeric(Mod.int$epi$missed.pos[,i]), na.rm=TRUE)

}



for (i in 1:length(names)){

  #CUM INCID
    fn <- paste0(names[i], ".cum.incid")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.incid", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.incid", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.incid", ".mean_low")
    assign(fn,x[low.cut])
    
    
    fn <- paste0(names[i], ".cum.ptar")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.ptar", ".mean")
    assign(fn,mean(x))
  
    #cum.rna.tests
    fn <- paste0(names[i], ".cum.rna.tests")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.rna.tests", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.rna.tests", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.rna.tests", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.ab.tests
    fn <- paste0(names[i], ".cum.ab.tests")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.ab.tests", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.ab.tests", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.ab.tests", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.tests
    fn <- paste0(names[i], ".cum.tests")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.tests", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.tests", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.tests", ".mean_low")
    assign(fn,x[low.cut])
    
#cum.diag
    fn <- paste0(names[i], ".cum.diag")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.diag", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.diag", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.diag", ".mean_low")
    assign(fn,x[low.cut])
    
#cum.diag.p
    fn <- paste0(names[i], ".cum.diag.p")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.diag.p", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.diag.p", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.diag.p", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.diag.a
    fn <- paste0(names[i], ".cum.diag.a")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.diag.a", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.diag.a", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.diag.a", ".mean_low")
    assign(fn,x[low.cut])
    
#cum.partners
    fn <- paste0(names[i], ".cum.partners")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.partners", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.partners", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.partners", ".mean_low")
    assign(fn,x[low.cut])
    
#cum.partners.f
    fn <- paste0(names[i], ".cum.partners.f")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.partners.f", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.partners.f", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.partners.f", ".mean_low")
    assign(fn,x[low.cut])
   
#cum.n.tests.tst.ps
    fn <- paste0(names[i], ".cum.n.tests.tst.ps")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.tests.tst.ps", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.tests.tst.ps", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.tests.tst.ps", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.bg.tests
    fn <- paste0(names[i], ".cum.bg.tests")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.bg.tests", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.bg.tests", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.bg.tests", ".mean_low")
    assign(fn,x[low.cut])
    
##########################################
    #cum.n.false.pos
    fn <- paste0(names[i], ".cum.n.false.pos")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.false.pos", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.false.pos", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.false.pos", ".mean_low")
    assign(fn,x[low.cut])
    
    
    #cum.n.false.neg
    fn <- paste0(names[i], ".cum.n.false.neg")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.false.neg", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.false.neg", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.false.neg", ".mean_low")
    assign(fn,x[low.cut])
    
    
    #cum.n.true.pos
    fn <- paste0(names[i], ".cum.n.true.pos")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.true.pos", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.true.pos", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.true.pos", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.n.true.neg
    fn <- paste0(names[i], ".cum.n.true.neg")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.true.neg", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.true.neg", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.true.neg", ".mean_low")
    assign(fn,x[low.cut])
    
    
    #cum.n.presented
    fn <- paste0(names[i], ".cum.n.presented")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.presented", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.presented", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.presented", ".mean_low")
    assign(fn,x[low.cut])
    
    #cum.n.presented.pos
    fn <- paste0(names[i], ".cum.n.presented.pos")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.presented.pos", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.presented.pos", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.presented.pos", ".mean_low")
    assign(fn,x[low.cut])
    

    #cum.n.presented.neg
    fn <- paste0(names[i], ".cum.n.presented.neg")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.presented.neg", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.presented.neg", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.presented.neg", ".mean_low")
    assign(fn,x[low.cut])
    
    
    #cum.n.missed.pos
    fn <- paste0(names[i], ".cum.n.missed.pos")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.n.missed.pos", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.n.missed.pos", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.n.missed.pos", ".mean_low")
    assign(fn,x[low.cut])
    
    
    
    #cum.prep.t
    fn <- paste0(names[i], ".cum.prep.t")
    x  <- get(fn)
    x<-sort(x)
    
    fn <- paste0(names[i], ".cum.prep.t", ".mean")
    assign(fn,mean(x))
    
    fn <- paste0(names[i], ".cum.prep.t", ".mean_hi")
    assign(fn,x[hi.cut])
    
    fn <- paste0(names[i], ".cum.prep.t", ".mean_low")
    assign(fn,x[low.cut])   

}

#MAKE NIA / 10Kpt and PIA

for (i in 1:length(names)){
  

  ##NIA
  fn <- paste0(names[i], ".NIA.100K")
  fn2 <- paste0(names[i], ".NIA.100K.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,mean(x))
  
  fn <- paste0(names[i], ".NIA.100K_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(names[i], ".NIA.100K_low")
  assign(fn,x[low.cut])
  
  ##PIA
  fn <- paste0(names[i], ".PIA")
  fn2 <- paste0(names[i], ".PIA.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,mean(x))
  
  fn <- paste0(names[i], ".PIA_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(names[i], ".PIA_low")
  assign(fn,x[low.cut])
  
  
  ##NNT
  fn <- paste0(names[i], ".NNT")
  fn2 <- paste0(names[i], ".NNT.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,median(x,na.rm = TRUE))
  
  fn <- paste0(names[i], ".NNT_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(names[i], ".NNT_low")
  assign(fn,x[low.cut])
  
  ##DPTST
  fn <- paste0(names[i], ".DPTST")
  fn2 <- paste0(names[i], ".DPTST.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,mean(x))
  
  fn <- paste0(names[i], ".DPTST_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(names[i], ".DPTST_low")
  assign(fn,x[low.cut])
  
  ##DPTM
  fn <- paste0(names[i], ".DPTM")
  fn2 <- paste0(names[i], ".DPTM.list")
  x <- get(fn2)
  x<-sort(x)
  assign(fn,mean(x))
  
  fn <- paste0(names[i], ".DPTM_hi")
  assign(fn,x[hi.cut])
  
  fn <- paste0(names[i], ".DPTM_low")
  assign(fn,x[low.cut])
}


#TABLE of point estimates

scenario <- incid.mean <- incid.mean.hi <- incid.mean.low  <-ptar.mean <- 
rna.tests.mean <- rna.tests.mean.hi <- rna.tests.mean.low <- ab.tests.mean <- ab.tests.mean.hi <- ab.tests.mean.low <-
tests.mean <- tests.mean.hi <- tests.mean.low <-
diag.mean <- diag.mean.hi <- diag.mean.low <- diag.p.mean <- diag.p.mean.hi <- diag.p.mean.low <-
diag.a.mean <- diag.a.mean.hi <- diag.a.mean.low <-
partners.mean <- partners.mean.hi <- partners.mean.low <-
partners.f.mean <- partners.f.mean.hi <- partners.f.mean.low <-
n.tests.tst.ps.mean <- n.tests.tst.ps.mean.hi <- n.tests.tst.ps.mean.low <-
NIA  <-NIA.low  <-NIA.hi <- 
PIA <- PIA.low <- PIA.hi <-
prep.t.mean <- prep.t.mean.hi <- prep.t.mean.low <-
NNT <- NNT.low <- NNT.hi <-
DPTST <- DPTST.low <- DPTST.hi <-
DPTM <- DPTM.low <- DPTM.hi <-
Prev <- Prev.low <- Prev.hi <-rep(NA,length(names))
ir100 <- ir100.low <- ir100.hi <-rep(NA,length(names))
bg.tests.mean <- bg.tests.mean.hi <- bg.tests.mean.low <-
n.false.pos.mean <-n.false.pos.mean.low <-n.false.pos.mean.hi <-
n.false.neg.mean <- n.false.neg.mean.low <- n.false.neg.mean.hi <-
n.true.pos.mean <- n.true.pos.mean.low <- n.true.pos.mean.hi <-
n.true.neg.mean <- n.true.neg.mean.low <- n.true.neg.mean.hi <-
n.presented.mean <- n.presented.mean.low <- n.presented.mean.hi <-
n.presented.pos.mean <- n.presented.pos.mean.low <- n.presented.pos.mean.hi <-
n.presented.neg.mean <- n.presented.neg.mean.low <- n.presented.neg.mean.hi <- rep(NA,length(names))
n.missed.pos.mean <- n.missed.pos.mean.low <- n.missed.pos.mean.hi <- rep(NA,length(names))

for (i in 1:length(names)){
  
 
  fn <- paste0(names[i], ".cum.incid.mean")
  incid.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.incid.mean_hi")
  incid.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.incid.mean_low")
  incid.mean.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.ptar.mean")
  ptar.mean[i] <- get(fn)
  

  fn <- paste0(names[i], ".cum.rna.tests.mean")
  rna.tests.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.rna.tests.mean_hi")
  rna.tests.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.rna.tests.mean_low")
  rna.tests.mean.low[i] <- get(fn)
  
 
   fn <- paste0(names[i], ".cum.ab.tests.mean")
  ab.tests.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.ab.tests.mean_hi")
  ab.tests.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.ab.tests.mean_low")
  ab.tests.mean.low[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".cum.tests.mean")
  tests.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.tests.mean_hi")
  tests.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.tests.mean_low")
  tests.mean.low[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".cum.diag.mean")
  diag.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.diag.mean_hi")
  diag.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.diag.mean_low")
  diag.mean.low[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".cum.diag.p.mean")
  diag.p.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.diag.p.mean_hi")
  diag.p.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.diag.p.mean_low")
  diag.p.mean.low[i] <- get(fn)

  
  fn <- paste0(names[i], ".cum.diag.a.mean")
  diag.a.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.diag.a.mean_hi")
  diag.a.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.diag.a.mean_low")
  diag.a.mean.low[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".cum.partners.mean")
  partners.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.partners.mean_hi")
  partners.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.partners.mean_low")
  partners.mean.low[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".cum.partners.f.mean")
  partners.f.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.partners.f.mean_hi")
  partners.f.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.partners.f.mean_low")
  partners.f.mean.low[i] <- get(fn)

  
  fn <- paste0(names[i], ".cum.n.tests.tst.ps.mean")
  n.tests.tst.ps.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.tests.tst.ps.mean_hi")
  n.tests.tst.ps.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.tests.tst.ps.mean_low")
  n.tests.tst.ps.mean.low[i] <- get(fn)
    

  fn <- paste0(names[i], ".NIA.100K")
  NIA[i] <- get(fn)
  
  fn <- paste0(names[i], ".NIA.100K_low")
  NIA.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".NIA.100K_hi")
  NIA.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".PIA")
  PIA[i] <- get(fn)
  
  fn <- paste0(names[i], ".PIA_low")
  PIA.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".PIA_hi")
  PIA.hi[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".NNT")
  NNT[i] <- get(fn)
  
  fn <- paste0(names[i], ".NNT_low")
  NNT.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".NNT_hi")
  NNT.hi[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".DPTST")
  DPTST[i] <- get(fn)
  
  fn <- paste0(names[i], ".DPTST_low")
  DPTST.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".DPTST_hi")
  DPTST.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".DPTM")
  DPTM[i] <- get(fn)
  
  fn <- paste0(names[i], ".DPTM_low")
  DPTM.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".DPTM_hi")
  DPTM.hi[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".prev")
  x  <- get(fn) 
  Prev[i] <- tail(x,1)
  
  fn <- paste0(names[i], ".prev_low")
  x  <- get(fn) 
  Prev.low[i] <- tail(x,1)
  
  fn <- paste0(names[i], ".prev_hi")
  x  <- get(fn) 
  Prev.hi[i] <- tail(x,1)
  
  fn <- paste0(names[i], ".ir100")
  x  <- get(fn) 
  ir100[i] <- tail(x,1)
  
  fn <- paste0(names[i], ".ir100_low")
  x  <- get(fn) 
  ir100.low[i] <- tail(x,1)
  
  fn <- paste0(names[i], ".ir100_hi")
  x  <- get(fn) 
  ir100.hi[i] <- tail(x,1)
  
  fn <- paste0(names[i], ".cum.bg.tests.mean")
  bg.tests.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.bg.tests.mean_hi")
  bg.tests.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.bg.tests.mean_low")
  bg.tests.mean.low[i] <- get(fn)
  
 
  fn <- paste0(names[i], ".cum.n.false.pos.mean")
  n.false.pos.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.false.pos.mean_hi")
  n.false.pos.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.false.pos.mean_low")
  n.false.pos.mean.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.false.neg.mean")
  n.false.neg.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.false.neg.mean_hi")
  n.false.neg.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.false.neg.mean_low")
  n.false.neg.mean.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.true.pos.mean")
  n.true.pos.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.true.pos.mean_hi")
  n.true.pos.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.true.pos.mean_low")
  n.true.pos.mean.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.true.neg.mean")
  n.true.neg.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.true.neg.mean_hi")
  n.true.neg.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.true.neg.mean_low")
  n.true.neg.mean.low[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".cum.n.presented.mean")
  n.presented.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.presented.mean_hi")
  n.presented.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.presented.mean_low")
  n.presented.mean.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.presented.pos.mean")
  n.presented.pos.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.presented.pos.mean_hi")
  n.presented.pos.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.presented.pos.mean_low")
  n.presented.pos.mean.low[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.presented.neg.mean")
  n.presented.neg.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.presented.neg.mean_hi")
  n.presented.neg.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.presented.neg.mean_low")
  n.presented.neg.mean.low[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".cum.n.missed.pos.mean")
  n.missed.pos.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.missed.pos.mean_hi")
  n.missed.pos.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.n.missed.pos.mean_low")
  n.missed.pos.mean.low[i] <- get(fn)
  
  
  fn <- paste0(names[i], ".cum.prep.t.mean")
  prep.t.mean[i]  <- get(fn)
  
  fn <- paste0(names[i], ".cum.prep.t.mean_hi")
  prep.t.mean.hi[i] <- get(fn)
  
  fn <- paste0(names[i], ".cum.prep.t.mean_low")
  prep.t.mean.low[i] <- get(fn)
  
}
  
scenario <-rbind("noint", scenarios[q])
if(q == 2){
table<-cbind(scenario,incid.mean, incid.mean.hi, incid.mean.low, ptar.mean, rna.tests.mean, rna.tests.mean.hi, rna.tests.mean.low,
             ab.tests.mean, ab.tests.mean.hi, ab.tests.mean.low, tests.mean, tests.mean.hi, tests.mean.low,
             diag.mean, diag.mean.hi, diag.mean.low, diag.p.mean, diag.p.mean.hi, diag.p.mean.low, diag.a.mean, diag.a.mean.hi, diag.a.mean.low,
             partners.mean, partners.mean.hi, partners.mean.low, partners.f.mean, partners.f.mean.hi, partners.f.mean.low,
             n.tests.tst.ps.mean, n.tests.tst.ps.mean.hi, n.tests.tst.ps.mean.low,
             NIA, NIA.low, NIA.hi, PIA, PIA.low, PIA.hi, prep.t.mean, prep.t.mean.hi, prep.t.mean.low, NNT, NNT.low, NNT.hi,
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
}

if(q > 2){
  table.temp<-cbind(scenario,incid.mean, incid.mean.hi, incid.mean.low, ptar.mean, rna.tests.mean, rna.tests.mean.hi, rna.tests.mean.low,
               ab.tests.mean, ab.tests.mean.hi, ab.tests.mean.low, tests.mean, tests.mean.hi, tests.mean.low,
               diag.mean, diag.mean.hi, diag.mean.low, diag.p.mean, diag.p.mean.hi, diag.p.mean.low, diag.a.mean, diag.a.mean.hi, diag.a.mean.low,
               partners.mean, partners.mean.hi, partners.mean.low, partners.f.mean, partners.f.mean.hi, partners.f.mean.low,
               n.tests.tst.ps.mean, n.tests.tst.ps.mean.hi, n.tests.tst.ps.mean.low,
               NIA, NIA.low, NIA.hi, PIA, PIA.low, PIA.hi, prep.t.mean, prep.t.mean.hi, prep.t.mean.low, NNT, NNT.low, NNT.hi, 
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
  
  table <- rbind(table,table.temp[2,])
  
}

}

table<-as.data.frame(table)
table
test<-cbind(table$V1,table$NIA,table$NNT,table$Prev)
test


library(xlsx) #load the package
write.xlsx(x = table, file = "out/KTM.prep.results.xlsx",
           sheetName = "KTM prep", row.names = FALSE)


##95% outcomes at the end of the simulation



#Start loop to table concatenation
for(q in 2:length(scenarios)){
  
  x <- paste0(simno[1])
  y <- paste0(simno[q])
  
  noint <- merge_simfiles(x, indir = "data/" ,ftype="min")
  int <- merge_simfiles(y, indir = "data/" ,ftype="min")
  
  nsims <- 200
  level <- c("","_hi","_low")
  trim <- ((22 * 52) * 10)
  steps<-noint$control$nsteps - trim
  hi.cut <- round(.9*nsims)
  low.cut <- round(.1*nsims)
  
  
  Mod.noint <- truncate_sim(noint, at = trim)
  Mod.int <- truncate_sim(int, at = trim)

for(i in 1:length(names)){
  
  for(j in 1:length(level)){
    
    fn <- paste0(names[i], ".pct.diag", level[j])
    assign(fn,rep(NA,1))
  
    
    fn <- paste0(names[i], ".pct.txt", level[j])
    assign(fn,rep(NA,1))
    
    fn <- paste0(names[i], ".pct.vsup", level[j])
    assign(fn,rep(NA,1))
    
    fn <- paste0(names[i], ".fprev", level[j])
    assign(fn,rep(NA,1))
    
    fn <- paste0(names[i], ".time.inf.diag", level[j])
    assign(fn,rep(NA,1))
    
    
  }}

hi.cut <- round(.95*nsims)
low.cut <- round(.05*nsims)
 
 #no int 
  x<-sort(as.numeric(Mod.noint$epi$pct.diag[521,1:nsims]))
  noint.pct.diag.list <- x * 100
  noint.pct.diag <-mean(x) * 100
  noint.pct.diag_hi <-x[hi.cut] * 100
  noint.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint$epi$pct.txt[521,1:nsims]))
  noint.pct.txt <-mean(x) * 100
  noint.pct.txt_hi <-x[hi.cut] * 100
  noint.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint$epi$pct.vsup[521,1:nsims]))
  noint.pct.vsup <-mean(x) * 100
  noint.pct.vsup_hi <-x[hi.cut] * 100
  noint.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint$epi$pct.test.ly[521,1:nsims]))
  noint.pct.test.ly <-mean(x) * 100
  noint.pct.test.ly_hi <-x[hi.cut] * 100
  noint.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.noint$epi$time.inf.diag[521,1:nsims]))
  noint.time.inf.diag <-mean(x)
  noint.time.inf.diag_hi <-x[hi.cut]
  noint.time.inf.diag_low <-x[low.cut]
  

  #"int", 
  x<-sort(as.numeric(Mod.int$epi$pct.diag[521,1:nsims]))
  int.pct.diag.list <- x * 100
  int.pct.diag <-mean(x) * 100
  int.pct.diag_hi <-x[hi.cut] * 100
  int.pct.diag_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int$epi$pct.txt[521,1:nsims]))
  int.pct.txt <-mean(x) * 100
  int.pct.txt_hi <-x[hi.cut] * 100
  int.pct.txt_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int$epi$pct.vsup[521,1:nsims]))
  int.pct.vsup <-mean(x) * 100
  int.pct.vsup_hi <-x[hi.cut] * 100
  int.pct.vsup_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int$epi$pct.test.ly[521,1:nsims]))
  int.pct.test.ly <-mean(x) * 100
  int.pct.test.ly_hi <-x[hi.cut] * 100
  int.pct.test.ly_low <-x[low.cut] * 100
  
  x<-sort(as.numeric(Mod.int$epi$time.inf.diag[521,1:nsims]))
  int.time.inf.diag <-mean(x)
  int.time.inf.diag_hi <-x[hi.cut]
  int.time.inf.diag_low <-x[low.cut]
  


  
  
##MAKE AGE AT INFECTION
  for(i in 1:length(names)){
    

    for(j in 1:length(level)){
      
      fn <- paste0(names[i], ".age.inf", level[j])
      assign(fn,rep(NA,nsims))
      
    }}
  
  for (i in 1:nsims){
  #no int 
    noint.age.inf[i] <- sum((Mod.noint$epi$incid.poi[,i] * Mod.noint$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.noint$epi$incid.poi[,i])
    int.age.inf[i] <- sum((Mod.int$epi$incid.poi[,i] * Mod.int$epi$incid.age.poi[,i]),na.rm=TRUE) / sum(Mod.int$epi$incid.poi[,i])

 }

  ##AGE INF
  #noint
  x<-sort(noint.age.inf)
  noint.age.inf_hi <-x[hi.cut]
  noint.age.inf_low <-x[low.cut]
  noint.age.inf <- mean(noint.age.inf)
  
  ## int
  x<-sort(int.age.inf)
  int.age.inf_hi <-x[hi.cut]
  int.age.inf_low <-x[low.cut]
  int.age.inf <- mean(int.age.inf)
  
  ##fprev
  #noint
  x<-sort(as.numeric(Mod.noint$epi$prev.poi[521,1:nsims]))
  noint.fprev.list <- x * 100
  noint.fprev <-mean(x) * 100
  noint.fprev_hi <-x[hi.cut] * 100
  noint.fprev_low <-x[low.cut] * 100
  
  #"int", 
  x<-sort(as.numeric(Mod.int$epi$prev.poi[521,1:nsims]))
  int.fprev.list <- x * 100
  int.fprev <-mean(x) * 100
  int.fprev_hi <-x[hi.cut] * 100
  int.fprev_low <-x[low.cut] * 100
  
  
  
  #TABLE of point estimates
  

  pct.diag <- pct.diag.low <- pct.diag.hi <-rep(NA,length(names))
  pct.txt <- pct.txt.low <- pct.txt.hi <-rep(NA,length(names))
  pct.vsup <- pct.vsup.low <- pct.vsup.hi <-rep(NA,length(names))
  pct.test.ly <- pct.test.ly.low <- pct.test.ly.hi <-rep(NA,length(names))
  age.inf <- age.inf.low <- age.inf.hi <-rep(NA,length(names))
  fprev <- fprev.low <- fprev.hi <-rep(NA,length(names))
  time.inf.diag <- time.inf.diag.low <- time.inf.diag.hi <- rep(NA,length(names))
  

  
  for (i in 1:length(names)){
    
    
    fn <- paste0(names[i], ".pct.diag")
    x  <- get(fn) 
    pct.diag[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.diag_low")
    x  <- get(fn) 
    pct.diag.low[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.diag_hi")
    x  <- get(fn) 
    pct.diag.hi[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.txt")
    x  <- get(fn) 
    pct.txt[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.txt_low")
    x  <- get(fn) 
    pct.txt.low[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.txt_hi")
    x  <- get(fn) 
    pct.txt.hi[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.vsup")
    x  <- get(fn) 
    pct.vsup[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.vsup_low")
    x  <- get(fn) 
    pct.vsup.low[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.vsup_hi")
    x  <- get(fn) 
    pct.vsup.hi[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.test.ly")
    x  <- get(fn) 
    pct.test.ly[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.test.ly_low")
    x  <- get(fn) 
    pct.test.ly.low[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".pct.test.ly_hi")
    x  <- get(fn) 
    pct.test.ly.hi[i] <- tail(x,1)

    
    fn <- paste0(names[i], ".age.inf")
    x  <- get(fn) 
    age.inf[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".age.inf_low")
    x  <- get(fn) 
    age.inf.low[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".age.inf_hi")
    x  <- get(fn) 
    age.inf.hi[i] <- tail(x,1)
    
    
    fn <- paste0(names[i], ".fprev")
    x  <- get(fn) 
    fprev[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".fprev_low")
    x  <- get(fn) 
    fprev.low[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".fprev_hi")
    x  <- get(fn) 
    fprev.hi[i] <- tail(x,1)
    
    
    fn <- paste0(names[i], ".time.inf.diag")
    x  <- get(fn) 
    time.inf.diag[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".time.inf.diag_low")
    x  <- get(fn) 
    time.inf.diag.low[i] <- tail(x,1)
    
    fn <- paste0(names[i], ".time.inf.diag_hi")
    x  <- get(fn) 
    time.inf.diag.hi[i] <- tail(x,1)
    
    
      }
  
  scenario <- c(scenarios[1], scenarios[q])
  if (q == 2){
  table2<-cbind(scenario,
               pct.diag, pct.diag.low, pct.diag.hi,
               pct.txt, pct.txt.low, pct.txt.hi,
               pct.vsup, pct.vsup.low, pct.vsup.hi,
               pct.test.ly, pct.test.ly.low, pct.test.ly.hi,
               age.inf, age.inf.low, age.inf.hi,
               fprev, fprev.low, fprev.hi,
               time.inf.diag, time.inf.diag.low, time.inf.diag.hi)
  }
  
  if (q > 2){
    table2.temp<-cbind(scenario,
                  pct.diag, pct.diag.low, pct.diag.hi,
                  pct.txt, pct.txt.low, pct.txt.hi,
                  pct.vsup, pct.vsup.low, pct.vsup.hi,
                  pct.test.ly, pct.test.ly.low, pct.test.ly.hi,
                  age.inf, age.inf.low, age.inf.hi,
                  fprev, fprev.low, fprev.hi,
                  time.inf.diag, time.inf.diag.low, time.inf.diag.hi)
    
    table2<-rbind(table2,table2.temp[2,])
  }
  
  }
table2

  write.xlsx(x = table2, file = "out/KTM.prep.results2.xlsx",
             sheetName = "KTM prep", row.names = FALSE)

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

