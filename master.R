# write master and runsim scripts for each set of scenarios

library(EpiModelHPC)
setwd("~/kenyaTM/PREPint")


##No intervention

vars <- list(PREP_START_PROB = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
              PS_TEST_PROB = c(.023, .1, .169, .2, .3, .4, .5, .6, .7, .8, .9, 1),
              PREP_R_win = c(0,3,6,9,12),
              PREP_R = c("NONE","RISK","ALL"),
              PREP_DISC = c(FALSE, TRUE),
              PPREP_PS = c(FALSE, TRUE),
              TMPINT = c("NONE","TMP"))


sbatch_master(vars,
              simno.start = 1000,
              master.file = "master.sh",
              runsim.file = "runsim.sh",
              build.runsim = TRUE,
              ckpt = TRUE,
              walltime = "100:00:00",
              nsims = 16,
              ncores = 8,
              mem = "100G",
              rscript.file = "sim.R")


