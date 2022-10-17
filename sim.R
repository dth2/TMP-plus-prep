

library("methods")
library("EpiModelHIV")
library("EpiModelHPC")

pull_env_vars(num.vars = c("PREP_START_PROB", "PS_TEST_PROB", "PREP_R_win"),
              char.vars = c("PREP_R", "PREP_DISC", "PPREP_PS", "TMPINT"))

# Epidemic model


time.unit <- 7
method<-1

param <- param_KTM(

  intervention_TM = TMPINT,
  prep.ps = PPREP_PS,
  prep.disc = PREP_DISC,
  prep.risk = PREP_R,
  prep.start.prob = PREP_START_PROB,

  prep.start = 1,
  riskh.start = 1,
  prep.risk.int = PREP_R_win,
  
  URVI.prob = 0.0275,
  UIVI.prob = 0.0255,
  
  prep.start = ((22 * 52) * 10)+1,
  riskh.start = ((22 * 52) * 10)+1
)

init <- init_KTM()

control <- control_KTM(simno = fsimno,
                      nsteps = ((22 * 52) * 10) + 520,
                      nsims = ncores, 
                      ncores = ncores,
                      start = ((22 * 52) * 10)+1,
                      initialize.FUN = reinit_shamp,
                      verbose = FALSE)

netsim_hpc("est/sim.burnin3.rda", param, init, control, compress=FALSE, verbose = FALSE)

#process_simfiles(simno = simno, min.n = njobs, nsims = nsims, delete.sub = FALSE)
