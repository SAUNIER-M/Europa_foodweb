
library(deSolve)
library(ggplot2)
library(here)


source("R/Fct.R")
source("R/Var.R")

#-----------------------------------------------
########## DEF PARAMETERS ################
#-----------------------------------------------

SRCH_seq   <- seq(0.2, 1, by = 0.2)
GCH_seq    <- seq(3, 8, by = 1)
ECH_seq    <- seq(1, 2, by = 0.5)
phiJCH_seq <- seq(0.2, 0.6, by = 0.2)
phiACH_seq <- seq(0.4, 0.96, by = 0.2)

param_grid <- expand.grid(
  SRCH = SRCH_seq,
  GCH = GCH_seq,
  ECH = ECH_seq,
  phiJCH = phiJCH_seq,
  phiACH = phiACH_seq
)

#--------------------------------------------
######### INITIAL CONDITIONS ###############
#--------------------------------------------

yini <- c(
  YBJ = YBJ, JBJ1 = JBJ1, JBJ2 = JBJ2, JBJ3 = JBJ3,
  JBJ4 = JBJ4, JBJ5 = JBJ5, JBJ6 = JBJ6, ABJ = ABJ,
  YBR = YBR, JBR1 = JBR1, JBR2 = JBR2, JBR3 = JBR3,
  JBR4 = JBR4, JBR5 = JBR5, JBR6 = JBR6, ABR = ABR,
  YCH = YCH, JCH1 = JCH1, JCH2 = JCH2, ACH = ACH,
  Co = Co,
  R = R
)

times <- seq(0,100,by=1)

#-----------------------------------------------
########## COMPUTE LAMBDA ###########
#-----------------------------------------------

calc_lambda <- function(time,pop){
  
  idx <- tail(seq_along(time),20)
  
  fit <- lm(log(pop[idx]) ~ time[idx])
  
  lambda <- exp(coef(fit)[2])
  
  return(lambda)
}


results <- data.frame()


for(i in 1:nrow(param_grid)){
  
  SRCH  <- param_grid$SRCH[i]
  GCH   <- param_grid$GCH[i]
  ECH   <- param_grid$ECH[i]
  
  phiJCH1 <- param_grid$phiJCH[i]
  phiJCH2 <- phiJCH1
  
  phiACH  <- param_grid$phiACH[i]
  
  FCH <- aCH * GCH * ECH
  
  parms <- c(
    Au, I, Pl, Sc, Tor,
    
    FBJ, KBJ,
    muYBJ, muJBJ1, muJBJ2, muJBJ3,
    muJBJ4, muJBJ5, muJBJ6, muABJ,
    phiYBJ, phiJBJ1, phiJBJ2, phiJBJ3,
    phiJBJ4, phiJBJ5, phiJBJ6,
    
    FBR, KBR,
    muYBR, muJBR1, muJBR2, muJBR3,
    muJBR4, muJBR5, muJBR6, muABR,
    phiYBR, phiJBR1, phiJBR2, phiJBR3,
    phiJBR4, phiJBR5, phiJBR6,
    
    FCH, KCH,
    muYCH, muJCH1, muJCH2, muACH,
    phiYCH, phiJCH1, phiJCH2, phiACH,
    
    rR, KR, roR,
    rCo, KCo, roCo,
    
    prCoPBJ, prRPBJ, prCHPBJ, prCHABJ,
    prCoPBR, prRPBR, prCHPBR,
    prCoR, prCHR
  )
  
  out <- ode(
    y = yini,
    times = times,
    func = europa_fct,
    parms = parms,
    rtol = 1e-8,
    atol = 1e-8,
    maxsteps = 1e6
  )
  
  out <- as.data.frame(out)
  
  lambda <- calc_lambda(out$time,out$ACH)
  
  results <- rbind(
    results,
    cbind(param_grid[i,],lambda)
  )
  
}



write.csv(
  results,
  here("output/Sensibilite_chouettes/lambda_results.csv"),
  row.names = FALSE
)

#-------------------------------------------
############ PLOT RESULTS #################
#-------------------------------------------

p <- ggplot(results,
            aes(phiACH,lambda,color=factor(GCH))) +
  geom_line() +
  facet_grid(SRCH ~ ECH) +
  geom_hline(yintercept=1,linetype="dashed") +
  theme_bw() +
  labs(
    x="Adult survival (phiACH)",
    y="Population growth rate (lambda)",
    color="GCH",
    title="Barn owl population growth rate"
  )

ggsave(
  here("output/Sensibilite_chouettes/lambda_plot.png"),
  p,
  width=12,
  height=10,
  dpi=300
)
