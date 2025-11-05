library(deSolve)
library(here)

source("R/Fct.R") 

###############################################
##############    Run model      ##############
###############################################

parms <- c(Au, I, Pl, Sc, Tor,
           
           aBJ, FBJ, KBJ,  EBJ, SRBJ,
           muYBJ, muJBJ1, muJBJ2,  muJBJ3,
           muJBJ4, muJBJ5, muJBJ6, muABJ,
           phiYBJ,  phiPBJ, phiJBJ1, phiJBJ2, phiJBJ3,
           phiJBJ4, phiJBJ5, phiJBJ6, phiABJ,
           
           aBR, FBR, KBR, EBR, SRBR,
           muYBR, muJBR1, muJBR2,  muJBR3,
           muJBR4, muJBR5, muJBR6, muABR,
           phiYBR,  phiPBR, phiJBR1, phiJBR2, phiJBR3,
           phiJBR4, phiJBR5, phiJBR6, phiABR,
           
           aCH, FCH, KCH, SRCH, ECH, GCH,
           muYCH, muJCH1, muJCH2, muACH, 
           phiYCH, phiPCH, phiJCH1, phiJCH2, phiACH, roCH,
           
           aR, BR, KR, roR, ER, GR, rR,
           
           aCo, BCo, KCo, roCo, ECo, GCo, rCo,
           
           prCoPBJ,  prRPBJ, prCHPBJ, prCHABJ,
           prCoPBR, prRPBR, prCHPBR,
           prCoR, prCHR,
           
           Proies_Co, Proies_Ch, Proies_R
)


# Initial conditions
yini <- c(PBJ = PBJ, JBJ1 = JBJ1, JBJ2 = JBJ2, JBJ3 = JBJ3,
          JBJ4 = JBJ4, JBJ5 = JBJ5, JBJ6 = JBJ6, ABJ = ABJ,
          PBR = PBR, JBR1 = JBR1, JBR2 = JBR2, JBR3 = JBR3,
          JBR4 = JBR4, JBR5 = JBR5, JBR6 = JBR6, ABR = ABR,
          PCH = PCH, JCH1 = JCH1, JCH2 = JCH2, ACH = ACH,
          Co = Co, 
          R = R
)



# SimulationS
times <- seq (0, 100, by = 1) 
out   <- ode(y = yini, times = times, func = europa_fct, parms = parms,
             rtol = 1e-8, atol = 1e-8, maxsteps = 1e6) #Force minimum time steps to avoid crashes
out   <- as.data.frame(out)  

vars  <- c("ABJ", "ABR", "ACH", "R", "Co") 
range(out$time)
tail(out$time)



###############################################
################     Plot      ################
###############################################


png(paste0(here::here("output/", "CRIME_NO_PREDA"), ".png"), 
    width = 1000, height = 1000, units = "px")

par(mar = c(27, 9, 5, 3), mgp = c(5, 2, 0))

matplot(out$time, out[, vars], type = "l", lty = 1, lwd = 2,
        col = c("yellow3", "red", "brown", "black", "purple"),
        xlab = "Years", ylab = "Population size (adults only)",
        cex.lab = 3.5,    
        cex.main = 4,    
        cex.axis = 3,
        font.lab = 2,
        log = "y")  #pour échelle log

par(xpd = NA)
legend(
  x = mean(out$time),
  y = 0.00001 * min(out[out[, vars] > 0, vars], na.rm = TRUE),   #avoid 0
  col = c("yellow3", "red", "brown", "black", "purple"),
  lty = 1, lwd = 2, cex = 2.5,
  legend = c("White-tailed tropicbirds",
             "Red-tailed tropicbirds",
             "Barn owls",
             "Rats",
             "Pied crows"),
  xjust = 0.5)

dev.off()
