# =====================================================
#' @description
#' The purpose of this script is to define the variables used in the 
#' `europa_fct` function. Some parameters used here are not defined in the
#' function script, as they are used later in fecundity or predation formulas. 
#' They are therefore described here. 

#' Variable names may combine a biological parameter prefix with 
#' a species code. The prefix indicates the parameter type,
#' and the suffix refers to the species.
#' 
#' @param E     Number of clutch per dt 
#' @param a     Birth sex-ratio
#' @param G     Clutch size
#' @param SR    Natural breeding success ("Succès reproducteur" in french)
#' @param Au    Other birds ("Autres" in french, including sooty terns and zosterops)
#' @param I     Insects
#' @param Pl    Plants
#' @param Sc    Skinks ("Scinque" in french)
#' @param Tor   Baby turtles
#' 
# =====================================================

source("R/Eqdiff_fct_deSolve_PEQ_Repro6.R") 


###################################################
###### Define the initial population sizes  #######
###################################################

#Main species
PBJ  <- 80
JBJ1 <- 70
JBJ2 <- 60
JBJ3 <- 50
JBJ4 <- 40
JBJ5 <- 30
JBJ6 <- 25
ABJ  <- 200

PBR  <- 800
JBR1 <- 700
JBR2 <- 600
JBR3 <- 500
JBR4 <- 400
JBR5 <- 300
JBR6 <- 250
ABR  <- 2000

PCH  <- 40 
JCH1 <- 40
JCH2 <- 30
ACH  <- 40

Co   <- 1000

R    <- 50000

#Secondary species
Au   <- 1000000
I    <- 1000000
Pl   <- 1000000
Sc   <- 1000000
Tor  <- 10000000



# Define the prey eaten by each predator to simplify predation equations    

Proies_Co <- PBJ + PBR +  Au + I + Pl + Tor + R
Proies_Ch <- PBJ + PBR + ABJ + Au + I + Pl + Sc + R
Proies_R  <- PBJ + PBR + Au + I + Pl + Sc +  PCH 


###################################################
#########  1. White-tailed tropicbirds    #########
###################################################

KBJ   <- 50000

SRBJ  <- 0.50 
EBJ   <- 1.61 
aBJ   <- 0.5 
FBJ   <- EBJ* aBJ


phiYBJ  <- SRBJ * phiJBJ1 
phiPBJ  <- 0.70
phiJBJ1 <- 0.70
phiJBJ2 <- 0.75
phiJBJ3 <- 0.92
phiJBJ4 <- 0.92
phiJBJ5 <- 0.92
phiJBJ6 <- 0.92
phiABJ  <- 0.92


muYBJ  <- 1 - phiYBJ
muPBJ  <- 1 - phiPBJ
muJBJ1 <- 1 - phiJBJ1
muJBJ2 <- 1 - phiJBJ2
muJBJ3 <- 1 - phiJBJ3
muJBJ4 <- 1 - phiJBJ4
muJBJ5 <- 1 - phiJBJ5
muJBJ6 <- 1 - phiJBJ6
muABJ  <- 1 - phiABJ


prCoPBJ <- 0.005
prRPBJ  <- 0.005
prCHPBJ <- 0.005
prCHABJ <- 0.005




###################################################
##########  2. Red-tailed tropicbirds    ##########
###################################################
KBR  <- 50000

SRBR <- 0.79 
EBR  <- 1 
aBR  <- 0.5 
FBR  <- EBR* aBR


phiYBR  <- SRBR * phiJBR1 
phiPBR  <- 0.70
phiJBR1 <- 0.70
phiJBR2 <- 0.75
phiJBR3 <- 0.92
phiJBR4 <- 0.92
phiJBR5 <- 0.92
phiJBR6 <- 0.92
phiABR  <- 0.92


muYBR  <- 1 - phiYBR
muPBR  <- 1 - phiPBR
muJBR1 <- 1 - phiJBR1
muJBR2 <- 1 - phiJBR2
muJBR3 <- 1 - phiJBR3
muJBR4 <- 1 - phiJBR4
muJBR5 <- 1 - phiJBR5
muJBR6 <- 1 - phiJBR6
muABR  <- 1 - phiABR


prCoPBR <- 0.005
prRPBR  <- 0.005
prCHPBR <- 0.005
prCHABR <- 0.005


  
##################################################
################   3. Barn owls   ################
##################################################

KCH  <- 1000

SRCH <- 0.2
aCH  <- 0.5
GCH  <- 6 
ECH  <- 1.5 
FCH  <- aCH * GCH * ECH 

phiYCH  <- SRCH * phiPCH
phiPCH  <- 0.50
phiJCH1 <- 0.50
phiJCH2 <- 0.50
phiACH  <- 0.80

muYCH  <- 1 - phiYCH
muJCH1 <- 1 - phiJCH1
muJCH2 <- 1 - phiJCH2
muACH  <- 1 - phiJCH
roCH   <- 0 
  
prRPCH <- 0.005
  
  
##################################################
################   4. Pied crow   ################
##################################################   
KCo  <- 50000
  
aCo  <- 0.5
BCo  <- 1
ECo  <- 1 
GCo  <- 5
rCo  <- aCo * GCo * BCo * ECo * Co #A VERIFIER
  
roCo <- 100 
  
##################################################
#################    5. Rats     #################
##################################################

KR  <-  50000

aR  <- 0.5  
BR  <- 1
GR  <- 5
ER  <-  4
rR  <- aR * GR * BR * ER * R #A VERIFIER
roR <- 10000    
  
prCoR <- 0.005
prCHR <- 0.005
  

##################################################
##############    6. Parameters     ##############
##################################################

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

# Plot
library(here)


png(paste0(here::here("output/", "CRIME_PREDA_ALL0.005_regCo"), ".png"), 
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
  y = 0.00001 * min(out[out[, vars] > 0, vars], na.rm = TRUE),  # évite les 0
  col = c("yellow3", "red", "brown", "black", "purple"),
  lty = 1, lwd = 2, cex = 2.5,
  legend = c("White-tailed tropicbirds",
             "Red-tailed tropicbirds",
             "Barn owls",
             "Rats",
             "Pied crows"),
  xjust = 0.5)

dev.off()
