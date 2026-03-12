###################################################
#####  Define the prey eaten by each predator #####
######    to simplify predation equations     #####
###################################################


###################################################
###### Define the initial population sizes  #######
###################################################

#Main species
PBJ  <- 80
YBJ  <- 80
JBJ1 <- 70
JBJ2 <- 60
JBJ3 <- 50
JBJ4 <- 40
JBJ5 <- 30
JBJ6 <- 25
ABJ  <- 200

PBR  <- 1600
YBR  <- 1600
JBR1 <- 1400
JBR2 <- 1200
JBR3 <- 1000
JBR4 <- 800
JBR5 <- 600
JBR6 <- 500
ABR  <- 5000

PCH  <- 40 
YCH <- 40
JCH1 <- 20
JCH2 <- 30
ACH  <- 40

Co   <- 1000

R    <- 1000

#Secondary species
Au   <- 1000000
I    <- 1000000
Pl   <- 1000000
Sc   <- 1000000
Tor  <- 10000000





###################################################
#######  1. White-tailed tropicbird rates   #######
###################################################

KBJ   <- 5000000

SRBJ  <- 0.50 
EBJ   <- 1.61 
aBJ   <- 0.5 
FBJ   <- EBJ * aBJ


phiYBJ  <- SRBJ 
phiPBJ  <- 0.79
phiJBJ1 <- 0.79
phiJBJ2 <- 0.79
phiJBJ3 <- 0.89
phiJBJ4 <- 0.89
phiJBJ5 <- 0.89
phiJBJ6 <- 0.89
phiABJ  <- 0.89


muYBJ  <- 1 - phiYBJ
muPBJ  <- 1 - phiPBJ
muJBJ1 <- 1 - phiJBJ1
muJBJ2 <- 1 - phiJBJ2
muJBJ3 <- 1 - phiJBJ3
muJBJ4 <- 1 - phiJBJ4
muJBJ5 <- 1 - phiJBJ5
muJBJ6 <- 1 - phiJBJ6
muABJ  <- 1 - phiABJ


prCoPBJ <- 0
prRPBJ  <- 0
prCHPBJ <- 0
prCHABJ <- 0




###################################################
########  2. Red-tailed tropicbird rates   ########
###################################################
KBR  <- 500000

SRBR <- 0.79 
EBR  <- 1
aBR  <- 0.5
FBR  <- EBR* aBR


phiYBR  <- SRBR 
phiPBR  <- 0.79
phiJBR1 <- 0.79
phiJBR2 <- 0.79
phiJBR3 <- 0.89
phiJBR4 <- 0.89
phiJBR5 <- 0.89
phiJBR6 <- 0.89
phiABR  <- 0.89


muYBR  <- 1 - phiYBR
muPBR  <- 1 - phiPBR
muJBR1 <- 1 - phiJBR1
muJBR2 <- 1 - phiJBR2
muJBR3 <- 1 - phiJBR3
muJBR4 <- 1 - phiJBR4
muJBR5 <- 1 - phiJBR5
muJBR6 <- 1 - phiJBR6
muABR  <- 1 - phiABR


prCoPBR <- 0
prRPBR  <- 0
prCHPBR <- 0
prCHABR <- 0


  
##################################################
#############   3. Barn owls rates   #############
##################################################

KCH  <- 200

SRCH <- 1
aCH  <- 0.5
GCH  <- 6 
ECH  <- 1.3 
FCH  <- aCH * GCH * ECH 


phiJCH1 <- 0.58
phiJCH2 <- 0.58
phiACH  <- 0.66
phiYCH  <- SRCH 


muYCH  <- 1 - phiYCH
muJCH1 <- 1 - phiJCH1
muJCH2 <- 1 - phiJCH2
muACH  <- 1 - phiACH

prRPCH <- 0
  
  
##################################################
#############   4. Pied crow rates  ##############
##################################################   
KCo  <- 10000
  
aCo  <- 0.5
BCo  <- 1
ECo  <- 1 
GCo  <- 5
rCo  <- aCo * GCo * BCo * ECo * Co #A VERIFIER
  
roCo <- 0 
  
##################################################
###############    5. Rats rates   ###############
##################################################

KR  <-  50000

aR  <- 0.5  
BR  <- 1
GR  <- 5
ER  <-  4
rR  <- aR * GR * BR * ER * R #A VERIFIER
roR <- 0    
  
prCoR <- 0
prCHR <- 0
  

##################################################
###############   6. Prey values   ###############
##################################################

Proies_Co <- PBJ + PBR +  Au + I + Pl + Tor + R
Proies_Ch <- PBJ + PBR + ABJ + Au + I + Pl + Sc + R
Proies_R  <- PBJ + PBR + Au + I + Pl + Sc +  PCH 


