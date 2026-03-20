###################################################
#####  Define the prey eaten by each predator #####
######    to simplify predation equations     #####
###################################################


###################################################
###### Define the initial population sizes  #######
###################################################

#Main species

YBJ  <- 80
JBJ1 <- 70
JBJ2 <- 60
JBJ3 <- 50
JBJ4 <- 40
JBJ5 <- 30
JBJ6 <- 25
ABJ  <- 200


YBR  <- 800
JBR1 <- 700
JBR2 <- 600
JBR3 <- 500
JBR4 <- 400
JBR5 <- 300
JBR6 <- 250
ABR  <- 2000

#PCH  <- 40 
YCH <- 40
JCH1 <- 20
JCH2 <- 30
ACH  <- 40

Co   <- 200

R    <- 45500

#Secondary species
Au <- 1000
I <- 1000
Pl <- 1000
Sc <- 300
Tor <- 1000



###################################################
#######  1. White-tailed tropicbird rates   #######
###################################################

KBJ   <- 50000

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


prCoPBJ <- 10
prRPBJ  <- 20
prCHPBJ <- 2
prCHABJ <- 1




###################################################
########  2. Red-tailed tropicbird rates   ########
###################################################
KBR  <- 50000

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


prCoPBR <- 15
prRPBR  <- 30
prCHPBR <- 5
prCHABR <- 0


  
##################################################
#############   3. Barn owls rates   #############
##################################################

KCH  <- 400

SRCH <- 0.8
aCH  <- 0.5
GCH  <- 6

#ECH  <- 0.5
FCH  <- aCH * GCH * ECH 


phiJCH1 <- 0.5
phiJCH2 <- phiJCH1
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
KCo  <- 100000
  
#aCo  <- 0.5
#BCo  <- 1
#ECo  <- 1 
#GCo  <- 5
#rCo  <- aCo * GCo * BCo * ECo * Co #A VERIFIER
rCo <- 0.6 #fixé arbitrairement/ équivaut à lambda 1.82
roCo<- 0 #0.6

##################################################
###############    5. Rats rates   ###############
##################################################

KR  <- 1000000

#aR  <- 0.5  
#BR  <- 1
#GR  <- 5
#ER  <-  2
#rR  <- aR * GR * BR * ER * R #A VERIFIER
rR <-  5
roR <- 0
  
prCoR <- 10
prCHR <- 251 #en considérant une pelote par nuit et occurence de rat 0.69 cf pelotes
  


