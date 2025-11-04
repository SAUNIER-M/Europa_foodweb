source("Eqdiff_fct_deSolve_PEQ_Repro6.R")  
###Définir la taille intiales des populations

OBJ <- 100
PBJ <- 80
JBJ1 <- 70
JBJ2 <- 60
JBJ3 <- 50
JBJ4 <- 40
JBJ5 <- 30
JBJ6 <- 25
ABJ <- 200
OBR <- 1000
PBR  <- 800
JBR1 <- 700
JBR2 <- 600
JBR3 <- 500
JBR4 <- 400
JBR5 <- 300
JBR6 <- 250
ABR <- 2000
OCH <- 60
PCH <- 40
JCH <- 30
ACH <- 40
Co <- 1000
R <- 50000

################################
###Simplifier les prédations ###
################################

#Corbeaux
Proies_Co <- PBJ + PBR +  Au + I + Pl + Tor + R

#Chouettes
Proies_Ch <- PBJ + PBR + ABJ + Au + I + Pl + Sc + R

#Rats
Proies_R <- PBJ + PBR + Au + I + Pl + Sc +  PCH 


### définir les valeurs de toutes les variables ###
##########################
##  Proies secondaires  ##
##########################

Au <- 1000000
I <- 1000000
Pl <- 1000000
Sc <- 1000000
Tor <- 10000000



########################################
##    Pailles en queue à bec jaune    ##
########################################


#######################################
### 1. Se reproduisent tous à 6 ans ###
#######################################
KBJ <- 10000

#paramètres reproducteurs 

FBJ <- 1.61 * 0.5 #fréquence d'individuelle de reproduction * sexratio car un oeuf par ponde et tous les Ad sont considérés reproduceurs


#survie
SR <- 0.50 #succès reproducteur

phiJBJ1 <- 0.70
phiJBJ2 <- 0.71
phiJBJ3 <- 0.92
phiJBJ4 <- 0.92
phiJBJ5 <- 0.92
phiJBJ6 <- 0.92
phiABJ <- 0.92

phiYBJ <- SR * phiJBJ1 #Survie entre la ponte et première année / Avec SR naturel (sans prédateurs)
muABJ <- 1 - phiABJ #mortalité adulte

#prédation
prCoOBJ <- 0
prCoPBJ <- 0
prROBJ <- 0
prRPBJ <- 0
prCHPBJ <- 0
prCHABJ <- 0




#######################################
### 2. Avec proba de repro par age ####
#######################################
KBJ <- 10000

#paramètres reproducteurs 

FBJ <- 1.61 * 0.5 #fréquence d'individuelle de reproduction * sexratio car un oeuf par ponde et tous les Ad sont considérés reproduceurs

#survie
SRBJ <- 0.50
phiJBJ1 <- 0.70
phiJBJ2 <- 0.71
phiJBJ3 <- 0.92
phiJBJ4 <- 0.92
phiJBJ5 <- 0.92
phiJBJ6 <- 0.92
phiABJ <- 0.92
phiPBJ <- SRBJ * phiJBJ1 

#mature
mPBJ <- phiPBJ * 0
mJBJ1 <- phiJBJ1 * 0.004
mJBJ2 <- phiJBJ2 * 0.025
mJBJ3 <- phiJBJ3 * 0.252
mJBJ4 <- phiJBJ4 * 0.457
mJBJ5 <- phiJBJ5 * 0.438
mJBJ6 <- phiJBJ6 * 1

#mortalité
muYBJ <- 1 - phiYBJ
muPBJ <- 1 - phiPBJ
muJBJ1 <- 1 - phiJBJ1
muJBJ2 <- 1 - phiJBJ2
muJBJ3 <- 1 - phiJBJ3
muJBJ4 <- 1 - phiJBJ4
muJBJ5 <- 1 - phiJBJ5
muJBJ6 <- 1 - phiJBJ6
muABJ <- 1 - phiABJ


#prédation
prCoOBJ <- 0
prCoPBJ <- 0
prROBJ <- 0
prRPBJ <- 0
prCHPBJ <- 0
prCHABJ <- 0


##########################################
##    Pailles en queue à brins rouges   ##
##########################################
KBR <- 50000

#paramètres reproducteurs  
aBR <- 0.5  
BBR <- 1
FBR <- 1

#survie
SRBR <- 0.79 #succès reproducteur naturel, sur l'ilôt dératisé d'Europa
phiPBR <- 0.70
phiJBR1 <- 0.75
phiJBR2 <- 0.92
phiJBR3 <- 0.92
phiJBR4 <- 0.92
phiJBR5 <- 0.92
phiJBR6 <- 0.92
phiABR <- 0.92

phiYBR <- SRBR * phiPBR 

#mature
mPBR <- phiPBR * 0
mJBR1 <- phiJBR1 * 0.004
mJBR2 <- phiJBR2 * 0.025
mJBR3 <- phiJBR3 * 0.252
mJBR4 <- phiJBR4 * 0.457
mJBR5 <- phiJBR5 * 0.438
mJBR6 <- phiJBR6 * 1

muYBR <- 1 - phiyBR
muPBR <- 1 - phiPBR
muJBR1 <- 1 - phiJBR1
muJBR2 <- 1 - phiJBR2
muJBR3 <- 1 - phiJBR3
muJBR4 <- 1 - phiJBR4
muJBR5 <- 1 - phiJBR5
muJBR6 <- 1 - phiJBR6
muABR <- 1 - phiABR

#prédation
prCoOBR <- 0
prCoPBR <- 0
prROBR <- 0
prRPBR <- 0
prCHPBR <- 0


#############################
##    Chouettes effraies   ##
############################# 
KCH <- 200

# Paramètres reproducteurs
SRCH <- 0.2
aCH <- 0.5
GCH <- 6 
ECH <- 1.5 #nombre de ponte par dt => à chercher dans a littérature mais pontiellement plus que 1
FCH <- aCH * GCH * ECH 

#Survie

phiPCH <- 0.50
phiJCH <- 0.50
phiACH <- 0.80
phiYCH <- SRCH * phiPCH

#Mortalité
muYCH <- 1 - phiYCH
muPCH <- 1 - phiPCH
muJCH <- 1 - phiPCH
muACH <- 1 - phiJCH
roCH <- 0 #taux de régulation des choeuttes

#prédation
prRPCH <- 0.001



########################
##    Corbeaux pies   ##
########################   
KCo <- 1000

aCo <- 0.5
BCo <- 0.8
FCo <- 4

Co <- 800

roCo <- 0 #taux de régulation des corbeaux pies 


#####################
##    Rats noirs   ##
##################### 
KR <-  50000#voir avec taille domaine vital et taille de l'ile

aR <- 0.5  
BR <- 1    
FR <-  40 

R <- 10000

roR <- 0    #taux de régulation des rats

prCoR <- 0.00001
prCHR <- 0.0005

# Paramètres 
parms <- c(
  Au, I, Pl, Sc, Tor,
  aBJ, BBJ, FBJ, KBJ,  
  muOBJ, muPBJ, muJBJ1, muJBJ2,  muJBJ3,
  muJBJ4, muJBJ5, muJBJ6, muABJ,
  phiOBJ, phiPBJ,  phiJBJ1, phiJBJ2, phiJBJ3,
  phiJBJ4, phiJBJ5, phiJBJ6, phiABJ,
  
  aBR, BBR, FBR, KBR, 
  muOBR, muPBR, muJBR1, muJBR2,  muJBR3,
  muJBR4, muJBR5, muJBR6, muABR,
  phiOBR, phiPBR,  phiJBR1, phiJBR2, phiJBR3,
  phiJBR4, phiJBR5, phiJBR6, phiABR,
  
  aCH, BCH, FCH, KCH, 
  muOCH, muPCH, muJCH, muACH,
  phiOCH, phiPCH,  phiJCH, phiACH, roCH,
  
  aR, BR, FR, KR, roR,
  
  aCo, BCo, FCo, KCo, roCo,
  
  prCoOBJ, prCoPBJ, prROBJ, prRPBJ, prCHPBJ, prCHABJ,
  prCoOBR, prCoPBR, prROBR, prRPBR, prCHPBR,
  prROCH, prCoR, prCHR
  
  
)

# Conditions initiales 
yini <- c(
  OBJ = OBJ, PBJ = PBJ, JBJ1 = JBJ1, JBJ2 = JBJ2, JBJ3 = JBJ3,
  JBJ4 = JBJ4, JBJ5 = JBJ5, JBJ6 = JBJ6, ABJ = ABJ,
  OBR = OBR, PBR = PBR, JBR1 = JBR1, JBR2 = JBR2, JBR3 = JBR3,
  JBR4 = JBR4, JBR5 = JBR5, JBR6 = JBR6, ABR = ABR,
  OCH = OCH, PCH = PCH, JCH = JCH, ACH = ACH,
  Co = Co, R = R
)


# Temps de simulation
times <- seq(0, 500, by = 1)


# Simulation
out <- ode(y = yini, times = times, func = europa_fct , parms = parms)
out <- as.data.frame(out)  


# Affichage des résultats
print(out)

# Variables à garder visieullement (les adultes)

vars <- c("ABJ", "ABR", "ACH", "R", "Co")
range(out$time)
tail(out$time)
# Graphique
png(paste0(here::here("output/", "CRIME_NO_PREDA"), sep = "", ".png"), width = 1000, height = 800, units = "px")
par(mar = c(7, 9, 5, 3), mgp = c(5, 2, 0))  
matplot(out$time, out[, vars], type = "l", lty = 1, lwd = 2,
        col = c("yellow3", "red", "brown", "black", "purple"),
        xlab = "Années", ylab = "Taille des populations",
        cex.lab = 3.5,    
        cex.main = 4,    
        cex.axis = 3,
        font.lab = 2)

legend("topright",  col = c("yellow3", "red", "brown", "black", "purple"),
       lty = 1, lwd = 2,cex = 3, legend = c("Pailles en queue à bec jaune",
                                            "Pailles en queue à brins rouges",
                                            "Chouettes effraies",
                                            "Rats noirs",
                                            "Corbeaux pies"))

dev.off()