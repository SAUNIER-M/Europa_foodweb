# =====================================================
#' @description
#' This script builds a multitrophic foodweb comprising pied crows, 
#' barn owls, rats, red-tailed tropicbirds and white-tailed tropicbirds
#' It simulates the population viability of each specie thank to
#' "europa_fct" fonction
#' 
#' 
#' #####################
#' Species code 
#' @param BJ       White-tailed tropicbird ("Bec Jaune" in french)
#' @param BR       Red-tailed tropicbird ("Brins Rouges" in french)
#' @param R        Rats
#' @param Co       Pied crows ("COrbeaux pies" in french)
#' @param CH       Barn owls ("CHouettes" in french)
#' 
#' 
#'
#' #####################
#' 
#' Age-class prefix, described here, is added before the species code.
#' We did not considered different age-classes for Rats et Pied Crows
#' 
#' @param A        Adult (sexually mature)
#' @param P        Combine egg and chick
#' @param Y        Yearling (from egg laying to one years old)
#' @param J        Juvenile of age species in suffixe
#' (example `JBJ1` is a one-year-old juvenile white-tailed tropicbird.
#' 
#' 
#' 
#' #####################
#' 
#' Variable names may combine a biological parameter prefix with 
#' an age-class and species code. The prefix indicates the parameter type,
#' and the suffix refers to the species.
#' For example: `FBJ` — fecundity (`F`) of white tailed tropicbird (`BJ`)
#' 
#' @param F         Fecundity
#' @param K         Maximum carrying capicity
#' @param phi       Survival rate
#' @param mu        Mortality rate
#' @param prXY      Predation rate of X, on Y
#' @param Proies_   All the prey consumed by the defined species 
#' @param r         Intrinsic population growth
#' @param ro        Regulation (individual per time step)
#' 
# =====================================================


library(deSolve)


europa_fct <- function(t, y, parms) { 
  
  y[y < 0] <- 0 # To avoid negative value
  eps <- 1e-6  # To avoid null denominator 
  with(as.list(c(y, parms)), { 
   
    # White-tailed tropibcirds
    dPBJ <- FBJ * (1-(ABJ/KBJ)) - phiYBJ * PBJ - muYBJ * PBJ - 
           (PBJ / Proies_R) * prRPBJ * R * PBJ - 
           (PBJ / Proies_Co) * prCoPBJ * Co * PBJ - 
           (PBJ / Proies_Ch) * prCHPBJ * ACH * PBJ 
    
    dJBJ1 <- phiYBJ * PBJ - muYBJ * PBJ - phiJBJ1 * JBJ1
    dJBJ2 <- phiJBJ1 * JBJ1 - muJBJ1 * JBJ1 - phiJBJ2 * JBJ2
    dJBJ3 <- phiJBJ2 * JBJ2 - muJBJ2 * JBJ2 - phiJBJ3 * JBJ3
    dJBJ4 <- phiJBJ3 * JBJ3 - muJBJ3 * JBJ3 - phiJBJ4 * JBJ4
    dJBJ5 <- phiJBJ4 * JBJ4 - muJBJ4 * JBJ4 - phiJBJ5 * JBJ5
    dJBJ6 <- phiJBJ5 * JBJ5 - muJBJ5 * JBJ5 - phiJBJ6 * JBJ6
    
    dABJ <- phiJBJ6*JBJ6 - muABJ - 
      (ABJ / Proies_Ch) * prCHABJ * ACH * ABJ
    
    
    # Red-tailed tropicbirds
    dPBR <- FBR * (1-(ABR/KBR)) - phiYBR * PBR - muYBR * PBR - 
           (PBR / Proies_R) * prRPBR * R * PBR - 
           (PBR / Proies_Co) * prCoPBR * Co * PBR - 
           (PBR / Proies_Ch) * prCHPBR * ACH * PBR 
    
    dJBR1 <- phiYBR * PBR - muYBR * PBR - phiJBR1 * JBR1
    dJBR2 <- phiJBR1 * JBR1 - muJBR1 * JBR1 - phiJBR2 * JBR2
    dJBR3 <- phiJBR2 * JBR2 - muJBR2 * JBR2 - phiJBR3 * JBR3
    dJBR4 <- phiJBR3 * JBR3 - muJBR3 * JBR3 - phiJBR4 * JBR4
    dJBR5 <- phiJBR4 * JBR4 - muJBR4 * JBR4 - phiJBR5 * JBR5
    dJBR6 <- phiJBR5 * JBR5 - muJBR5 * JBR5 - phiJBR6 * JBR6
    
    dABR <- phiJBR6*JBR6 - muABR - 
           (ABR / Proies_Ch) * prCHABR * ACH * ABR
    
    
    # Barn owls
    dPCH <-  FCH * ACH * (1 - ACH / KCH) - phiYCH * PCH - muYCH * PCH - 
            (PCH / Proies_R) * prRPCH * R * PCH
    
    dJCH1 <- phiPCH * PCH - muJCH1 * JCH1 - phiJCH1 * JCH1
    dJCH2 <- phiJCH1 * JCH1 - muJCH1 * JCH2 - phiJCH2 * JCH2
    dACH <- phiJCH2 * JCH2 - muACH * ACH + phiACH * ACH - roCH * ACH
    
    
    # Pied crows
    dCO <- rCo * (1-Co/KCo) - roCo * Co
    
    
    # Rats
    dR <- rR * (1-R/KR) - roR * R -
          R / (Proies_Ch) * prCHR* ACH * R -
          R / (Proies_Co) * prCoR * Co * R
    
    
    list(c(dPBJ = dPBJ, dJBJ1 = dJBJ1, dJBJ2 = dJBJ2, dJBJ3 = dJBJ3,
           dJBJ4 = dJBJ4, dJBJ5 = dJBJ5, dJBJ6 = dJBJ6, dABJ = dABJ, 
           dPBR = dPBR, dJBR1 = dJBR1, dJBR2 = dJBR2, dJBR3 = dJBR3,
           dJBR4 = dJBR4, dJBR5 = dJBR5, dJBR6 = dJBR6, dABR = dABR,
           dPCH = dPCH, dJCH1 = dJCH1, dJCH2 = dJCH2, dACH = dACH,
           dCo = dCO, dR = dR))
  })
}


