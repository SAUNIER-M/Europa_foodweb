europa_fct <- function(t, y, parms) { 
  
  y[y < 0] <- 0 # To avoid negative value
  eps <- 1e-6  # To avoid null denominator 
  with(as.list(c(y, parms)), { 
   
    # White-tailed tropibcirds
    dYBJ <- FBJ * ABJ * (1 - (ABJ / KBJ)) -
      phiYBJ * YBJ - muYBJ * YBJ -
      prRPBJ  * R   * (YBJ / Proies_R) -
      prCoPBJ * Co  * (YBJ / Proies_Co) -
      prCHPBJ * ACH * (YBJ / Proies_Ch)
      
    
    dJBJ1 <- phiYBJ * YBJ  - muJBJ1 * JBJ1 - phiJBJ1 * JBJ1
    dJBJ2 <- phiJBJ1 * JBJ1 - muJBJ2 * JBJ2 - phiJBJ2 * JBJ2
    dJBJ3 <- phiJBJ2 * JBJ2 - muJBJ3 * JBJ3 - phiJBJ3 * JBJ3
    dJBJ4 <- phiJBJ3 * JBJ3 - muJBJ4 * JBJ4 - phiJBJ4 * JBJ4
    dJBJ5 <- phiJBJ4 * JBJ4 - muJBJ5 * JBJ5 - phiJBJ5 * JBJ5
    dJBJ6 <- phiJBJ5 * JBJ5 - muJBJ6 * JBJ6 - phiJBJ6 * JBJ6
    
    
    dABJ<- phiJBJ6 * JBJ6 -  muABJ * ABJ -
      prCHABJ * ACH * (ABJ / (Proies_Ch))
    
    
    # Red-tailed tropicbirds
    
    dYBR <- FBR * ABR * (1 - (ABR / KBR)) - 
      phiYBR * YBR - muYBR * YBR -
      prRPBR  * R   * (YBR / Proies_R) -
      prCoPBR * Co  * (YBR / Proies_Co) -
      prCHPBR * ACH * (YBR / Proies_Ch)
    
    dJBR1 <- phiYBR * YBR  - muJBR1 * JBR1 - phiJBR1 * JBR1
    dJBR2 <- phiJBR1 * JBR1 - muJBR2 * JBR2 - phiJBR2 * JBR2
    dJBR3 <- phiJBR2 * JBR2 - muJBR3 * JBR3 - phiJBR3 * JBR3
    dJBR4 <- phiJBR3 * JBR3 - muJBR4 * JBR4 - phiJBR4 * JBR4
    dJBR5 <- phiJBR4 * JBR4 - muJBR5 * JBR5 - phiJBR5 * JBR5
    dJBR6 <- phiJBR5 * JBR5 - muJBR6 * JBR6 - phiJBR6 * JBR6
    
    
    
    dABR <- phiJBR6 * JBR6 - muABR * ABR -
      prCHABR * ACH * (ABR / (Proies_Ch))
    
    
    
    # Barn owls
    dYCH <- FCH * ACH * (1 - (ACH / KCH)) - phiYCH * YCH -  muYCH * YCH -
      prRPCH * R * (YCH / (Proies_R))
    
    dJCH1 <- phiYCH * YCH - muJCH1 * JCH1 - phiJCH1 * JCH1
    dJCH2 <- phiJCH1 * JCH1 - muJCH2 * JCH2 - phiJCH2 * JCH2
    
    dACH <- phiJCH2 * JCH2 - muACH * ACH
    
    
    
    
    # Pied crows
    dCO <- rCo * (1-Co/KCo) - roCo * Co
    
    
    # Rats
    dR <- rR * (1-R/KR) - roR * R -
          (R / (Proies_Ch)) * prCHR* ACH #* R 
          - (R / (Proies_Co)) * prCoR * Co #* R
    
    
    list(c(dYBJ = dYBJ, dJBJ1 = dJBJ1, dJBJ2 = dJBJ2, dJBJ3 = dJBJ3, #remplacement de DPBJ par DYBJ
           dJBJ4 = dJBJ4, dJBJ5 = dJBJ5, dJBJ6 = dJBJ6, dABJ = dABJ, 
           dYBR = dYBR, dJBR1 = dJBR1, dJBR2 = dJBR2, dJBR3 = dJBR3,
           dJBR4 = dJBR4, dJBR5 = dJBR5, dJBR6 = dJBR6, dABR = dABR,
           dYCH = dYCH, dJCH1 = dJCH1, dJCH2 = dJCH2, dACH = dACH, #remplacer dpch par dych
           dCo = dCO, dR = dR))
  })
}


