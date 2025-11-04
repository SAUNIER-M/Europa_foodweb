library(deSolve)

europa_fct <- function(t, y, parms) { 
  with(as.list(c(y, parms)), { 
    # PQBJ
    dOBJ <- aBJ * BBJ * FBJ * (1-ABJ/KBJ) - phiOBJ * OBJ - muOBJ * OBJ -
      OBJ / (OBJ + Au + I + Pl + Sc + PBJ + OCH + OBR + PBR) * prROBJ * R * OBJ -
      OBJ / (OBJ + Au + I + Sc + Tor + PBJ + R + OBR + PBR) * prCoOBJ * Co * OBJ
    
    dPBJ <- phiOBJ * OBJ - muOBJ * OBJ - phiPBJ * PBJ -
      PBJ / (PBJ + Au + I + Pl + Sc + OBJ + OCH + OBR + PBR) * prRPBJ * R * OBJ -
      PBJ / (PBJ + Au + I + Sc + Tor + OBJ + R + OBR + PBR) * prCoPBJ * Co * OBJ -
      PBJ / (PBJ + Au + I + Sc  + OBR + R + PBJ) * prCHPBJ * ACH * PBJ
    
    dJBJ1 <- phiPBJ * PBJ - muJBJ * PBJ - phiJBJ1 * JBJ1 - mPBJ * PBJ
    dJBJ2 <- phiJBJ1 * JBJ1 - muJBJ1 * JBJ1 - phiJBJ2 * JBJ2 - mJBJ1 * JBJ1
    dJBJ3 <- phiJBJ2 * JBJ2 - muJBJ2 * JBJ2 - phiJBJ3 * JBJ3 - mJBJ2 * JBJ2
    dJBJ4 <- phiJBJ3 * JBJ3 - muJBJ3 * JBJ3 - phiJBJ4 * JBJ4 - mJBJ3 * JBJ3
    dJBJ5 <- phiJBJ4 * JBJ4 - muJBJ4 * JBJ4 - phiJBJ5 * JBJ5 - mJBJ4 * JBJ4
    dJBJ6 <- phiJBJ5 * JBJ5 - muJBJ5 * JBJ5 - phiJBJ6 * JBJ6 - mJBJ5 * JBJ5
    
    dABJ <- phiJBJ6 * JBJ6 - muJBJ6 * JBJ6 + phiABJ * ABJ - muABJ * ABJ -
      ABJ / (PBJ+ABJ+Au+I+Sc+R+PBR) * prCHABJ * ACH * ABJ
      + mJBJ1 * JBJ1 + mJBJ2 * JBJ2 + mJBJ3 * JBJ3 + mJBJ4 * JBJ4 + mJBJ5 * JBJ5 #ceux qui maturent avant classe 6
    
    # PQBR
    dOBR <-  aBR * BBR * FBR * (1-ABR/KBR) - phiOBR * OBR - muOBR * OBR - 
      OBR / (OBR + Au + I + Pl + Sc + PBR + OCH + OBJ + PBJ) * prROBR * R * OBR - 
      OBR / (OBR + Au + I + Sc + Tor + PBR + R + OBJ + PBJ) * prCoOBR * Co * OBR 
    
    dPBR <-  phiOBR * OBR - muOBR * OBR - phiPBR * PBR -
      PBR / (PBR + Au + I + Pl + Sc + OBR + OCH + OBJ + PBJ) * prRPBR * R * PBR -
      PBR / (PBR + Au + I + Sc + Tor + OBR + R + OBJ + PBJ) * prCoPBR * Co * PBR -
      PBR / (PBR + Au + I + Sc  + OBR + R + PBJ) * prCHPBR * ACH * PBR
    
    dJBR1 <- phiPBR * PBR - muPBR * PBR - phiJBR1 * JBR1
    dJBR2 <- phiJBR1 * JBR1 - muJBR1 * JBR2 - phiJBR2 * JBR2 - mJBR1 * JBR1
    dJBR3 <- phiJBR2 * JBR2 - muJBR2 * JBR3 - phiJBR3 * JBR3 - mJBR2 * JBR2
    dJBR4 <- phiJBR3 * JBR3 - muJBR3 * JBR4 - phiJBR4 * JBR4 - mJBR3 * JBR3
    dJBR5 <- phiJBR4 * JBR4 - muJBR4 * JBR5 - phiJBR5 * JBR5 - mJBR4 * JBR4 
    dJBR6 <- phiJBR5 * JBR5 - muJBR5 * JBR6 - phiJBR6 * JBR6 - mJBR5 * JBR5
    
    dABR <- phiJBR6 * JBR6 - muJBR6 * JBR6 + phiABR * ABR - muABR * ABR
      + mJBR1 * JBR1 + mJBR2 * JBR2 + mJBR3 * JBR3 + mJBR4 * JBR4 + mJBR5 * JBR5
    
    # CHOUETTE
    dOCH <- aCH * BCH * FCH * (1-ACH/KCH) - phiOCH * OCH - muOCH * OCH -
      OCH / (OCH + Au + I + Pl + Sc + PBR + OBR + OBJ + PBJ) * prROCH * R * OCH
    
    dPCH <- phiOCH * OCH - muOCH * OCH - phiPCH * PCH 
    dJCH <- phiPCH * PCH - muJCH * OCH - phiJCH * JCH
    dACH <- phiJCH * JCH - muACH * OCH + phiACH * ACH - roCH * ACH
    
    # Corbeaux pies
    dCO <- aCo * BCo * FCo * (1-Co/KCo) - roCo * Co
    
    # Rats
    dR <- aR * BR * FR * (1-R/KR) - roR * R -
      R / (PBJ + Au + I + Sc  + OBR + R + PBJ) * prCHR* ACH * R -
      R / (OBR + Au + I + Sc + Tor + PBR + R + OBJ + PBJ) * prCoR * Co * R
    
    list(c(
      dOBJ = dOBJ, dPBJ = dPBJ, dJBJ1 = dJBJ1, dJBJ2 = dJBJ2, dJBJ3 = dJBJ3,
      dJBJ4 = dJBJ4, dJBJ5 = dJBJ5, dJBJ6 = dJBJ6, dABJ = dABJ,
      dOBR = dOBR, dPBR = dPBR, dJBR1 = dJBR1, dJBR2 = dJBR2, dJBR3 = dJBR3,
      dJBR4 = dJBR4, dJBR5 = dJBR5, dJBR6 = dJBR6, dABR = dABR,
      dOCH = dOCH, dPCH = dPCH, dJCH = dJCH, dACH = dACH,
      dCo = dCO, dR = dR))
  })
}

