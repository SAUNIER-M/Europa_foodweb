library(deSolve)
library(ggplot2)
library(here)

# -----------------------------
#  Paramètres fixes 
# -----------------------------
yini <- c(
  YBJ=80, JBJ1=70, JBJ2=60, JBJ3=50, JBJ4=40, JBJ5=30, JBJ6=25, ABJ=200,
  YBR=800, JBR1=700, JBR2=600, JBR3=500, JBR4=400, JBR5=300, JBR6=250, ABR=2000,
  YCH=40, JCH1=20, JCH2=30, ACH=40,
  Co=300, R=45500
)

Au <- 10
I <- 10
Pl <- 10
Sc <- 10
Tor <- 50



# Tropicbirds
alpha <- 0.5
KBJ <- 50000
phiYBJ <- 0.63 #in Maldive Sebastian Steibl and James Russell paper in prep
phiJBJ1 <- 0.79
phiJBJ2 <- 0.79
phiJBJ3 <- 0.92
phiJBJ4 <- 0.92
phiJBJ5 <- 0.92
phiJBJ6 <- 0.92
phiABJ <- 0.92 #from Pouhou islet, Mayotte, pers.com
FBJ <- 1.61 * alpha #from catry et al 2009 Aride island

muYBJ <- 1 - phiYBJ
muJBJ1 <- 1 - phiJBJ1
muJBJ2 <- 1 - phiJBJ2
muJBJ3 <- 1 - phiJBJ3
muJBJ4 <- 1 - phiJBJ4
muJBJ5 <- 1 - phiJBJ5
muJBJ6 <- 1 - phiJBJ6
muABJ <- 1 - phiABJ



KBR <- 50000
phiYBR <- 0.79 # from Le Corre in Europa phd disertation
phiJBR1 <- 0.79
phiJBR2 <- 0.79
phiJBR3 <- 0.89
phiJBR4 <- 0.89
phiJBR5 <- 0.89
phiJBR6 <- 0.89 #Review Egerton et al 2022 from Doherty et al 2004 - Johnston atoll
muYBR <- 1 - phiYBR
muJBR1 <- 1 - phiJBR1
muJBR2 <- 1 - phiJBR2
muJBR3 <- 1 - phiJBR3
muJBR4 <- 1 - phiJBR4
muJBR5 <- 1 - phiJBR5
muJBR6 <- 1 - phiJBR6
muABR <- 1 - 0.89

FBR <- 1 * alpha 


# Barn owls
SRCH_base <- 0.27 #success probability of each egg (Mayotte Steven et 1999)
aCH <- 0.5
GCH_base <- 6 #seen in Europa
ECH_base <- 1.2
FCH_base <- aCH * GCH_base * ECH_base
phiJCH_base <- 0.29 #from Altwegg in Europe
phiACH_base <- 0.57
phiYCH_base <- SRCH_base
muYCH_base <- 1 - phiYCH_base
muJCH_base <- 1 - phiJCH_base
muACH_base <- 1 - phiACH_base

KCH <- 400

# Crows & rats
rCo_base <- 0.6 #lambda = 1.82
KCo <- 5000
roCo_base <- 0

rR_base <- 1.5 #4 #from Dumont et al 2010
KR <- 100000
roR_base <- 0


# Predation
prCoPBJ <- 0.5
prRPBJ <- 0.05
prCHPBJ <- 0.01
prCHABJ <- 0.002
prCoPBR <- 0.5
prRPBR <- 0.1
prCHPBR <- 0
prCHABR <- 0
prRPCH <- 0.000001
prCHR <- 250
prCoR <- 0 



# -----------------------------
# Fontction Europa
# -----------------------------
times <- seq(0,40,by=1) #for 20 years to avoid K effect

europa_fct_safe <- function(t, y, parms){
  y[y<0] <- 0 #to avoid negative value
  eps <- 1e-6 #to avoid null denominator
  
  with(as.list(c(y, parms)), {
    
    Proies_Co <- YBJ + YBR +  Co + R +
      Au + I + Pl + Sc + Tor + eps
    
    Proies_Ch <- YBJ + ABJ + YBR + R +
      Au + I + Sc + eps
    
    Proies_R <- YBJ + YBR +  YCH +
      Au + I + Pl + Sc + Tor + eps
    
    # WTTB
    dYBJ <- FBJ * ABJ * (1-ABJ / KBJ) - phiYBJ * YBJ - muYBJ * YBJ -
      prRPBJ * R *(YBJ / Proies_R) -
      prCoPBJ * Co *(YBJ / Proies_Co) -
      prCHPBJ * ACH * (YBJ / Proies_Ch)
    
    dJBJ1 <- phiYBJ * YBJ - muJBJ1 * JBJ1 - phiJBJ1 * JBJ1
    dJBJ2 <- phiJBJ1 * JBJ1 - muJBJ2 * JBJ2 - phiJBJ2 * JBJ2
    dJBJ3 <- phiJBJ2 * JBJ2 - muJBJ3 * JBJ3 - phiJBJ3 * JBJ3
    dJBJ4 <- phiJBJ3 * JBJ3 - muJBJ4 * JBJ4 - phiJBJ4 * JBJ4
    dJBJ5 <- phiJBJ4 * JBJ4 - muJBJ5 * JBJ5 - phiJBJ5 * JBJ5
    dJBJ6 <- phiJBJ5 * JBJ5 - muJBJ6 * JBJ6 - phiJBJ6 * JBJ6
    
    dABJ <- phiJBJ6 * JBJ6 - muABJ * ABJ - prCHABJ * ACH * (ABJ / Proies_Ch)
    
    #RTTB
    dYBR <- FBR * ABR * (1- ABR / KBR) - phiYBR * YBR - muYBR * YBR -
      prRPBR * R * (YBR / Proies_R) -
      prCoPBR * Co * (YBR / Proies_Co) -
      prCHPBR * ACH * (YBR / Proies_Ch)
    
    dJBR1 <- phiYBR * YBR - muJBR1 * JBR1 - phiJBR1 * JBR1
    dJBR2 <- phiJBR1 * JBR1 - muJBR2 * JBR2 - phiJBR2 * JBR2
    dJBR3 <- phiJBR2 * JBR2 - muJBR3 * JBR3 - phiJBR3 * JBR3
    dJBR4 <- phiJBR3 * JBR3 - muJBR4 * JBR4 - phiJBR4 * JBR4
    dJBR5 <- phiJBR4 * JBR4 - muJBR5 * JBR5 - phiJBR5 * JBR5
    dJBR6 <- phiJBR5 * JBR5 - muJBR6 * JBR6 - phiJBR6 * JBR6
    dABR <- phiJBR6 * JBR6 - muABR * ABR - prCHABR * ACH *(ABR / Proies_Ch)
    
    # Barn owls 
    dYCH <- FCH * ACH *(1- ACH / KCH) - phiYCH * YCH - 
      muYCH * YCH - prRPCH * R * (YCH / Proies_R)
    
    dJCH1 <- phiYCH * YCH - muJCH1 * JCH1 - phiJCH * JCH1
    dJCH2 <- phiJCH * JCH1 - muJCH2 * JCH2 - phiJCH * JCH2
    dACH <- phiJCH * JCH2 - muACH * ACH
    
    # Pied crws and rats
    dCO <- rCo * Co * (1 - Co / KCo) - roCo * Co
    dR <- rR * R * (1 - R / KR) - roR * R -
      prCHR * ACH * (R / Proies_Ch) - prCoR *Co *(R / Proies_Co)
    
    return(list(c(
      dYBJ,dJBJ1,dJBJ2,dJBJ3,dJBJ4,dJBJ5,dJBJ6,dABJ,
      dYBR,dJBR1,dJBR2,dJBR3,dJBR4,dJBR5,dJBR6,dABR,
      dYCH,dJCH1,dJCH2,dACH,dCO,dR
    )))
  })
}

# -----------------------------
#  Lambda
# -----------------------------
calc_lambda <- function(time,pop){
  eps <- 1e-6
  pop <- pop + eps
  idx <- which(time<=10)
  if(all(pop[idx]<=eps)) return(NA)
  r <- mean(diff(log(pop[idx]))/diff(time[idx]), na.rm=TRUE)
  return(exp(r))
}

# --------------------------------------------------
# Estimation lambda selon variation de param_list
# --------------------------------------------------
param_list <- list(
  ECH = seq(0,2,0.1),
  SRCH = seq(0,1,0.1),
  GCH = seq(1,8,1),
  phiJCH = seq(0.1,1,0.1),
  phiACH = seq(0.1,1,0.1),
  rR = seq(0,1,0.1),
  rCo = seq(0,1,0.1),
  roCo = seq(0,1,0.1),
  roR = seq(0,1.5,0.1)
)

dir.create(here("output"), showWarnings = FALSE)

for(param_name in names(param_list)){
  vals <- param_list[[param_name]]
  results <- data.frame(val=vals, lambda_CH=NA)
  
  for(i in seq_along(vals)){
    val <- vals[i]
    
    parms <- list(
      FBJ = FBJ, KBJ = KBJ, FBR = FBR, KBR = KBR,
      FCH = aCH * GCH_base * ECH_base, KCH = KCH,
      phiYCH = phiYCH_base, muYCH = muYCH_base,
      phiJCH = phiJCH_base, phiJCH = phiJCH_base,
      muJCH1 = muJCH1_base, muJCH2 = muJCH2_base,
      phiACH = phiACH_base, muACH = muACH_base,
      rR = rR_base, KR = KR, roR = roR_base,
      rCo = rCo_base, KCo = KCo, roCo = roCo_base,
      prCHR = prCHR, prCoR = prCoR,
      prCoPBJ = prCoPBJ, prRPBJ = prRPBJ, prCHPBJ = prCHPBJ, prCHABJ = prCHABJ,
      prCoPBR = prCoPBR, prRPBR = prRPBR, prCHPBR = prCHPBR, prCHABR = prCHABR,
      prRPCH = prRPCH
    )
    
    # Sensibilité
    if(param_name == "ECH"){parms$FCH <- aCH * GCH_base * val }
    if(param_name == "SRCH"){ parms$phiYCH <- val; parms$muYCH <- 1-val }
    if(param_name == "phiACH"){ parms$phiACH <- val; parms$muACH <- 1-val }
    if(param_name == "phiJCH"){ parms$phiJCH <- val; parms$phiJCH <- 1-val }
    if(param_name == "GCH"){ parms$GCH <- val; parms$FCH <- aCH * GCH_base * val}
    
    if(param_name == "rR"){ parms$rR <- val }
    if(param_name == "rCo"){ parms$rCo <- val }
    if(param_name == "roR"){ parms$roR <- val }
    if(param_name == "roCo"){ parms$roCo <- val }
    
    out <- ode(y = yini, times = times, func = europa_fct_safe, parms = parms)
    out <- as.data.frame(out)
    
    results$lambda_CH[i] <- calc_lambda(out$time, out$YCH + out$JCH1 + out$JCH2 + out$ACH)
    results$lambda_BJ[i] <- calc_lambda(out$time, out$YBJ + out$JBJ1 + out$JBJ2 + out$JBJ3 + out$JBJ4 + out$JBJ5 + out$JBJ6 + out$ABJ)
    results$lambda_BR[i] <- calc_lambda(out$time, out$YBR + out$JBR1 + out$JBR2 + out$JBR3 + out$JBR4 + out$JBR5 + out$JBR6 + out$ABR)
    results$lambda_R[i] <- calc_lambda(out$time, out$R)
    results$lambda_Co[i] <- calc_lambda(out$time, out$Co)
  }
  
  # OUTPUT 
  write.csv(results, here("output", paste0("lambda_",param_name,".csv")), row.names=FALSE)
  
    p_CH <- ggplot(results, aes(x=val,y=lambda_CH)) +
      geom_line() + geom_point() +
      geom_hline(yintercept=1, linetype="dashed",color="red") +
      labs(x=param_name, y="λ Chouettes",
         title=paste("Sensibilité λ chouettes selon", param_name)) +
      theme_bw()
    ggsave(here("output",paste0("lambda_CH_",param_name,".png")), p_CH, width=8, height=6, dpi=300)

    p_BJ <- ggplot(results, aes(x=val,y=lambda_BJ)) +
      geom_line() + geom_point() +
      geom_hline(yintercept=1, linetype="dashed",color="red") +
      labs(x=param_name, y="λ BJ",
       title=paste("Sensibilité λ BJ selon", param_name)) +
      theme_bw()
    ggsave(here("output",paste0("lambda_BJ_",param_name,".png")), p_BJ, width=8, height=6, dpi=300)


    p_BR <- ggplot(results, aes(x=val,y=lambda_BR)) +
     geom_line() + geom_point() +
     geom_hline(yintercept=1, linetype="dashed",color="red") +
     labs(x=param_name, y="λ BR",
       title=paste("Sensibilité λ BR selon", param_name)) +
     theme_bw()
    ggsave(here("output",paste0("lambda_BR_",param_name,".png")), p_BR, width=8, height=6, dpi=300)


    p_R <- ggplot(results, aes(x=val,y=lambda_R)) +
      geom_line() + geom_point() +
      geom_hline(yintercept=1, linetype="dashed",color="red") +
      labs(x=param_name, y="λ Rats",
       title=paste("Sensibilité λ Rats selon", param_name)) +
      theme_bw()
    ggsave(here("output",paste0("lambda_R_",param_name,".png")), p_R, width=8, height=6, dpi=300)


    p_Co <- ggplot(results, aes(x=val,y=lambda_Co)) +
      geom_line() + geom_point() +
      geom_hline(yintercept=1, linetype="dashed",color="red") +
      labs(x=param_name, y="λ Corbeaux pies",
       title=paste("Sensibilité λ Corbeaux pies selon", param_name)) +
      theme_bw()
    ggsave(here("output",paste0("lambda_Co_",param_name,".png")), p_Co, width=8, height=6, dpi=300)
  }
  



# ---------------------------------------------------------------------
#  Sensibilité combinée roCo × roR (isolignes) pour WTTB WTTR et Barn owls
# ---------------------------------------------------------------------
roCo_vals <- seq(0, 0.6, 0.1) 
roR_vals <- seq(1, 1.5, 0.1)
results_comb <- expand.grid(roCo = roCo_vals, roR = roR_vals)
results_comb$lambda_PBJ <- NA
results_comb$lambda_PBR <- NA
results_comb$lambda_CH <- NA


for(i in 1:nrow(results_comb)){
  parms <- list(
    Au = Au, I = I, Pl = Pl, Sc = Sc, Tor = Tor,
    FBJ = FBJ, KBJ = KBJ, muYBJ = muYBJ, muJBJ1 = muJBJ1, muJBJ2 = muJBJ2,
    muJBJ3 = muJBJ3, muJBJ4 = muJBJ4, muJBJ5 = muJBJ5, muJBJ6 = muJBJ6, muABJ = muABJ,
    phiYBJ = phiYBJ, phiJBJ1 = phiJBJ1, phiJBJ2 = phiJBJ2, phiJBJ3 = phiJBJ3,
    phiJBJ4 = phiJBJ4, phiJBJ5 = phiJBJ5, phiJBJ6 = phiJBJ6, phiABJ = phiABJ,
    
    FBR = FBR, KBR = KBR, muYBR = muYBR, muJBR1 = muJBR1, muJBR2 = muJBR2, muJBR3 = muJBR3,
    muJBR4 = muJBR4, muJBR5 = muJBR5, muJBR6 = muJBR6, muABR = muABR,
    phiYBR = phiYBR, phiJBR1 = phiJBR1, phiJBR2 = phiJBR2, phiJBR3 = phiJBR3,
    phiJBR4 = phiJBR4, phiJBR5 = phiJBR5, phiJBR6 = phiJBR6,
    
    FCH = aCH * GCH_base * ECH_base, KCH = KCH,
    muYCH = muYCH_base, muJCH1 = muJCH1_base, muJCH2 = muJCH2_base, muACH = muACH_base,
    phiYCH = phiYCH_base, phiJCH = phiJCH_base, phiJCH = phiJCH_base, phiACH = phiACH_base,
    
    rR = rR_base, KR = KR, roR = results_comb$roR[i],
    rCo = rCo_base, KCo = KCo, roCo = results_comb$roCo[i],
    
    prCoPBJ = prCoPBJ, prRPBJ = prRPBJ, prCHPBJ = prCHPBJ, prCHABJ = prCHABJ,
    prCoPBR = prCoPBR, prRPBR = prRPBR, prCHPBR = prCHPBR, prCHABR = prCHABR,
    prRPCH = prRPCH, prCHR = prCHR, prCoR = prCoR
  )
  
  out <- ode(y = yini, times = times, func = europa_fct_safe, parms = parms,
             rtol = 1e-5, atol = 1e-5, maxsteps = 1e6)
  out <- as.data.frame(out)
  results_comb$lambda_PBJ[i] <- calc_lambda(out$time, out$YBJ + out$JBJ1 + out$JBJ2 + out$JBJ3 + out$JBJ4 + out$JBJ5 + out$JBJ6 + out$ABJ)
  results_comb$lambda_PBR[i] <- calc_lambda(out$time, out$YBR + out$JBR1 + out$JBR2 + out$JBR3 + out$JBR4 + out$JBR5 + out$JBR6 + out$ABR)
  results_comb$lambda_CH[i] <- calc_lambda(out$time, out$YCH + out$JCH1 + out$JCH2 + out$ACH)
  
  
}

# Sauvegarder CSV combiné
write.csv(results_comb, here("output","lambda_roCo_roR_combined.csv"), row.names=FALSE)

# Plot isolignes PBJ
p_BJ_iso <- ggplot(results_comb, aes(x=roCo, y=roR, z=lambda_PBJ)) +
  geom_contour(color="black") +
  geom_text(stat="contour", aes(label=..level..), size=3) +
  labs(x="roCo", y="roR ", title="Isolignes λ BJ") +
  theme_minimal()
ggsave(here("output","isoligne_BJ_roR_roCo.png"), p_BJ_iso, width=8, height=6, dpi=300)

# Plot isolignes PBR
p_BR_iso <- ggplot(results_comb, aes(x=roCo, y=roR, z=lambda_PBR)) +
  geom_contour(color="black") +
  geom_text(stat="contour", aes(label=..level..), size=3) +
  labs(x="roCo ", y="roR ", title="Isolignes λ BR") +
  theme_minimal()
ggsave(here("output","isoligne_BR_roR_roCo.png"), p_BR_iso, width=8, height=6, dpi=300)


# Plot isolignes PBJ
p_CH_iso<- ggplot(results_comb, aes(x=roCo, y=roR, z=lambda_CH)) +
  geom_contour(color="black") +
  geom_text(stat="contour", aes(label=..level..), size=3) +
  labs(x="roCo", y="roR", title="Isolignes λ CH") +
  theme_minimal()
ggsave(here("output","isoligne_CH_roR_roCo.png"), p_CH_iso, width=8, height=6, dpi=300)


