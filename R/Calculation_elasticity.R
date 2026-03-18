library(deSolve)
library(ggplot2)
library(here)

# -----------------------------
# 1️⃣ Paramètres fixes de base
# -----------------------------
yini <- c(
  YBJ=80, JBJ1=70, JBJ2=60, JBJ3=50, JBJ4=40, JBJ5=30, JBJ6=25, ABJ=200,
  YBR=800, JBR1=700, JBR2=600, JBR3=500, JBR4=400, JBR5=300, JBR6=250, ABR=2000,
  YCH=40, JCH1=20, JCH2=30, ACH=40,
  Co=200, R=45500
)

# Secondary preys
Au <- 10000; I <- 1000; Pl <- 1000; Sc <- 100; Tor <- 1000

# White-tailed tropicbird
FBJ <- 1.61*0.5; KBJ <- 50000
phiYBJ <- 0.5; phiJBJ1 <- 0.79; phiJBJ2 <- 0.79; phiJBJ3 <- 0.89
phiJBJ4 <- 0.89; phiJBJ5 <- 0.89; phiJBJ6 <- 0.89
muYBJ <- 1-phiYBJ; muJBJ1 <- 1-phiJBJ1; muJBJ2 <- 1-phiJBJ2
muJBJ3 <- 1-phiJBJ3; muJBJ4 <- 1-phiJBJ4; muJBJ5 <- 1-phiJBJ5
muJBJ6 <- 1-phiJBJ6; muABJ <- 1-0.89

# Red-tailed tropicbird
FBR <- 1*0.5; KBR <- 50000
phiYBR <- 0.79; phiJBR1 <- 0.79; phiJBR2 <- 0.79; phiJBR3 <- 0.89
phiJBR4 <- 0.89; phiJBR5 <- 0.89; phiJBR6 <- 0.89; muYBR <- 1-phiYBR
muJBR1 <- 1-phiJBR1; muJBR2 <- 1-phiJBR2; muJBR3 <- 1-phiJBR3
muJBR4 <- 1-phiJBR4; muJBR5 <- 1-phiJBR5; muJBR6 <- 1-phiJBR6
muABR <- 1-0.89

# Barn owls fixes
SRCH_base <- 0.8; aCH <- 0.5; GCH_base <- 6
phiJCH1_base <- 0.5; phiJCH2_base <- 0.5; phiACH_base <- 0.66
phiYCH_base <- SRCH_base
muYCH_base <- 1-phiYCH_base; muJCH1_base <- 1-phiJCH1_base
muJCH2_base <- 1-phiJCH2_base; muACH_base <- 1-phiACH_base
KCH <- 400

# Pied crows
rCo_base <- 0.6; KCo <- 100000; roCo_base <- 0

# Rats
rR_base <- 5; KR <- 1000000; roR_base <- 0
prCHR <- 251; prCoR <- 10

# Predation autres
prCoPBJ <- 10; prRPBJ <- 20; prCHPBJ <- 2; prCHABJ <- 1
prCoPBR <- 15; prRPBR <- 30; prCHPBR <- 5; prCHABR <- 0
prRPCH <- 0

times <- seq(0,50,by=1)

# -----------------------------
# 2️⃣ Fonction ODE sécurisée
# -----------------------------
europa_fct_safe <- function(t, y, parms){
  y[y<0] <- 0
  eps <- 1e-6
  
  YBJ <- y["YBJ"]; JBJ1 <- y["JBJ1"]; JBJ2 <- y["JBJ2"]; JBJ3 <- y["JBJ3"]
  JBJ4 <- y["JBJ4"]; JBJ5 <- y["JBJ5"]; JBJ6 <- y["JBJ6"]; ABJ <- y["ABJ"]
  YBR <- y["YBR"]; JBR1 <- y["JBR1"]; JBR2 <- y["JBR2"]; JBR3 <- y["JBR3"]
  JBR4 <- y["JBR4"]; JBR5 <- y["JBR5"]; JBR6 <- y["JBR6"]; ABR <- y["ABR"]
  YCH <- y["YCH"]; JCH1 <- y["JCH1"]; JCH2 <- y["JCH2"]; ACH <- y["ACH"]
  Co <- y["Co"]; R <- y["R"]
  
  with(as.list(parms), {
    
    Proies_Co <- YBJ+JBJ1+JBJ2+JBJ3+JBJ4+JBJ5+JBJ6+ABJ +
      YBR+JBR1+JBR2+JBR3+JBR4+JBR5+JBR6+ABR +
      Co+R+eps
    Proies_Ch <- YBJ+JBJ1+JBJ2+JBJ3+JBJ4+JBJ5+JBJ6+ABJ +
      YBR+JBR1+JBR2+JBR3+JBR4+JBR5+JBR6+ABR +
      ACH+R+eps
    Proies_R <- YBJ+JBJ1+JBJ2+JBJ3+JBJ4+JBJ5+JBJ6 +
      YBR+JBR1+JBR2+JBR3+JBR4+JBR5+JBR6 +
      ACH+eps
    
    Proies_Co <- max(Proies_Co, eps)
    Proies_Ch <- max(Proies_Ch, eps)
    Proies_R <- max(Proies_R, eps)
    
    # White-tailed tropicbird
    dYBJ <- FBJ*ABJ*(1-ABJ/KBJ) - phiYBJ*YBJ - muYBJ*YBJ -
      prRPBJ*R*(YBJ/Proies_R) - prCoPBJ*Co*(YBJ/Proies_Co) - prCHPBJ*ACH*(YBJ/Proies_Ch)
    dJBJ1 <- phiYBJ*YBJ - muJBJ1*JBJ1 - phiJBJ1*JBJ1
    dJBJ2 <- phiJBJ1*JBJ1 - muJBJ2*JBJ2 - phiJBJ2*JBJ2
    dJBJ3 <- phiJBJ2*JBJ2 - muJBJ3*JBJ3 - phiJBJ3*JBJ3
    dJBJ4 <- phiJBJ3*JBJ3 - muJBJ4*JBJ4 - phiJBJ4*JBJ4
    dJBJ5 <- phiJBJ4*JBJ4 - muJBJ5*JBJ5 - phiJBJ5*JBJ5
    dJBJ6 <- phiJBJ5*JBJ5 - muJBJ6*JBJ6 - phiJBJ6*JBJ6
    dABJ <- phiJBJ6*JBJ6 - muABJ*ABJ - prCHABJ*ACH*(ABJ/Proies_Ch)
    
    # Red-tailed tropicbird
    dYBR <- FBR*ABR*(1-ABR/KBR) - phiYBR*YBR - muYBR*YBR -
      prRPBR*R*(YBR/Proies_R) - prCoPBR*Co*(YBR/Proies_Co) - prCHPBR*ACH*(YBR/Proies_Ch)
    dJBR1 <- phiYBR*YBR - muJBR1*JBR1 - phiJBR1*JBR1
    dJBR2 <- phiJBR1*JBR1 - muJBR2*JBR2 - phiJBR2*JBR2
    dJBR3 <- phiJBR2*JBR2 - muJBR3*JBR3 - phiJBR3*JBR3
    dJBR4 <- phiJBR3*JBR3 - muJBR4*JBR4 - phiJBR4*JBR4
    dJBR5 <- phiJBR4*JBR4 - muJBR5*JBR5 - phiJBR5*JBR5
    dJBR6 <- phiJBR5*JBR5 - muJBR6*JBR6 - phiJBR6*JBR6
    dABR <- phiJBR6*JBR6 - muABR*ABR - prCHABR*ACH*(ABR/Proies_Ch)
    
    # Barn owls
    dYCH <- max(aCH*GCH*ECH*ACH*(1-ACH/KCH),eps) - phiYCH*YCH - muYCH*YCH - prRPCH*R*(YCH/Proies_R)
    dJCH1 <- phiYCH*YCH - muJCH1*JCH1 - phiJCH1*JCH1
    dJCH2 <- phiJCH1*JCH1 - muJCH2*JCH2 - phiJCH2*JCH2
    dACH <- phiJCH2*JCH2 - muACH*ACH
    
    # Pied crows
    dCO <- rCo*Co*(1 - Co/KCo) - roCo*Co
    
    # Rats
    dR <- rR*R*(1 - R/KR) - roR*R - prCHR*ACH*(R/Proies_Ch) - prCoR*Co*(R/Proies_Co)
    
    return(list(c(
      dYBJ=dYBJ,dJBJ1=dJBJ1,dJBJ2=dJBJ2,dJBJ3=dJBJ3,dJBJ4=dJBJ4,dJBJ5=dJBJ5,
      dJBJ6=dJBJ6,dABJ=dABJ,dYBR=dYBR,dJBR1=dJBR1,dJBR2=dJBR2,dJBR3=dJBR3,
      dJBR4=dJBR4,dJBR5=dJBR5,dJBR6=dJBR6,dABR=dABR,dYCH=dYCH,dJCH1=dJCH1,
      dJCH2=dJCH2,dACH=dACH,dCo=dCO,dR=dR
    )))
  })
}

# -----------------------------
# 3️⃣ Fonction lambda sécurisée sur 10 ans
# -----------------------------
calc_lambda <- function(time,pop){
  eps <- 1e-6
  pop <- pop + eps
  idx <- which(time<=10)
  if(all(pop[idx]<=eps)) return(NA)
  log_pop <- log(pop[idx])
  growth_rate <- diff(log_pop)/diff(time[idx])
  r <- mean(growth_rate,na.rm=TRUE)
  return(exp(r))
}

# -----------------------------
# 4️⃣ Boucle sur les paramètres
# -----------------------------
param_list <- list(
  SRCH=seq(0,1,0.01),
  GCH=seq(1,8,1),
  phiJCH=seq(0.1,1,0.1),
  phiACH=seq(0.1,1,0.1),
  rR=seq(1,12,1),
  rCo=seq(0,1,0.1),
  roCo=seq(0,1,0.1),
  roR=seq(1,120,1)
)

for(param_name in names(param_list)){
  vals <- param_list[[param_name]]
  results <- data.frame(val=vals, lambda=NA)
  
  for(i in seq_along(vals)){
    val <- vals[i]
    
    parms <- list(
      Au=Au,I=I,Pl=Pl,Sc=Sc,Tor=Tor,
      FBJ=FBJ,KBJ=KBJ,muYBJ=muYBJ,muJBJ1=muJBJ1,muJBJ2=muJBJ2,
      muJBJ3=muJBJ3,muJBJ4=muJBJ4,muJBJ5=muJBJ5,muJBJ6=muJBJ6,muABJ=muABJ,
      phiYBJ=phiYBJ,phiJBJ1=phiJBJ1,phiJBJ2=phiJBJ2,phiJBJ3=phiJBJ3,
      phiJBJ4=phiJBJ4,phiJBJ5=phiJBJ5,phiJBJ6=phiJBJ6,
      
      FBR=FBR,KBR=KBR,muYBR=muYBR,muJBR1=muJBR1,muJBR2=muJBR2,muJBR3=muJBR3,
      muJBR4=muJBR4,muJBR5=muJBR5,muJBR6=muJBR6,muABR=muABR,
      phiYBR=phiYBR,phiJBR1=phiJBR1,phiJBR2=phiJBR2,phiJBR3=phiJBR3,
      phiJBR4=phiJBR4,phiJBR5=phiJBR5,phiJBR6=phiJBR6,
      
      FCH=aCH*GCH_base*ECH_seq[1],KCH=KCH,
      muYCH=muYCH_base,muJCH1=muJCH1_base,muJCH2=muJCH2_base,muACH=muACH_base,
      phiYCH=phiYCH_base,phiJCH1=phiJCH1_base,phiJCH2=phiJCH2_base,phiACH=phiACH_base,
      
      rR=rR_base,KR=KR,roR=roR_base,
      rCo=rCo_base,KCo=KCo,roCo=roCo_base,
      
      prCoPBJ=prCoPBJ,prRPBJ=prRPBJ,prCHPBJ=prCHPBJ,prCHABJ=prCHABJ,
      prCoPBR=prCoPBR,prRPBR=prRPBR,prCHPBR=prCHPBR,prCHABR=prCHABR,
      prRPCH=prRPCH,prCHR=prCHR,prCoR=prCoR
    )
    
    if(param_name=="SRCH") { parms$phiYCH <- val; parms$muYCH <- 1-val }
    if(param_name=="GCH") { parms$GCH <- val }
    if(param_name=="phiJCH") { parms$phiJCH1 <- val; parms$phiJCH2 <- val; parms$muJCH1 <- 1-val; parms$muJCH2 <- 1-val }
    if(param_name=="phiACH") { parms$phiACH <- val; parms$muACH <- 1-val }
    if(param_name=="rR") { parms$rR <- val }
    if(param_name=="rCo") { parms$rCo <- val }
    if(param_name=="roR") { parms$roR <- val }
    if(param_name=="roCo") { parms$roCo <- val }
    
    out <- ode(y=yini, times=times, func=europa_fct_safe, parms=parms,
               rtol=1e-5, atol=1e-5,maxsteps=1e6)
    out <- as.data.frame(out)
    pop_ch <- out$YCH + out$JCH1 + out$JCH2 + out$ACH
    results$lambda[i] <- calc_lambda(out$time, pop_ch)
  }
  
  #EXPORTER EN CSV LES RESULTATS
  write.csv(
    results, 
    file = here::here("output", paste0("lambda_chouette", param_name, ".csv")), 
    row.names = FALSE
  )
  
  # Sauvegarder le graphique
  p <- ggplot(results, aes(x=val,y=lambda)) +
    geom_line() + geom_point() +
    geom_hline(yintercept=1, linetype="dashed",color="red") +
    labs(x=param_name, y="Croissance de la population de chouettes(10 premières années)",
         title=paste("Sensibilité de la croissance de la population de chouette selon ",param_name)) +
    theme_bw()
  
  dir.create(here("output"), showWarnings = FALSE)
  ggsave(here("output",paste0("lambda_chouettes",param_name,".png")),p,width=8,height=6,dpi=300)
}