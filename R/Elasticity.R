# Chargement des données
df1<- read.csv(
  here("output/csv_lambda", "lambda_SRCH.csv"),
  header = TRUE, sep =","
)

# Fonction pour élasticité 
elasticity <- function(x, y) {
  n <- length(x)
  e <- rep(NA, n)
  
  for (i in 2:(n-1)) {
    dy_dx <- (y[i+1] - y[i-1]) / (x[i+1] - x[i-1])
    e[i] <- dy_dx * (x[i] / y[i])
  }
  
  return(e)
}


cols <- names(df1)[-1]

# Calcul automatique pour chaque population
for (col in cols) {
  df[[paste0("elas_", col)]] <- elasticity(df1$val, df1[[col]])
}

# Résultat
df1

write.csv(df1, here("output","elast_SRCH.csv"), row.names=FALSE)



#-----------------------------------------
#      Plot des valeurs d'élasticité
#-----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)


df <- read.csv(
  here("output/", "summary_elasticity.csv"),
  header = TRUE, sep = ";"
)


df_mean <- df %>%
  select(param, starts_with("elas_lambda_"), -ends_with("_sd"))

df_sd <- df %>%
  select(param, ends_with("_sd"))

df_mean_long <- df_mean %>%
  pivot_longer(
    cols = -param,
    names_to = "lambda",
    values_to = "mean"
  )

df_sd_long <- df_sd %>%
  pivot_longer(
    cols = -param,
    names_to = "lambda",
    values_to = "sd"
  ) %>%
  mutate(lambda = gsub("_sd$", "", lambda))

df_long <- left_join(df_mean_long, df_sd_long,
                     by = c("param", "lambda"))


df_long <- df_long %>%
  mutate(lambda = gsub("elas_lambda_", "", lambda))


labels_lambda <- c(
  CH = "Croissance des chouettes effraies",
  BJ = "Croissance des pailles en queue \nà bec jaune",
  BR = "Croissance des pailles en queue \nà brins rouges",
  R  = "Croissance des rats",
  Co = "Croissance des corbeaux pies"
)


plots <- plots <- lapply(unique(df_long$lambda), function(lam) {
  
  df_tmp <- df_long %>% filter(lambda == lam)
  
  # Filtre sp pour ne pas avoir d'info redondantes sur Co et Rats
  if(lam == "Co") {
    df_tmp <- df_tmp %>% filter(!param %in% c("rCo mean", "roCo mean"))
  }
  
  if(lam == "R") {
    df_tmp <- df_tmp %>% filter(!param %in% c("rR mean", "roR mean"))
  }
  
  title_lab <- ifelse(lam %in% names(labels_lambda),
                      labels_lambda[lam],
                      lam)
  
  ggplot(df_tmp, aes(x = param, y = mean)) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = mean - sd, ymax = mean + sd),
      width = 0.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    theme_minimal() +
    labs(
      title = "",
      x = "Paramètres",
      y = title_lab
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
      axis.text.y = element_text(size = 20),
      axis.title = element_text(size = 20),
      plot.title = element_text(size = 20, face = "bold")
    )
})
  
  

lambdas <- unique(df_long$lambda)

for(i in seq_along(plots)) {
  
  ggsave(
    filename = here("output/Elasticity", paste0("plot_", lambdas[i], ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}