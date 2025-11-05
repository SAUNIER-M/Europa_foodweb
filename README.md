
# <h1 align="center">  🐦 Modeling the Trophic Network of Europa: Focus on Tropicbirds as Prey

---

## 🌍 Context and Aims

Europa is a small French island with an exceptional diversity of seabird species.  
Two species of tropicbirds (**white-tailed** and **red-tailed tropicbird**), one of which is an endemic subspecies (the white-tailed), are currently threatened due to predation by:

- 🐀 **Black rats**  
- 🐦 **Pied crows**  
- 🦉 **Barn owls**  

The latter two predators also feed on rats, creating a complex web of trophic interactions.  

The aim of this study is to **reconstruct this trophic network using differential equations**, in order to:
- Model the viability of each population  
- Simulate the effects of conservation actions on ecosystem stability  

---

## 🗂 Project Structure

This project follows a simple structure:

- output/       ➜ Model outputs and generated plots

- R/
  
    ├─ Make.R    ➜ *Main script to run the full workflow*

    ├─ Fct.R    ➜ *Functions defining the system of differential equations using deSolve package*

    ├─ Var.R  ➜ *Script specifying the initial conditions for each variable*

- README.md


## 🔢 Variable Abbreviations and Meanings

### Species Codes
| Code | Species |
|------|---------|
| BJ   | White-tailed tropicbird ("Bec Jaune" in french) |
| BR   | Red-tailed tropicbird ("Brins Rouges" in french) |
| R    | Rats |
| Co   | Pied crows ("COrbeaux pies" in french) |
| CH   | Barn owls ("CHouettes" in french) |

### Age-Class Prefix
| Prefix | Meaning |
|--------|---------|
| A      | Adult (sexually mature) |
| P      | Combine egg and chick |
| Y      | Yearling (from egg laying to one year old) |
| J      | Juvenile of age species in suffix (e.g., `JBJ1` = one-year-old juvenile white-tailed tropicbird) |

> ⚠️ Note: Different age-classes were **not considered** for Rats and Pied Crows.

### Biological Parameter Prefix
| Prefix | Meaning |
|--------|---------|
| F      | Fecundity |
| K      | Maximum carrying capacity |
| φ      | Survival rate |
| μ      | Mortality rate |
| prXY   | Predation rate of X on Y |
| Proies_ | All the prey ("Proies" in french") consumed by the defined species |
| r      | Intrinsic population growth |
| ro     | Regulation (individual per time step) |

### Other Parameters
| Code | Meaning |
|------|---------|
| E    | Number of clutches per dt |
| a    | Birth sex ratio |
| G    | Clutch size |
| SR   | Natural breeding success ("Succès reproducteur" in french) |
| Au   | Other birds (including sooty terns and zosterops, "Autres" in french) |
| I    | Insects |
| Pl   | Plants |
| Sc   | Skinks ("Scinque" in french) |
| Tor  | Baby turtles |

---

## ⚡ Notes

- Variable names may **combine a biological parameter prefix with an age-class and species code**.  
- Example: `phiJBJ1` → Survival rate (`phi`) of juvenile of 1years old (J__1) of white-tailed tropicbird (`BJ`).

---


