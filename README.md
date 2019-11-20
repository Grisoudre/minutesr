# minutesr
distribution du temps de parole - application

## Installation et démarrage

**Installation du package :**
```{r}
if (!require("devtools")) install.packages("devtools", dep=T)
devtools::install_github("Grisoudre/minutesr")
```
A la question 'Enter one or more numbers, or an empty line to skip updates:', taper 1 puis touche entrée.
*Le chargement peut être long*

**Ouverture de l'application :**
```{r}
minutesr::minutesr()
```
Puis, sur la nouvelle fenêtre, cliquer sur "Open in browser". **/!\** Ne pas fermer de fenêtre.
