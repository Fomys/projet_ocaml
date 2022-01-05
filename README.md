# Projet programmation fonctionnelle

Implémentation de Ford-Fulkerson dans lib/algorithms.ml
Implémentation de Tricount (partage des dépenses) dans lib/tricount.ml
- Supporte les transactions qui ne concernent pas tout le monde
- Pas besoin de déclarer la liste des participants
- Les participants ne peuvent pas avoir d'espaces dans leurs noms

# Compilation
dune build

./_build/default/bin/tricount.exe [fichier_d_entree] [fichier_de_sortie]

# Format de fichier d'entrée

celuiquipaye montant tous les gens concernés
str float str str str
# Optimisations possibles
 
- Minimisation des transactions
  - Éviter les "chaines" A doit à B 5€, B doit à C 5€, passer directement à A doit à C 5€
- Minimiser le nombre de personnes devant rembourser
  - A doit à B 10€, B doit à C 5€ devient A doit à B 5€, A doit à C 5€ 