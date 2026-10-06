Minimalist graph viewer/editor for the classroom.

## Version web

Une version web complète est publiée sur GitHub Pages :
https://marcdefalco.github.io/graphlab/

- **Éditeur** : ajout et suppression de sommets et d'arêtes à la souris,
  renommage, poids, graphes orientés ou non, annuler / rétablir.
- **Générateurs** : complet, cycle, chemin, étoile, roue, biparti complet,
  grille, hypercube, Möbius, Petersen, arbres et graphes aléatoires, graphes
  orientés sans cycle, diviseurs, exemples pondérés (avec poids aléatoires
  en option).
- **Algorithmes pas à pas**, avec pseudo-code surligné, contenu de la
  structure de données et tableau d'état : parcours en largeur, en
  profondeur (pile et récursif avec classification des arêtes), Dijkstra,
  Bellman-Ford, Floyd-Warshall, Prim, Kruskal, tri topologique, composantes
  connexes et fortement connexes, test de biparticité, coloration gloutonne.
- **Exports** : texte (liste d'arêtes, réimportable), code OCaml (listes et
  matrice d'adjacence), TikZ, PNG, JSON, lien de partage ; sauvegarde
  automatique dans le navigateur.
- **Remplissage** : version web de `flood.ml`, qui compare pile, file et
  tirage aléatoire sur une grille (murs, labyrinthe).

Organisation : les graphes (`graph.ml`), la disposition (`layout.ml`) et
les algorithmes instrumentés (`algos.ml`, chaque algorithme produit une
trace d'étapes) forment la bibliothèque `graph`, compilée en JavaScript
avec `js_of_ocaml`. `web/graphlab_web.ml` expose ces fonctions à la page
(`web/index.html`, `app.js`, `flood.js`, `app.css`), qui s'occupe de
l'édition et du dessin.

```
opam install js_of_ocaml js_of_ocaml-ppx
dune build --profile release @web/web
```

puis ouvrir `_build/default/web/index.html`. Le workflow
`.github/workflows/pages.yml` publie la version web à chaque push sur
`master`.
