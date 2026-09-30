Minimalist graph viewer/editor for the classroom.

## Version web

Le calcul (`graph.ml`, `layout.ml`, regroupés dans la bibliothèque `graph`)
est compilé en JavaScript avec `js_of_ocaml` ; le dessin et l'interaction
sont refaits en HTML/canvas dans `web/` (glisser les sommets, zoom, parcours
BFS/DFS pas à pas, réglages de la disposition, saisie d'un graphe,
documentation intégrée).

```
opam install js_of_ocaml js_of_ocaml-ppx
dune build --profile release @web/web
```

puis ouvrir `_build/default/web/index.html` (les fichiers `index.html` et
`graphlab.js` suffisent). Le workflow `.github/workflows/pages.yml` publie
la version web sur GitHub Pages à chaque push sur `master` (source « GitHub
Actions » dans *Settings → Pages*).
