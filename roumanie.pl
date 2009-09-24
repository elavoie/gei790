/*
   Données pour la carte de la Roumanie, selon le livre de
   Russel et Norvig, 2eme edition, pages 63 et 95

   Auteur: Charles-Antoine Brunet
   Date: 2005-09-20
*/

% Representation de la fonction successeur S(n)
s(arad,          [sibiu, timisoara, zerind]).
s(bucharest,     [fagaras,giurgiu,pitesti,urziceni]).
s(craiova,       [dobreta,pitesti,rimnicuVilcea]).
s(dobreta,       [craiova,mehadia]).
s(eforie,        [hirsova]).
s(fagaras,       [bucharest,sibiu]).
s(giurgiu,       [bucharest]).
s(hirsova,       [eforie,urziceni]).
s(iasi,          [neamt,vaslui]).
s(lugoj,         [mehadia,timisoara]).
s(mehadia,       [dobreta,lugoj]).
s(neamt,         [iasi]).
s(oradea,        [sibiu,zerind]).
s(pitesti,       [bucharest,craiova,rimnicuVilcea]).
s(rimnicuVilcea, [craiova,pitesti,sibiu]).
s(sibiu,         [arad,fagaras,oradea,rimnicuVilcea]).
s(timisoara,     [arad,lugoj]).
s(urziceni,      [bucharest,hirsova,vaslui]).
s(vaslui,        [iasi,urziceni]).
s(zerind,        [arad,oradea]).

% Distance à vol d'oiseau de Bucharest: heuristique h(n)
h(arad, 366).      h(bucharest, 0). h(craiova, 160).       h(dobreta, 242).
h(eforie, 161).    h(fagaras, 176). h(giurgiu, 77).        h(hirsova, 151).
h(iasi, 226).      h(lugoj, 244).   h(mehadia, 241).       h(neamt, 234).
h(oradea, 380).    h(pitesti, 100).  h(rimnicuVilcea, 193). h(sibiu, 253).
h(timisoara, 329). h(urziceni, 80). h(vaslui, 199).        h(zerind, 374).

% Distance entre 2 villes
d(arad, sibiu, 140).            d(arad, timisoara, 118).
d(arad, zerind, 75).
d(bucharest, fagaras, 211).     d(bucharest, giurgiu, 90).
d(bucharest, pitesti, 101).     d(bucharest, urziceni, 85).
d(craiova,dobreta, 120).        d(craiova,pitesti, 138).
d(craiova,rimnicuVilcea, 146).
d(dobreta, craiova, 120).       d(dobreta, mehadia, 75).
d(eforie, hirsova, 86).
d(fagaras, bucharest, 211).     d(fagaras, sibiu, 99).
d(giurgiu, bucharest, 90).
d(hirsova, eforie, 86).         d(hirsova, urziceni, 98).
d(iasi, neamt, 87).             d(iasi, vaslui, 92).
d(lugoj, mehadia, 70).          d(lugoj, timisoara, 111).
d(mehadia, dobreta, 75).        d(mehadia, lugoj, 70).
d(neamt, iasi, 87).
d(oradea, sibiu, 151).          d(oradea, zerind, 71).
d(pitesti,bucharest,101).       d(pitesti,craiova,138).
d(pitesti,rimnicuVilcea,97).
d(rimnicuVilcea, craiova, 146). d(rimnicuVilcea, pitesti, 97).
d(rimnicuVilcea, sibiu, 80).
d(sibiu, arad, 140).            d(sibiu, fagaras, 99).
d(sibiu, oradea, 151).          d(sibiu, rimnicuVilcea, 80).
d(timisoara, arad, 118).        d(timisoara, lugoj, 111).
d(urziceni, bucharest, 85).     d(urziceni, hirsova, 98).
d(urziceni, vaslui, 142).
d(vaslui, iasi, 92).            d(vaslui, urziceni, 142).
d(zerind, arad, 75).            d(zerind, oradea, 71).

