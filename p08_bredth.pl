%-----------------------------------------------------------------------------
% Auteur: Charles-Antoine Brunet
% Version: 3.0
% Date: 2009-09-04
%-----------------------------------------------------------------------------
% Le nom du fichier, le nom du module, le préfixe des prédicats et le nom
% du mutex ont tous la même valeur. Dans ce cas ci, c'est p08. Changez TOUTES
% les occurrences de p08 dans ce fichier pour le préfixe qui vous est assigné.
%-----------------------------------------------------------------------------

% Un JI doit être un module afin d'éviter les conflits de noms entre les JI.
:- module(p08,[p08_nom/1,p08_auteurs/1,p08_reset/0,p08_plan/1,p08_action/2]).

%-----------------------------------------------------------------------------
% Prédicats de jeu.
%-----------------------------------------------------------------------------

% Nom du JI: p08_nom(-Nom)
p08_nom(Nom) :- nom(Nom).

% Auteurs du JI: p08_auteurs(-Auteurs)
p08_auteurs('Erick Lavoie et Alexandre Malo').

% Remise à zero du JI: p08_reset
p08_reset :-
    planInitial(P),
    setPlan(P).

% Plan courant du JI: p08_plan(-PlanCourant)
p08_plan(Plan) :-
    getPlan(Plan).

% Prochaine action du JI: p08_action(+Etat, -Action)
p08_action(Etat, Action) :-
    build_env(Etat,EtatIntelligent),
    trouveAction(EtatIntelligent, Action).

%-----------------------------------------------------------------------------
% Prédicats internes de plans.
%-----------------------------------------------------------------------------
% La consultation d'un plan et la modification d'un plan sont protégées par
% mutex afin d'éviter les conflits possibles d'appels en parallèle.
%
% Le prédicat planRestant est déclaré dynamique, car sa valeur change au cours
% de l'exécution.
%-----------------------------------------------------------------------------

:- dynamic([planRestant/1]).

planInitial([]).

planRestant([]).

getPlan(Plan) :-
    with_mutex(p08,planRestant(Plan)).

setPlan(Plan) :-
    with_mutex(p08,changePlan(Plan)).

changePlan(Plan) :-
    retractall(planRestant(_)),
    assert(planRestant(Plan)).

%-----------------------------------------------------------------------------
% Prédicats internes d'action
%-----------------------------------------------------------------------------
% Calcul de la prochaine action du JI. Ce JI ne fera jamais rien de bon...
%-----------------------------------------------------------------------------

%-----------------------------------------------------------------------------
% Recherche de la prochaine action lorsque le planRestant n'est pas vite et
% que la plan est toujours valide. La prochaine action du plan est retourné.
%-----------------------------------------------------------------------------
trouveAction(Env, Action) :-
    nom(Nom),
    query(player(_,Nom,_,_,0), Env),
    planRestant([Action|Plan]),
    validate_plan(Env,[Action|Plan]),
    setPlan(Plan).

trouveAction(Env, Action) :-
    nom(Nom),
    query(player(_,Nom,_,_,0), Env),
    planRestant([A|Plan]),
    \+ validate_plan(Env,[A|Plan]),
    find(Env,Nodes),
    build_path(Nodes,[],[Action|Plan2]),
    setPlan(Plan2).

trouveAction(Env, none) :-
    nom(Nom),
    query(player(_,Nom,_,_,_), Env),
    \+query(player(_,Nom,_,_,0), Env),!.

trouveAction(Env, Action) :-
    find(Env,Nodes),
    build_path(Nodes,[],[Action|Plan]),
    setPlan(Plan).

%-----------------------------------------------------------------------------
% Definition du code de recherche 
%-----------------------------------------------------------------------------
%Ajoute la liste des blocks dans le set de l'environnement
build_blocks_list([],Set,Set).
build_blocks_list([[Id,PosX,PosY]|List],Set,NewSet) :-
   build_blocks_list(List,Set,TmpSet),
   add_in_set(block(Id,PosX,PosY),TmpSet,NewSet).

%Ajoute la liste des joueurs dans le set de l'environnement
build_players_list([],Set,Set).
build_players_list([[Id,Nom,PosX,PosY,BlockId]|List],Set,NewSet) :-
   build_players_list(List,Set,TmpSet),
   add_in_set(player(Id,Nom,PosX,PosY,BlockId),TmpSet,NewSet).

%Converti la liste de l'environement fournis par la problematique dans un set
%contenant les faits correspondants.
build_env([NbJoueurs,NbBlocks, NbColonnes,NbRangees,EtatJoueurs,EtatBlocksLibres],Env) :-
   add_in_set(nbJoueurs(NbJoueurs),[],TmpEnv1),
   add_in_set(nbBlocks(NbBlocks),TmpEnv1,TmpEnv2),
   add_in_set(nbColonnes(NbColonnes),TmpEnv2,TmpEnv3),
   add_in_set(nbRangees(NbRangees),TmpEnv3,TmpEnv4),
   build_players_list(EtatJoueurs,TmpEnv4,TmpEnv5),
   build_blocks_list(EtatBlocksLibres,TmpEnv5,Env).
nom('LaMa').

% Check that a block doesn't have a player or a block on it
empty(PosX,PosY,Env) :-
    \+ query(block(_,PosX,PosY),Env),
    \+ query(player(_,_,PosX,PosY,_),Env).

% Check that position is valid in an environment
exist(PosX, PosY, Env) :-
    PosX >= 0, PosY >= 0,
    query(nbColonnes(NbColonnes), Env),
    query(nbRangees(NbRangees), Env),
    PosX < NbColonnes,
    PosY < NbRangees.

% Defines the new position that a player will occupy when moving in a particular
% direction
% direction(PosX, PosY, NewX, NewY, Direction)
direction(PosX, PosY, PosX, NewY, 1) :- NewY is PosY + 1.
direction(PosX, PosY, NewX, PosY, 2) :- NewX is PosX + 1.
direction(PosX, PosY, PosX, NewY, 3) :- NewY is PosY - 1.
direction(PosX, PosY, NewX, PosY, 4) :- NewX is PosX - 1.

direction(PosX, PosY, NewX, NewY, 5) :- NewX is PosX + 1, NewY is PosY + 1.
direction(PosX, PosY, NewX, NewY, 6) :- NewX is PosX + 1, NewY is PosY - 1.
direction(PosX, PosY, NewX, NewY, 7) :- NewX is PosX - 1, NewY is PosY - 1.
direction(PosX, PosY, NewX, NewY, 8) :- NewX is PosX - 1, NewY is PosY + 1.

% ---- Actions predicates -----
% for each action of the form type(direction)
% we define a predicate type(direction, env)
% that test the validity on this environment
move(D, Env) :-
    nom(Nom),
    query(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    empty(NewX,NewY,Env).

% Case where the player has no block on him
take(D, Env) :-
    nom(Nom),
    query(player(_,Nom,PosX,PosY,0),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    query(block(_,NewX,NewY),Env),
    \+ query(player(_,_,NewX,NewY,_),Env).

% Case where the player already has block on him
take(D, Env) :-
    nom(Nom),
    \+ query(player(_,Nom,PosX,PosY,0),Env),
    query(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    query(block(_,NewX,NewY),Env),
    \+ query(player(_,_,NewX,NewY,_),Env).

% Drop the block at the free cell
drop(D, Env) :-
    nom(Nom),
    \+ query(player(_,Nom,PosX,PosY,0),Env),
    query(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    empty(NewX,NewY,Env).

% Try to steal the block from another player
attack(D, Env) :-
    nom(Nom),
    query(player(_,Nom,PosX,PosY,0),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    \+ query(player(_,_,NewX,NewY,0),Env),
    query(player(_,_,NewX,NewY,_),Env),
    \+ query(block(_,NewX,NewY),Env).

% Try to exchange your block with the one from another player
attack(D, Env) :-
    nom(Nom),
    \+ query(player(_,Nom,PosX,PosY,0),Env),
    query(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    \+ query(player(_,_,NewX,NewY,0),Env),
    query(player(_,_,NewX,NewY,_),Env),
    \+ query(block(_,NewX,NewY),Env).

%% Actions that modifies the environment %%
% We add the Post condition even if for performance reason
% we might not want to keep it because we think it is clearer
% to clarify the effect on the environment

move(D, Env,R) :-
    % Pre
    nom(Nom),
    query(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    empty(NewX,NewY,Env),
    % Move
    delete_in_set(player(ID,Nom,PosX,PosY,IDBlock),Env,E2),
    add_in_set(player(ID,Nom,NewX,NewY,IDBlock),E2,R),
    % Post 
    query(player(ID,Nom,NewX,NewY,IDBlock),R).

% Case where the player has no block on him
take(D, Env,R) :-
    % Pre
    nom(Nom),
    query(player(_,Nom,PosX,PosY,0),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    query(block(_,NewX,NewY),Env),
    \+ query(player(_,_,NewX,NewY,_),Env),
    % Take
    delete_in_set(block(BID,NewX,NewY),Env,E2),
    delete_in_set(player(PID,Nom,PosX,PosY,0),E2,E3),
    add_in_set(player(PID,Nom,PosX,PosY,BID),E3,R),  
    % Post
    query(player(PID,Nom,PosX,PosY,BID),R),
    \+ query(block(BID,NewX,NewY),R).


% Case where the player already has block on him
take(D, Env,R) :-
    % Pre
    nom(Nom),
    \+ query(player(_,Nom,PosX,PosY,0),Env),
    query(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    query(block(_,NewX,NewY),Env),
    \+ query(player(_,_,NewX,NewY,_),Env),
    % Take
    delete_in_set(block(BID,NewX,NewY),Env,E2),
    delete_in_set(player(PID,Nom,PosX,PosY,PBID),E2,E3),
    add_in_set(block(PBID,NewX,NewY),E3,E4),
    add_in_set(player(PID,Nom,PosX,PosY,BID),E4,R),  
    % Post
    query(player(PID,Nom,PosX,PosY,BID),R),
    query(block(PBID,NewX,NewY),R).

% Drop the block at the free cell
drop(D, Env,R) :-
    % Pre
    nom(Nom),
    \+ query(player(_,Nom,PosX,PosY,0),Env),
    query(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    empty(NewX,NewY,Env),
    % Drop
    delete_in_set(player(PID,Nom,PosX,PosY,PBID),Env,E2),
    add_in_set(block(PBID,NewX,NewY),E2,E3),
    add_in_set(player(PID,Nom,PosX,PosY,0),E3,R),  
    % Post
    query(player(PID,Nom,PosX,PosY,0),R),
    query(block(PBID,NewX,NewY),R).


% Whether we have a block or not, the other player
% will end having what we have. We will also
% assume that the attact always succeeds.
attack(D, Env,R) :-
    % Pre
    nom(Nom),
    query(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    \+ query(player(_,_,NewX,NewY,0),Env),
    query(player(_,_,NewX,NewY,_),Env),
    \+ query(block(_,NewX,NewY),Env),
    % Attack
    delete_in_set(player(Us,OurName,PosX,PosY,OurBlock),Env,E2),
    delete_in_set(player(Them,TheirName,NewX,NewY,TheirBlock),E2,E3),
    add_in_set(player(Us,OurName,PosX,PosY,TheirBlock),E3,E4),
    add_in_set(player(Them,TheirName,NewX,NewY,OurBlock),E4,R), 
    % Post
    query(player(Us,OurName,PosX,PosY,TheirBlock),R),
    query(player(Them,TheirName,NewX,NewY,OurBlock),R).

none.

%Prototype of a node
%
%  node(ResultingEnvAfterAction,Parent,ActionToApplyOnParentEnv,Depth,PathCost)
%
cost(move,2).
cost(take,1).
cost(drop,1).
cost(attack,1).
cost(none,10).

action(move(A),Env,PreviousCost,ResultEnv,Cost) :-
   cost(move,TmpCost),
   Cost is PreviousCost + TmpCost,
   move(A,Env,ResultEnv).

action(take(A),Env,PreviousCost,ResultEnv,Cost) :-
   cost(take,TmpCost),
   Cost is PreviousCost + TmpCost,
   take(A,Env,ResultEnv).

action(drop(A),Env,PreviousCost,ResultEnv,Cost) :-
   cost(drop,TmpCost),
   Cost is PreviousCost + TmpCost,
   drop(A,Env,ResultEnv).

action(attack(A),Env,PreviousCost,ResultEnv,Cost) :-
   cost(attack,TmpCost),
   Cost is PreviousCost + TmpCost,
   attack(A,Env,ResultEnv).

action(none,Env,PreviousCost,Env,Cost) :-
    cost(none,TmpCost),
    Cost is PreviousCost + TmpCost.

% We add another action definition to simply validate the moves
action(move(A),Env,ResultEnv):-
   move(A,Env,ResultEnv).

action(take(A),Env,ResultEnv) :-
   take(A,Env,ResultEnv).

action(drop(A),Env,ResultEnv) :-
   drop(A,Env,ResultEnv).

action(attack(A),Env,ResultEnv) :-
   attack(A,Env,ResultEnv).

action(none,Env,Env).

%-----------------------------------------------------------------------------
% Auteur: Erick Lavoie & Alexandre Malo
% Date: 2009-09-24
% Bloc: Fonctions spécifiques à l'implémentation de la recherche d'un plan
%-----------------------------------------------------------------------------
%
%    Ce bloc contient les fonctions qui doivent être implémenter pour
% effectuer le graph search. Il contient aussi la fonction qui déclanche
% la recherche a partir de l'état d'un environnement.
%
%-----------------------------------------------------------------------------

%-----------------------------------------------------------------------------
% FONCTION DE DÉCLANCHEMENT DE LA RECHERCHE
%-----------------------------------------------------------------------------
find(StartingEnvironment, Result) :-
   graph_search([node(StartingEnvironment,nil,nil,0,0)],_,Result,[]).

%-----------------------------------------------------------------------------
% FONCTION D'EXPLOSION D'UN NOEUD POUR ÉNUMÉRER LES MOUVEMENTS POSSIBLES
%-----------------------------------------------------------------------------
expand(Node,Successors) :-
   node(OriginalEnv,_,_,OriginalDepth,OriginalPathCost)=Node,
   Depth is OriginalDepth+1,
   findall(node(ResultEnv,Node,Action,Depth,PathCost),
           action(Action,OriginalEnv,OriginalPathCost,ResultEnv,PathCost),
           Successors).
%-----------------------------------------------------------------------------
% FONCTION DE VÉRIFICATION DE L'ATTEINTE DE NOTER BUT
%
% -Notre but est simplement de trouver un bloc peu importe sa valeur
%-----------------------------------------------------------------------------
at_goal(node(EnvToValidate,_,_,_,_),_) :-
   nom(Nom),
   query(player(_,Nom,_,_,Block),EnvToValidate),
   has_block(player(_,_,_,_,Block)).
%-----------------------------------------------------------------------------
% FONCTION D'AJOUT D'UN ETAT A LA LISTE DES ÉTATS FERMÉES
%-----------------------------------------------------------------------------
add_to_closed(node(State,_,_,_,_),Closed,NewClosed):-
    add_in_set(State,Closed,NewClosed).

%-----------------------------------------------------------------------------
% FONCTION D'AJOUT D'UNE LISTE DE NOEUDS A VISITER DANS UNE LISTE À PRIORITÉ
%-----------------------------------------------------------------------------
add_to_fringe(Successors, Fringe, NewFringe) :-
    add_list_to_queue(Successors,Fringe,NewFringe).

%-----------------------------------------------------------------------------
% FONCTION QUI ENLEVE L'ÉLÉMENT LE PLUS PRIORITAIRE DE LA LISTE À PRIORITÉ
%-----------------------------------------------------------------------------
delete_from_fringe(Element, Fringe, NewFringe) :-
    dequeue(Element, Fringe, NewFringe).


%Find the nearest block of our player %%A optimiser
closestBlockDistance(Env,Distance) :-
   findall(Block,blockInEnv(Block,Env),Blocks),
   nom(Nom),
   query(player(_,Nom,PPosX,PPosY,_),Env),
   query(nbColonnes(NbColonnes),Env),
   query(nbRangees(NbRangees),Env),
   max(NbColonnes,NbRangees,MaxDistance),
   distance_min(Blocks,PPosX,PPosY,MaxDistance,Distance).

where_is_block(Id,PosX,PosY,Env) :-
   query(player(_,_,PosX,PosY,Id),Env).

where_is_block(Id,PosX,PosY,Env) :-
   query(block(Id,PosX,PosY),Env).


blockInEnv(block(Id,PosX,PosY),Env) :- query(block(Id,PosX,PosY),Env).
blockInEnv(block(Id,PosX,PosY),Env) :- 
   query(player(_,_,PosX,PosY,Id),Env),
   \+Id = 0.

distance_min([],_,_,Min,Min).
distance_min([block(_,BlockX,BlockY)|T],PlayerX,PlayerY,Min,R) :-
   distance(PlayerX,PlayerY,BlockX,BlockY,Distance),
   Distance < Min,
   distance_min(T,PlayerX,PlayerY,Distance,R).
distance_min([block(_,BlockX,BlockY)|T],PlayerX,PlayerY,Min,R) :-
   distance(PlayerX,PlayerY,BlockX,BlockY,Distance),
   Distance >= Min,
   distance_min(T,PlayerX,PlayerY,Min,R).

%Calculate the distance between a player and a block
distance(X1,Y1,X2,Y2,Distance) :-
   Dx is abs(X1 - X2),
   Dy is abs(Y1 - Y2),
   max(Dx,Dy,Distance).

min(X,Y,X) :- X =< Y,!.
min(X,Y,Y) :- Y =< X,!.
max(X,Y,X) :- Y =< X,!.
max(X,Y,Y) :- X =< Y,!.

%Heurisitc 
%Priority queue function to know how to add stuff in the queue
precedes(node(Env1,_,_,_,PathCost1),node(Env2,_,_,_,PathCost2)) :-
   closestBlockDistance(Env1,H1), 
   closestBlockDistance(Env2,H2),
   F1 is PathCost1 + H1, F2 is PathCost2 + H2,
   F1 < F2.
has_block(player(_,_,_,_,Block)) :-
   \+Block = 0.

%Comparing the set of state no mather the order of what's in the state
is_closed(node(State,_,_,_,_),Closed):-
   in_set(State,Closed).


% Usefull Tools
%Verify is an item is in a list and force to stop the search
query(Q,[Q|_]) :- !.
query(Q,[_|T]) :-
   query(Q,T).


in_set(_,[]) :- \+true.
in_set(State1,[State2|_]) :-
   equal_set(State1,State2), !.
in_set(State1,[State2|Closed]) :-
   \+equal_set(State1,State2),
   in_set(State1,Closed).

build_path(node(_,nil,nil,_,_),R,R).
build_path(node(_,Parent,Action,_,_),PreviousR,R) :-
   build_path(Parent,[Action|PreviousR],R).

validate_plan(Env, Actions) :-
   validate(Env, Actions, R),
   at_goal(node(R,_,_,_,_),_). 


validate(Env, [], Env).
validate(Env, [Action|T], R) :-
   action(Action, Env,E2),
   validate(E2,T,R).

%-----------------------------------------------------------------------------
% Structure de donnée de set (collection),
%   Le set est sans dupliques et les valeurs ne sont pas ordonnees
%
% Auteur: Charles-Antoine Brunet
%------------------------------------------------------------------------------
% Version 1.0: Version initiale
% Date: 2005/04/11
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% +: parametre en entree
% -: parametre en sortie
% ?: parametre en entree ou sortie
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Tester si un set est vide ou cree un set
% empty_set(?Set)
%------------------------------------------------------------------------------
empty_set([]).

%------------------------------------------------------------------------------
% Verifier si un element est membre d'un set
% Utilise la fonction member de la librairie standard de liste
% member_set(+Item, +Set)
%------------------------------------------------------------------------------
member_set(E, S) :- member(E, S).

%------------------------------------------------------------------------------
% Enlever un element du set, s'il est present
% delete_in_set(+Item, +Set, -NewSet)
% Item=item a enlever, Set = ancien set, NewSet = nouveau set
%------------------------------------------------------------------------------
delete_in_set(_, [], []) :- !.
delete_in_set(E, [E|T], T) :- !.
delete_in_set(E, [H|T], [H|Tnew]) :- delete_in_set(E,T,Tnew).

%------------------------------------------------------------------------------
% Ajouter un element au set, s'il n'est pas present
% add_in_set(+Item, +Set, -NewSet)
% Item=item a ajouter, Set = ancien set, NewSet = nouveau set
%------------------------------------------------------------------------------
add_in_set(E, S, S) :- member(E,S), !.
add_in_set(E, S, [E|S]).

%------------------------------------------------------------------------------
% Fusionner 2 sets
% set_union(+Set1, +Set2, -Set3)
% Set3 contient les items de Set1 et de Set2
%------------------------------------------------------------------------------
set_union([], S, S).
set_union([H|T], S, Snew) :- set_union(T, S, Tmp), add_in_set(H, Tmp, Snew).

%------------------------------------------------------------------------------
% Verifier si un set est un sous-ensemble d'un autre
% sub_set(+Set1, +Set2) est vrai si Set1 est un sous-ensemble de Set2.
%------------------------------------------------------------------------------
sub_set([],_).
sub_set([H|T], S) :- member_set(H,S), sub_set(T,S).

%------------------------------------------------------------------------------
% Trouver les elements communs a 2 sets
% set_intersection(+Set1, +Set2, -Intersection)
% Intersection contien les items communs a Set1 et Set2.
%------------------------------------------------------------------------------
set_intersection([], _, []).
set_intersection([H|T],S,[H|Snew]) :-
    member_set(H,S), set_intersection(T,S,Snew), !.
set_intersection([H|T],S,Snew) :- \+ member_set(H,S), set_intersection(T, S, Snew).

%------------------------------------------------------------------------------
% Calculer la difference entre 2 sets.
% set_difference(+Set1,+Set2,-Difference)
% Difference contient les elements qui sont dans Set1 mais pas dans Set2
%------------------------------------------------------------------------------
set_difference([], _, []).
set_difference([H|T], S, Tnew) :-
    member_set(H,S), set_difference(T, S, Tnew), !.
set_difference([H|T], S, [H|Tnew]) :- set_difference(T,S,Tnew).

%------------------------------------------------------------------------------
% Verifier si 2 sets sont equivalents
% equal_set(+S1, +S2)
% Vrai si tous membres de Set1 sont dans Set2 et ceux de Set2 dans Set1
%------------------------------------------------------------------------------
equal_set(S1, S2) :- sub_set(S1, S2), sub_set(S2, S1).

%------------------------------------------------------------------------------
% Imprimer un set
%------------------------------------------------------------------------------
print_set([]).
print_set([H|Q]) :- write(H), nl, print_set(Q).

%------------------------------------------------------------------------------
% Structure de donnée de file (queue) et file (queue) avec priorite
% Auteur: Charles-Antoine Brunet
%------------------------------------------------------------------------------
% Version 1.0: Version initiale
% Date: 2005/04/11
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% +: parametre en entree
% -: parametre en sortie
% ?: parametre en entree ou sortie
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Tester si une file est vide ou cree une file
% empty_queue(?Stack)
%------------------------------------------------------------------------------
empty_queue([]).

%------------------------------------------------------------------------------
% Ajouter un item dans la file
% enqueue(+Item, +Queue, -NewQueue)
% Item=item a ajouter, Y=ancienne file, Z=nouvelle file
%------------------------------------------------------------------------------
enqueue(E, [], [E]).
enqueue(E, [H|T], [H|Tnew]) :- enqueue(E, T, Tnew).

%------------------------------------------------------------------------------
% Elever un item de la file
% dequeue(-Item, +Queue, -NewQueue)
% Item= item enleve, Queue=ancienne file, NewQueue la nouvelle file
%------------------------------------------------------------------------------
dequeue(E, [E|T], T).

%------------------------------------------------------------------------------
% Consulte le premier item de la file
% peek_queue(-Item, +Queue), Item=premier item, Queue= file a consulter
%------------------------------------------------------------------------------
peek_queue(E, [E|_]).

%------------------------------------------------------------------------------
% Verifier si un element est membre d'une file
% Utilise la fonction member de la librairie standard de liste
%------------------------------------------------------------------------------
member_queue(E, T) :- member(E, T).

%------------------------------------------------------------------------------
% Ajoute une liste d'elements à une file
% add_list_to_queue(+List, +Queue, -NewQueue)
% List=liste a ajouter, Queue=ancienne file, NewQueue=nouvelle file
% Utilise la fonction append de la librairie standard de liste
%------------------------------------------------------------------------------
add_list_to_queue(List, T, NewT) :- append(T, List, NewT).
%------------------------------------------------------------------------------
% QUEUE AVEC PRIORITE
%------------------------------------------------------------------------------
% Les operateurs empty_queue, member_queue, dequeue et peek sont les memes
%      que plus haut. Les 2 operateurs qui changent sont les suivants
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Ajouter un item dans la file avec priorite
% insert_pq(+Item, +Queue, -NewQueue)
% Item=item a ajouter, Y=ancienne file, Z=nouvelle file
%------------------------------------------------------------------------------
insert_pq(E, [], [E]) :- !.
insert_pq(E, [H|T], [E, H|T]) :- precedes(E,H), !.
insert_pq(E, [H|T], [H|Tnew]) :- insert_pq(E, T, Tnew).

%------------------------------------------------------------------------------
% Ajouter une liste d'elements (non ordonnes) à une file avec priorite
% insert_list_pq(+List, +Queue, -NewQueue)
% List=liste a ajouter, Queue=ancienne file, NewQueue=nouvelle file
%------------------------------------------------------------------------------
insert_list_pq([], L, L).
insert_list_pq([E|T], L, NewL) :-
    insert_pq(E, L, Tmp), insert_list_pq(T, Tmp, NewL).

%-----------------------------------------------------------------------------
% Auteur: Erick Lavoie & Alexandre Malo
% Date: 2009-09-24
% Bloc: Fonctions de recherche par graphe générique
%-----------------------------------------------------------------------------
%    Les fonctions de ce bloc implémente l'algorithme générique de la
% recherche par graphe donnée dans le livre [Artificial Intelligence,
% A Modern Approach, 2nd Edition] à la page 83.
%
%    Afin d'être utiliser, il faut implémenter quelques fonctions qui
% déterminera le comportement de la recherche. Voici les fonctions à
% implémenter.
%
%     [delete_from_fringe(Node,Fringe,NewFringe)]
%     [add_to_fringe(Nodes,Fringe,NewFringe)    ]
%     [at_goal(Node,Goal)                       ]
%     [add_to_closed(Node,Closed,NewClosed)     ]
%     [is_closed(Node,Closed)                   ]
%     [expand(Node,Successors)                  ]
%
%    Remarqué que aucune de c'est fonction ne force un type de structure
% lors des appels. C'est à l'implémentation d'effectuer les restrictions.
%
%-----------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Fonction de recherche qui prend un prochain noeud de la liste pour démarer
% une nouvelle itération de recherche.
%
% graph_search(Fringe,Goal,Result,Closed).
%------------------------------------------------------------------------------
graph_search([H|T],Goal,Solution,Closed) :-
    delete_from_fringe(Node, [H|T], NewFringe),
    graph_search_helper(Node,NewFringe,Goal,Solution,Closed).
%------------------------------------------------------------------------------
% Fonction de recherche qui est utilisé si le but est atteint
%------------------------------------------------------------------------------
graph_search_helper(Node,_,Goal,Node,Closed):-
    at_goal(Node,Goal),
    \+is_closed(Node,Closed).
%------------------------------------------------------------------------------
% Fonction de recherche qui est utilisé si le but n'est pas atteint et si
% le noeud est dans la liste des noeud fermés.
%------------------------------------------------------------------------------
graph_search_helper(Node,Fringe,Goal,Solution,Closed) :-
    \+at_goal(Node,Goal),
    is_closed(Node,Closed), 
    graph_search(Fringe,Goal,Solution,Closed).
%------------------------------------------------------------------------------
% Fonction de recherche qui est utilisé si le but n'est pas atteint et que
% le noeud n,est pas dans la liste des noeuds fermés. Dans ce cas l'expansion
% du noeud est effectué et la recherche est recommencé avec un nouveau fringe.
%------------------------------------------------------------------------------
graph_search_helper(Node,Fringe,Goal,Solution,Closed) :-
    \+at_goal(Node,Goal),
    \+is_closed(Node,Closed),
    add_to_closed(Node,Closed,NewClosed),
    expand(Node, Successors),
    add_to_fringe(Successors,Fringe,NewFringe),
    graph_search(NewFringe,Goal,Solution,NewClosed).

%-----------------------------------------------------------------------------
% Auteur: Erick Lavoie & Alexandre Malo
% Date: 2009-09-24
% Bloc: Fonctions de test
%-----------------------------------------------------------------------------
%    Les fonctions suivantes sont utilisé afin de vérifier les multiples
% fonctionnalités de toutes nos algorithmes. Voici la listes des testes
% effectués.
%
%      [test_all                    ] 
%      [test_planif                 ] 
%      [test_move                   ]
%      [test_take_no_block_on_player]
%      [test_take_a_block_on_player ]
%      [test_drop_a_block           ]
%      [test_attack                 ]
%      [test_attack_exchange        ]
%      [test_priority_queue         ]
%      [test_precedes               ]
%      [test_env                    ]
% 
%-----------------------------------------------------------------------------

%------------------------------------------------------------------------------
% EFFECTUE TOUTEE LES TESTS
%------------------------------------------------------------------------------
test_all:-
    test_planif,
    test_move,
    test_take_no_block_on_player,
    test_take_a_block_on_player,
    test_drop_a_block,
    test_attack,
    test_attack_exchange,
    test_env.

%------------------------------------------------------------------------------
% VÉRIFICATION DE LA PLANIFICATION
%------------------------------------------------------------------------------

build_test_env(1,Env) :-
    nom(Nom), 
    L=[5,15,75,75,[[1,'Inconnu1',38,14,0],[2,'Inconnu2',45,39,0],[3,'Inconnu3',31,60,0],[4,Nom,22,45,0],[5,'Inconnu5',13,35,12]],[[1,5,0],[2,52,61],[3,3,48],[4,7,27],[5,23,17],[6,28,29],[7,55,21],[8,12,0],[9,64,48],[10,48,16],[11,67,14],[13,11,11],[14,18,62],[15,42,24]]],
    build_env(L,Env).
build_test_env(2,Env) :-
    nom(Nom), 
    L=[5,30,50,50,[[1,'Inconnu1',13,26,2],[2,'Inconnu2',43,44,4],[3,Nom,16,38,0],[4,'Inconnu4',1,32,0],[5,'Inconnu5',16,16,16]],[[1,5,0],[3,24,46],[5,1,6],[6,22,13],[7,11,0],[8,5,41],[9,9,8],[10,4,20],[11,46,13],[12,37,34],[13,32,44],[14,5,10],[15,19,0],[17,21,0],[18,22,0],[19,41,8],[20,42,26],[21,28,28],[22,18,29],[23,27,0],[24,7,47],[25,33,3],[26,27,8],[27,40,17],[28,16,5],[29,47,18],[30,28,16]]],
    build_env(L,Env).
build_test_env(3,Env) :-
    nom(Nom), 
    L=[5,10,25,25,[[1,Nom,10,16,0],[2,'Inconnu2',7,23,0],[3,'Inconnu3',8,9,0],[4,'Inconnu4',0,5,0],[5,'Inconnu5',4,16,3]],[[1,17,13],[2,20,2],[4,5,3],[5,4,14],[6,14,10],[7,18,17],[8,20,21],[9,15,5],[10,4,15]]],
    build_env(L,Env).
build_test_env(4,Env) :-
    nom(Nom), 
    L=[5,15,15,15,[[1,'Inconnu1',7,7,0],[2,'Inconnu2',8,10,13],[3,'Inconnu3',2,0,15],[4,'Inconnu4',1,10,1],[5,Nom,14,5,0]],[[2,12,10],[3,7,0],[4,2,4],[5,12,14],[6,14,10],[7,7,4],[8,10,3],[9,13,0],[10,4,7],[11,0,1],[12,5,12],[14,5,3]]],
    build_env(L,Env).

test_planif(NoTest) :-
    build_test_env(NoTest,Env),
    find(Env,Plan),
    build_path(Plan,[],R).

test_all_planif :-
   time(test_planif(4)),
   time(test_planif(3)),
   time(test_planif(2)),
   time(test_planif(1)).



test_planif :-
    nom(Nom), 
    L=[1,1,3,3,[[_,Nom,0,0,0]],[[1,2,1]]],
    build_env(L,Env),
    find(Env,Plan),
    build_path(Plan,[],R),
    write(R).
%------------------------------------------------------------------------------
% VÉRIFICATION D'UN MOUVEMENT
%------------------------------------------------------------------------------
test_move :-
    nom(Nom), 
    L=[1,1,2,2,[[_,Nom,0,0,0]],[]],
    R=[1,1,2,2,[[_,Nom,0,1,0]],[]],
    build_env(L,Env),
    build_env(R,EnvR),
    move(1,Env,EnvR).
%------------------------------------------------------------------------------
% VÉRIFICATION DE PRENDRE UN BLOC SANS EN AVOIR
%------------------------------------------------------------------------------
test_take_no_block_on_player :-
    nom(Nom), 
    L=[1,1,2,2,[[_,Nom,0,0,0]],[[1,0,1]]],
    R=[1,1,2,2,[[_,Nom,0,0,1]],[]],
    build_env(L,Env),
    build_env(R,EnvR),
    take(1,Env,EnvR).
%------------------------------------------------------------------------------
% VÉRIFICATION DE PRENDRE UN BLOC EN AYANT DÉJÀ UN
%------------------------------------------------------------------------------
test_take_a_block_on_player :-
    nom(Nom), 
    L=[1,2,2,2,[[_,Nom,0,0,2]],[[1,0,1]]],
    R=[1,2,2,2,[[_,Nom,0,0,1]],[[2,0,1]]],
    build_env(L,Env),
    build_env(R,EnvR),
    take(1,Env, TakeR),
    equal_set(EnvR,TakeR).
%------------------------------------------------------------------------------
% VÉRIFICATION DE DÉPOSER UN BLOC
%------------------------------------------------------------------------------
test_drop_a_block :-
    nom(Nom), 
    L=[1,1,2,2,[[_,Nom,0,0,1]],[]],
    R=[1,1,2,2,[[_,Nom,0,0,0]],[[1,0,1]]],
    build_env(L,Env),
    build_env(R,EnvR),
    drop(1,Env, DropR),
    equal_set(EnvR,DropR).
%------------------------------------------------------------------------------
% VÉRIFICATION D'UN ATTAQUE SUR UNE PERSONNE
%------------------------------------------------------------------------------
test_attack :-
    nom(Nom), 
    L=[2,1,2,2,[[_,Nom,0,0,0],[2,zeouf,0,1,1]],[]],
    R=[2,1,2,2,[[_,Nom,0,0,1],[2,zeouf,0,1,0]],[]],
    build_env(L,Env),
    build_env(R,EnvR),
    attack(1,Env,AttackR),
    equal_set(EnvR,AttackR).
%------------------------------------------------------------------------------
% VÉRIFICATION D'UN ATTAQUE QUI CAUSE UN ÉCHANGE
%------------------------------------------------------------------------------
test_attack_exchange :-
    nom(Nom), 
    L=[2,2,2,2,[[_,Nom,0,0,2],[2,zeouf,0,1,1]],[]],
    R=[2,2,2,2,[[_,Nom,0,0,1],[2,zeouf,0,1,2]],[]],
    build_env(L,Env),
    build_env(R,EnvR),
    attack(1,Env,AttackR),
    equal_set(EnvR,AttackR).

%------------------------------------------------------------------------------
% VÉRIFICATION DE L'ENVIRONEMENT
%------------------------------------------------------------------------------
test_env :-
    nom(Nom),
    R=[1,2,4,4,[[_, Nom,1,0,0]],[[1,0,3],[3,3,3]]],
    build_env(R,Env),
    Env = [block(1, 0, 3), block(3, 3, 3), player(_, 'LaMa', 1, 0, 0), nbRangees(4), nbColonnes(4), nbBlocks(2), nbJoueurs(1)].

 
%/* vim: set filetype=prolog : */
