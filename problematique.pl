:- ensure_loaded(set).

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

% block(ID,PosX,PosY).
% player(ID,Nom,PosX,PosY,IDBlock)

nom('brutus').

x_nom(Nom) :- nom(Nom).

% Check that a block doesn't have a player or a block on it
empty(PosX,PosY,Env) :-
    \+ member_set(block(_,PosX,PosY),Env),
    \+ member_set(player(_,_,PosX,PosY,_),Env).

% Check that position is valid in an environment
exist(PosX, PosY, Env) :-
    PosX >= 0, PosY >= 0,
    member_set(nbColonnes(NbColonnes), Env),
    member_set(nbRangees(NbRangees), Env),
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
    member_set(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    empty(NewX,NewY,Env).

% Case where the player has no block on him
take(D, Env) :-
    nom(Nom),
    member_set(player(_,Nom,PosX,PosY,0),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    member_set(block(_,NewX,NewY),Env),
    \+ member_set(player(_,_,NewX,NewY,_),Env).

% Case where the player already has block on him
take(D, Env) :-
    nom(Nom),
    \+ member_set(player(_,Nom,PosX,PosY,0),Env),
    member_set(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    member_set(block(_,NewX,NewY),Env),
    \+ member_set(player(_,_,NewX,NewY,_),Env).

% Drop the block at the free cell
drop(D, Env) :-
    nom(Nom),
    \+ member_set(player(_,Nom,PosX,PosY,0),Env),
    member_set(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    empty(NewX,NewY,Env).

% Try to steal the block from another player
attack(D, Env) :-
    nom(Nom),
    member_set(player(_,Nom,PosX,PosY,0),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    \+ member_set(player(_,_,NewX,NewY,0),Env),
    member_set(player(_,_,NewX,NewY,_),Env),
    \+ member_set(block(_,NewX,NewY),Env).

% Try to exchange your block with the one from another player
attack(D, Env) :-
    nom(Nom),
    \+ member_set(player(_,Nom,PosX,PosY,0),Env),
    member_set(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    \+ member_set(player(_,_,NewX,NewY,0),Env),
    member_set(player(_,_,NewX,NewY,_),Env),
    \+ member_set(block(_,NewX,NewY),Env).

% Tests
%test_env(Env) :- Env=[4,3,4,4,[[2,brutus,0,2,0],[3,zouf,1,0,0],[1,ares, 3,0,0],[4,buddy,2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]].
test_env(Env) :- Env=[1,1,2,2,[[1,brutus,0,0,0]],[[1,0,1]]].
%/* vim: set filetype=prolog : */
