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

%% Actions that modifies the environment %%
% We add the Post condition even if for performance reason
% we might not want to keep it because we think it is clearer
% to clarify the effect on the environment

move(D, Env,R) :-
    % Pre
    nom(Nom),
    member_set(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    empty(NewX,NewY,Env),
    % Move
    delete_in_set(player(ID,Nom,PosX,PosY,IDBlock),Env,E2),
    add_in_set(player(ID,Nom,NewX,NewY,IDBlock),E2,R),
    % Post 
    member_set(player(ID,Nom,NewX,NewY,IDBlock),R).

% Case where the player has no block on him
take(D, Env,R) :-
    % Pre
    nom(Nom),
    member_set(player(_,Nom,PosX,PosY,0),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    member_set(block(_,NewX,NewY),Env),
    \+ member_set(player(_,_,NewX,NewY,_),Env),
    % Take
    delete_in_set(block(BID,NewX,NewY),Env,E2),
    delete_in_set(player(PID,Nom,PosX,PosY,0),E2,E3),
    add_in_set(player(PID,Nom,PosX,PosY,BID),E3,R),  
    % Post
    member_set(player(PID,Nom,PosX,PosY,BID),R),
    \+ member_set(block(BID,NewX,NewY),R).


% Case where the player already has block on him
take(D, Env,R) :-
    % Pre
    nom(Nom),
    \+ member_set(player(_,Nom,PosX,PosY,0),Env),
    member_set(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    member_set(block(_,NewX,NewY),Env),
    \+ member_set(player(_,_,NewX,NewY,_),Env),
    % Take
    delete_in_set(block(BID,NewX,NewY),Env,E2),
    delete_in_set(player(PID,Nom,PosX,PosY,PBID),E2,E3),
    add_in_set(block(PBID,NewX,NewY),E3,E4),
    add_in_set(player(PID,Nom,PosX,PosY,BID),E4,R),  
    % Post
    member_set(player(PID,Nom,PosX,PosY,BID),R),
    member_set(block(PBID,NewX,NewY),R).

% Drop the block at the free cell
drop(D, Env,R) :-
    % Pre
    nom(Nom),
    \+ member_set(player(_,Nom,PosX,PosY,0),Env),
    member_set(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    empty(NewX,NewY,Env),
    % Drop
    delete_in_set(player(PID,Nom,PosX,PosY,PBID),Env,E2),
    add_in_set(block(PBID,NewX,NewY),E2,E3),
    add_in_set(player(PID,Nom,PosX,PosY,0),E3,R),  
    % Post
    member_set(player(PID,Nom,PosX,PosY,0),R),
    member_set(block(PBID,NewX,NewY),R).


% Whether we have a block or not, the other player
% will end having what we have. We will also
% assume that the attact always succeeds.
attack(D, Env,R) :-
    % Pre
    nom(Nom),
    member_set(player(_,Nom,PosX,PosY,_),Env),
    direction(PosX,PosY,NewX,NewY,D),
    exist(NewX,NewY,Env),
    \+ member_set(player(_,_,NewX,NewY,0),Env),
    member_set(player(_,_,NewX,NewY,_),Env),
    \+ member_set(block(_,NewX,NewY),Env),
    % Attack
    delete_in_set(player(Us,OurName,PosX,PosY,OurBlock),Env,E2),
    delete_in_set(player(Them,TheirName,NewX,NewY,TheirBlock),E2,E3),
    add_in_set(player(Us,OurName,PosX,PosY,TheirBlock),E3,E4),
    add_in_set(player(Them,TheirName,NewX,NewY,OurBlock),E4,R), 
    % Post
    member_set(player(Us,OurName,PosX,PosY,TheirBlock),R),
    member_set(player(Them,TheirName,NewX,NewY,OurBlock),R).

none.

%Prototype of a node
%
%  node(ResultingEnvAfterAction,Parent,ActionToApplyOnParentEnv,Depth,PathCost)
%
cost(move,1).
cost(take,1).
cost(drop,1).
cost(attack,1).
cost(none,1).

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

%Expand one node (Find all following actions)
expand(Node,Successors) :-
   node(OriginalEnv,_,_,OriginalDepth,OriginalPathCost)=Node,
   Depth is OriginalDepth+1,
   findall(node(ResultEnv,Node,Action,Depth,PathCost),
           action(Action,OriginalEnv,OriginalPathCost,ResultEnv,PathCost),
           Successors).



%/* vim: set filetype=prolog : */
