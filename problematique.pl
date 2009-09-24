:- ensure_loaded(set).
:- ensure_loaded(queue).
:- ensure_loaded(graphsearch).

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

query(Q,[Q|_]) :- !.
query(Q,[_|T]) :-
   query(Q,T).

nom('brutus').

x_nom(Nom) :- nom(Nom).

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

%Search specific functions

%Expand one node (Find all following actions)
expand(Node,Successors) :-
   node(OriginalEnv,_,_,OriginalDepth,OriginalPathCost)=Node,
   Depth is OriginalDepth+1,
   findall(node(ResultEnv,Node,Action,Depth,PathCost),
           action(Action,OriginalEnv,OriginalPathCost,ResultEnv,PathCost),
           Successors).

where_is_block(Id,PosX,PosY,Env) :-
   query(player(_,_,PosX,PosY,Id),Env).

where_is_block(Id,PosX,PosY,Env) :-
   query(block(Id,PosX,PosY),Env).

%Find the nearest block of our player %%A optimiser
closestBlockDistance(Env,Distance) :-
   findall(Block,blockInEnv(Block,Env),Blocks),
   nom(Nom),
   query(player(_,Nom,PPosX,PPosY,_),Env),
   query(nbColonnes(NbColonnes),Env),
   query(nbRangees(NbRangees),Env),
   max(NbColonnes,NbRangees,MaxDistance),
   distance_min(Blocks,PPosX,PPosY,MaxDistance,Distance).

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

%Predicat to verify if the goal is achieved
at_goal(node(EnvToValidate,_,_,_,_),_) :-
   nom(Nom),
   query(player(_,Nom,_,_,Block),EnvToValidate),
   has_block(player(_,_,_,_,Block)).


%Add a node to the closed set
add_to_closed(node(State,_,_,_,_),Closed,NewClosed):-
    add_in_set(State,Closed,NewClosed).
%Add a successor to the fringe
add_to_fringe(Successors, Fringe, NewFringe) :-
    %write('s '), list_valid_moves(Successors),nl,
    insert_list_pq(Successors,Fringe,NewFringe).
    %write('f '),list_valid_moves(Fringe),nl,
    %write('nf '),list_valid_moves(NewFringe),nl,nl.
%Delete a successor from the fringe
delete_from_fringe(Element, Fringe, NewFringe) :-
    dequeue(Element, Fringe, NewFringe).
    %write(Element),nl.

has_block(player(_,_,_,_,Block)) :-
   \+Block = 0.

%Comparing the set of state no mather the order of what's in the state
is_closed(node(State,_,_,_,_),Closed):-
   in_set(State,Closed).

in_set(_,[]) :- \+true.
in_set(State1,[State2|_]) :-
   equal_set(State1,State2), !.
in_set(State1,[State2|Closed]) :-
   \+equal_set(State1,State2),
   in_set(State1,Closed).
   
find(Start, Result) :-
   graph_search([node(Start,nil,nil,0,0)],_,Result,[]).

build_path(node(_,nil,nil,_,_),R,R).
build_path(node(_,Parent,Action,_,_),PreviousR,R) :-
   build_path(Parent,[Action|PreviousR],R).

validate(Env, [], Env).
validate(Env, [Action|T], R) :-
   action(Action, Env,E2),
   validate(E2,T,R).

validate_plan(Env, Actions) :-
   validate(Env, Actions, R),
   at_goal(node(R,_,_,_,_),_). 





%/* vim: set filetype=prolog : */
