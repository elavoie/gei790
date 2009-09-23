:- ensure_loaded(problematique).

%test :-
%    test_attack_sans_block,
%    test_attack_avec_block.
%
%test_attack_sans_block :-
%    L=[2,1,2,2,[[1,brutus,0,0,0],[2,zouf,0,1,1]],[]],
%    build_env(L, Env),
%    attack(1, Env).
%
%test_attack_avec_block :-
%    L=[2,2,2,2,[[1,brutus,0,0,2],[2,zouf,0,1,1]],[]],
%    build_env(L, Env),
%    attack(1, Env).

test_atgoal :-
    L=[1,1,2,2,[[1,brutus,0,0,1]],[]],
    build_env(L,Env),
    at_goal(Env,_).

test_notatgoal :-
    L=[1,1,2,2,[[1,brutus,0,0,0]],[]],
    build_env(L,Env),
    \+at_goal(Env,_).

test_planif :-
    L=[1,1,2,2,[[1,brutus,0,0,0]],[[1,0,1]]],
    build_env(L,Env),
    find(Env,Plan),
    print(Plan).

test_move :-
    L=[1,1,2,2,[[1,brutus,0,0,0]],[]],
    R=[1,1,2,2,[[1,brutus,0,1,0]],[]],
    build_env(L,Env),
    build_env(R,EnvR),
    move(1,Env,EnvR).

test_take_no_block_on_player :-
    L=[1,1,2,2,[[1,brutus,0,0,0]],[[1,0,1]]],
    R=[1,1,2,2,[[1,brutus,0,0,1]],[]],
    build_env(L,Env),
    build_env(R,EnvR),
    take(1,Env,EnvR).

test_take_a_block_on_player :-
    L=[1,2,2,2,[[1,brutus,0,0,2]],[[1,0,1]]],
    R=[1,2,2,2,[[1,brutus,0,0,1]],[[2,0,1]]],
    build_env(L,Env),
    build_env(R,EnvR),
    take(1,Env, TakeR),
    equal_set(EnvR,TakeR).

test_drop_a_block :-
    L=[1,1,2,2,[[1,brutus,0,0,1]],[]],
    R=[1,1,2,2,[[1,brutus,0,0,0]],[[1,0,1]]],
    build_env(L,Env),
    build_env(R,EnvR),
    drop(1,Env, DropR),
    equal_set(EnvR,DropR).

test_attack :-
    L=[2,1,2,2,[[1,brutus,0,0,0],[2,zeouf,0,1,1]],[]],
    R=[2,1,2,2,[[1,brutus,0,0,1],[2,zeouf,0,1,0]],[]],
    build_env(L,Env),
    build_env(R,EnvR),
    attack(1,Env,AttackR),
    equal_set(EnvR,AttackR).

test_attack_exchange :-
    L=[2,2,2,2,[[1,brutus,0,0,2],[2,zeouf,0,1,1]],[]],
    R=[2,2,2,2,[[1,brutus,0,0,1],[2,zeouf,0,1,2]],[]],
    build_env(L,Env),
    build_env(R,EnvR),
    attack(1,Env,AttackR),
    equal_set(EnvR,AttackR).

test_priority_queue :-
   N1=node([player(1, brutus, 0, 1, 0), block(1, 0, 2), nbRangees(3), nbColonnes(3), nbBlocks(1), nbJoueurs(1)],
            node([block(1, 0, 2), player(1, brutus, 0, 0, 0), nbRangees(3), nbColonnes(3), nbBlocks(1), nbJoueurs(1)], nil, nil, 0, 0),
            move(1), 1, 1),
   N2=node([player(1, brutus, 1, 0, 0), block(1, 0, 2), nbRangees(3), nbColonnes(3), nbBlocks(1), nbJoueurs(1)],
            node([block(1, 0, 2), player(1, brutus, 0, 0, 0), nbRangees(3), nbColonnes(3), nbBlocks(1), nbJoueurs(1)], nil, nil, 0, 0),
            move(2), 1, 1),
   insert_pq(N1,[],R),
   [N1] = R. 


test_precedes :-
   N1=node([player(1, brutus, 0, 1, 0), block(1, 0, 2), nbRangees(3), nbColonnes(3), nbBlocks(1), nbJoueurs(1)], node([block(1, 0, 2), player(1, brutus, 0, 0, 0), nbRangees(3), nbColonnes(3), nbBlocks(1), nbJoueurs(1)], nil, nil, 0, 0), move(1), 1, 1),
   N2=node([player(1, brutus, 1, 0, 0), block(1, 0, 2), nbRangees(3), nbColonnes(3), nbBlocks(1), nbJoueurs(1)], node([block(1, 0, 2), player(1, brutus, 0, 0, 0), nbRangees(3), nbColonnes(3), nbBlocks(1), nbJoueurs(1)], nil, nil, 0, 0), move(2), 1, 1),
   precedes(N1,N2).

test_env(Env) :-
    R=[1,4,3,3,[[1,brutus,0,0,4]],[[1,0,2],[2,0,1],[3,1,1]]],
    build_env(R,Env).

list_valid_moves([]).
list_valid_moves([node(Env,_,Action,_,PathCost)|T]) :-
   closestBlockDistance(Env,H), 
   write(Action),write(':'),
   Cost is H + PathCost,
   write(Cost),
   write(','),
   list_valid_moves(T).

list_successors([]).
list_successors([node(State,_,Action,Depth,PathCost)|T]) :- write(State),write(' '),write(Action),write(' '),write(Depth),write(' '),write(PathCost),nl, list_successors(T).

build_path(node(_,nil,nil,_,_),R,R).
build_path(node(_,Parent,Action,_,_),PreviousR,R) :-
   build_path(Parent,[Action|PreviousR],R).

%/* vim: set filetype=prolog : */

