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

test_env(Env) :-
    R=[1,2,2,2,[[1,brutus,0,0,1]],[[2,0,1]]],
    build_env(R,Env).
