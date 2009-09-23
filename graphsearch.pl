:- ensure_loaded(roumanie).
:- ensure_loaded(set).
:- ensure_loaded(queue).
:- ensure_loaded(stack).

% Node structure
% node( State, Parent, PathCost, Depth)

expand(N, Result) :-
    N=node(State, _, _, _),
    s(State, Nexts),
    add_successors(N,Nexts, Result).

% add_successors(Node, Successors, Result).
add_successors(_, [], []).
add_successors(N, [H|T], [node(H,N,NewPathCost,NewDepth)|R]) :-
    N=node(State, _, PathCost, Depth),
    d(State, H, Distance),
    NewPathCost is PathCost + Distance,
    NewDepth is Depth+1,
    add_successors(N,T,R). 

add_to_fringe(Successors, Fringe, NewFringe) :-
    insert_list_pq(Successors,Fringe,NewFringe).

delete_from_fringe(Element, Fringe, NewFringe) :-
    dequeue(Element, Fringe, NewFringe).

precedes(node(State1,_,PathCost1,_),node(State2,_,PathCost2,_)) :- 
    h(State1, H1), h(State2, H2),
    F1 is PathCost1 + H1,
    F2 is PathCost2 + H2,
    F1 < F2.

% graph_search(Fringe,Goal,Result,Closed).
graph_search([H|T],Goal,Solution,Closed) :-
    delete_from_fringe(Node, [H|T], NewFringe),
    graph_search_helper(Node,NewFringe,Goal,Solution,Closed).

graph_search_helper(node(Goal,Parent,PathCost,Depth),_,Goal,node(Goal,Parent,PathCost,Depth),Closed) :- not(member_set(Goal,Closed)).

graph_search_helper(Node,Fringe,Goal,Solution,Closed) :-
    node(State,_,_,_)=Node,
    State \= Goal,
    member_set(State,Closed), 
    graph_search(Fringe,Goal,Solution,Closed).

graph_search_helper(Node,Fringe,Goal,Solution,Closed) :-
    node(State,_,_,_)=Node,
    State \= Goal,
    not(member_set(State,Closed)),
    add_in_set(State,Closed,NewClosed),
    expand(Node, Successors),
    add_to_fringe(Successors,Fringe,NewFringe),
    graph_search(NewFringe,Goal,Solution,NewClosed).

find(Start, End, Result) :-
   graph_search([node(Start,nil,0,0)],End,Result,[]).

% Somehow this returns only one result but I would expect more than one...


