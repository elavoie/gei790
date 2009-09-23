:-ensure_loaded(graphsearch).
:-ensure_loaded(roumanie).
:-ensure_loaded(queue).
:-ensure_loaded(set).

find(Start, End, Result) :-
   graph_search([node(Start,nil,0,0)],End,Result,[]).

delete_from_fringe(Node,Fringe,NewFringe):-
   dequeue(Node,Fringe,NewFringe).

add_to_fringe(Nodes,Fringe,NewFringe):-
   insert_list_pq(Nodes,Fringe,NewFringe).

precedes(node(State1,_,PathCost1,_),node(State2,_,PathCost2,_)) :-
   h(State1,H1),h(State2,H2),
   F1 is PathCost1 + H1,
   F2 is PathCost2 + H2,
   F1 < F2.

at_goal(node(Goal,_,_,_),Goal).
   
add_to_closed(node(State,_,_,_),Closed,NewClosed):-
   add_in_set(State,Closed,NewClosed).

is_closed(node(State,_,_,_),Closed):-
   member_set(State,Closed).

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
                         

%/* vim: set filetype=prolog : */

