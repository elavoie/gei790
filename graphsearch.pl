
%To use the generic graph search, the caller must implement it's own
%methods to access to the data. Here are the following method to write
%  -delete_from_fringe(Node,Fringe,NewFringe)
%  -add_to_fringe(Nodes,Fringe,NewFringe)
%  -at_goal(Node,Goal)
%  -add_to_closed(Node,Closed,NewClosed)
%  -is_closed(Node,Closed)
%  -expand(Node,Successors)


% graph_search(Fringe,Goal,Result,Closed).
graph_search([H|T],Goal,Solution,Closed) :-
    delete_from_fringe(Node, [H|T], NewFringe),
    graph_search_helper(Node,NewFringe,Goal,Solution,Closed).

graph_search_helper(Node,_,Goal,Node,Closed):-
    at_goal(Node,Goal),
    \+is_closed(Node,Closed).

graph_search_helper(Node,Fringe,Goal,Solution,Closed) :-
    \+at_goal(Node,Goal),
    is_closed(Node,Closed), 
    graph_search(Fringe,Goal,Solution,Closed).

graph_search_helper(Node,Fringe,Goal,Solution,Closed) :-
    \+at_goal(Node,Goal),
    \+is_closed(Node,Closed),
    add_to_closed(Node,Closed,NewClosed),
    expand(Node, Successors),
    add_to_fringe(Successors,Fringe,NewFringe),
    graph_search(NewFringe,Goal,Solution,NewClosed).


