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

%------------------------------------------------------------------------------
% IMPORTANT! Selon le type de donnee, peut-etre necessaire de changer la
%     definition de precedes
%------------------------------------------------------------------------------
%precedes(X,Y) :- X < Y.


