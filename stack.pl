%------------------------------------------------------------------------------
% Structure de donnee de pile
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
% Test si une pile est vide ou cree une pile
% empty_stack(?Stack)
%------------------------------------------------------------------------------
empty_stack([]).

%------------------------------------------------------------------------------
% Pousser (push) et enlever (pop) un item sur une pile
% push : stack(+X, +Y, -Z), X=item a ajouter, Y=ancienne pile, Z=nouvelle pile
% pop: stack(-X, -Y, +Z), X=item dessus, Y=nouvelle pile, Z=ancienne pile
%------------------------------------------------------------------------------
stack(Top, Stack, [Top|Stack]).
push(X,Stack,NewStack) :- stack(X, Stack, NewStack).
pop(X,Stack,NewStack) :- stack(X, NewStack, Stack).

%------------------------------------------------------------------------------
% Consulter le premier item de la pile
% Top contiendra a valeur du premier item de Stack
% peek_stack(-Top, +Stack)
%------------------------------------------------------------------------------
peek_stack(Top,[Top|_]).

%------------------------------------------------------------------------------
% Verifier si un item est membre d'une pile
% Utilise la fonction member de la librairie standard de liste
% member_stack(+Item, +Stack)
%------------------------------------------------------------------------------
member_stack(Item, Stack) :- member(Item, Stack).

%------------------------------------------------------------------------------
% Ajouter une liste d'items à une pile
% add_list_to_stack(+List, +Stack, -NewStack)
% List=liste a ajouter, Stack=ancienne pile, NewStack=nouvelle pile
% Utilise la fonction append de la librairie standard de liste
%------------------------------------------------------------------------------
add_list_to_stack(List, Stack, NewStack) :- append(List, Stack, NewStack).

