%-----------------------------------------------------------------------------
% Auteur: Charles-Antoine Brunet
% Version: 3.0
% Date: 2009-09-04
%-----------------------------------------------------------------------------
% Le nom du fichier, le nom du module, le préfixe des prédicats et le nom
% du mutex ont tous la même valeur. Dans ce cas ci, c'est p00. Changez TOUTES
% les occurrences de p00 dans ce fichier pour le préfixe qui vous est assigné.
%-----------------------------------------------------------------------------

% Un JI doit être un module afin d'éviter les conflits de noms entre les JI.
:- module(p00,[p00_nom/1,p00_auteurs/1,p00_reset/0,p00_plan/1,p00_action/2]).

%-----------------------------------------------------------------------------
% Prédicats de jeu.
%-----------------------------------------------------------------------------

% Nom du JI: p00_nom(-Nom)
p00_nom('LaMa').

% Auteurs du JI: p00_auteurs(-Auteurs)
p00_auteurs('Erick Lavoie et Alexandre Malo').

% Remise à zero du JI: p00_reset
p00_reset :-
    planInitial(P),
    setPlan(P).

% Plan courant du JI: p00_plan(-PlanCourant)
p00_plan(Plan) :-
    getPlan(Plan).

% Prochaine action du JI: p00_action(+Etat, -Action)
p00_action(Etat, Action) :-
    trouveAction(Etat, Action).

%-----------------------------------------------------------------------------
% Prédicats internes de plans.
%-----------------------------------------------------------------------------
% La consultation d'un plan et la modification d'un plan sont protégées par
% mutex afin d'éviter les conflits possibles d'appels en parallèle.
%
% Le prédicat planRestant est déclaré dynamique, car sa valeur change au cours
% de l'exécution.
%-----------------------------------------------------------------------------

:- dynamic([planRestant/1]).

planInitial([]).

planRestant([]).

getPlan(Plan) :-
    with_mutex(p00,planRestant(Plan)).

setPlan(Plan) :-
    with_mutex(p00,changePlan(Plan)).

changePlan(Plan) :-
    retractall(planRestant(_)),
    assert(planRestant(Plan)).

%-----------------------------------------------------------------------------
% Prédicats internes d'action
%-----------------------------------------------------------------------------
% Calcul de la prochaine action du JI. Ce JI ne fera jamais rien de bon...
%-----------------------------------------------------------------------------
trouveAction(EtatJeu, ProchaineAction) :-
    getPlan([ProchaineAction]), !, planInitial(P), setPlan(P).
trouveAction(EtatJeu, ProchaineAction) :-
    getPlan([ProchaineAction|PlanRestant]), setPlan(PlanRestant).
