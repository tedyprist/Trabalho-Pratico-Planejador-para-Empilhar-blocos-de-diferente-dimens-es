%--------------------------------------------------------------------------------------------------
%                            Disciplina: IEC034 - INTELIGÊNCIA ARTIFICIAL
%--------------------------------------------------------------------------------------------------
% 1º Trabalho: Planejador para Empilhar blocos de diferente dimensões
%--------------------------------------------------------------------------------------------------
% Professor: Edjard Mota
% Alunos: Tedy Prist       - 22050676
%         Isabelly Rohana  - 21352282
%--------------------------------------------------------------------------------------------------           
%                Project: Prolog Programming for AI, Ivan Bratko, 4th edition
%                               Chapter 17 - Planning          
%--------------------------------------------------------------------------------------------------

% Questão 02: Modificar o código do planner da figura 17.6 de tal maneira que este manipule 
% corretamente variáveis sobre goals e também ações conforme discussão na sessão 17.5. Indique 
% esta mudança com a explicação

%--------------------------------------------------------------------------------------------------
%                                         Planejador
%--------------------------------------------------------------------------------------------------

% A means-ends planner with goal regression
% plan(State, Goals, Plan)

plan(State, Goals, [ ]) :-
	satisfied(State, Goals). 					% Goals true in State
plan(State, Goals, Plan) :-
	append(PrePlan, [Action], Plan), 			% Divide plan achieving breadth-first effect
	select(State, Goals, Goal), 				% Select a goal
	achieves(Action, Goal),
	can(Action, Condition), 					% Ensure Action contains no variables
	preserves(Action, Goals), 					% Protect Goals
	regress(Goals, Action, RegressedGoals), 	% Regress Goals through Action
	plan(State, RegressedGoals, PrePlan).

% -----------------------------------------------------------------------
satisfied(State, Goals) :-
	delete_all(Goals, State, []). 				% All Goals in State

% -----------------------------------------------------------------------
select(State, Goals, Goal) :- 					% Select Goal from Goals
	member(Goal, Goals). 						% A simple selection principle

% -----------------------------------------------------------------------
achieves(Action, Goal) :-
	adds(Action, Goals),
	member(Goal, Goals).

% -----------------------------------------------------------------------
preserves(Action, Goals) :- 					% Action does not destroy Goals
	deletes(Action, Relations),
	\+ (member(Goal, Relations), member(Goal, Goals)).

% -----------------------------------------------------------------------
regress(Goals, Action, RegressedGoals) :- 		% Regress Goals through Action
	adds(Action, NewRelations),
	delete_all(Goals, NewRelations, RestGoals),
	can(Action, Condition),
	addnew(Condition, RestGoals, RegressedGoals).

% -----------------------------------------------------------------------
% addnew(NewGoals, OldGoals, AllGoals):
%	AllGoals is the union of NewGoals and OldGoals
%	NewGoals and OldGoals must be compatible 

addnew([], L, L).
addnew([Goal | _], Goals, _) :-
	impossible(Goal, Goals), 					% Goal incompatible with Goals
	!, fail. 									        % Cannot be added
addnew([X | L1], L2, L3) :-
	member(X, L2), !, 							  % Ignore duplicate
	addnew(L1, L2, L3).
addnew([X | L1], L2, [X | L3]) :-
	addnew(L1, L2, L3).

% -----------------------------------------------------------------------
% delete_all(L1, L2, Diff): Diff is set-difference of lists L1 and L2

delete_all([], _, []).
delete_all([X | L1], L2, Diff) :-
	member(X, L2), !,
	delete_all(L1, L2, Diff).
delete_all([X | L1], L2, [X | Diff]) :-
	delete_all(L1, L2, Diff). 

%--------------------------------------------------------------------------------------------------
%                                     Funcoes da questão Anterior 
%--------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------
%                                         Ações Permitidas
%--------------------------------------------------------------------------------------------------

% Ação de mover um objeto de um local para outro
can(move(Block,From,To),[clear(Block),clear(To),on(Block,From),seguro_de_empilhar(From,To)]) :-
    block(Block),
    object(To),
    To \== Block,
    object(From),
    From \== To,
    Block \== From.
    
%--------------------------------------------------------------------------------------------------
%                          Efeitos das Ações (Adição e Remoção de Relações)
%--------------------------------------------------------------------------------------------------

% Efeitos da ação de mover um objeto
adds(move(X, From, To), [on(X, To), clear(From)]).

% Efeitos da ação de mover um objeto
deletes(move(X, From, To), [on(X, From), clear(To)]).

