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
% PROJECTS
%
% Develop a program, using the techniques of this chapter, for planning in a more interesting variation 
% of the simple blocks world used throughout this chapter. Figure 17.9 shows an example task in this 
% new world. The new world contains blocks of different sizes and the stability of structures has to 
% be taken into account. To make the problem easier, assume that blocks can only be placed at whole-
% numbered positions, and they always have to be properly supported, so that they are always stable. 
% Also assume that they are never rotated by the robot and the move trajectories are simple: straight 
% up until the block is above any other block, then horizontally, and then straight down. Design 
% specialized heuristics to be used by this planner.
% A robot world, more realistic and interesting than the one defined in Figure 17.2, would also 
% comprise perception actions by a camera or touch sensor. For example, the action look( Position, Object) 
% would recognize the object seen by the camera at Position (that is, instantiate variable Object). 
% In such a world it is realistic to assume that the scene is not completely known to the robot, so it 
% will possibly include, in its plan, actions whose only purpose is to acquire information. This can be 
% further complicated by the fact that some measurements of this kind cannot be done immediately, as some 
% objects cannot be seen (bottom objects cannot be seen by a top-view camera). Introduce other relevant goal 
% relations and modify our planners if necessary. Modify the goal regression planner of Figure 17.6 so that it 
% will correctly handle variables in goals and actions, according to the discussion in Section 17.5. 
% Figure 17.5 Relations between various sets of conditions in goal regression through action A. The shaded 
% area represents the resulting regressed goals Goals0: GoalsO = can(A) U Goals —add(A). Notice that the intersection 
% between Goals and the delete-list of A must be empty.

%----------------------------------------------------------------------
% Descrição do mundo dos blocos
%----------------------------------------------------------------------

block(a).
block(b).
block(c).
block(d).

place(1).
place(2).
place(3).
place(4).
place(5).
place(6).

size(a, 1).
size(b, 1).
size(c, 2).
size(d, 3).

%----------------------------------------------------------------------
% Estado Inicial
%----------------------------------------------------------------------

%
%             d         
%       c     a   b
%       = = = = = =
% place 1 2 3 4 5 6
State = [on(a, 3), on(b, 3), on(c, 2), on(d, 1), clear(a), clear(b), clear(c), clear(6)].

%----------------------------------------------------------------------
% Meta/Goal
%----------------------------------------------------------------------

%               a 
%               c        
%               d b
%       = = = = = =
% place 1 2 3 4 5 6
Goals = [on(a, 1), on(b, 3), on(c, 2), on(d, 4), clear(b), clear(c), clear(d), clear(6)].

% A means-ends planner with goal regression (updated for variables)
% plan(State, Goals, Plan)
plan(State, Goals, []) :-
    satisfied(State, Goals). % Goals true in State
plan(State, Goals, Plan) :-
    conc(PrePlan, [Action], Plan), % Divide plan achieving breadth-first effect
    select(State, Goals, Goal), % Select a goal
    achieves(Action, Goal),
    can(Action, Condition), % Ensure Action contains no variables
    substitute_vars(Condition, State, SubstitutedCondition), % Substitute variables in condition
    preserves(Action, Goals), % Protect Goals
    regress(Goals, Action, SubstitutedCondition, RegressedGoals), % Regress Goals through Action
    plan(State, RegressedGoals, PrePlan).

can( move( Block, From, To), [ clear( Block), clear( To), on( Block, From)] ) :-
	block( Block), % Block to be moved
	object( To), % 'To' is a block or a place
	To \== Block, % Block cannot be moved to itself
	object( From), % 'From' is a block or a place
	From \== To, % Move to new position
	Block \== From. % Block not moved from itself

% adds( Action, Relationships): Action establishes Relationships
adds( move(X,From,To), [ on(X,To), clear(From)]).

% deletes( Action, Relationships): Action destroys Relationships
deletes( move(X,From,To), [ on(X,From), clear(To)]).

object( X) :- % X is an object if
	place( X) % X is a place
	; % or
	block( X). % X is a block

satisfied(State, Goals) :-
    delete_all(Goals, State, []). % All Goals in State

select(State, Goals, Goal) :- % Select Goal from Goals
    member(Goal, Goals). % A simple selection principle

achieves(Action, Goal) :-
    adds(Action, Goals),
    member(Goal, Goals).

preserves(Action, Goals) :- % Action does not destroy Goals
    deletes(Action, Relations),
    \+ (member(Goal, Relations), member(Goal, Goals)).

regress(Goals, Action, Condition, RegressedGoals) :- % Regress Goals through Action
    adds(Action, NewRelations),
    delete_all(Goals, NewRelations, RestGoals),
    addnew(Condition, RestGoals, RegressedGoals). % Add preconditions, check impossibility

% substitute_vars(Condition, State, SubstitutedCondition): Substitute variables in Condition using State
substitute_vars([], _, []).
substitute_vars([clear(X) | Rest], State, [clear(X) | SubstitutedRest]) :-
    substitute_vars(Rest, State, SubstitutedRest).
substitute_vars([on(Block, Place) | Rest], State, [on(Block, SubstitutedPlace) | SubstitutedRest]) :-
    member(on(Block, SubstitutedPlace), State),
    substitute_vars(Rest, State, SubstitutedRest).

% addnew(NewGoals, OldGoals, AllGoals):
% AllGoals is the union of NewGoals and OldGoals
% NewGoals and OldGoals must be compatible
addnew([], L, L).
addnew([Goal | _], Goals, _) :-
    impossible(Goal, Goals), % Goal incompatible with Goals
    !,
    fail. % Cannot be added
addnew([X | L1], L2, L3) :-
    member(X, L2), !, % Ignore duplicate
    addnew(L1, L2, L3).
addnew([X | L1], L2, [X | L3]) :-
    addnew(L1, L2, L3).

% delete_all(L1, L2, Diff): Diff is set-difference of lists L1 and L2
delete_all([], _, []).
delete_all([X | L1], L2, Diff) :-
    member(X, L2), !,
    delete_all(L1, L2, Diff).
delete_all([X | L1], L2, [X | Diff]) :-
    delete_all(L1, L2, Diff).

%----------------------------------------------------------------------
% Execução do planejador
%----------------------------------------------------------------------

% Chamar o planejador para resolver o problema
solve(Plan) :- plan(State, Goals, Plan).

% Imprimir o resultado
print_result([]).
print_result([Action | Rest]) :-
    write(Action), nl,
    print_result(Rest).

% Testar o planejador
:- solve(Plan), print_result(Plan).
