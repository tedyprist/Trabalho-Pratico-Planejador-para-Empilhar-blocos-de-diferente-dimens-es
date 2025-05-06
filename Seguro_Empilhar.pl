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

% Questão 01: Estenda a definição do mundo dos blocos da Figura 17.2 para 
% incluir outros objetos de diferentes formas como pirâmides, bolas, caixas 
% fechadas ou abertas em forma de cubo ou paralelogramo (3 dimensões). 
% Adicione condições extras para garantir segurança de empilha (constante 
% ‘seguro_de_empilhar’ pode ser usada como parâmetro). Por exemplo, não é
% seguro_de_empilhar um bloco (cubo ou caixa) sobre uma pirâmide ou uma 
% caixa maior sobre uma caixa muito menor fora do centro de gravidade 
% (isso pode ser usado na questão seguinte); é seguro_de_empilhar uma 
% bola dentro de uma caixa maior que a bola; etc.

%--------------------------------------------------------------------------------------------------
%                                Descrição do mundo dos blocos
%--------------------------------------------------------------------------------------------------

block(a).
block(b).
block(c).
pyramid(p).
ball(ball1).
rectangle(rect).
closed_box_cube(cube).

place(1).
place(2).
place(3).
place(4).
place(5).
place(5).
place(6).
place(6).

% Defina o comprimento de cada objeto
object_length(a, 3).
object_length(b, 4).
object_length(c, 2).
object_length(p, 2).
object_length(ball1, 1).
object_length(cube, 2).
object_length(rect, 3).

% Definição dos objetos no mundo dos blocos
object(X) :- block(X) ; pyramid(X) ; ball(X) ; closed_box_cube(X) ; place(X) ; rectangle(X).

%--------------------------------------------------------------------------------------------------
%                                         Inicial State 
%--------------------------------------------------------------------------------------------------

% Representação de um estado inicial no mundo dos blocos
%
%       c          
%       a   b  
%       = = = = = =
% place 1 2 3 4 5 6


initial_state([clear(2), clear(4), clear(b), clear(c), on(a,1), on(b,3), on(c,a)]).

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
%                                  Regras de Empilhamento Seguro 
%--------------------------------------------------------------------------------------------------

% Defina a relação para verificar se um objeto é alto o suficiente para empilhar
alto_suficiente(_, 0).  % Altura de um lugar é 0

alto_suficiente(Object, Height) :-
    Object \= place(_),
    length(Object, Length),
    Length >= Height.

% Definição das regras para empilhamento seguro
seguro_de_empilhar(_,block(_)). % Qualquer objeto pode ser empilhado sobre um bloco
seguro_de_empilhar(_,pyramid(_)) :- fail. % Pirâmides não podem ter objetos empilhados sobre elas
seguro_de_empilhar(_,ball(_)) :- fail.  % Bolas não podem ter objetos empilhados sobre elas
seguro_de_empilhar(_,closed_box_cube(_)) . % Qualquer objeto pode ser empilhado sobre uma caixa fechada
seguro_de_empilhar(_,place(_)). % Blocos podem ser empilhados sobre lugares
seguro_de_empilhar(_, rectangle(_)).
seguro_de_empilhar(place(_),_):- fail. % Lugares não podem ser empilhados sobre outros objetos
seguro_de_empilhar(rectangle(_), rectangle(_)).

seguro_de_empilhar(X, Y) :-
  alto_suficiente(X, HeightX),
  alto_suficiente(Y, HeightY),
  HeightX =< HeightY.

%--------------------------------------------------------------------------------------------------
%                          Efeitos das Ações (Adição e Remoção de Relações)
%--------------------------------------------------------------------------------------------------

% Efeitos da ação de mover um objeto
adds(move(X, From, To), [on(X, To), clear(From)]).

% Efeitos da ação de mover um objeto
deletes(move(X, From, To), [on(X, From), clear(To)]).
