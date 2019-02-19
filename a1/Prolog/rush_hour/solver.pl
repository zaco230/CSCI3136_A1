%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% rush_hour/solver.pl
%
% Rush Hour puzzle solver
% (C) 2019 Norbert Zeh (nzeh@cs.dal.ca)
%
% The solver logic
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Import the state predicates
:- [rush_hour/state].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% IMPLEMENT THE FOLLOWING PREDICATE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Solve the puzzle
%solve_puzzle(Puzzle, Moves).

solve_puzzle(Puzzle, Moves) :-
	puzzle_state(Puzzle, State),
	_search(State,Moves).

_search(State,Moves) :-
	sol(State,Moves) ; find(State, next_state), 
	_search(next_state,Moves).
	sol([[State|Moves]|State],Moves) :- 
		state_is_solved(State),
		sol(State,Moves).

find(State, next_state) :-
	findall([State|Moves] , moves(State,State,Moves,L), next_state),
	append(next_state,L,next_state).

seen(State,L) :- member(State,L).

moves(States,new_state,[next_move|Moves],L) :-
	Pos < 64, (Moves is Pos; Pos is Pos+1, moves(States,new_state,[next_move|Moves],L))
	offset < 5, (Moves is offset; offset is offset+1,
	(state_is_horizontal(State,Pos), horizontal_move(State,Pos,offset,new_state); vertical_move(State,Pos,offset,new_state)), 
	\+ seen(new_state,L), pos_offset_move(Pos,offset,next_move).
