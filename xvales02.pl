% Kostra grafu
% Funkcionalni a logicke programovani
% Nikola Valesova, xvales02


start :-
	prompt(_, ''),
	% read and process input lines
	read_lines(LL),
	split_lines(LL, S),
	flatten(S, XS),
	% create a list of all vertexes of given graph
	vertex_list(XS, [], V),
	reverse(V, VR),
	% get all possible spanning trees of given graph
	setof(T, get_stree(S, VR, T), Trees),
	% remove trees that are a permutation of another tree
	remove_duplicates(Trees, Unique_trees),
	% output valid solutions
	print_solutions(Unique_trees),
	halt.

% get spanning tree of given graph
get_stree(S, VR, T) :- 
	member(X, S),
	delete(S, X, XS),
	create_stree(XS, VR, [X], T).

% create a single spanning tree by adding edges
% S - list of yet not added edges, VR - vertex list, X - already added edges, current state of spanning tree
create_stree(S, VR, X, R) :-
	% check if tree already contains |V| - 1 edges
	length(X, LX),
	length(VR, LVR),
	DLVR is LVR - 1,
	% check if there are any edges left to add
	length(S, LS),
	% stop conditions not met, add more edges
	\+ stop_conditions_met(LX, DLVR, LS),
	% test if adding current edge would create a cycle
	member(E, S),
	delete(S, E, ES),
	contains_cycle(VR, [E|X], Bool_list),
	flatten(Bool_list, Bool),
	(member(false, Bool) -> create_stree(ES, VR, X, R); create_stree(ES, VR, [E|X], R)).

% create a single spanning tree by adding edges
create_stree(S, VR, X, XR) :-
	% check if tree already contains |V| - 1 edges
	length(X, LX),
	length(VR, LVR),
	DLVR is LVR - 1,
	% check if there are any edges left to add
	length(S, LS),
	% stop conditions met, the tree construction is done
	stop_conditions_met(LX, DLVR, LS),
	% check if created spaning tree is complete -> the graph is connected
	% contains_all_vertexes(VR, X, Bool),
	(is_equal(LX, DLVR) -> XR = X; XR = []).
	% (is_equal(LX, DLVR), is_equal(Bool, true) -> XR = X; XR = []).

% check if stop conditions are met -> the spanning tree construction is done
stop_conditions_met(LX, DLVR, LS) :-
	is_equal(LX, DLVR);
	is_equal(LS, 0).

% check if edges in Edges contain a cycle
contains_cycle(Vertexes, Edges, Bool) :- maplist(check_cycle(Edges, []), Vertexes, Bool).

% check for current vertex if in edges there is a cycle 
check_cycle(Edges, Visited, Curr_vertex, [false]) :- member(Curr_vertex, Visited), !.
check_cycle([], Visited, Curr_vertex, [true]) :- \+ member(Curr_vertex, Visited).
check_cycle([H|Edges], Visited, Curr_vertex, [true|Bool]) :-
	((nth0(0, H, Curr_vertex); nth0(1, H, Curr_vertex)) -> get_next_vertex(H, Curr_vertex, Next),
	check_cycle(Edges, [Curr_vertex|Visited], Next, Bool); check_cycle(Edges, Visited, Curr_vertex, Bool)).

% get vertex Next that is connected by an edge to vertex Curr_vertex
get_next_vertex(Curr_edge, Curr_vertex, Next) :-
	(nth0(0, Curr_edge, Curr_vertex), nth0(1, Curr_edge, Next));
	(nth0(0, Curr_edge, Next), nth0(1, Curr_edge, Curr_vertex)).

% contains_all_vertexes(VR, Tree, Bool) :-
% 	length(VR, LVR),
% 	flatten(Tree, All_vertexes),
% 	vertex_list(All_vertexes, [], Vertexes),
% 	length(Vertexes, LVertexes),
% 	(is_equal(LVR, LVertexes) -> Bool = true; Bool = false).	

% check if arguments are equal
is_equal(A, B) :- A == B.

% create a list of all vertexes in a graph
vertex_list([], S, S).
vertex_list([H|T], S, M) :- (\+ member(H, S) -> vertex_list(T, [H|S], M); vertex_list(T, S, M)).

% remove all duplicitous spanning trees
remove_duplicates([], []).
remove_duplicates([H|Trees], [H|Unique]) :-
	findall(Permuted_tree, permutation(H, Permuted_tree), Permutations),
	remove_permutations(Trees, Permutations, Unique_trees),
	remove_duplicates(Unique_trees, Unique).

% remove all permutations of current tree
remove_permutations(Unique, [], Unique).
remove_permutations(Tree, [H|Permuted_tree], Unique_tree) :-
	delete(Tree, H, Unique),
	remove_permutations(Unique, Permuted_tree, Unique_tree).

% read line from stdin, terminate on LF or EOF
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !; read_line(LL,_), [C|LL] = L).

% test if character is EOF or LF
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C, Code), Code == 10).

% read from stdin line by line
read_lines(Ls) :-
	read_line(L,C),
	(C == end_of_file, Ls = []; read_lines(LLs), Ls = [L|LLs]).

% split single input line
split_line([], [[]]) :- !.
split_line([' '|T], S1) :- !, split_line(T, S1).
split_line([H|T], X) :-
	split_line(T, [G|S1]),
	flatten([[H|G]|S1], X).

% split all lines of parsed input
split_lines([], []).
split_lines([L|Ls], [H|T]) :-
	split_lines(Ls, T),
	split_line(L, H).

% print list of solutions
print_solutions([]).
print_solutions([H|T]) :-
	print_solution(H),
	print_solutions(T).

% print single solution
print_solution([]).
print_solution([H|T]) :- 
	nth0(0, H, Vert1),
	nth0(1, H, Vert2),
	write(Vert1),
	write('-'),
	write(Vert2),
	list_empty(T, Is_last_edge),
	(Is_last_edge -> write('\n'); write(' '), print_solution(T)).

% check if list is empty
list_empty([], true).
list_empty([_|_], false).
