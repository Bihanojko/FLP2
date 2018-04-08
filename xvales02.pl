% Kostra grafu
% Funkcionalni a logicke programovani
% Nikola Valesova, xvales02


start :-
	prompt(_, ''),
	% read and process input lines
	read_lines(LL),
	split_lines(LL, Edges),
	% create a list of all vertices of given graph
	vertex_list(Edges, Vertices),
	% get all possible spanning trees of given graph
	findall(T, get_stree(Edges, Vertices, T), Trees),
	% remove all duplicitous spanning trees
	sort(Trees, Unique_trees),
	% output valid solutions
	print_solutions(Unique_trees),
	halt.

% get spanning tree of given graph
get_stree(Edges, VR, Sorted_tree) :- 
	member(X, Edges),
	delete(Edges, X, RE),
	length(VR, VC),
	create_stree(RE, VR, VC, [X], Tree),
	sort(Tree, Sorted_tree).

% create a single spanning tree by adding edges
% S - list of yet not added edges, VR - vertex list, X - already added edges, current state of spanning tree
create_stree(S, VR, VC, X, R) :-
	% check if tree already contains |V| - 1 edges
	length(X, LX),
	length(VR, LVR),
	DLVR is LVR - 1,
	% check if there are any edges left to add
	length(S, LS),
	% stop conditions not met, add more edges
	\+ stop_conditions_met(LX, DLVR, LS, VC),
	% test if adding current edge would create a cycle
	member(E, S),
	delete(S, E, ES),
	(contains_cycle(VR, [E|X]) -> create_stree(ES, VR, VC, X, R); create_stree(ES, VR, VC, [E|X], R)).

% create a single spanning tree by adding edges
create_stree(S, VR, VC, X, XR) :-
	% check if tree already contains |V| - 1 edges
	length(X, LX),
	length(VR, LVR),
	DLVR is LVR - 1,
	% check if there are any edges left to add
	length(S, LS),
	% stop conditions met, the tree construction is done
	stop_conditions_met(LX, DLVR, LS, VC),
	% check if created spanning tree is complete and contains all vertices -> the graph is connected
	contains_all_vertices(LVR, X, Bool),
	(is_equal(LX, DLVR), is_equal(Bool, true) -> XR = X; XR = []).

% check if stop conditions are met -> the spanning tree construction is done
stop_conditions_met(LX, DLVR, LS, VC) :-
	is_equal(LX, DLVR);
	is_equal(LS, 0).

% check if edges in Edges contain a cycle
contains_cycle([], _) :- false.
contains_cycle([H|T], Edges) :- 
	check_cycle(Edges, H);
	contains_cycle(T, Edges).

% check for current vertex if in edges there is a cycle 
check_cycle(Edges, Curr_vertex) :- check_cycle2(Edges, Curr_vertex, []), !.
check_cycle2(_, Curr_vertex, Visited) :- member(Curr_vertex, Visited), !.
check_cycle2(Edges, Curr_vertex, Visited) :-
	member(Edge, Edges),
	member(Curr_vertex, Edge),
	member(Next, Edge),
	\+ is_equal(Curr_vertex, Next),
	delete(Edges, Edge, New_edges),
	check_cycle2(New_edges, Next, [Curr_vertex|Visited]).

% check if spanning tree connects all vertices
contains_all_vertices(LVR, Tree, Bool) :-
 	flatten(Tree, All_vertices),
 	vertex_list(All_vertices, Vertices),
 	length(Vertices, Lvertices),
 	(is_equal(LVR, Lvertices) -> Bool = true; Bool = false).

% check if arguments are equal
is_equal(A, B) :- A == B.

% create a list of all vertices in a graph
vertex_list([], []).
vertex_list([H|T], V) :- flatten([H|T], F), sort(F, V).

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

% split all lines of parsed input
split_lines([], []).
split_lines([[V1, ' ', V2]|Ls], [[V1, V2]|T]) :-
	split_lines(Ls, T).
% skip invalid lines
split_lines([_|Ls], T) :- split_lines(Ls, T).

% print list of solutions
print_solutions([]).
print_solutions([H|T]) :-
	print_solution(H),
	print_solutions(T).

% print single solution
print_solution([]).
print_solution([[V1, V2]|T]) :- 
	write(V1),
	write('-'),
	write(V2),
	list_empty(T, Is_last_edge),
	(Is_last_edge -> write('\n'); write(' '), print_solution(T)).

% check if list is empty
list_empty([], true).
list_empty([_|_], false).
