% Kostra grafu
% Funkcionalni a logicke programovani
% Nikola Valesova, xvales02


start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL, S),
		write(S),
		write('\n'),
		write_lines2(S),
		flatten(S, XS),
		vextex_count(XS, [], N),
		write(N),
		write('\n'),
		halt.


vextex_count([], _, 0).
vextex_count([H|T], S, M) :- (not_member(H, S) -> vextex_count(T, [H|S], N), M is N + 1; vextex_count(T, S, N), M is N).


not_member(_, []) :- !.
not_member(X, [Head|Tail]) :-
     X \= Head,
    not_member(X, Tail).


%Reads line from stdin, terminates on LF or EOF.
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

%Tests if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).



split_line([], [[]]) :- !.
split_line([' '|T], S1) :- !, split_line(T, S1).
split_line([H|T], X) :- split_line(T, [G|S1]), flatten([[H|G]|S1], X). % G je prvni seznam ze seznamu seznamu G|S1



% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines([], []).
split_lines([L|Ls], [H|T]) :- split_lines(Ls, T), split_line(L, H).



/* nacte zadany pocet radku */
read_lines2([],0).
read_lines2(Ls,N) :-
	N > 0,
	read_line(L,_),
	N1 is N-1,
	read_lines2(LLs, N1),
	Ls = [L|LLs].


/* vypise seznam radku (kazdy radek samostatne) */
write_lines2([]).
write_lines2([H|T]) :- writeln(H), write_lines2(T). %(writeln je "knihovni funkce")


/* rozdeli radek na podseznamy -- pracuje od konce radku */
%zalozit prvni (tzn. posledni) seznam:
split_line2([],[[]]) :- !.
%pridat novy seznam:
split_line2([' '|T], [[]|S1]) :- !, split_line2(T,S1).
%pridat novy seznam, uchovat oddelujici znak:
split_line2([H|T], [[],[H]|S1]) :- (H=','; H=')'; H='('), !, split_line2(T,S1).
%pridat znak do existujiciho seznamu:
split_line2([H|T], [[H|G]|S1]) :- split_line2(T,[G|S1]).


/* pro vsechny radky vstupu udela split_line2 */
% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines2([],[]).
split_lines2([L|Ls],[H|T]) :- split_lines2(Ls,T), split_line2(L,H).

