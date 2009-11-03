:- module(regex,
	  [ re_compile/3,		% +Text, -Regex, +Options
	    re_match/3			% +Regex, +Text, -Registers
	  ]).
:- use_module('regex.dcg').
:- use_module(g_opts).

/** <module> Convert regular expressions into DCGs

Regular expressions are a  powerful  and   well  understood  method  for
parsing text. Prolog DCGs form a  well understood mechanism for defining
parsers.  DCGs  are  more  verbose  and    more  powerful  than  regular
expressions, so why would we  want   support  for regular expressions in
Prolog?  We see two reasons:

    * We must implement a system where processing regexes is part
    of the application

    * Including regexes can greatly reduce the size of simple grammars
    and make them more accessible for non-Prolog experts.

This  module  implements  two  interfaces  to    suit   these  needs:  a
conventional regex interface to compile a  regex   and  match it on some
text and a goal-expanded non-terminal  that   allows  for  including the
power of regular expressions directly in DCGs.
*/

		 /*******************************
		 *	   COMPILATION		*
		 *******************************/

%%	re_compile(+Text, -Regex, +Options) is det.
%
%	Compile a regular  expression  into   a  Regex  object.  Defined
%	Options are:
%
%	    * syntax(+Syntax)

re_compile(Text, Regex, Options) :-
	default_grammar(Def),
	option(syntax(Syntax), Options, Def),
	default_grammar(Syntax, Grammar),
	to_chars(Text, Chars),
	phrase(regExp(Grammar, Regex), Chars).

%%	re_match(+Regex)//

re_match(or(Branch, Branch)) -->
	(   re_match(Branch)
	->  []
	;   re_match(Branch)
	).
re_match(seq(Pieces)) -->
	pieces(Pieces).
re_match(char(Ch)) -->
	[Ch].
re_match(charclass(Class)) -->
	charclass(Class).
re_match(regex(Re)) -->			% This is a (...) expression
	re_match(Re).

pieces([]) -->
	[].
pieces([H|T]) -->
	piece(H),
	pieces(T).

piece(count(1, 1, Atom)) --> !,
	re_match(Atom).
piece(count(0, 1, Atom)) --> !,
	(   re_match(Atom)
	;   []
	).
piece(count(0, unbounded, Atom)) --> !,
	unbounded(Atom).
piece(count(Min, Max, Atom)) -->
	piece(0, Min, Max, Atom).

piece(I, Min, Max, Atom) -->
	re_match(Atom),
	{ I2 is I + 1 },
	(   { I2 == Max }		% unbounded never matches
	->  []
	;   piece(I2, Min, Max, Atom)
	).
piece(I, Min, _, _) -->
	{ I >= Min }.

unbounded(Atom) -->
	re_match(Atom),
	unbounded(Atom).
unbounded(_) -->
	[].

charclass(any(Any)) -->
	any(Any).
charclass(none(Any)) -->
	none(Any).
charclass(sce(Char)) -->		% \n, \t, ...
	sce(Char).
charclass(kw(KW)) -->
	kw(KW).

any([H|_]) -->
	chargroup(H), !.
any([_|T]) -->
	any(T).

none(CharGroups) -->
	any(CharGroups), !,
	{ fail }.
none(_) -->
	[_].


chargroup(range(Low, High)) -->
	  [C],
	  { char_code(Low, LC),
	    char_code(High, HC),
	    between(LC, HC, C)
	  }.


sce(n) --> !, "\n".
sce(r) --> !, "\n".
sce(t) --> !, "\n".
sce(C) --> [C].

kw(anycharacter_but_nl) -->
	[C],
	{ C \== 0'\n }.
kw(whitespace) -->
	[C],
	{ code_type(C, white) }.
kw(namechar) -->
	[C],
	{ code_type(C, csym) }.
kw(alldecimaldigits) -->
	[C],
	{ code_type(C, digit) }.
kw(nonpuncsepother) -->
	[C],
	{ \+ code_type(C, punct) }.
kw(not(KW)) -->
	(   kw(KW)
	->  { fail }
	;   [_]
	).


		 /*******************************
		 *		UTIL		*
		 *******************************/

to_chars(Atom, Chars) :-
	atom(Atom), !,
	atom_chars(Atom, Chars).
