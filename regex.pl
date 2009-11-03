:- module(regex,
	  [ re_compile/3,		% +Text, -Regex, +Options
	    re_match//1,		% +Regex//
	    re_match//2,		% +Regex, -Registers//
	    regex//1,			% +Regex//
	    regex//2			% +Regex, -Registers//
	  ]).
:- use_module('regex.dcg').
:- use_module(g_opts).
:- use_module(library(option)).
:- use_module(library(unicode/unicode_data)).

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

@tbd	Optimizations
@tbd	Specialize the general matcher for a given input.
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


		 /*******************************
		 *	     MATCHING		*
		 *******************************/

%%	re_match(+Regex)// is semidet.
%%	re_match(+Regex, Regs)// is semidet.
%
%	Match the Regex against an input list.  re_match//2 returns the
%	location of matched registers.  Each element of the list can be
%	one of
%
%	    * atom(-Atom)
%	    Return the match as an atom
%	    * number(-Number)
%	    Return the match as a number
%	    * codes(Codes, Tail)
%	    Return as a difference list.
%	    * codes(Codes)
%	    Return as a plain list
%	    * Var
%	    No assignment.

re_match(Regex) -->
	re_match(Regex, -, -).
re_match(Regex, Registers) -->
	re_match(Regex, Registers, []).


re_match(or(Branch, Branch), R, RT) -->
	(   re_match(Branch, R, RT)
	->  []
	;   re_match(Branch, R, RT)
	).
re_match(seq(Pieces), R, RT) -->
	pieces(Pieces, R, RT).
re_match(char(Ch), R, R) -->
	[C],
	{ char_code(Ch, C) }.
re_match(charClass(Class), R, R) -->
	char_class(Class).
re_match(regexp(Re), R, RT) -->	% This is a (...) expression
	(   { R == (-) }
	->  re_match(Re)
	;   { R = [H|RT] },
	    (	{ var(H) }
	    ->	re_match(Re)
	    ;	here(Start),
		re_match(Re),
		here(End),
		{ difflist(Start, End, Codes, Tail),
		  unify_register(H, Codes, Tail)
		}
	    )
	).

unify_register(atom(Atom), Codes, []) :-
	atom_codes(Atom, Codes).
unify_register(number(Number), Codes, []) :-
	number_codes(Number, Codes).
unify_register(codes(Codes), Codes, []).
unify_register(codes(Codes, Tail), Codes, Tail).


%%	here(Mark)// is det.
%%	difflist(Start, End, Data, Tail) is det.
%
%	Fetch the registers. To avoid  the   complexity  of  passing the
%	characters around everywhere, we pick up  pointers into the list
%	using here//1 and  extract  the   sublist  later.  An additional
%	advantage of this approach is that   if  there are no registers,
%	there is no copying of data, not   a  reference to the list. The
%	latter ensures unbounded matches in library(pio).

here(L, L, L).

difflist(End, End1, T, T) :-
	same_term(End, End1), !.
difflist([H|T0], End, [H|T], Tail) :-
	difflist(T0, End, T, Tail).

pieces([], R, R) -->
	[].
pieces([H|T], R, RT) -->
	piece(H, R, R1),
	pieces(T, R1, RT).

piece(count(1, 1, Atom), R, RT) --> !,
	re_match(Atom, R, RT).
piece(count(0, 1, Atom), R, RT) --> !,
	(   re_match(Atom, R, RT)
	;   { RT = R }
	).
piece(count(0, unbounded, Atom), R, RT) --> !,
	unbounded(Atom, R, RT).
piece(count(Min, Max, Atom), R, RT) -->
	piece(0, Min, Max, Atom, R, RT).

piece(I, Min, Max, Atom, R, RT) -->
	re_match(Atom, R, R1),
	{ I2 is I + 1 },
	(   { I2 == Max }		% unbounded never matches
	->  { RT = R1 }
	;   piece(I2, Min, Max, Atom, R1, RT)
	).
piece(I, Min, _, _, R, R) -->
	{ I >= Min }.

unbounded(Atom, R, RT) -->
	re_match(Atom, R, R1),
	unbounded(Atom, R1, RT).
unbounded(_, R, R) -->
	[].

char_class(any(Any)) -->
	any(Any).
char_class(none(Any)) -->
	none(Any).
char_class(diff(Pos, Neg)) -->
	diff(Pos, Neg).
char_class(category(Cat)) -->
	category(Cat).
char_class(notcategory(Cat)) -->
	(   category(Cat)
	->  {fail}
	;   [_]
	).
char_class(sce(Char)) -->		% \n, \t, ...
	sce(Char).
char_class(kw(KW)) -->
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

diff(Pos, Neg) -->
	(   char_class(Neg)
	->  { fail }
	;   char_class(Pos)
	).

sce(n) --> !, "\n".
sce(r) --> !, "\n".
sce(t) --> !, "\n".
sce(C) --> [C].

%%	category(+Cat)// is semidet.
%
%	Matches a UNICODE description.
%
%	@tbd	Efficiency
%	@tbd	What categories are defined and how do these relate to
%		the Unicode general categories.

category(block(_BlockName, Start, End)) --> !, % Is<BlockName>
	[C],
	{ between(Start, End, C) }.
category(Cat) -->
	[C],
	{ unicode_property(C, general_category(Cat)) }.


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
		 *	    DCG EMBEDDING	*
		 *******************************/

%%	regex(+Pattern)// is semidet.
%%	regex(+Pattern, ?Registers)// is semidet.
%
%	Match the given regular expression at the current location.
%	This goal is normally expanded into a call to re_match//2
%	at compile-time.

regex(Pattern) -->
	{ re_compile(Pattern, Regex, []) },
	re_match(Regex).
regex(Pattern, Registers) -->
	{ re_compile(Pattern, Regex, []) },
	re_match(Regex, Registers).

user:goal_expansion(regex(Pattern, In, Out),
		    re_match(Regex, In, Out)) :-
	atom(Pattern),
	prolog_load_context(module, M),
	predicate_property(M:re_match(_,_,_),
			   imported_from(regex)),
	re_compile(Pattern, Regex, []).
user:goal_expansion(regex(Pattern, Registers, In, Out),
		    re_match(Regex, Registers, In, Out)) :-
	atom(Pattern),
	prolog_load_context(module, M),
	predicate_property(M:re_match(_,_,_,_),
			   imported_from(regex)),
	re_compile(Pattern, Regex, []).



		 /*******************************
		 *		UTIL		*
		 *******************************/

to_chars(Atom, Chars) :-
	atom(Atom), !,
	atom_chars(Atom, Chars).
