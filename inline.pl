:- module(inline,
	  [ inline_expansion/3		% +Goal, -NewGoal, -Clauses
	  ]).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Perform inline expansion of goals

The idea behind this module is to   allow for inline expansion of goals,
optionally generating a specialised version of the program by exploiting
partial evaluation.

The development of this  library  was   triggered  by  our  Prolog-based
regular expression library. The ultimate aim is to rewrite many commonly
used constructs into efficient code.

@tbd	How much of this is in the JIT compiler of YAP?
@tbd	Can we turn this into a hot-spot compiler?
		* S_VIRGIN --> S_COUNT, ...
		* S_COUNT triggers compiler after N calls.
@tbd	Carry context module around
*/

%%	inline_expansion(+Goal, -NewGoal, -Clauses) is det.
%
%	NewGoal is an optimised version of   Goal  that depends, besides
%	the original program, on the program defined by Clauses.

inline_expansion(Goal, Goal, []) :-
	var(Goal), !.
inline_expansion(Call, Goal, []) :-
	expand_call_n(Call, Goal), !.
inline_expansion(Goal, Goal, []) :-
	predicate_property(Goal, foreign), !.
inline_expansion(Goal, Goal, []) :-
	predicate_property(Goal, dynamic), !.
inline_expansion(Goal, Goal, []) :-
	predicate_property(Goal, multifile), !.
inline_expansion(Control, NewControl, []) :-
	control(Control), !,
	optimize_control(Control, NewControl).
inline_expansion(Goal, NewGoal, Clauses) :-
	catch(findall(Body, clause(Goal, Body), Bodies), _, true), !,
	(   clauses_to_control_structure(Bodies, Goal1)
	->  inline_expansion(Goal1, NewGoal, Clauses)
	;   NewGoal = Goal,
	    Clauses = []
	).
inline_expansion(Goal, Goal, []).

clauses_to_control_structure(Bodies, Goal) :-
	maplist(split_on_cut, Bodies, SplitBodies),
	splitted_to_goal(SplitBodies, Goal).

%%	splitted_to_goal(+SplittedTerms, -Goal) is det.
%
%	Recreate a goal from the splitting done by split_on_cut/2.

splitted_to_goal([], fail).
splitted_to_goal([cut(B,A)], Goal) :- !,
	(   B == true
	->  Goal = A
	;   Goal = (B -> A)
	).
splitted_to_goal([cut(B,A)|T], (B -> A ; R)) :-
	splitted_to_goal(T, R).
splitted_to_goal([simple(A)], A) :- !.
splitted_to_goal([simple(A),T], (A;B)) :-
	splitted_to_goal(T, B).


%%	split_on_cut(+GoalTerm, -Split) is semidet.
%
%	If GoalTerm is a term a,b,!,c,d *and*   there is no cut in (a,b)
%	nor in (c,d), unify Split with   cut((a,b),  (c,d)). If GoalTerm
%	contains a cut, but the  other   conditions  are  not met, fail.
%	Otherwise  (i.e.  there  are   no    cuts),   unify  Split  with
%	simple(GoalTerm).

split_on_cut(Body, cut(Before, After)) :-
	comma_list(Body, List),
	append(BL, [Cut|AL], List),
	Cut == (!), !,
	no_cut_in(BL),
	no_cut_in(AL),
	comma_list(Before, BL),
	comma_list(After, AL).
split_on_cut(Body, Body) :-
	no_cut_in(Body).

no_cut_in(List) :-
	\+ ( member(L, List),
	     cut_in(L)
	   ).

cut_in(Var) :-
	var(Var), !,
	fail.
cut_in(!).
cut_in((A,B)) :-
	(   cut_in(A)
	;   cut_in(B)
	).
cut_in((A;B)) :-
	(   cut_in(A)
	;   cut_in(B)
	).
cut_in((A->B)) :-
	(   cut_in(A)
	;   cut_in(B)
	).
cut_in((A*->B)) :-
	(   cut_in(A)
	;   cut_in(B)
	).
cut_in(\+(A)) :-
	cut_in(A).

%%	control(@Term) is semidet.
%
%	True if Term is a control-structure

control(Var) :-
	var(Var), !, fail.
control((_,_)).
control((_;_)).
control((_->_)).
control((_*->_)).


		 /*******************************
		 *	      CALL/N		*
		 *******************************/

%%	expand_call_n(+Call, -Goal) is semidet.
%
%	Partial evaluation may have  instantiated   the  arguments  of a
%	meta-call sufficiently such that we can   turn  it into a normal
%	call.

expand_call_n(Call, Goal) :-
	callable(Call),
	functor(Call, call, _),
	arg(1, Call, Template),
	callable(Template), !,
	Call =.. [_,_|Extra],
	Template =.. [Name|First],
	append(First, Extra, Args),
	Goal =.. [Name|Args].


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	comma_list(+Conjunction, -List) is det.
%%	comma_list(-Conjunction, +List) is det.
%
%	Translate between a Prolog  conjunction   and  a  list. Elements
%	=true= are removes from both  translations.   The  empty list is
%	mapped to a single =true=.
%
%	@tbd	Move to library?

comma_list(Conj, List) :-
	is_list(List),
	list_comma(List, Conj).
comma_list(Conj, List) :-
	phrase(comma_list(Conj), List).

comma_list(A)     --> {var(A)}, !, [A].
comma_list((A,B)) --> !, comma_list(A), comma_list(B).
comma_list(true)  --> !, [].
comma_list(A)     --> [A].

list_comma([], true).
list_comma([H|T], C) :-
	(   T == []
	->  C = H
	;   H == true
	->  list_comma(T, C)
	;   C = (H,B),
	    list_comma(T, B)
	).



