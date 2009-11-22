:- module(inline,
	  [ inline_expansion/4		% +Goal, +Module, -NewGoal, -Clauses
	  ]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(record)).

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
*/

:- record
	unfold(module:atom,
	       max_depth:integer=5,
	       depth:integer=0,
	       clauses:list=[]).

%%	inline_expansion(+Goal, +Module, -NewGoal, -Clauses) is det.
%
%	NewGoal is an optimised version of   Goal  that depends, besides
%	the original program, on the program defined by Clauses.

inline_expansion(Goal, Module, NewGoal, Clauses) :-
	make_unfold([module(Module)], State),
	expand(Goal, NewGoal, State),
	unfold_clauses(State, List0),
	reverse(List0, Clauses).

expand(Goal, Goal, _) :-
	var(Goal), !.
expand(M:Goal0, Goal, State0) :-
	atom(M), !,
	set_module_of_unfold(M, State0, State),
	expand(Goal0, Goal, State).
expand(Call, Goal, _) :-
	expand_call_n(Call, Goal), !.
expand(Goal, Goal, State) :-
	unfold_module(State, M),
	predicate_property(M:Goal, foreign), !.
expand(Goal, Goal, State) :-
	unfold_module(State, M),
	predicate_property(M:Goal, dynamic), !.
expand(Goal, Goal, State) :-
	unfold_module(State, M),
	predicate_property(M:Goal, multifile), !.
expand(Control, NewControl, State) :-
	control(Control), !,
	optimize_control(Control, NewControl, State).
expand(Goal, NewGoal, State) :-
	catch(bagof_or_nil(Body, substitution(Goal, Body, State), Bodies),
	      _, fail), !,
	(   clauses_to_control_structure(Bodies, Goal1)
	->  expand(Goal1, NewGoal, State)
	;   NewGoal = Goal
	).
expand(Goal, Goal, _).

:- meta_predicate
	bagof_or_nil(?, 0, -).

bagof_or_nil(Templ, Goal, List) :-
	bagof(Templ, Goal, List), !.
bagof_or_nil(_, _, []).

%%	substitution(+Goal, -NewGoal, +State) is nondet.
%
%	According to some clause, Goal can   be  substituted by NewGoal.
%	Note that this is different from   clause/2 because we must take
%	care of the head-unification.

substitution(Goal, NewGoal, State) :-
	unfold_module(State, M),
	copy_term(Goal, G2),
	clause(M:G2, Body),
	unifiable(Goal, G2, Unifier),
	head_unification(Unifier, Body, NewGoal).

head_unification([], Body, Body).
head_unification([X=Y|HU], Body0, Body) :-
	assertion(var(X)),
	(   var(Y)
	->  X = Y,
	    Body1 = Body0
	;   Body1 = (X=Y,Body0)
	),
	head_unification(HU, Body1, Body).


clauses_to_control_structure(Bodies, Goal) :-
	maplist(split_on_cut, Bodies, SplitBodies),
	splitted_to_goal(SplitBodies, Goal).

%%	optimize_control(+ControlIn, -ControlOut, +State) is det.

optimize_control(Var, Var, _) :-
	var(Var), !.
optimize_control((True,G0), G, State) :- % (A,B)
	always_true(True), !,
	optimize_control(G0, State, G).
optimize_control((G0,True), G, State) :-
	always_true(True), !,
	optimize_control(G0, G, State).
optimize_control((A0,B0), (A,B), State) :- !,
	optimize_control(A0, A, State),
	optimize_control(B0, B, State).
optimize_control((True->T0;_), T, State) :-	% if->then;else
	always_true(True), !,
	optimize_control(T0, T, State).
optimize_control((False->_;E0), E, State) :-
	always_false(False), !,
	optimize_control(E0, E, State).
optimize_control((I0->T0;E0), (I->T;E), State) :- !,
	optimize_control(I0, I, State),
	optimize_control(T0, T, State),
	optimize_control(E0, E, State).
optimize_control((False;B0), B, State) :-	% (A;B)
	always_false(False), !,
	optimize_control(B0, B, State).
optimize_control((B0;False), B, State) :-
	always_false(False), !,
	optimize_control(B0, B, State).
optimize_control((A0;B0), (A;B), State) :- !,
	optimize_control(A0, A, State),
	optimize_control(B0, B, State).
optimize_control(\+(G0), G, State) :- !,
	optimize_control(G0, G, State).
optimize_control(Control, Control, _).

always_true(Var) :-
	var(Var), !, fail.
always_true(true).

always_false(Var) :-
	var(Var), !, fail.
always_false(fail).
always_false(false).


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
splitted_to_goal([simple(A)|T], (A;B)) :-
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
split_on_cut(Body, simple(Body)) :-
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
control(\+(_)).
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



