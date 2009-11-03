/* parseregex.pl:  accept strings or atoms and run regex parser on them.
 */

/* Copyright (c) 2008 World Wide Web Consortium, 
 * (Massachusetts Institute of Technology, European Research 
 * Consortium for Informatics and Mathematics, Keio University). 
 * All Rights Reserved. This work is distributed under the 
 * W3C(TM) Software License [1] in the hope that it will be 
 * useful, but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * [1] http://www.w3.org/Consortium/Legal/2002/copyright-software-20021231
 */

/* Revisions:
 * 2008-03-28 : CMSMcQ : made file 
 */

:- module(parseregex,[
		      regex/2,
		      regex/3,
		      ambig/3,
		      allparses/3,
		      divergent/3
		     ]).

/* experiment:  try suppressing this call.  
:- ensure_loaded('regex.dcg.pl').
*/
:- use_module(g_opts).

/* regex(+String,-AST):  parse string with regex grammar, return AST 
 * Use the default grammar, i.e. 'D8'
 */

regex(S,AST) :-
	default_grammar(G),
	regex(S,G,AST).

/* If we get the grammar, we must still figure out which
 * form the input is given in: SWI string, atom, or
 * old fashioned list-of-integers string
 */ 
regex(S,G,AST) :-
	string(S),
	string_to_atom(S,A),
	atom_chars(A,L),
	regex2(L,G,AST).
regex(A,G,AST) :-
	atom(A),
	atom_chars(A,L),
	regex2(L,G,AST).
regex(S,G,AST) :-
	is_list(S),
	atom_codes(A,S),
	atom_chars(A,L),
	regex2(L,G,AST).


regex2(List,G,parse(G,AST)) :-
	get_grammar_options(G,Opts),
	user:regExp(Opts,AST,List,[]).

/* ambig(S,G,ASTs).  A sentence is ambiguous against a grammar iff 
 * there is more than one parse tree for that string in that grammar.
 */
ambig(S,G,ASTs) :-
	grammar(G),
	findall(AST,regex(S,G,AST), ASTs),
	length(ASTs,L),
	L > 1.

/* S is ambiguous against a list of grammars if it's ambiguous against
 * any of them.  Return a list of grammar + AST-list pairs.
 */
ambig(_S,[],[]).
ambig(S,[G|Gs],As) :-
	ambig(S,Gs,As0),
	(   ambig(S,G,ASTs)
	->  As = [ambiguous(G,ASTs) | As0]
	;   As = As0
	).


/* divergent(S,Gs,As).  S is divergent in a list of grammars if there is
 * more than one parse tree for it in that set of grammars.
 */
divergent(S,Gs,As) :-
	allparses(S,Gs,As),
	length(As,L),
	L > 1.

allparses(S,Gs,As) :-
	allparses(S,Gs,[],As).

allparses(_S,[],As,As).
allparses(S,[G|Gs],As0,As) :-
	findall(AST,regex(S,G,AST),GASTs),
	length(GASTs,L),
	(   L =:= 0
	->  Lgasts = [parse(G,noparse)]
	;   Lgasts = GASTs
	),
	merge_asts(Lgasts,As0,As1),
	allparses(S,Gs,As1,As).

/* merge_asts(New,Old,Merged):  merge a new set of parses into an existing list */
merge_asts([],As,As).
merge_asts([P|Ps],As0,As) :-
	merge_ast(P,As0,As1),
	merge_asts(Ps,As1,As).

/* merge_ast(A,As0,As):  merge a new parse tree into an existing list */
/* If list is empty, there was no match.  Add the new parse, wrapping 
 * its grammar name in a list.
 */
merge_ast(parse(G,A),[],[parses([G],A)]).
/* If parse tree matches head of list, add it to the list of grammars */
merge_ast(parse(G,A),
	  [parses(Gs,A)|As],
	  [parses([G|Gs],A)|As]).
/* If no match, keep looking */
merge_ast(parse(G,A0),
	  [parses(Gs,A1)|As0],
	  [parses(Gs,A1)|As]) :-
	A0 \== A1,
	merge_ast(parse(G,A0),As0,As).




