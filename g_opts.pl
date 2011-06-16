/* g_opts.pl:  grammar options for regex parser(s).
 */

/* Copyright (c) 2008 World Wide Web Consortium, 
 * (Massachusetts Institute of Technology, European Research 
 * Consortium for Informatics and Mathematics, Keio University). 
 */

/* This file is part of Xerophily, a parser for XSD regular expressions.
 *
 * Xerophily is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser Public License for more details.
 *
 * You should have received a copy of the GNU Lesser Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

/* Revisions:
 * 2008-03-26 : CMSMcQ : made file to handle option setting and reading.
 *                       The idiom is based on O'Keefe, Craft of Prolog, p 20
 */
 :- module(g_opts,
	   [ 
	     grammar/1,
	     default_grammar/1,
	     default_grammar/2,
	     get_grammars/1,
	     grammar_option/2,
	     get_grammar_options/2,
	     get_grammar_options/3
	   ]).

grammar(G) :- get_grammar_options(G,_).
/*
grammar('1E').
grammar('PER').
grammar('2E').
grammar('D6').
grammar('LC1').
grammar('D8').
grammar('W').
*/

get_grammars(Gs) :-
	findall(G,grammar(G),Gs).

default_grammar('W2'). /* W2 adopted 30 May 2008 */
/* default_grammar('D8'). */
default_grammar(G,Opts) :-
	default_grammar(G),
	get_grammar_options(G,Opts).

/* g_opts(VBar,Braces,WCEsc,CCSub,CGPart,CharRange,SERange,CharRef,
	  XMLChar,XMLCharIncDash,SCharNoEsc,Blocks,CaretHack,XGHacks)
*/
/* defaults are fixed, no need to expose them. */
defaults(g_opts(meta, % VBar = {meta, normal}
		meta, % Braces = {meta, normal}
		cc,   % WCEsc = {cc, nocc}
		defined,   % CCSub = {defined, undefined}
		undefined, % CGPart,
		'2E',      % CharRange = {1E,PER,2E,W,W2}
		defined,   % SERange,
		undefined, % CharRef,
		defined,   % XMLChar,
		defined,   % XMLCharIncDash,
		undefined, % SCharNoEsc,
		'D6',      % Blocks = {'1E', '2E', 'D6'}
		yes,       % CaretHack = {yes, no}
		yes,       % Extra-grammatical hacks = {yes, no}
		no         % cgp_has_cce (CharGrpPart includes CClassExpr) 
	       )).

/* set_grammar_options(Overrides, Result):  
 * bind Result to default options, as overridden by Overrides.
 * Does this need to be public?  Probably only if you expect
 * run-time selection of options, so probably never.
 * Use get_grammar_options for known values of the grammar, 
 * instead.
 */

set_grammar_options(Overrides,Result) :-
	defaults(Opts),
	set_options(Overrides,Opts,Result).
set_options([],Opts,Opts).
set_options([O|Os],Opts0,Opts) :-
	set_option(O,Opts0,Opts1),
	set_options(Os,Opts1,Opts).

set_option(vbar(A),
	   g_opts(_,B,C,D,E,F,G,H,I,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(braces(B),
	   g_opts(A,_,C,D,E,F,G,H,I,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(wcesc(C),
	   g_opts(A,B,_,D,E,F,G,H,I,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(ccsub(D),
	   g_opts(A,B,C,_,E,F,G,H,I,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(cgpart(E),
	   g_opts(A,B,C,D,_,F,G,H,I,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(charrange(F),
	   g_opts(A,B,C,D,E,_,G,H,I,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(serange(G),
	   g_opts(A,B,C,D,E,F,_,H,I,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(charref(H),
	   g_opts(A,B,C,D,E,F,G,_,I,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(xmlchar(I),
	   g_opts(A,B,C,D,E,F,G,H,_,J,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(xmlcharincdash(J),
	   g_opts(A,B,C,D,E,F,G,H,I,_,K,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(scharnoesc(K),
	   g_opts(A,B,C,D,E,F,G,H,I,J,_,L,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(blocks(L),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,_,M,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(carethack(M),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,_,N,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(xghacks(N),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,_,O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).
set_option(cgp_has_cce(O),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,_),
	   g_opts(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)).



/* grammar_option(optname(Value),Options):
 * select option value from among Options.
 * This is what the grammar will use to check option values.
 */
grammar_option(g_opts(A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O), vbar(A)).
grammar_option(g_opts(_A,B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O), braces(B)).
grammar_option(g_opts(_A,_B,C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O), wcesc(C)).
grammar_option(g_opts(_A,_B,_C,D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O), ccsub(D)).
grammar_option(g_opts(_A,_B,_C,_D,E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O), cgpart(E)).
grammar_option(g_opts(_A,_B,_C,_D,_E,F,_G,_H,_I,_J,_K,_L,_M,_N,_O), charrange(F)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,G,_H,_I,_J,_K,_L,_M,_N,_O), serange(G)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,_G,H,_I,_J,_K,_L,_M,_N,_O), charref(H)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,_G,_H,I,_J,_K,_L,_M,_N,_O), xmlchar(I)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,_G,_H,_I,J,_K,_L,_M,_N,_O), xmlcharincdash(J)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,K,_L,_M,_N,_O), scharnoesc(K)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,L,_M,_N,_O), blocks(L)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,M,_N,_O), carethack(M)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,N,_O), xghacks(N)).
grammar_option(g_opts(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,O), cgp_has_cce(O)).

get_grammar_options('1E',Opts) :-
	set_grammar_options([% vbar(meta), 
			     braces(normal),
			     wcesc(nocc),
			     % ccsub(defined),
			     % cgpart(undefined),
			     charrange('1E'),
			     % serange(defined),
			     charref(defined),
			     % xmlchar(defined),
			     % xmlcharincdash(defined),
			     % scharnoesc(undefined),
			     blocks('1E'),
			     % carethack(yes).
			     xghacks(yes)
			    ],Opts).
get_grammar_options('1Epure',Opts) :-
	get_grammar_options('1E',Opts,pure).

get_grammar_options('PER',Opts) :-
	set_grammar_options([% vbar(meta), 
			     braces(normal),
			     % wcesc(cc),
			     % ccsub(defined),
			     % cgpart(undefined),
			     charrange('PER'),
			     % serange(defined),
			     % charref(undefined),
			     % xmlchar(defined),
			     xmlcharincdash(undefined),
			     % scharnoesc(undefined),
			     blocks('2E'),
			     % carethack(yes).
			     xghacks(yes)
			    ],Opts).
get_grammar_options('PERpure',Opts) :-
	get_grammar_options('PER',Opts,pure).
			     
get_grammar_options('2E',Opts) :-
	set_grammar_options([% vbar(meta), 
			     braces(normal),
			     % wcesc(cc),
			     % ccsub(defined),
			     % cgpart(undefined),
			     % charrange('2E'),
			     % serange(defined),
			     % charref(undefined),
			     % xmlchar(defined),
			     % xmlcharincdash(defined),
			     % scharnoesc(undefined),
			     blocks('2E'),
			     % carethack(yes).
			     xghacks(yes)
			    ],Opts).
get_grammar_options('2Epure',Opts) :-
	get_grammar_options('2E',Opts,pure).

		     
get_grammar_options('D4',Opts) :- get_grammar_options('2E',Opts).
get_grammar_options('D5',Opts) :- get_grammar_options('2E',Opts).
get_grammar_options('D6',Opts) :- 
	set_grammar_options([% vbar(meta), 
			     % braces(meta), 
			     % wcesc(cc),
			     % ccsub(defined),
			     % cgpart(undefined),
			     % charrange('2E'),
			     % serange(defined),
			     % charref(undefined),
			     % xmlchar(defined),
			     % xmlcharincdash(defined),
			     % scharnoesc(undefined),
			     % blocks('D6'),
			     % carethack(yes).
			     xghacks(yes)
			    ],Opts).
get_grammar_options('D6pure',Opts) :-
	get_grammar_options('D6',Opts,pure).


get_grammar_options('LC1',Opts) :- 
	set_grammar_options([% vbar(meta), 
			     % braces(meta), 
			     % wcesc(cc),
			     % ccsub(defined),
			     % cgpart(undefined),
			     % charrange('2E'),
			     % serange(defined),
			     % charref(undefined),
			     % xmlchar(defined),
			     % xmlcharincdash(defined),
			     % scharnoesc(undefined),
			     % blocks('D6'),
			     % carethack(yes).
			     xghacks(yes)
			    ],Opts).
get_grammar_options('LC1pure',Opts) :-
	get_grammar_options('LC1',Opts,pure).

get_grammar_options('D8',Opts) :- 
	set_grammar_options([% vbar(meta), 
			     % braces(meta), 
			     % wcesc(cc),
			     % ccsub(defined),
			     % cgpart(undefined),
			     % charrange('2E'),
			     % serange(defined),
			     % charref(undefined),
			     % xmlchar(defined),
			     % xmlcharincdash(defined),
			     % scharnoesc(undefined),
			     % blocks('D6'),
			     % carethack(yes).
			     xghacks(yes)
			    ],Opts).
get_grammar_options('D8pure',Opts) :-
	get_grammar_options('D8',Opts,pure).

get_grammar_options('W',Opts) :- 
	set_grammar_options([% vbar(meta), 
			     % braces(meta), 
			     % wcesc(cc),
			     ccsub(undefined),
			     cgpart(defined),
			     charrange('W'),
			     serange(undefined),
			     % charref(undefined),
			     xmlchar(undefined),
			     xmlcharincdash(undefined),
			     scharnoesc(defined),
			     % blocks('D6'),
			     % carethack(yes).
			     xghacks(yes)
			    ],Opts).
get_grammar_options('Wpure',Opts) :-
	get_grammar_options('W',Opts,pure).
get_grammar_options('W2',Opts) :-
	get_grammar_options('W',Opts0),
	set_options([cgp_has_cce(yes)],Opts0,Opts).
get_grammar_options('W2pure',Opts) :-
	get_grammar_options('W2',Opts,pure).


get_grammar_options(G,Opts,full) :-
	get_grammar_options(G,Opts).
get_grammar_options(G,Opts,pure) :-
	get_grammar_options(G,Opts0),
	set_options([xghacks(no)],Opts0,Opts).
get_grammar_options(G,Opts,nohacks) :-
	get_grammar_options(G,Opts,pure).
get_grammar_options(G,Opts,impure) :-
	get_grammar_options(G,Opts,full).









