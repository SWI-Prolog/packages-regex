/* The lookahead rule is copied pretty directly from O'Keefe, Craft
 * of Prolog, chapter on DCGs.  If you don't understand this rule,
 * either accept it as magic or read (carefully), the grammar for
 * DCGs provided by O'Keefe and section 9.6 of Clocksin and Mellish.
 * (Briefly, if the NT on the LHS is followed by a terminal, that
 * terminal is pushed onto the input list after the rule has consumed
 * whatever it wants.  That means the first lookahead rule consumes one
 * token and then pushes it back on the input; the second consumes two
 * and pushes them back.  So far, we don't need a three-token 
 * lookahead.
 */

/* Used by regex.dcg.pl. */

:- module('lookahead.dcg',[
			   lookahead/3,
			   lookahead/4
			  ]).

lookahead(Token),[Token] --> [Token].
lookahead(T1,T2),[T1,T2] --> [T1],[T2].
