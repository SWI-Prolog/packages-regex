/* DCG for XML Schema 1.0 regular expressions.
 * Parse the expression and return a structure.
 * 
 * Revisions:
 * 2008-05-23 : CMSMcQ : add options to ignore ad hoc / extragrammatical rules
 *			 in each grammar (to support checking statements like
 *                       "Expression E is not legal against the grammar, but
 *                       is allowed by ..." or "E is ambiguous against the
 *			 grammar taken by itself, but ..."
 * 2008-03-28 : CMSMcQ : finish working through grammar adding options 
 *                       and cleaning.
 * 2008-03-27 : CMSMcQ : add options argument, to allow this grammar to parse
 *                       according to various versions of the XSD regex grammar.
 * 2004-10-17 : CMSMcQ : make list of structures returned (to compare with
 *                       what materials in ../fsa are using), add some
 *                       sample parse trees
 * 2004-02-24 : CMSMcQ : don't know
 * 2004-02-06 : CMSMcQ : re-sequence quantifier rules to make x{5} parse
 *                       as x with quantifier before four-char string.
 *                     * Correct error in production [10]:
 *                       treat {} as metacharacters, not normal characters.
 *                       (add include_fix predicate to allow switching back
 *                       and forth).
 *                     * Add flags to positive character groups (and their
 *                       descendants) to show whether a literal caret is
 *                       allowed to appear.
 *                     * make single-character escapes return sce(Char), 
 *                       not just Char.
 *                     ? I believe this is now parsing and producing good
 *                       ASTs for all test cases. 
 * 2004-02-05 : CMSMcQ : cosmetic changes to comments, to make
 *                       it easier to see dead code.
 *                     * Remove most uses of char_type(Char,_Type)
 *                       in favor of is_singlechar(Char),
 *                       and add definition of that.
 *                     * change 'atom' to 're_atom' to avoid name clash.
 *                       (You should learn about modules someday ...)
 * 2003-10-17 : CMSMcQ : recast several rules to reduce the
 *                       rework necessary.  Previous version is regex.dcg.
 *                       PUZZLE: why does this grammar find ten ways to
 *                       recognize 'x' as a regular expression?  That
 *                       seems a rather steep cost in ambiguity.  (Even
 *                       though all ten return the same structure.)
 *                       Possibly do a DCTG version to find out?
 * 2003-07-10 : CMSMcQ : made file, as a way to relax for an hour
 *                       or so.
 */


/* The structure returned by this parser is:
 *
 * regex  = or(Branch1,Branch2)
 *        = Branch
 * branch = seq([Piece | Pieces]
 * piece  = count(Min,Max,Atom)
 * atom   = char(Ch)
 *        = charClass(Cl)
 *        = charClass(any(Cl))
 *        = charClass(none(Chargroup))
 *        = charClass(diff(any(Cl),Charclass))
 *        = charClass(diff(none(Cl),Charclass))
 *        = charClass(sce(SEC))           // single-character escape (backslash)
 *                                        // \n r t \ | . ? * + ( ) { } - [ ] ^
 *        = charClass(category(CE))       // \p{L} \p{Lu} etc
 *                                        // \p{IsBasicLatin} etc
 *        = charClass(notcategory(CE))    // \P{L} etc
 *        = charClass(kw(KW))             // multi-char escapes: \s \i etc
 *        = charClass(kw(not(KW)))        // multi-char escapes, neg: \S \I etc
 *        = regexp(RE)
 *
 */

/* Import grammar_option predicate from g_opts module, for checking option values.
 * The internals of the options need not concern us here.
 */
:- use_module(g_opts,[grammar_option/2]).
:- use_module(guards).
:- use_module('lookahead.dcg.pl').

/* "A regular expression is composed from zero or more branches,
 * separated by | characters."
 *
 * [1] regExp ::= branch ( '|' branch )* 
 */
/* (Actually, the grammar requires at least one branch; the prose is wrong,
 * misguided perhaps by the fact that the branch can be empty. 
 */

/* regExp(+Opts,-Expr) takes two arguments:
 *   Opts includes the grammar options used in some rules to choose
 *        different right-hand sides.
 *   Expr is an abstract syntax tree or expression structure, suitable
 *        for semantic checking (see ast.pl) and interpretation (it can
 *        be used to recognize strings that match the regex).
 */

regExp(Opts,Expr) --> branch(Opts,Left), altbranches(Opts,Left,Expr).

/* altbranches == ('| branch)*
 * The left-hand argument of the or-bar is passed as an argument, to
 * allow the top-level AST to be constructed appropriately.  I assume
 * I learned the idiom from O'Keefe, Craft of Prolog, but I can't find the
 * passage now.  Possibly I got it elsewhere.
 * 
 * altbranches(+Opts,+Left,-Expr) takes three arguments:
 *   Opts is the grammatical options, as always.
 *   Left is the left-hand operand of the or-bar, produced by 
 *        branch(Opts,Left) in the rule for regExp, and passed
 *        in as a bound argument to altbranches.
 *   Expr is the final AST for the expression in which this
 *        particular or-bar is the operator.
 *
 */

/* If there is no right-hand branch, let Expr be the same as Left. */

altbranches(_Opts,Left,Left) --> [].

/* If any Prolog novices are reading this, you'll need to know that _Opts
 * is a way of signaling that the first argument to altbranches is not
 * actually used here, so we don't care what it's bound to.  The leading
 * _ serves both as a reassurance to the reader (no, not a typo) and a 
 * signal to the interpreter (don't issue a warning message about unused
 * variables).
 */


/* If there is a right-hand branch, OR the Left argument and the
 * next branch together, and pass it as an argument to altbranches.
 */

altbranches(Opts,Left,Expr) --> 
	['|'], branch(Opts,Right), altbranches(Opts,or(Left,Right),Expr).

/* N.B. the rule given makes '|' left-associative.  
 * We could make it right-associative by writing  
 * 
 *   altbranches(Opts,Left,or(Left,Right)) --> 
 *        ['|'], branch(Opts,Middle), altbranches(Opts,Middle, Right).
 *
 */


/* [Definition:] A branch consists of zero or more pieces,
 * concatenated together.
 */
/* [2] branch ::= piece* */
/* We represent a branch as a Prolog list of pieces.  Empty list
 * for the empty branch.
 */

branch(_Opts,seq([])) --> [].
branch(Opts,seq([P|T])) --> piece(Opts,P), branch(Opts,seq(T)).
 

/* [Definition:] A piece is an atom, possibly followed by a
 * quantifier.
 *
 * [3] piece ::= atom quantifier?  
 */

piece(Opts,QofA) --> re_atom(Opts,A), quantifier(Opts,count(Min,Max)),
                     { QofA =.. [count, Min, Max, A] }.

/* In the AST, the quantifier is not optional:  we wrap ALL atoms in 
 * a count structure.  The optionality of quantifier is expressed
 * by allowing the non-terminal to match the empty string (which 
 * returns count(1,1)).
 */


/* [Definition:] A quantifier is one of ?, *, +, {n,m} or {n,}, which
 * have the meanings defined in the table above.
 *
 * [4] quantifier ::= [?*+] | ( '{' quantity '}' ) 
 */

quantifier(_Opts,count(0,1)) --> ['?'].
quantifier(_Opts,count(1,unbounded)) --> ['+'].
quantifier(_Opts,count(0,unbounded)) --> ['*'].
quantifier(Opts,Q) --> ['{'], quantity(Opts,Q), ['}'].
quantifier(_Opts,count(1,1)) --> [].

/* [5] quantity ::= quantRange | quantMin | QuantExact */
/* [6] quantRange ::= QuantExact ',' QuantExact */
/* [7] quantMin ::= QuantExact ',' */
/* [8] QuantExact ::= [0-9]+ */

/* The literal translation of this into DCG would be something like this:
 *
 *   quantity(Q) --> quantRange(Q). 
 *   quantity(count(Min,unbounded)) --> quantMin(Min). 
 *   quantity(count(N,N)) --> quantExact(N). 
 *   quantRange(count(Min,Max)) --> quantExact(Min), comma, quantExact(Max). 
 *   comma --> [',']. 
 *   quantMin(N) --> quantExact(N), comma. 
 *
 */
/* Less lookahead is required (and less rework, which is useful when
 * debugging the grammar in a trace utility) if we reformulate it as 
 * shown below.  Quantifiers of the form {n,m} catch the first rule
 * for maxspec, {n,} catches the second rule (which binds Max
 * to 'unbounded'), and {n} catches the third maxspec rule (which
 * binds the min and max to the same value, read by the quantExact
 * in the right-hand side of the rule for quantity).
 */

quantity(Opts,count(Min,Max)) --> quantExact(Opts,Min), maxspec(Opts,Min,Max).
maxspec(Opts,_Min,Max) --> [','], quantExact(Opts,Max).
maxspec(_Opts,_Min,unbounded) --> [','].
maxspec(_Opts,Minmax,Minmax) --> [].

/* quantExact requires at least one decimal digit */
quantExact(Opts,N) --> digit(Opts,D), digits(Opts,Lchar), 
                       { number_chars(N,[D|Lchar]) }.

/* digits == zero or more decimal digits == [0-9]* */
digits(Opts,[H|T]) --> digit(Opts,H), digits(Opts,T).
digits(_Opts,[]) --> [].
digit(_Opts,D) --> [D], { char_type(D,digit) }.


/* [Definition:] An atom is either a normal character, a character
 * class, or a parenthesized regular expression.
 * 
 * [9] atom ::= Char | charClass | ( '(' regExp ')' ) 
 */
/* The term 'atom' has other meanings in Prolog, so we rename the
 * non-terminal re_atom (regular-expression atom).
 */
re_atom(Opts,char(Ch)) --> char(Opts,Ch).
re_atom(Opts,charClass(Cl)) --> charClass(Opts,Cl).
re_atom(Opts,regexp(RE)) --> ['('], regExp(Opts,RE), [')'].

/* ? why did I bother to wrap this in regexp()?  Excess of caution,
 * perhaps.  It would be simpler all around just to write 
 *
 *   re_atom(RE) --> lpar, regExp(RE), rpar.
 *
 * But wrapping it stays a bit closer to the syntax, which 
 * is useful for my present purposes. 
 */


/* [Definition:] A metacharacter is either ., \, ?, *, +, {, } (, ), |, 
 * [, or ]. These characters have special meanings in regular
 * expressions, but can be escaped to form atoms that denote the sets
 * of strings containing only themselves, i.e., an escaped
 * metacharacter behaves like a normal character.
 */

/* N.B. XSD 1.0 omitted | from this list, as did drafts D4 and D5 
 * of XSD 1.1.  D6 and later include it.  The grammar rules have 
 * always had it, so it's unlikely that any parsers treat | as a 
 * normal character, but there's a grammar option for it, just in
 * case.
 */

/* [Definition:] A normal character is any XML character that is not a
 * metacharacter. In regular expressions, a normal character is an
 * atom that denotes the singleton set of strings containing only
 * itself.
 *
 * 1.0 1E, PER, 2E, D4, and D5 have:
 *
 *     [10] Char ::= [^.\?*+()|#x5B#x5D] 
 *
 * Later versions (D6, LC, D8, W) have:
 *
 *     [10] Char ::= [^.\?*+{}()|#x5B#x5D] 
 */

/* For this grammar, characters are represented as Prolog atoms.
 * (Some experts would prefer that they be represented as integers,
 * but I wrote the first version of the grammar a long time ago.)
 */

char(Opts,Atom) --> [Atom], 
  { 
    is_singlechar(Atom),   % if this fails, infrastructure has failed

    not(member(Atom, ['.', '\\', '?', '*', '+', '(', ')', '[', ']'])),

    ( grammar_option(Opts,vbar(meta))
    -> not(Atom = '|')
    ;  true
    ),

    ( grammar_option(Opts,braces(meta))
    -> not(member(Atom, [ '{', '}' ]))
    ;  true
    )
  }.

/* Note that a normal character can be represented either as itself,
 * or with a character reference.
 */

/* F.1 Character Classes */

/* [Definition:] A character class is an atom R that identifies a set
 * of characters C(R). The set of strings L(R) denoted by a character
 * class R contains one single-character string "c" for each character
 * c in C(R).
 *
 * 1.0 1E has:
 * 
 *     [11] charClass ::= charClassEsc | charClassExpr
 * 
 * All other forms of the grammar have:
 *
 * [11] charClass ::= charClassEsc | charClassExpr | WildcardEsc
 *
 * We therefore guard the third rule for charClass by checking 
 * the option wcesc (the value 'cc' means:  the non-terminal
 * WildcardEsc is defined and is a character class).
 */

charClass(Opts,Cl) --> charClassEsc(Opts,Cl).
charClass(Opts,Cl) --> charClassExpr(Opts,Cl).
charClass(Opts,Cl) --> { grammar_option(Opts,wcesc(cc)) }, 
                       wildcardEsc(Opts,Cl).
 
/* A character class is either a character class escape or a character
 * class expression or a wildcard escape.
 */
/* [Definition:] A character class expression is a character group
 * surrounded by [ and ] characters. For all character groups G, [G]
 * is a valid character class expression, identifying the set of
 * characters C([G]) = C(G).
 *
 * [12] charClassExpr ::= '[' charGroup ']' 
 */
charClassExpr(Opts,Cl) --> ['['], charGroup(Opts,Cl), [']'].

/* [Definition:] A character group is either a positive character
 * group, a negative character group, or a character class
 * subtraction.
 *
 * Grammars 1E through D8 have:
 *
 *     [13] charGroup ::= posCharGroup | negCharGroup | charClassSub 
 * 
 * W has:
 * 
 *     [76] charGroup ::= (posCharGroup | negCharGroup) ('-' charClassExpr)?
 *
 * Grammar option ccsub(X) controls this variation.
 */
charGroup(Opts,any(Cl)) --> 
	{ grammar_option(Opts,ccsub(defined)) },
        posCharGroup(Opts,nolc,Cl).
charGroup(Opts,Cl) --> 
	{ grammar_option(Opts,ccsub(defined)) },
	negCharGroup(Opts,Cl).
charGroup(Opts,Cl) --> 
	{ grammar_option(Opts,ccsub(defined)) },
	charClassSub(Opts,Cl).

charGroup(Opts,AST) --> 
	{ grammar_option(Opts,ccsub(undefined)) },
	minuend(Opts,Minuend),
	opt_subtrahend(Opts,Minuend,AST).
minuend(Opts,any(Cl)) --> posCharGroup(Opts,nolc,Cl).
minuend(Opts,Cl)      --> negCharGroup(Opts,Cl).
opt_subtrahend(Opts,Minuend,diff(Minuend,Subtrahend)) -->
	ccsub_minus, charClassExpr(Opts,Subtrahend).
opt_subtrahend(_Opts,E,E) --> [].
ccsub_minus --> ['-'], lookahead('[').


/* [Definition:] A positive character group consists of one or more
 * character ranges or character class escapes, concatenated
 * together. A positive character group identifies the set of
 * characters containing all of the characters in all of the sets
 * identified by its constituent ranges or escapes.
 *
 * Most forms of the grammar have:
 *
 *     [14] posCharGroup ::= ( charRange | charClassEsc )+ 
 * 
 * W has:
 * 
 *     [77] posCharGroup ::= (charGroupPart)+
 *
 * The grammar option is cgpart(defined|undefined).
 */
/* N.B. "The ^ character is only valid at the beginning of a positive 
 * character group if it is part of a negative character group".
 *
 * So we pass a parameter on the call to posCharGroup, to act as a flag: 
 *   - 'nolc' means no leading caret is allowed.
 *   - 'oklc' means leading caret is allowed.
 * We could check this in the AST, instead of here.  But for most
 * purposes, it's more convenient to impose that rule here instead
 * of generating multiple parse trees and then winnowing them.
 *
 * Set grammar option carethack(no) to override this check.
 */

/* posCharGroup with cgpart(undefined):
 * This is a little awkward, but in order to stay close to the 
 * source grammar we will treat charRange and charClassEsc
 * separately. 
 */
posCharGroup(Opts,CaretFlag,[Range|More]) -->
	{ grammar_option(Opts,cgpart(undefined)) },
        charRange(Opts,CaretFlag,Range),
	crcce(Opts,More),
	{ grammar_option(Opts,xghacks(XGH)),
	  grammar_option(Opts,charrange(CR)),
	  hyphen_ok([Range|More],CR,XGH) 
	  }.
posCharGroup(Opts,_CaretFlag,[CCE|More]) -->
	{ grammar_option(Opts,cgpart(undefined)) },
        charClassEsc(Opts,CCE),
	crcce(Opts,More),
	{ grammar_option(Opts,xghacks(XGH)),
	  grammar_option(Opts,charrange(CR)),
	  hyphen_ok([CCE|More],CR,XGH) 
	}.

/* posCharGroup with cgpart(defined) 
 */
posCharGroup(Opts,CaretFlag,[Part|Parts]) -->
	{ grammar_option(Opts,cgpart(defined)) },
        charGroupPart(Opts,CaretFlag,Part),
	cgp_star(Opts,Parts),
	{ grammar_option(Opts,charrange(CR)),
	  grammar_option(Opts,xghacks(XGH)),
	  hyphen_ok([Part|Parts],CR,XGH) }.


/* crcce == (charRange | charClassEsc)* 
 * (Only for cgpart(undefined).
 * Once we are here, ^ is ok as a character range, so we
 * pass the argument 'oklc' to charRange.
 */
crcce(_Opts,[]) --> [].
crcce(Opts,[Range|More]) --> 
        charRange(Opts,oklc,Range),
	crcce(Opts,More).
crcce(Opts,[CCE|More]) --> 
        charClassEsc(Opts,CCE),
	crcce(Opts,More).

/* cgp_star == charGroupPart*
 * Only applies (only reachable) if cgpart(defined).
 */
cgp_star(_Opts,[]) --> [].
cgp_star(Opts,[Part|Parts]) --> 
        charGroupPart(Opts,oklc,Part),
	cgp_star(Opts,Parts).


/* old: keep for a moment
posCharGroup(Opts,CaretFlag,[CGI]) --> 
	charGroupItem(Opts,CaretFlag,CGI).
posCharGroup(Opts,CaretFlag,[CGI|CGIs]) --> 
	charGroupItem(Opts,CaretFlag,CGI), 
	posCharGroup(Opts,oklc,CGIs).
charGroupItem(Opts,CaretFlag,CGI) --> charRange(Opts,CaretFlag,CGI).
charGroupItem(Opts,_CaretFlag,CGI) --> charClassEsc(Opts,CGI).
*/
 

/* [Definition:] A negative character group is a positive character
 * group preceded by the ^ character. For all positive character
 * groups P, ^P is a valid negative character group, and C(^P)
 * contains all XML characters that are not in C(P).
 *
 * [15] negCharGroup ::= '^' posCharGroup 
 */
negCharGroup(Opts,none(CG)) --> ['^'], posCharGroup(Opts,oklc,CG).

 
/* [Definition:] A character class subtraction is a character class
 * expression subtracted from a positive character group or negative
 * character group, using the - character.
 *
 * [16] charClassSub ::= ( posCharGroup | negCharGroup ) '-'
 *                       charClassExpr 
 */
/* Only reachable if ccsub(defined) */
charClassSub(Opts,diff(Base,Subtrahend)) --> 
        { grammar_option(Opts,ccsub(defined)) },
	baseGroup(Opts,Base), 
	['-'], 
	charClassExpr(Opts,Subtrahend).
baseGroup(Opts,any(Cl)) --> posCharGroup(Opts,nolc,Cl).
baseGroup(Opts,Cl) --> negCharGroup(Opts,Cl).


/* [Definition:] A character group part (charGroupPart) is either a
 * single unescaped character (SingleCharNoEsc), a single escaped
 * character (SingleCharEsc), or a character range (charRange).
 *
 * 
 * [79] charGroupPart ::= singleChar | charRange
 * [80] singleChar ::= SingleCharEsc | SingleCharNoEsc 
 *
 * If a charGroupPart starts with a singleChar and this is immediately
 * followed by a hyphen, and if the hyphen is part of the character
 * group (that is, it is not being treated as a substraction operator
 * because it is followed by '['), then the hyphen must be followed by
 * another singleChar, and the sequence (singleChar, hyphen,
 * singleChar) is treated as a charRange. It is an error if either of
 * the two singleChars in a charRange is a SingleCharNoEsc comprising
 * an unescaped hyphen.
 *
 * (This seems to rule out groups like [a-], perhaps intentionally.)
 *
 * More formally, I read that paragraph as saying
 * if (lookahead('-') and not lookahead('-]'), then fail.
 * Reversed to be a condition of success, this becomes the guard
 * ( not lookahead('-') or lookahead('-[') ), or in Prolog as below.
 */
/* Only reachable if cgpart(defined) */

charGroupPart(Opts,CaretFlag,Part) --> 
        { grammar_option(Opts,cgpart(defined)) },
	singleChar(Opts,CaretFlag,Part),

	/* succeed in recognizing a charGroupPart if:
         * lookahead is '-['
         * or lookahead is not '-'
         * or we aren't doing extra-grammatical hacks. 
         */
	( lookahead('-','[')
	| ( lookahead(C), { C \=  '-' } )
	| { grammar_option(Opts,xghacks(no)) }
	).
	/* We can rely on C being there, because even if the
           charGroup ends with the singleChar, there needs to
           be a following ']' 
        */
        /* If we couldn't count on at least one more character
           being there, we'd need to say as a third option
           "or lookahead(C) fails for all C".  The only way I know
           how to do this is with a cut:

                  ( lookahead('-','[')
                  | lookahead(C), { !, C \= '-' }
                  | []
                  )       
          
           The cut in the second branch says, in effect, "if 
           a one-character lookahead succeeded, commit to it; the third
           (empty-string) option is not a choice open to you now."            
         */

charGroupPart(Opts,CaretFlag,Part) --> 
	{ grammar_option(Opts,cgpart(defined)) },
	charRange(Opts,CaretFlag,Part).

charGroupPart(Opts,_CaretFlag,Part) --> 
	{ grammar_option(Opts,cgpart(defined)),
	  grammar_option(Opts,cgp_has_cce(yes))
	},
        charClassEsc(Opts,Part).

singleChar(Opts,_CaretFlag,C) --> singleCharEsc(Opts,C).
singleChar(Opts,CaretFlag,C) --> singleCharNoEsc(Opts,CaretFlag,C).


/* [Definition:] A character range R identifies a set of characters
 * C(R) containing all XML characters with UCS code points in a
 * specified range.
 *
 * Several variants.  charrange('1E') (1.0 1E) has:
 *
 *     [17] charRange ::= seRange | XmlCharRef | XmlCharIncDash  
 *
 * charrange('PER') (1.0 2E PER) has:
 * 
 *     [17] charRange ::= seRange | XmlChar  
 *
 * charrange('2E') (2E, D4-D8) has:
 *
 *     [17] charRange ::= seRange | XmlCharIncDash
 *
 * charRange('W') (draft proposal for bug 1889) has:
 * 
 *     [82] charRange ::= singleChar '-' singleChar
 */  
/* And remember we have the CaretFlag hack to worry about. */

charRange(Opts,CaretFlag,R) --> 
	{ grammar_option(Opts,charrange(CR)),
	  member(CR,['1E','PER','2E'])
        },
	seRange(Opts,CaretFlag,R).
charRange(Opts,_CaretFlag,R) --> 
	{ grammar_option(Opts,charrange('1E')) },
	xmlCharRef(Opts,R).
charRange(Opts,CaretFlag,R) --> 
	{ grammar_option(Opts,charrange(CR)),
	  member(CR,['1E','2E'])
        },
	xmlCharIncDash(Opts,CaretFlag,R),
	/* checking prose restrictions now:
           [, ], and \ are not valid character ranges 
         */
	{ ( grammar_option(Opts,xghacks(yes))
	  ->  not(member(R,[ '\\', '[', ']' ])) 
	  ;   true
	  ) }	  
        .
charRange(Opts,CaretFlag,R) --> 
	{ grammar_option(Opts,charrange('PER')) },
	xmlChar(Opts,CaretFlag,R),
	/* checking prose restrictions now:
           [, ], and \ are not valid character ranges 
         */
	{ ( grammar_option(Opts,xghacks(yes))
	  ->  not(member(R,[ '\\', '[', ']' ])) 
	  ;   true
	  ) } 
	.

/* The charRange rule for W has a prose constraint that says
 * no unescaped hyphens can appear.  Hence the guard at the end.
 * (If the hyphens are escaped, the AST will be 'sce(-)', not
 * '-').
 */
charRange(Opts,CaretFlag,range(Min,Max)) --> 
	{ grammar_option(Opts,charrange('W')) },
	singleChar(Opts,CaretFlag,Min),
	['-'],
	singleChar(Opts,oklc,Max),
	{ ( grammar_option(Opts,xghacks(yes))
	  -> ( Min \= '-', Max \= '-' )
	  ;   true
	  ) }
	.


/* [18] seRange ::= charOrEsc '-' charOrEsc */
/* not reachable if we have grammar option charrange('W'). */

seRange(Opts,CaretFlag,range(Min,Max)) --> 
        { grammar_option(Opts,serange(defined)) },
	charOrEsc(Opts,CaretFlag,Min), 
	['-'], 
	charOrEsc(Opts,oklc,Max),
	/* check prose constraints, see s-e discussion below */
	{ ( grammar_option(Opts,xghacks(yes))
	  ->  ( Min \= '\\',
	        Max \= '\\',
		Max \= '[',
		(  codepoint(Min,CPMin),
		   codepoint(Max,CPMax),
		   CPMax >= CPMin
		)
	      )
	  ;   true
	  )
	}.

/* [19] XmlCharRef ::= ( '&#' [0-9]+ ';' ) 
 *                   | (' &#x' [0-9a-fA-F]+ ';' ) 
 */
/* Only reachable in 1E, the grammar option guard is belt and suspenders */
xmlCharRef(Opts,charref(dec,N)) --> 
        { grammar_option(Opts,xmlcharref(defined)) },
	['&', '#'], 
	digit(Opts,D), 
	digits(Opts,Ds), 
	[';'],
	{ atom_chars(N,[D|Ds]) }.
xmlCharRef(Opts,charref(hex,N)) --> 
        { grammar_option(Opts,xmlcharref(defined)) },
	['&', '#', 'x'], 
	hexdigit(Opts,D), 
	hexdigits(Opts,Ds), 
	[';'],
	{ atom_chars(N,[D|Ds]) }.

/* hexdigits == zero or more hexadecimal digits == [0-9a-fA-F]* */
hexdigits(Opts,[H|T]) --> hexdigit(Opts,H), hexdigits(Opts,T).
hexdigits(_Opts,[]) --> [].
hexdigit(_Opts,D) --> [D], { char_type(D,xdigit(_)) }.


/* [20] charOrEsc ::= XmlChar | SingleCharEsc */
/* not reachable if we have grammar option charrange('W'). */

charOrEsc(Opts,CaretFlag,C) --> 
        { grammar_option(Opts,xmlchar(defined)) },
	xmlChar(Opts,CaretFlag,C).
charOrEsc(Opts,_CaretFlag,C) --> 
        { grammar_option(Opts,xmlchar(defined)) },
	singleCharEsc(Opts,C).

/* [21] XmlChar ::= [^\#x2D#x5B#x5D] 
 * i.e. any character but "\", "-",  "[", or "]".
 * If the carethack is on and the CaretFlag argument is 'nolc',
 * then also exclude '^'.
 */
/* There may be a more clever way of checking that positive 
 * character groups contain carets at the beginning only
 * if the positive group is in a negative group; passing the
 * CaretFlag parameter down from charGroup through posCharGroup,
 * charGroupItem, charRange, seRange, charOrEsc, and xmlChar
 * seems surprisingly cumbersome.  But I understand it, and
 * it works.
 */
xmlChar(Opts,oklc,Atom) --> 
        { grammar_option(Opts,xmlchar(defined)) },
	[Atom], 
	{ is_singlechar(Atom),
	not(member(Atom, ['\\','-','[',']'] )) }.
xmlChar(Opts,nolc,Atom) --> 
        { grammar_option(Opts,xmlchar(defined)) },
	[Atom], 
	{ is_singlechar(Atom),
	  not(member(Atom, ['\\','-','[',']'] )),
	  ( grammar_option(Opts,xghacks(yes)),
	    grammar_option(Opts,carethack(yes))
	  -> Atom \= '^'
	  ;  true
	  )
	}.


/* XmlCharIncDash is present in 1E, 2E, D4-D8, but not in PER or W. */
/* [22] XmlCharIncDash ::= [^\#x5B#x5D] 
 * i.e. any but \ [ ]
 */

xmlCharIncDash(Opts,CaretFlag,Atom) -->
        { grammar_option(Opts,xmlcharincdash(defined)) },
	[Atom], 
	{ is_singlechar(Atom),
	  not(member(Atom, ['\\','[',']'] )),
	  ( CaretFlag = nolc, 
	    grammar_option(Opts,carethack(yes)),
	    grammar_option(Opts,xghacks(yes))
	  -> Atom \= '^'
	  ;  true
	  )
	}.
 	

/* 1E through D8 impose some form or other of the following constraints
 * in prose; W omits all of them.
 *
 * "A single XML character is a character range that identifies the set
 * of characters containing only itself. All XML characters are valid
 * character ranges, except as follows:"
 *
 *   "The [, ], - and \ characters are not valid character ranges;"
 *
 * Imposed in this program by a guard on charRange (not checked for 
 * grammar version W).
 *
 *   "The ^ character is only valid at the beginning of a positive
 *   character group if it is part of a negative character group"
 *
 * Imposed here by the caret hack (CaretFlag argument).
 * W has a similar rule elsewhere, so this is checked pretty much
 * always.
 *
 *   "The - character is a valid character range only at the
 *   beginning or end of a positive character group."
 *
 * Not in PER or W.  Constraint imposed here by call to hyphen_ok
 * with the content of any positive character group.
 *
 *   A character range may also be written in the form s-e,
 *   identifying the set that contains all XML characters with UCS
 *   code points greater than or equal to the code point of s, but not
 *   greater than the code point of e.
 */
/* All versions up to W say:
 * "s-e is a valid character range iff:
 *
 *   "s is a single character escape, or an XML character; 
 *   "s is not \ 
 *   "If s is the first character in a character class expression, 
 *      [??? surely you mean in a positive character group?!]
 *      then s is not ^ 
 *   "e is a single character escape, or an XML character; 
 *   "e is not \ or [; and 
 *   "The code point of e is greater than or equal to the code point of
 *      s."
 */
/* Of these, 1 and 4 are enforced by the grammar,
 * 3 by the carethack flag, and 2, 5, and 6 are enforced 
 * by a guard in the rule for serange.
 */

/* W adds:
 * 
 * [87] SingleCharNoEsc ::= [^\#x5B#x5D] 
 * 
 * A single unescaped character (SingleCharNoEsc) is any character
 * except '[' or ']'. There are special rules, described earlier, that
 * constraint the use of the characters '-' and '^' in order to
 * disambiguate the syntax.
 *
 * Grammar option:  scharnoesc(defined).
 */

singleCharNoEsc(Opts,CaretFlag,Atom) -->
        { grammar_option(Opts,scharnoesc(defined)) },
	[Atom], 
	{ is_singlechar(Atom),
	  not(member(Atom, ['\\', '[',']'])),
	  ( CaretFlag = nolc
	  -> Atom \= '^'
	  ;   true
	  )
	}.

/* N.B. The prose of proposal W does not mention \, so the correct
 * rule may be 	not(member(Atom, [ '[',']' ])) }.
 * Not something to make a grammar option for, though.
 */


/* F.1.1 Character Class Escapes
 *
 * [Definition:] A character class escape is a short sequence of
 * characters that identifies predefined character class. The valid
 * character class escapes are the single character escapes, the
 * multi-character escapes, and the category escapes (including the
 * block escapes).
 *
 * [23] charClassEsc ::= ( SingleCharEsc | MultiCharEsc | catEsc |
 *                         complEsc ) 
 */

charClassEsc(Opts,Cl) --> singleCharEsc(Opts,Cl).
charClassEsc(Opts,Cl) --> multiCharEsc(Opts,Cl).
charClassEsc(Opts,Cl) --> catEsc(Opts,Cl).
charClassEsc(Opts,Cl) --> complEsc(Opts,Cl).


/* [Definition:] A single character escape identifies a set containing
 * a only one character -- usually because that character is difficult
 * or impossible to write directly into a regular expression.
 */
/* [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E] */
/*                             nrt\|.?*+(){} -   [   ]   ^    */
singleCharEsc(Opts,sce(SEC)) --> backslash, singleEscChar(Opts,SEC).
singleEscChar(_Opts,Atom) --> [Atom], 
 { is_singlechar(Atom),
   member(Atom,['n','r','t','\\','|','.','?','*','+',
                '(',')','{','}','-','[',']','^']) }.
backslash --> ['\\'].

/* The valid single character escapes are: Identifying the set of
 * characters C(R) containing:
 *
 *   \n the newline character (#xA) 
 *   \r the return character (#xD) 
 *   \t the tab character (#x9) 
 *   \\ \ 
 *   \| | 
 *   \. . 
 *   \- - 
 *   \^ ^ 
 *   \? ? 
 *   \* * 
 *   \+ + 
 *   \{ { 
 *   \} } 
 *   \( ( 
 *   \) ) 
 *   \[ [ 
 *   \] ] 
 */
/* [Definition:] [Unicode Database] specifies a number of possible
 * values for the "General Category" property and provides mappings
 * from code points to specific character properties. The set
 * containing all characters that have property X, can be identified
 * with a category escape \p{X}. The complement of this set is
 * specified with the category escape \P{X}. ([\P{X}] = [^\p{X}]).
 */
/* [25] catEsc ::= '\p{' charProp '}' */
catEsc(Opts,category(CE)) --> catEscStart, charProp(Opts,CE), ['}'].
catEscStart --> backslash, lowercasep, ['{'].
lowercasep --> ['p'].

/* [26] complEsc ::= '\P{' charProp '}' */
complEsc(Opts,notcategory(CE)) --> complEscStart, charProp(Opts,CE), ['}'].
complEscStart --> backslash, uppercasep, ['{'].
uppercasep --> ['P'].

/* [27] charProp ::= IsCategory | IsBlock */
charProp(Opts,P) --> isCategory(Opts,P).
charProp(Opts,P) --> isBlock(Opts,P).

/* Note: [Unicode Database] is subject to future revision. For
 * example, the mapping from code points to character properties might
 * be updated. All minimally conforming processors must support the
 * character properties defined in the version of [Unicode Database]
 * that is current at the time this specification became a W3C
 * Recommendation. However, implementors are encouraged to support the
 * character properties defined in any future version.
 */
/* The following table specifies the recognized values of the "General
 * Category" property.
 *
 *   Category Property Meaning 
 *   Letters     L  All Letters 
 *               Lu uppercase 
 *               Ll lowercase 
 *               Lt titlecase 
 *               Lm modifier 
 *               Lo other 
 *
 *   Marks       M  All Marks 
 *               Mn nonspacing 
 *               Mc spacing combining 
 *               Me enclosing 
 *
 *   Numbers     N  All Numbers 
 *               Nd decimal digit 
 *               Nl letter 
 *               No other 
 *
 *   Punctuation P  All Punctuation 
 *               Pc connector 
 *               Pd dash 
 *               Ps open 
 *               Pe close 
 *               Pi initial quote (may behave like Ps or Pe 
 *                  depending on usage) 
 *               Pf final quote (may behave like Ps or Pe 
 *                  depending on usage) 
 *               Po other 
 *
 *   Separators  Z  All Separators 
 *               Zs space 
 *               Zl line 
 *               Zp paragraph 
 *
 *   Symbols     S All Symbols 
 *               Sm math 
 *               Sc currency 
 *               Sk modifier 
 *               So other 
 *
 *   Other       C  All Others 
 *               Cc control 
 *               Cf format 
 *               Co private use 
 *               Cn not assigned 
 */

/* [28] IsCategory ::= Letters | Marks | Numbers | Punctuation |
 *                     Separators | Symbols | Others */

isCategory(_Opts,'L')  --> ['L'].  /*  All Letters  */
isCategory(_Opts,'Lu') --> ['L'],['u'].  /* uppercase  */
isCategory(_Opts,'Ll') --> ['L'],['l'].  /* lowercase  */
isCategory(_Opts,'Lt') --> ['L'],['t'].  /* titlecase  */
isCategory(_Opts,'Lm') --> ['L'],['m'].  /* modifier  */
isCategory(_Opts,'Lo') --> ['L'],['o'].  /* other  */

isCategory(_Opts,'M')  --> ['M'].  /*  All Marks  */
isCategory(_Opts,'Mn') --> ['M'],['n'].  /* nonspacing  */
isCategory(_Opts,'Mc') --> ['M'],['c'].  /* spacing combining  */
isCategory(_Opts,'Me') --> ['M'],['e'].  /* enclosing  */

isCategory(_Opts,'N')  --> ['N'].  /*  All Numbers  */
isCategory(_Opts,'Nd') --> ['N'],['d'].  /* decimal digit  */
isCategory(_Opts,'Nl') --> ['N'],['l'].  /* letter  */
isCategory(_Opts,'No') --> ['N'],['o'].  /* other  */

isCategory(_Opts,'P')  --> ['P'].  /*  All Punctuation  */
isCategory(_Opts,'Pc') --> ['P'],['c'].  /* connector  */
isCategory(_Opts,'Pd') --> ['P'],['d'].  /* dash  */
isCategory(_Opts,'Ps') --> ['P'],['s'].  /* open  */
isCategory(_Opts,'Pe') --> ['P'],['e'].  /* close  */
isCategory(_Opts,'Pi') --> ['P'],['i'].  /* initial quote (like Ps or Pe)  */
isCategory(_Opts,'Pf') --> ['P'],['f'].  /* final quote (like Ps or Pe)  */
isCategory(_Opts,'Po') --> ['P'],['o'].  /* other  */

isCategory(_Opts,'Z')  --> ['Z'].  /*  All Separators  */
isCategory(_Opts,'Zs') --> ['Z'],['s'].  /* space  */
isCategory(_Opts,'Zl') --> ['Z'],['l'].  /* line  */
isCategory(_Opts,'Zp') --> ['Z'],['p'].  /* paragraph  */

isCategory(_Opts,'S')  --> ['S'].  /* All Symbols  */
isCategory(_Opts,'Sm') --> ['S'],['m'].  /* math  */
isCategory(_Opts,'Sc') --> ['S'],['c'].  /* currency  */
isCategory(_Opts,'Sk') --> ['S'],['k'].  /* modifier  */
isCategory(_Opts,'So') --> ['S'],['o'].  /* other  */

isCategory(_Opts,'C')  --> ['C'].  /*  All Others  */
isCategory(_Opts,'Cc') --> ['C'],['c'].  /* control  */
isCategory(_Opts,'Cf') --> ['C'],['f'].  /* format  */
isCategory(_Opts,'Co') --> ['C'],['o'].  /* private use  */
isCategory(_Opts,'Cn') --> ['C'],['n'].  /* not assigned  */

/* [29] Letters ::= 'L' [ultmo]?  */
/* [30] Marks ::= 'M' [nce]?  */
/* [31] Numbers ::= 'N' [dlo]?  */
/* [32] Punctuation ::= 'P' [cdseifo]?  */
/* [33] Separators ::= 'Z' [slp]?  */
/* [34] Symbols ::= 'S' [mcko]?  */
/* [35] Others ::= 'C' [cfon]?  */

/* Note: The properties mentioned above exclude the Cs property. The
 * Cs property identifies "surrogate" characters, which do not occur
 * at the level of the "character abstraction" that XML instance
 * documents operate on.
 */
/* [Definition:] [Unicode Database] groups code points into a number
 * of blocks such as Basic Latin (i.e., ASCII), Latin-1 Supplement,
 * Hangul Jamo, CJK Compatibility, etc. The set containing all
 * characters that have block name X (with all white space stripped
 * out), can be identified with a block escape \p{IsX}. The complement
 * of this set is specified with the block escape \P{IsX}. ([\P{IsX}]
 * = [^\p{IsX}]).
 */
/* [36] IsBlock ::= 'Is' [a-zA-Z0-9#x2D]+ */
isBlock(Opts,RB) --> is-prefix, blockName(Opts,Blist),
  { 
     atom_chars(B,Blist),
     grammar_option(Opts,blocks(BlockOption)),
     grammar_option(Opts,xghacks(XGH)),
     block_recognition(B,opts(BlockOption,XGH),RB)
  }.

is-prefix  --> ['I'],['s'].
blockName(Opts,[H|T])  --> blockNameChar(Opts,H), blockName(Opts,T).
blockName(Opts,[H])  --> blockNameChar(Opts,H).
blockNameChar(_Opts,Ch) --> [Ch],
  { char_type(Ch,alnum) }.
  /* alnum matches a-zA-Z0-9 */
blockNameChar(_Opts,'-') --> ['-'].
  /* '-' = x2D */



/* [Definition:] A multi-character escape provides a simple way to
 * identify a commonly used set of characters:
 *
 * 1E has:
 *
 * [37] MultiCharEsc ::= '.' | ('\' [sSiIcCdDwW])  
 *
 * All later versions have:
 *
 * [37] MultiCharEsc ::= '\' [sSiIcCdDwW]  
 */

/* MSM: note that we'll need further work on all of these.
 * Either we compile them down to fundamentals, as suggested by the
 * rule multiCharEscCode(not(or(cr,lf))) --> ['.'], or we
 * make them keywords, as suggested by the rule
 * multiCharEscCode(nameStart) --> ['i']. 
 *
 * Consistency would be useful here.
 */

multiCharEsc(Opts,Cl) --> backslash, multiCharEscCode(Opts,Cl).
multiCharEsc(Opts,kw(anycharacter_but_nl)) --> 
	{ grammar_option(Opts,wcesc(nocc)) },
	['.'].

/* sketch 1: compile down to basics:
 * multiCharEscCode(not(or(cr,lf))) --> ['.'].
 * multiCharEscCode(or(' ',or(tab,or(cr,or(lf))))) --> ['s'].
 * multiCharEscCode(not(or(' ',or(tab,or(cr,or(lf)))))) --> ['S'].
 */

/* sketch 2: just do keywords. */
/* multiCharEscCode(kw(anycharacter-but-nl)) --> ['.']. */
/* this rule is a stray, I think - came in from the table of
 * special meanings.  But . in its special meaning doesn't follow
 * a backslash, so it should not be a multiCharEscCode.
 */

multiCharEscCode(_Opts,kw(whitespace)) --> ['s'].
multiCharEscCode(_Opts,kw(not(whitespace))) --> ['S'].

multiCharEscCode(_Opts,kw(nameStart)) --> ['i']. 
multiCharEscCode(_Opts,kw(not(nameStart))) --> ['I'].
multiCharEscCode(_Opts,kw(namechar)) --> ['c'].
multiCharEscCode(_Opts,kw(not(namechar))) --> ['C'].
multiCharEscCode(_Opts,kw(alldecimaldigits)) --> ['d'].
multiCharEscCode(_Opts,kw(not(alldecimaldigits))) --> ['D'].
multiCharEscCode(_Opts,kw(nonpuncsepother)) --> ['w'].
multiCharEscCode(_Opts,kw(not(nonpuncsepother))) --> ['W'].


/* ? [37a] WildcardEsc ::= '.' ?  */
wildcardEsc(Opts,kw(anycharacter_but_nl)) --> 
	{ grammar_option(Opts,wcesc(cc)) },
	['.'].

/*   Character sequence Equivalent character class 
 *
 *   .  [^\n\r] 
 *   \s [#x20\t\n\r] 
 *   \S [^\s] 
 *   \i the set of initial name characters, 
 *      those matched by Letter | '_' | ':' 
 *   \I [^\i] 
 *   \c the set of name characters, those matched by NameChar 
 *   \C [^\c] 
 *   \d \p{Nd} 
 *   \D [^\d] 
 *   \w [#x0000-#x10FFFF]-[\p{P}\p{Z}\p{C}] 
 *      (all characters except the set of "punctuation", 
 *      "separator" and "other" characters)  
 *   \W [^\w] 
 */
/* Note: The regular expression language defined here does not attempt
 * to provide a general solution to "regular expressions" over UCS
 * character sequences. In particular, it does not easily provide for
 * matching sequences of base characters and combining marks. The
 * language is targeted at support of "Level 1" features as defined in
 * [Unicode Regular Expression Guidelines]. It is hoped that future
 * versions of this specification will provide support for "Level 2"
 * features.
 */

