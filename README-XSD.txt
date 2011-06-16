Xerophily: XSD Regex parser 
2008-08-08, rev. 2009-12-09

This directory contains Xerophily, a parser for regular expressions,
as they are defined by the XML Schema Definition Language (XSD),
versions 1.0 and 1.1.  (Xerophily is the property possessed by plants
well adapted to growing in dry, especially hot and dry, conditions.
It's also one of the few words I could find containing an 'X', an 'R',
and a 'P' for the keywords 'XSD', 'regex', and 'parser'.)  Xerophily
was prepared by C. M. Sperberg-McQueen, a first version in 2004 or
earlier, and the current version in May/June 2008.

Copyright in the code is held by the World Wide Web Consortium, which
licenses the code both under the W3C license and under the Lesser Gnu
Public License (LGPL).  The code in directory

  http://www.w3.org/XML/2008/03/xsdl-regex/

carries standard W3C license notices. For the LGPL version, see the
files in the subdirectory  

  http://www.w3.org/XML/2008/03/xsdl-regex/LGPL

Apart from the license differences, the two versions should be
identical.

In its current state, the parser has no user interface to speak of;
that may change.  If you are comfortable working with Prolog, have at
it.

This code has been developed and tested with SWI Prolog; most of it
should run with other Prologs, but the routines for reading schema
documents probably will need adjustment.

If SWI Prolog is in the path as swipl, and the code is in the
./xsdl-regex directory, then one simple way to run the parser on all
the patterns in a schema document test.xsd, and produce an annotated
version of the schema document out.xsd, is to issue the following
command from the command line.

  swipl -f ../xsdl-regex/load.pl -g "annotate_xsd('testdoc.xsd')" -t 'halt(13)' > out.xsd

For example, the annotated version of the dummy schema document with
the regexes from the Last Call draft of 20 June 2008 was produced
using

  swipl -f load.pl \\
        -g "annotate_xsd('regexes.20080620.xsd')" \\
        -t 'halt(13)' \\
      > regexes.20080620.annotated.xsd

Good luck.


Contents:

load.pl             loads everything needed.  This is what you should load, if
                    you want to run the parser.

readxsd.pl          Predicates for reading an XSD schema document and
                    annotating all the patterns in it, with parse trees.

                    Exports the following predicates (also some others, but
                    at this point I think the others are all cruft and should 
                    be ignored):
 
                      annotate_xsd(+Filename):  reads Filename, parses all
                        patterns using the default grammar (at the moment, 
                        that's the grammar of the Last Call draft of June 2008),
                        and writes an annotated copy of the schema document
                        to stdout.

                      annotate_xsd(+Filename,+List_of_grammars):  reads Filename,
                        parses patterns using the grammars specified,
                        and writes an annotated copy of the schema document
                        to stdout.
                    
                      annotate_xsd(+Inputfile,+List_of_grammars,+Outputfile): 
                        reads Inputfile, parses patterns using the grammars 
                        specified, and writes an annotated copy of the schema document
                        to Outputfile.

parseregex.pl       Predicates for running the parser on a single string.

		      regex(String,AST):  parses String using the default
                        grammar, binds AST to the corresponding abstract syntax 
                        tree.  The String may be given in several forms:  as 
                        a Prolog atom, as a double-quoted string (list of 
                        character codes), or as an SWI Prolog string.

		      regex(String,Grammar,AST):  parses String against grammar G,
                        binds AST to the corresponding abstract syntax tree.

		      ambig(String,G,ASTs):  succeeds if String is ambiguous
                        against grammar G, binds ASTs to a list of the corresponding 
                        abstract syntax trees.

		      ambig(String,Gs,ASTs):  succeeds if String is ambiguous
                        against any grammar in the list of grammars Gs, binds ASTs to a 
                        list of grammar + AST-list pairs.

		      allparses(String,Grammars,ASTs):  parses String against the
                        grammars in Grammars, returns a list of all parse trees.

		      divergent(String,Gs,ASTs):  succeeds if String has more than
                        on parse in the grammars given in Gs; fails if all grammars
                        in Gs produce the same parse.


regex.dcg.pl        the regex grammar itself, in definite-clause grammar form.
                    Variants are included for 1.0 First Edition, PER, 
                    Second Edition, and various drafts of 1.1.

                    The start symbol is regExp(Options,Expression), where 
                    Options is a set of grammar options which determines
                    precisely which grammar is used; the g_opts module
                    can and should be used to get a useful value for Options.
                    The Expression returned is an abstract syntax tree for
                    the parse.  Various utility routines elsewhere can
                    emit this in an XML form.

g_opts.pl           Utilities for managing grammar options.  The predicates most
                    likely to be useful to casual users are:

                      get_grammars(-Grammars):  unifies Grammars with a list of 
                        all the grammrs known to the system.  If you want to
                        check a schema document against all known grammars, 
                        you can say get_grammars(Gs), annotate_xsd(File,Gs).

                        At the moment, the known grammars are the following.
                        The following have code embedded in the parser to
                        enforce the non-grammatical constraints expressed in the 
                        prose of the spec (disambiguation rules, etc.)

                         1E       the grammar of 1.0 1E (May 2001)
                         PER      the grammar of the Proposed Edited Recommendation
                                  of June 2004
                         2E       the grammar of 1.0 2E (November 2004)
                         D4       the XSD 1.1 draft of July 2004
                         D5       the XSD 1.1 draft of February 2005 
                         D6       the XSD 1.1 draft of January 2006
                         LC1      the XSD 1.1 last-call draft of February 2006
                         D8       the XSD 1.1 status-quo text of early 2008
                         W        a change proposal of early 2008
                         W2       the XSD 1.1 last-call draft of June 2008 

                       There are also 'pure' versions of the grammars, which 
                       do NOT enforce the non-grammatical rules in the prose.
                       These were added to make it easy to check statements like
                       "As given, the grammar is ambiguous, so the prose rule is
                       needed to disambiguate it."  They are unlikely to be useful
                       outside the Working Group.

                         1Epure 
                         PERpure 
                         2Epure 
                         D6pure 
                         LC1pure 
                         D8pure 
                         Wpure 
                         W2pure

                    The other exported predicates are useful when writing predicates
                    to use the parser in various ways:

                      grammar(G):  true if G is an atom naming a grammar.  

                      default_grammar(G): true if G is the default grammar.

                      default_grammar(G, Opts):  true if G is the default grammar
                        and Opts is the set of grammar options for G.

                      grammar_option(Options, Option):  true if Option is among
                        the grammar options given in Option.  For example, if Opts
                        is bound to the options for a given grammar, then 
                        grammar_option(Opts,vbar(V)) will bind V to the appropriate 
                        value of the vbar option.  Used only inside of grammar rules.

                      get_grammar_options(+G,-Opts):  gets the options for the 
                        given grammar.  (Could also run backwards, but why?)

                      get_grammar_options(+G,Opts,pure):  get the options for the
                        pure variant of a grammar.

ast.pl              Utilities for working with the abstract syntax trees
                    returned by the parser.

guards.pl           Routines for checking the ad-hoc restrictions on
                    regexes (i.e. restrictions not written into the grammar).
                    Called from regex.dcg.      

lookahead.dcg.pl    Defines lookahead/2 and lookahead/3, utilities for 
                    handling lookahead in DCGs.  Copied very directly from
                    O'Keefe, Craft of Prolog, so no W3C copyright is claimed.

testgenerator1.pl   Generates test strings using a sort of kludged up random
                    string generator.  Makes maketest.pl obsolete.

                      teststring(N,S):  generates a random string S with N parts.
                        Each part is randomly selected from a list of strings;
                        each string in the list matches some non-terminal in the
                        grammar, or is a terminal specifically mentioned in the
                        grammar.  This makes it easier to get 'interesting' 
                        strings than a random selection of N characters.

                      teststring(N,A,S):  the same, but also returns an atom A,
                        which is convenient for some purposes.

license.pl          Defines the W3C license (for use with the W3C-licensed
                    copy of this code), and calls :- license(w3c).
                    Currently not clear whether every file needs to have
                    license(w3c) or license(lgpl) added.

ast_dot.xsl         Stylesheet for translating from an XML dump of the AST
                    to Graphviz 'dot' notation.  Used to generate at least
                    some of the images in ./images.
                    
show-asts.xsl       Stylesheet to translate abstract syntax trees into
                    HTML using nested lists.

images              A directory with .dot and .png files showing the structure
                    of various expression types. 

maketests.pl        An early attempt at generating test strings.  The file 
                    testgenerator1.pl is later and better.

re.xml              Mind-numbingly detailed summary of all the changes 
                    ever made to the regex grammar in published drafts
                    (and a few WG-internal proposals, too).  Some
                    discussion of test case generation.  Never completely
                    finished; some rough edges remain.  Useful for the 
                    dedicated, perhaps, but only for them.

recognize.pl        Appears to be replaced by parseregex.pl. Or vice
                    versa.
