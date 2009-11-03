/* guards.pl:  routines for checking ad-hoc grammar restrictions
 * on regular expressions; used by regex.dcg.
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
 * 2008-03-27 : CMSMcQ : made file, beginning with cruft moved out of regex.dcg.
 */
:- module(guards,
	  [ 
            is_singlechar/1,
	    hyphen_ok/3,
	    codepoint/2,
	    block_recognition/3
	  ]).

/* Utility: is_singlechar(X).  True if X is an atom representing a
 * single character.  If the grammar didn't assume characters were
 * atoms, we wouldn't need this.
 */
is_singlechar(X) :- atom(X), atom_chars(X,[X]).


/* hyphen_ok(List,GrammarID,XGHacks):  true if extra-grammatical hacks
 * are not being enforced, or if the grammar doesn't have an xg hack
 * for hyphens and ranges, or if the grammar does have an extra-grammatical
 * hack and it is satisfied. */
hyphen_ok(_,_,no).
hyphen_ok(List,GrammarID,yes) :- hyphen_ok(List,GrammarID).

/* hyphen_ok(List,Opts): enforce the rule "The - character is a valid
 * character range only at the beginning or end of a positive
 * character group" if the grammar options require it, otherwise
 * succeed regardless.
 */
hyphen_ok([],_).
hyphen_ok(_,'PER').
hyphen_ok(_,'W').
hyphen_ok([_|T],CROpt) :-
	member(CROpt, ['1E', '2E']),
	/* ignore the list head, if it's a hyphen it's OK, if it's
         * not a hyphen we don't care. 
         */
        hyphen_ok(T).

/* hyphen_ok/1 is called only when we're checking, and only
 * on the tail of the list.  A literal hyphen is OK if and only
 * if we are at the last item in the list.
 */
hyphen_ok([]).
hyphen_ok(['-'|[]]).
hyphen_ok([H|T]) :-
	H \= '-',
	hyphen_ok(T).


/* codepoint(CharOrEsc,CodePoint) : true iff CodePoint is the UCS code point
 * of the character indicated in CharOrEsc.
 */
codepoint(C,CP) :-
	is_singlechar(C),
	atom_codes(C,[CP]).
codepoint(sce(n),10).
codepoint(sce(r),13).
codepoint(sce(t),9).
codepoint(sce(SEC),CP) :-
	is_singlechar(SEC), % if this fails, boss, we in big trouble
	atom_codes(SEC,[CP]).


/* block_recognition(B,RB):  
 * true if R is a recognized block and RB is known(R,Start,End), 
 * or if R is an unrecognized block and RB is unknown(R).
 */
block_recognition(B,opts(_,no),blockname(B)) :- true.

block_recognition(B,opts(BlockOption,_XGH),RB) :-
	( option_year(BlockOption,Date),
	  block(B,Y1,Y2,Start,End),
	  Y1 =< Date,
	  Y2 >= Date
	->  RB = block(B,Start,End)
	;   RB = unknownblock(B)
	).

option_year('1E',2001).
option_year('PER',2004).
option_year('2E',2004).
option_year('D6',2006).

block(                        'BasicLatin', 2001, 2008, '#x0000', '#x007F').  
block(                 'Latin-1Supplement', 2001, 2008, '#x0080', '#x00FF').  
block(                   'LatinExtended-A', 2001, 2008, '#x0100', '#x017F').  
block(                   'LatinExtended-B', 2001, 2008, '#x0180', '#x024F').  
block(                     'IPAExtensions', 2001, 2008, '#x0250', '#x02AF').  
block(            'SpacingModifierLetters', 2001, 2008, '#x02B0', '#x02FF').  
block(         'CombiningDiacriticalMarks', 2001, 2008, '#x0300', '#x036F').  
block(                             'Greek', 2001, 2008, '#x0370', '#x03FF').  
block(                          'Cyrillic', 2001, 2008, '#x0400', '#x04FF').  
block(                          'Armenian', 2001, 2008, '#x0530', '#x058F').  
block(                            'Hebrew', 2001, 2008, '#x0590', '#x05FF').  
block(                            'Arabic', 2001, 2008, '#x0600', '#x06FF').  
block(                            'Syriac', 2001, 2008, '#x0700', '#x074F').  
block(                            'Thaana', 2001, 2008, '#x0780', '#x07BF').  
block(                        'Devanagari', 2001, 2008, '#x0900', '#x097F').  

block(                           'Bengali', 2001, 2008, '#x0980', '#x09FF').  
block(                          'Gurmukhi', 2001, 2008, '#x0A00', '#x0A7F').  
block(                          'Gujarati', 2001, 2008, '#x0A80', '#x0AFF').  
block(                             'Oriya', 2001, 2008, '#x0B00', '#x0B7F').  
block(                             'Tamil', 2001, 2008, '#x0B80', '#x0BFF').  
block(                            'Telugu', 2001, 2008, '#x0C00', '#x0C7F').  
block(                           'Kannada', 2001, 2008, '#x0C80', '#x0CFF').  
block(                         'Malayalam', 2001, 2008, '#x0D00', '#x0D7F').  
block(                           'Sinhala', 2001, 2008, '#x0D80', '#x0DFF').  
block(                              'Thai', 2001, 2008, '#x0E00', '#x0E7F').  
block(                               'Lao', 2001, 2008, '#x0E80', '#x0EFF').  
block(                           'Tibetan', 2001, 2008, '#x0F00', '#x0FFF').  
block(                           'Myanmar', 2001, 2008, '#x1000', '#x109F').  
block(                          'Georgian', 2001, 2008, '#x10A0', '#x10FF').  
block(                        'HangulJamo', 2001, 2008, '#x1100', '#x11FF').  
block(                          'Ethiopic', 2001, 2008, '#x1200', '#x137F').  

block(                          'Cherokee', 2001, 2008, '#x13A0', '#x13FF').  
block('UnifiedCanadianAboriginalSyllabics', 2001, 2008, '#x1400', '#x167F').  
block(                             'Ogham', 2001, 2008, '#x1680', '#x169F').  
block(                             'Runic', 2001, 2008, '#x16A0', '#x16FF').  
block(                             'Khmer', 2001, 2008, '#x1780', '#x17FF').  
block(                         'Mongolian', 2001, 2008, '#x1800', '#x18AF').  
block(           'LatinExtendedAdditional', 2001, 2008, '#x1E00', '#x1EFF').  
block(                     'GreekExtended', 2001, 2008, '#x1F00', '#x1FFF').  
block(                'GeneralPunctuation', 2001, 2008, '#x2000', '#x206F').  
block(         'SuperscriptsandSubscripts', 2001, 2008, '#x2070', '#x209F').  
block(                   'CurrencySymbols', 2001, 2008, '#x20A0', '#x20CF').  
block(          'CombiningMarksforSymbols', 2001, 2008, '#x20D0', '#x20FF').  
block(                 'LetterlikeSymbols', 2001, 2008, '#x2100', '#x214F').  
block(                      'NumberForms',  2001, 2008, '#x2150', '#x218F').  
block(                            'Arrows', 2001, 2008, '#x2190', '#x21FF').  
block(             'MathematicalOperators', 2001, 2008, '#x2200', '#x22FF').  

block(            'MiscellaneousTechnical', 2001, 2008, '#x2300', '#x23FF').  
block(                   'ControlPictures', 2001, 2008, '#x2400', '#x243F').  
block(       'OpticalCharacterRecognition', 2001, 2008, '#x2440', '#x245F').  
block(             'EnclosedAlphanumerics', 2001, 2008, '#x2460', '#x24FF').  
block(                        'BoxDrawing', 2001, 2008, '#x2500', '#x257F').  
block(                     'BlockElements', 2001, 2008, '#x2580', '#x259F').  
block(                   'GeometricShapes', 2001, 2008, '#x25A0', '#x25FF').  
block(              'MiscellaneousSymbols', 2001, 2008, '#x2600', '#x26FF').  
block(                          'Dingbats', 2001, 2008, '#x2700', '#x27BF').  
block(                   'BraillePatterns', 2001, 2008, '#x2800', '#x28FF').  
block(             'CJKRadicalsSupplement', 2001, 2008, '#x2E80', '#x2EFF').  
block(                    'KangxiRadicals', 2001, 2008, '#x2F00', '#x2FDF').  
block(  'IdeographicDescriptionCharacters', 2001, 2008, '#x2FF0', '#x2FFF').  
block(          'CJKSymbolsandPunctuation', 2001, 2008, '#x3000', '#x303F').  
block(                          'Hiragana', 2001, 2008, '#x3040', '#x309F').  
block(                          'Katakana', 2001, 2008, '#x30A0', '#x30FF').  

block(                          'Bopomofo', 2001, 2008, '#x3100', '#x312F').  
block(           'HangulCompatibilityJamo', 2001, 2008, '#x3130', '#x318F').  
block(                            'Kanbun', 2001, 2008, '#x3190', '#x319F').  
block(                  'BopomofoExtended', 2001, 2008, '#x31A0', '#x31BF').  
block(       'EnclosedCJKLettersandMonths', 2001, 2008, '#x3200', '#x32FF').  
block(                  'CJKCompatibility', 2001, 2008, '#x3300', '#x33FF').  
block(    'CJKUnifiedIdeographsExtensionA', 2001, 2008, '#x3400', '#x4DB5').  
block(              'CJKUnifiedIdeographs', 2001, 2008, '#x4E00', '#x9FFF').  
block(                       'YiSyllables', 2001, 2008, '#xA000', '#xA48F').  
block(                        'YiRadicals', 2001, 2008, '#xA490', '#xA4CF').  
block(                   'HangulSyllables', 2001, 2008, '#xAC00', '#xD7A3').  

/* HighSurrogates, LowSurrogates, and HighPrivateUseSurrogates dropped in PER, 2E */
block(                    'HighSurrogates', 2001, 2001, '#xD800', '#xDB7F').  
block(          'HighPrivateUseSurrogates', 2001, 2001, '#xDB80', '#xDBFF').
block(                     'LowSurrogates', 2001, 2001, '#xDC00', '#xDFFF').  
block(                        'PrivateUse', 2001, 2008, '#xE000', '#xF8FF').  
block(        'CJKCompatibilityIdeographs', 2001, 2008, '#xF900', '#xFAFF').  

block(       'AlphabeticPresentationForms', 2001, 2008, '#xFB00', '#xFB4F').  
block(         'ArabicPresentationForms-A', 2001, 2008, '#xFB50', '#xFDFF').  
block(                'CombiningHalfMarks', 2001, 2008, '#xFE20', '#xFE2F').  
block(             'CJKCompatibilityForms', 2001, 2008, '#xFE30', '#xFE4F').  
block(                 'SmallFormVariants', 2001, 2008, '#xFE50', '#xFE6F').  
block(         'ArabicPresentationForms-B', 2001, 2005, '#xFE70', '#xFEFE').  
block(         'ArabicPresentationForms-B', 2006, 2008, '#xFE70', '#xFEFF').  
block(                          'Specials', 2001, 2005, '#xFEFF', '#xFEFF').  
block(        'HalfwidthandFullwidthForms', 2001, 2008, '#xFF00', '#xFFEF').  
block(                          'Specials', 2001, 2005, '#xFFF0', '#xFFFD').  
block(                          'Specials', 2006, 2008, '#xFFF0', '#xFFFF').  


/* Numerous blocks introduced in 2006 */
block(                   'CyrillicSupplement', 2006, 2008,  '#x0500', '#x052F').
block(                     'ArabicSupplement', 2006, 2008,  '#x0750', '#x077F').
block(                   'EthiopicSupplement', 2006, 2008,  '#x1380', '#x139F').
block(                              'Tagalog', 2006, 2008,  '#x1700', '#x171F').
block(                              'Hanunoo', 2006, 2008,  '#x1720', '#x173F').
block(                                'Buhid', 2006, 2008,  '#x1740', '#x175F').
block(                             'Tagbanwa', 2006, 2008,  '#x1760', '#x177F').
block(                                'Limbu', 2006, 2008,  '#x1900', '#x194F').
block(                                'TaiLe', 2006, 2008,  '#x1950', '#x197F').
block(                            'NewTaiLue', 2006, 2008,  '#x1980', '#x19DF').
block(                         'KhmerSymbols', 2006, 2008,  '#x19E0', '#x19FF').
block(                             'Buginese', 2006, 2008,  '#x1A00', '#x1A1F').
block(                   'PhoneticExtensions', 2006, 2008,  '#x1D00', '#x1D7F').
block(         'PhoneticExtensionsSupplement', 2006, 2008,  '#x1D80', '#x1DBF').
block(  'CombiningDiacriticalMarksSupplement', 2006, 2008,  '#x1DC0', '#x1DFF').
block(   'MiscellaneousMathematicalSymbols-A', 2006, 2008,  '#x27C0', '#x27EF').

block(                 'SupplementalArrows-A', 2006, 2008,  '#x27F0', '#x27FF').
block(                 'SupplementalArrows-B', 2006, 2008,  '#x2900', '#x297F').
block(   'MiscellaneousMathematicalSymbols-B', 2006, 2008,  '#x2980', '#x29FF').
block(    'SupplementalMathematicalOperators', 2006, 2008,  '#x2A00', '#x2AFF').
block(        'MiscellaneousSymbolsandArrows', 2006, 2008,  '#x2B00', '#x2BFF').
block(                           'Glagolitic', 2006, 2008,  '#x2C00', '#x2C5F').
block(                               'Coptic', 2006, 2008,  '#x2C80', '#x2CFF').
block(                   'GeorgianSupplement', 2006, 2008,  '#x2D00', '#x2D2F').
block(                             'Tifinagh', 2006, 2008,  '#x2D30', '#x2D7F').
block(                     'EthiopicExtended', 2006, 2008,  '#x2D80', '#x2DDF').
block(              'SupplementalPunctuation', 2006, 2008,  '#x2E00', '#x2E7F').
block(                           'CJKStrokes', 2006, 2008,  '#x31C0', '#x31EF').
block(           'KatakanaPhoneticExtensions', 2006, 2008,  '#x31F0', '#x31FF').
block(                'YijingHexagramSymbols', 2006, 2008,  '#x4DC0', '#x4DFF').
block(                  'ModifierToneLetters', 2006, 2008,  '#xA700', '#xA71F').
block(                   'VariationSelectors', 2006, 2008,  '#xFE00', '#xFE0F').

block(                        'VerticalForms', 2006, 2008,  '#xFE10', '#xFE1F').
block(                     'LinearBSyllabary', 2006, 2008,  '#x10000', '#x1007F').
block(                     'LinearBIdeograms', 2006, 2008,  '#x10080', '#x100FF').
block(                        'AegeanNumbers', 2006, 2008,  '#x10100', '#x1013F').
block(                  'AncientGreekNumbers', 2006, 2008,  '#x10140', '#x1018F').
block(                            'OldItalic', 2006, 2008,  '#x10300', '#x1032F').
block(                               'Gothic', 2006, 2008,  '#x10330', '#x1034F').
block(                             'Ugaritic', 2006, 2008,  '#x10380', '#x1039F').
block(                           'OldPersian', 2006, 2008,  '#x103A0', '#x103DF').
block(                              'Deseret', 2006, 2008,  '#x10400', '#x1044F').
block(                              'Shavian', 2006, 2008,  '#x10450', '#x1047F').
block(                              'Osmanya', 2006, 2008,  '#x10480', '#x104AF').
block(                     'CypriotSyllabary', 2006, 2008,  '#x10800', '#x1083F').
block(                           'Kharoshthi', 2006, 2008,  '#x10A00', '#x10A5F').
block(              'ByzantineMusicalSymbols', 2006, 2008,  '#x1D000', '#x1D0FF').
block(                       'MusicalSymbols', 2006, 2008,  '#x1D100', '#x1D1FF').

block(          'AncientGreekMusicalNotation', 2006, 2008,  '#x1D200', '#x1D24F').
block(                   'TaiXuanJingSymbols', 2006, 2008,  '#x1D300', '#x1D35F').
block(      'MathematicalAlphanumericSymbols', 2006, 2008,  '#x1D400', '#x1D7FF').
block(       'CJKUnifiedIdeographsExtensionB', 2006, 2008,  '#x20000', '#x2A6DF').
block( 'CJKCompatibilityIdeographsSupplement', 2006, 2008,  '#x2F800', '#x2FA1F').
block(                                 'Tags', 2006, 2008,  '#xE0000', '#xE007F').
block(         'VariationSelectorsSupplement', 2006, 2008,  '#xE0100', '#xE01EF').
block(        'SupplementaryPrivateUseArea-A', 2006, 2008,  '#xF0000', '#xFFFFF').
block(        'SupplementaryPrivateUseArea-B', 2006, 2008,  '#x100000', '#x10FFFF').



