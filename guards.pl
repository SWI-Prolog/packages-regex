/* guards.pl:  routines for checking ad-hoc grammar restrictions
 * on regular expressions; used by regex.dcg.
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

block(                        'BasicLatin', 2001, 2008, 0x0000, 0x007F).
block(                 'Latin-1Supplement', 2001, 2008, 0x0080, 0x00FF).
block(                   'LatinExtended-A', 2001, 2008, 0x0100, 0x017F).
block(                   'LatinExtended-B', 2001, 2008, 0x0180, 0x024F).
block(                     'IPAExtensions', 2001, 2008, 0x0250, 0x02AF).
block(            'SpacingModifierLetters', 2001, 2008, 0x02B0, 0x02FF).
block(         'CombiningDiacriticalMarks', 2001, 2008, 0x0300, 0x036F).
block(                             'Greek', 2001, 2008, 0x0370, 0x03FF).
block(                          'Cyrillic', 2001, 2008, 0x0400, 0x04FF).
block(                          'Armenian', 2001, 2008, 0x0530, 0x058F).
block(                            'Hebrew', 2001, 2008, 0x0590, 0x05FF).
block(                            'Arabic', 2001, 2008, 0x0600, 0x06FF).
block(                            'Syriac', 2001, 2008, 0x0700, 0x074F).
block(                            'Thaana', 2001, 2008, 0x0780, 0x07BF).
block(                        'Devanagari', 2001, 2008, 0x0900, 0x097F).

block(                           'Bengali', 2001, 2008, 0x0980, 0x09FF).
block(                          'Gurmukhi', 2001, 2008, 0x0A00, 0x0A7F).
block(                          'Gujarati', 2001, 2008, 0x0A80, 0x0AFF).
block(                             'Oriya', 2001, 2008, 0x0B00, 0x0B7F).
block(                             'Tamil', 2001, 2008, 0x0B80, 0x0BFF).
block(                            'Telugu', 2001, 2008, 0x0C00, 0x0C7F).
block(                           'Kannada', 2001, 2008, 0x0C80, 0x0CFF).
block(                         'Malayalam', 2001, 2008, 0x0D00, 0x0D7F).
block(                           'Sinhala', 2001, 2008, 0x0D80, 0x0DFF).
block(                              'Thai', 2001, 2008, 0x0E00, 0x0E7F).
block(                               'Lao', 2001, 2008, 0x0E80, 0x0EFF).
block(                           'Tibetan', 2001, 2008, 0x0F00, 0x0FFF).
block(                           'Myanmar', 2001, 2008, 0x1000, 0x109F).
block(                          'Georgian', 2001, 2008, 0x10A0, 0x10FF).
block(                        'HangulJamo', 2001, 2008, 0x1100, 0x11FF).
block(                          'Ethiopic', 2001, 2008, 0x1200, 0x137F).

block(                          'Cherokee', 2001, 2008, 0x13A0, 0x13FF).
block('UnifiedCanadianAboriginalSyllabics', 2001, 2008, 0x1400, 0x167F).
block(                             'Ogham', 2001, 2008, 0x1680, 0x169F).
block(                             'Runic', 2001, 2008, 0x16A0, 0x16FF).
block(                             'Khmer', 2001, 2008, 0x1780, 0x17FF).
block(                         'Mongolian', 2001, 2008, 0x1800, 0x18AF).
block(           'LatinExtendedAdditional', 2001, 2008, 0x1E00, 0x1EFF).
block(                     'GreekExtended', 2001, 2008, 0x1F00, 0x1FFF).
block(                'GeneralPunctuation', 2001, 2008, 0x2000, 0x206F).
block(         'SuperscriptsandSubscripts', 2001, 2008, 0x2070, 0x209F).
block(                   'CurrencySymbols', 2001, 2008, 0x20A0, 0x20CF).
block(          'CombiningMarksforSymbols', 2001, 2008, 0x20D0, 0x20FF).
block(                 'LetterlikeSymbols', 2001, 2008, 0x2100, 0x214F).
block(                      'NumberForms',  2001, 2008, 0x2150, 0x218F).
block(                            'Arrows', 2001, 2008, 0x2190, 0x21FF).
block(             'MathematicalOperators', 2001, 2008, 0x2200, 0x22FF).

block(            'MiscellaneousTechnical', 2001, 2008, 0x2300, 0x23FF).
block(                   'ControlPictures', 2001, 2008, 0x2400, 0x243F).
block(       'OpticalCharacterRecognition', 2001, 2008, 0x2440, 0x245F).
block(             'EnclosedAlphanumerics', 2001, 2008, 0x2460, 0x24FF).
block(                        'BoxDrawing', 2001, 2008, 0x2500, 0x257F).
block(                     'BlockElements', 2001, 2008, 0x2580, 0x259F).
block(                   'GeometricShapes', 2001, 2008, 0x25A0, 0x25FF).
block(              'MiscellaneousSymbols', 2001, 2008, 0x2600, 0x26FF).
block(                          'Dingbats', 2001, 2008, 0x2700, 0x27BF).
block(                   'BraillePatterns', 2001, 2008, 0x2800, 0x28FF).
block(             'CJKRadicalsSupplement', 2001, 2008, 0x2E80, 0x2EFF).
block(                    'KangxiRadicals', 2001, 2008, 0x2F00, 0x2FDF).
block(  'IdeographicDescriptionCharacters', 2001, 2008, 0x2FF0, 0x2FFF).
block(          'CJKSymbolsandPunctuation', 2001, 2008, 0x3000, 0x303F).
block(                          'Hiragana', 2001, 2008, 0x3040, 0x309F).
block(                          'Katakana', 2001, 2008, 0x30A0, 0x30FF).

block(                          'Bopomofo', 2001, 2008, 0x3100, 0x312F).
block(           'HangulCompatibilityJamo', 2001, 2008, 0x3130, 0x318F).
block(                            'Kanbun', 2001, 2008, 0x3190, 0x319F).
block(                  'BopomofoExtended', 2001, 2008, 0x31A0, 0x31BF).
block(       'EnclosedCJKLettersandMonths', 2001, 2008, 0x3200, 0x32FF).
block(                  'CJKCompatibility', 2001, 2008, 0x3300, 0x33FF).
block(    'CJKUnifiedIdeographsExtensionA', 2001, 2008, 0x3400, 0x4DB5).
block(              'CJKUnifiedIdeographs', 2001, 2008, 0x4E00, 0x9FFF).
block(                       'YiSyllables', 2001, 2008, 0xA000, 0xA48F).
block(                        'YiRadicals', 2001, 2008, 0xA490, 0xA4CF).
block(                   'HangulSyllables', 2001, 2008, 0xAC00, 0xD7A3).

/* HighSurrogates, LowSurrogates, and HighPrivateUseSurrogates dropped in PER, 2E */
block(                    'HighSurrogates', 2001, 2001, 0xD800, 0xDB7F).
block(          'HighPrivateUseSurrogates', 2001, 2001, 0xDB80, 0xDBFF).
block(                     'LowSurrogates', 2001, 2001, 0xDC00, 0xDFFF).
block(                        'PrivateUse', 2001, 2008, 0xE000, 0xF8FF).
block(        'CJKCompatibilityIdeographs', 2001, 2008, 0xF900, 0xFAFF).

block(       'AlphabeticPresentationForms', 2001, 2008, 0xFB00, 0xFB4F).
block(         'ArabicPresentationForms-A', 2001, 2008, 0xFB50, 0xFDFF).
block(                'CombiningHalfMarks', 2001, 2008, 0xFE20, 0xFE2F).
block(             'CJKCompatibilityForms', 2001, 2008, 0xFE30, 0xFE4F).
block(                 'SmallFormVariants', 2001, 2008, 0xFE50, 0xFE6F).
block(         'ArabicPresentationForms-B', 2001, 2005, 0xFE70, 0xFEFE).
block(         'ArabicPresentationForms-B', 2006, 2008, 0xFE70, 0xFEFF).
block(                          'Specials', 2001, 2005, 0xFEFF, 0xFEFF).
block(        'HalfwidthandFullwidthForms', 2001, 2008, 0xFF00, 0xFFEF).
block(                          'Specials', 2001, 2005, 0xFFF0, 0xFFFD).
block(                          'Specials', 2006, 2008, 0xFFF0, 0xFFFF).


/* Numerous blocks introduced in 2006 */
block(                   'CyrillicSupplement', 2006, 2008,  0x0500, 0x052F).
block(                     'ArabicSupplement', 2006, 2008,  0x0750, 0x077F).
block(                   'EthiopicSupplement', 2006, 2008,  0x1380, 0x139F).
block(                              'Tagalog', 2006, 2008,  0x1700, 0x171F).
block(                              'Hanunoo', 2006, 2008,  0x1720, 0x173F).
block(                                'Buhid', 2006, 2008,  0x1740, 0x175F).
block(                             'Tagbanwa', 2006, 2008,  0x1760, 0x177F).
block(                                'Limbu', 2006, 2008,  0x1900, 0x194F).
block(                                'TaiLe', 2006, 2008,  0x1950, 0x197F).
block(                            'NewTaiLue', 2006, 2008,  0x1980, 0x19DF).
block(                         'KhmerSymbols', 2006, 2008,  0x19E0, 0x19FF).
block(                             'Buginese', 2006, 2008,  0x1A00, 0x1A1F).
block(                   'PhoneticExtensions', 2006, 2008,  0x1D00, 0x1D7F).
block(         'PhoneticExtensionsSupplement', 2006, 2008,  0x1D80, 0x1DBF).
block(  'CombiningDiacriticalMarksSupplement', 2006, 2008,  0x1DC0, 0x1DFF).
block(   'MiscellaneousMathematicalSymbols-A', 2006, 2008,  0x27C0, 0x27EF).

block(                 'SupplementalArrows-A', 2006, 2008,  0x27F0, 0x27FF).
block(                 'SupplementalArrows-B', 2006, 2008,  0x2900, 0x297F).
block(   'MiscellaneousMathematicalSymbols-B', 2006, 2008,  0x2980, 0x29FF).
block(    'SupplementalMathematicalOperators', 2006, 2008,  0x2A00, 0x2AFF).
block(        'MiscellaneousSymbolsandArrows', 2006, 2008,  0x2B00, 0x2BFF).
block(                           'Glagolitic', 2006, 2008,  0x2C00, 0x2C5F).
block(                               'Coptic', 2006, 2008,  0x2C80, 0x2CFF).
block(                   'GeorgianSupplement', 2006, 2008,  0x2D00, 0x2D2F).
block(                             'Tifinagh', 2006, 2008,  0x2D30, 0x2D7F).
block(                     'EthiopicExtended', 2006, 2008,  0x2D80, 0x2DDF).
block(              'SupplementalPunctuation', 2006, 2008,  0x2E00, 0x2E7F).
block(                           'CJKStrokes', 2006, 2008,  0x31C0, 0x31EF).
block(           'KatakanaPhoneticExtensions', 2006, 2008,  0x31F0, 0x31FF).
block(                'YijingHexagramSymbols', 2006, 2008,  0x4DC0, 0x4DFF).
block(                  'ModifierToneLetters', 2006, 2008,  0xA700, 0xA71F).
block(                   'VariationSelectors', 2006, 2008,  0xFE00, 0xFE0F).

block(                        'VerticalForms', 2006, 2008,  0xFE10, 0xFE1F).
block(                     'LinearBSyllabary', 2006, 2008,  0x10000, 0x1007F).
block(                     'LinearBIdeograms', 2006, 2008,  0x10080, 0x100FF).
block(                        'AegeanNumbers', 2006, 2008,  0x10100, 0x1013F).
block(                  'AncientGreekNumbers', 2006, 2008,  0x10140, 0x1018F).
block(                            'OldItalic', 2006, 2008,  0x10300, 0x1032F).
block(                               'Gothic', 2006, 2008,  0x10330, 0x1034F).
block(                             'Ugaritic', 2006, 2008,  0x10380, 0x1039F).
block(                           'OldPersian', 2006, 2008,  0x103A0, 0x103DF).
block(                              'Deseret', 2006, 2008,  0x10400, 0x1044F).
block(                              'Shavian', 2006, 2008,  0x10450, 0x1047F).
block(                              'Osmanya', 2006, 2008,  0x10480, 0x104AF).
block(                     'CypriotSyllabary', 2006, 2008,  0x10800, 0x1083F).
block(                           'Kharoshthi', 2006, 2008,  0x10A00, 0x10A5F).
block(              'ByzantineMusicalSymbols', 2006, 2008,  0x1D000, 0x1D0FF).
block(                       'MusicalSymbols', 2006, 2008,  0x1D100, 0x1D1FF).

block(          'AncientGreekMusicalNotation', 2006, 2008,  0x1D200, 0x1D24F).
block(                   'TaiXuanJingSymbols', 2006, 2008,  0x1D300, 0x1D35F).
block(      'MathematicalAlphanumericSymbols', 2006, 2008,  0x1D400, 0x1D7FF).
block(       'CJKUnifiedIdeographsExtensionB', 2006, 2008,  0x20000, 0x2A6DF).
block( 'CJKCompatibilityIdeographsSupplement', 2006, 2008,  0x2F800, 0x2FA1F).
block(                                 'Tags', 2006, 2008,  0xE0000, 0xE007F).
block(         'VariationSelectorsSupplement', 2006, 2008,  0xE0100, 0xE01EF).
block(        'SupplementaryPrivateUseArea-A', 2006, 2008,  0xF0000, 0xFFFFF).
block(        'SupplementaryPrivateUseArea-B', 2006, 2008,  0x100000, 0x10FFFF).



