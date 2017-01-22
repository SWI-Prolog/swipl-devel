/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c) 2005-2012, University of Amsterdam
                             VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*
Derived from:

# Blocks-4.1.0.txt
# Date: 2004-12-03, 15:20 [KW]
#
# Unicode Character Database
# Copyright (c) 1991-2005 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see UCD.html
#
# Note:   The casing of block names is not normative.
#         For example, "Basic Latin" and "BASIC LATIN" are equivalent.
#
# Format:
# Start Code..End Code; Block Name

# ================================================

# Note:   When comparing block names, casing, whitespace, hyphens,
#         and underbars are ignored.
#         For example, "Latin Extended-A" and "latin extended a" are equivalent.
#         For more information on the comparison of property values,
#            see UCD.html.
#
#  All code points not explicitly listed for Block
#  have the value No_Block.

# Property:     Block
*/

:- module(unicode_blocks,
          [ unicode_block/3             % Name, From, To
          ]).

%!  unicode_block(?BlockName, ?Start, ?End)
%
%   Provides the names of the  basic   unicode  blocks. Derived from
%   UNICODE 4.1.0 from www.unicode.org

unicode_block('Basic Latin', 0x0000, 0x007F).
unicode_block('Latin-1 Supplement', 0x0080, 0x00FF).
unicode_block('Latin Extended-A', 0x0100, 0x017F).
unicode_block('Latin Extended-B', 0x0180, 0x024F).
unicode_block('IPA Extensions', 0x0250, 0x02AF).
unicode_block('Spacing Modifier Letters', 0x02B0, 0x02FF).
unicode_block('Combining Diacritical Marks', 0x0300, 0x036F).
unicode_block('Greek and Coptic', 0x0370, 0x03FF).
unicode_block('Cyrillic', 0x0400, 0x04FF).
unicode_block('Cyrillic Supplement', 0x0500, 0x052F).
unicode_block('Armenian', 0x0530, 0x058F).
unicode_block('Hebrew', 0x0590, 0x05FF).
unicode_block('Arabic', 0x0600, 0x06FF).
unicode_block('Syriac', 0x0700, 0x074F).
unicode_block('Arabic Supplement', 0x0750, 0x077F).
unicode_block('Thaana', 0x0780, 0x07BF).
unicode_block('Devanagari', 0x0900, 0x097F).
unicode_block('Bengali', 0x0980, 0x09FF).
unicode_block('Gurmukhi', 0x0A00, 0x0A7F).
unicode_block('Gujarati', 0x0A80, 0x0AFF).
unicode_block('Oriya', 0x0B00, 0x0B7F).
unicode_block('Tamil', 0x0B80, 0x0BFF).
unicode_block('Telugu', 0x0C00, 0x0C7F).
unicode_block('Kannada', 0x0C80, 0x0CFF).
unicode_block('Malayalam', 0x0D00, 0x0D7F).
unicode_block('Sinhala', 0x0D80, 0x0DFF).
unicode_block('Thai', 0x0E00, 0x0E7F).
unicode_block('Lao', 0x0E80, 0x0EFF).
unicode_block('Tibetan', 0x0F00, 0x0FFF).
unicode_block('Myanmar', 0x1000, 0x109F).
unicode_block('Georgian', 0x10A0, 0x10FF).
unicode_block('Hangul Jamo', 0x1100, 0x11FF).
unicode_block('Ethiopic', 0x1200, 0x137F).
unicode_block('Ethiopic Supplement', 0x1380, 0x139F).
unicode_block('Cherokee', 0x13A0, 0x13FF).
unicode_block('Unified Canadian Aboriginal Syllabics', 0x1400, 0x167F).
unicode_block('Ogham', 0x1680, 0x169F).
unicode_block('Runic', 0x16A0, 0x16FF).
unicode_block('Tagalog', 0x1700, 0x171F).
unicode_block('Hanunoo', 0x1720, 0x173F).
unicode_block('Buhid', 0x1740, 0x175F).
unicode_block('Tagbanwa', 0x1760, 0x177F).
unicode_block('Khmer', 0x1780, 0x17FF).
unicode_block('Mongolian', 0x1800, 0x18AF).
unicode_block('Limbu', 0x1900, 0x194F).
unicode_block('Tai Le', 0x1950, 0x197F).
unicode_block('New Tai Lue', 0x1980, 0x19DF).
unicode_block('Khmer Symbols', 0x19E0, 0x19FF).
unicode_block('Buginese', 0x1A00, 0x1A1F).
unicode_block('Phonetic Extensions', 0x1D00, 0x1D7F).
unicode_block('Phonetic Extensions Supplement', 0x1D80, 0x1DBF).
unicode_block('Combining Diacritical Marks Supplement', 0x1DC0, 0x1DFF).
unicode_block('Latin Extended Additional', 0x1E00, 0x1EFF).
unicode_block('Greek Extended', 0x1F00, 0x1FFF).
unicode_block('General Punctuation', 0x2000, 0x206F).
unicode_block('Superscripts and Subscripts', 0x2070, 0x209F).
unicode_block('Currency Symbols', 0x20A0, 0x20CF).
unicode_block('Combining Diacritical Marks for Symbols', 0x20D0, 0x20FF).
unicode_block('Letterlike Symbols', 0x2100, 0x214F).
unicode_block('Number Forms', 0x2150, 0x218F).
unicode_block('Arrows', 0x2190, 0x21FF).
unicode_block('Mathematical Operators', 0x2200, 0x22FF).
unicode_block('Miscellaneous Technical', 0x2300, 0x23FF).
unicode_block('Control Pictures', 0x2400, 0x243F).
unicode_block('Optical Character Recognition', 0x2440, 0x245F).
unicode_block('Enclosed Alphanumerics', 0x2460, 0x24FF).
unicode_block('Box Drawing', 0x2500, 0x257F).
unicode_block('Block Elements', 0x2580, 0x259F).
unicode_block('Geometric Shapes', 0x25A0, 0x25FF).
unicode_block('Miscellaneous Symbols', 0x2600, 0x26FF).
unicode_block('Dingbats', 0x2700, 0x27BF).
unicode_block('Miscellaneous Mathematical Symbols-A', 0x27C0, 0x27EF).
unicode_block('Supplemental Arrows-A', 0x27F0, 0x27FF).
unicode_block('Braille Patterns', 0x2800, 0x28FF).
unicode_block('Supplemental Arrows-B', 0x2900, 0x297F).
unicode_block('Miscellaneous Mathematical Symbols-B', 0x2980, 0x29FF).
unicode_block('Supplemental Mathematical Operators', 0x2A00, 0x2AFF).
unicode_block('Miscellaneous Symbols and Arrows', 0x2B00, 0x2BFF).
unicode_block('Glagolitic', 0x2C00, 0x2C5F).
unicode_block('Coptic', 0x2C80, 0x2C8F).
unicode_block('Georgian Supplement', 0x2D00, 0x2D2F).
unicode_block('Tifinagh', 0x2D30, 0x2D7F).
unicode_block('Ethiopic Extended', 0x2D80, 0x2DDF).
unicode_block('Supplemental Punctuation', 0x2E00, 0x2E7F).
unicode_block('CJK Radicals Supplement', 0x2E80, 0x2EFF).
unicode_block('Kangxi Radicals', 0x2F00, 0x2FDF).
unicode_block('Ideographic Description Characters', 0x2FF0, 0x2FFF).
unicode_block('CJK Symbols and Punctuation', 0x3000, 0x303F).
unicode_block('Hiragana', 0x3040, 0x309F).
unicode_block('Katakana', 0x30A0, 0x30FF).
unicode_block('Bopomofo', 0x3100, 0x312F).
unicode_block('Hangul Compatibility Jamo', 0x3130, 0x318F).
unicode_block('Kanbun', 0x3190, 0x319F).
unicode_block('Bopomofo Extended', 0x31A0, 0x31BF).
unicode_block('CJK Basic Strokes', 0x31C0, 0x31EF).
unicode_block('Katakana Phonetic Extensions', 0x31F0, 0x31FF).
unicode_block('Enclosed CJK Letters and Months', 0x3200, 0x32FF).
unicode_block('CJK Compatibility', 0x3300, 0x33FF).
unicode_block('CJK Unified Ideographs Extension A', 0x3400, 0x4DBF).
unicode_block('Yijing Hexagram Symbols', 0x4DC0, 0x4DFF).
unicode_block('CJK Unified Ideographs', 0x4E00, 0x9FFF).
unicode_block('Yi Syllables', 0xA000, 0xA48F).
unicode_block('Yi Radicals', 0xA490, 0xA4CF).
unicode_block('Modifier Tone Letters', 0xA700, 0xA71F).
unicode_block('Syloti Nagri', 0xA800, 0xA82F).
unicode_block('Hangul Syllables', 0xAC00, 0xD7AF).
unicode_block('High Surrogates', 0xD800, 0xDB7F).
unicode_block('High Private Use Surrogates', 0xDB80, 0xDBFF).
unicode_block('Low Surrogates', 0xDC00, 0xDFFF).
unicode_block('Private Use Area', 0xE000, 0xF8FF).
unicode_block('CJK Compatibility Ideographs', 0xF900, 0xFAFF).
unicode_block('Alphabetic Presentation Forms', 0xFB00, 0xFB4F).
unicode_block('Arabic Presentation Forms-A', 0xFB50, 0xFDFF).
unicode_block('Variation Selectors', 0xFE00, 0xFE0F).
unicode_block('Vertical Forms', 0xFE10, 0xFE1F).
unicode_block('Combining Half Marks', 0xFE20, 0xFE2F).
unicode_block('CJK Compatibility Forms', 0xFE30, 0xFE4F).
unicode_block('Small Form Variants', 0xFE50, 0xFE6F).
unicode_block('Arabic Presentation Forms-B', 0xFE70, 0xFEFF).
unicode_block('Halfwidth and Fullwidth Forms', 0xFF00, 0xFFEF).
unicode_block('Specials', 0xFFF0, 0xFFFF).
unicode_block('Linear B Syllabary', 0x10000, 0x1007F).
unicode_block('Linear B Ideograms', 0x10080, 0x100FF).
unicode_block('Aegean Numbers', 0x10100, 0x1013F).
unicode_block('Ancient Greek Numbers', 0x10140, 0x1018F).
unicode_block('Old Italic', 0x10300, 0x1032F).
unicode_block('Gothic', 0x10330, 0x1034F).
unicode_block('Ugaritic', 0x10380, 0x1039F).
unicode_block('Old Persian', 0x103A0, 0x103DF).
unicode_block('Deseret', 0x10400, 0x1044F).
unicode_block('Shavian', 0x10450, 0x1047F).
unicode_block('Osmanya', 0x10480, 0x104AF).
unicode_block('Cypriot Syllabary', 0x10800, 0x1083F).
unicode_block('Kharoshthi', 0x10A00, 0x10A5F).
unicode_block('Byzantine Musical Symbols', 0x1D000, 0x1D0FF).
unicode_block('Musical Symbols', 0x1D100, 0x1D1FF).
unicode_block('Ancient Greek Musical Notation', 0x1D200, 0x1D24F).
unicode_block('Tai Xuan Jing Symbols', 0x1D300, 0x1D35F).
unicode_block('Mathematical Alphanumeric Symbols', 0x1D400, 0x1D7FF).
unicode_block('CJK Unified Ideographs Extension B', 0x20000, 0x2A6DF).
unicode_block('CJK Compatibility Ideographs Supplement', 0x2F800, 0x2FA1F).
unicode_block('Tags', 0xE0000, 0xE007F).
unicode_block('Variation Selectors Supplement', 0xE0100, 0xE01EF).
unicode_block('Supplementary Private Use Area-A', 0xF0000, 0xFFFFF).
unicode_block('Supplementary Private Use Area-B', 0x100000, 0x10FFFF).
