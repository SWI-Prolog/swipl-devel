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

# Blocks-16.0.0.txt
# Date: Date: 2024-02-02
#
# Unicode Character Database
# Copyright (c) 1991-2024 Unicode, Inc.
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

unicode_block('Basic Latin',                                      0x0000,   0x007F).
unicode_block('Latin-1 Supplement',                               0x0080,   0x00FF).
unicode_block('Latin Extended-A',                                 0x0100,   0x017F).
unicode_block('Latin Extended-B',                                 0x0180,   0x024F).
unicode_block('IPA Extensions',                                   0x0250,   0x02AF).
unicode_block('Spacing Modifier Letters',                         0x02B0,   0x02FF).
unicode_block('Combining Diacritical Marks',                      0x0300,   0x036F).
unicode_block('Greek and Coptic',                                 0x0370,   0x03FF).
unicode_block('Cyrillic',                                         0x0400,   0x04FF).
unicode_block('Cyrillic Supplement',                              0x0500,   0x052F).
unicode_block('Armenian',                                         0x0530,   0x058F).
unicode_block('Hebrew',                                           0x0590,   0x05FF).
unicode_block('Arabic',                                           0x0600,   0x06FF).
unicode_block('Syriac',                                           0x0700,   0x074F).
unicode_block('Arabic Supplement',                                0x0750,   0x077F).
unicode_block('Thaana',                                           0x0780,   0x07BF).
unicode_block('NKo',                                              0x07C0,   0x07FF).
unicode_block('Samaritan',                                        0x0800,   0x083F).
unicode_block('Mandaic',                                          0x0840,   0x085F).
unicode_block('Syriac Supplement',                                0x0860,   0x086F).
unicode_block('Arabic Extended-B',                                0x0870,   0x089F).
unicode_block('Arabic Extended-A',                                0x08A0,   0x08FF).
unicode_block('Devanagari',                                       0x0900,   0x097F).
unicode_block('Bengali',                                          0x0980,   0x09FF).
unicode_block('Gurmukhi',                                         0x0A00,   0x0A7F).
unicode_block('Gujarati',                                         0x0A80,   0x0AFF).
unicode_block('Oriya',                                            0x0B00,   0x0B7F).
unicode_block('Tamil',                                            0x0B80,   0x0BFF).
unicode_block('Telugu',                                           0x0C00,   0x0C7F).
unicode_block('Kannada',                                          0x0C80,   0x0CFF).
unicode_block('Malayalam',                                        0x0D00,   0x0D7F).
unicode_block('Sinhala',                                          0x0D80,   0x0DFF).
unicode_block('Thai',                                             0x0E00,   0x0E7F).
unicode_block('Lao',                                              0x0E80,   0x0EFF).
unicode_block('Tibetan',                                          0x0F00,   0x0FFF).
unicode_block('Myanmar',                                          0x1000,   0x109F).
unicode_block('Georgian',                                         0x10A0,   0x10FF).
unicode_block('Hangul Jamo',                                      0x1100,   0x11FF).
unicode_block('Ethiopic',                                         0x1200,   0x137F).
unicode_block('Ethiopic Supplement',                              0x1380,   0x139F).
unicode_block('Cherokee',                                         0x13A0,   0x13FF).
unicode_block('Unified Canadian Aboriginal Syllabics',            0x1400,   0x167F).
unicode_block('Ogham',                                            0x1680,   0x169F).
unicode_block('Runic',                                            0x16A0,   0x16FF).
unicode_block('Tagalog',                                          0x1700,   0x171F).
unicode_block('Hanunoo',                                          0x1720,   0x173F).
unicode_block('Buhid',                                            0x1740,   0x175F).
unicode_block('Tagbanwa',                                         0x1760,   0x177F).
unicode_block('Khmer',                                            0x1780,   0x17FF).
unicode_block('Mongolian',                                        0x1800,   0x18AF).
unicode_block('Unified Canadian Aboriginal Syllabics Extended',   0x18B0,   0x18FF).
unicode_block('Limbu',                                            0x1900,   0x194F).
unicode_block('Tai Le',                                           0x1950,   0x197F).
unicode_block('New Tai Lue',                                      0x1980,   0x19DF).
unicode_block('Khmer Symbols',                                    0x19E0,   0x19FF).
unicode_block('Buginese',                                         0x1A00,   0x1A1F).
unicode_block('Tai Tham',                                         0x1A20,   0x1AAF).
unicode_block('Combining Diacritical Marks Extended',             0x1AB0,   0x1AFF).
unicode_block('Balinese',                                         0x1B00,   0x1B7F).
unicode_block('Sundanese',                                        0x1B80,   0x1BBF).
unicode_block('Batak',                                            0x1BC0,   0x1BFF).
unicode_block('Lepcha',                                           0x1C00,   0x1C4F).
unicode_block('Ol Chiki',                                         0x1C50,   0x1C7F).
unicode_block('Cyrillic Extended-C',                              0x1C80,   0x1C8F).
unicode_block('Georgian Extended',                                0x1C90,   0x1CBF).
unicode_block('Sundanese Supplement',                             0x1CC0,   0x1CCF).
unicode_block('Vedic Extensions',                                 0x1CD0,   0x1CFF).
unicode_block('Phonetic Extensions',                              0x1D00,   0x1D7F).
unicode_block('Phonetic Extensions Supplement',                   0x1D80,   0x1DBF).
unicode_block('Combining Diacritical Marks Supplement',           0x1DC0,   0x1DFF).
unicode_block('Latin Extended Additional',                        0x1E00,   0x1EFF).
unicode_block('Greek Extended',                                   0x1F00,   0x1FFF).
unicode_block('General Punctuation',                              0x2000,   0x206F).
unicode_block('Superscripts and Subscripts',                      0x2070,   0x209F).
unicode_block('Currency Symbols',                                 0x20A0,   0x20CF).
unicode_block('Combining Diacritical Marks for Symbols',          0x20D0,   0x20FF).
unicode_block('Letterlike Symbols',                               0x2100,   0x214F).
unicode_block('Number Forms',                                     0x2150,   0x218F).
unicode_block('Arrows',                                           0x2190,   0x21FF).
unicode_block('Mathematical Operators',                           0x2200,   0x22FF).
unicode_block('Miscellaneous Technical',                          0x2300,   0x23FF).
unicode_block('Control Pictures',                                 0x2400,   0x243F).
unicode_block('Optical Character Recognition',                    0x2440,   0x245F).
unicode_block('Enclosed Alphanumerics',                           0x2460,   0x24FF).
unicode_block('Box Drawing',                                      0x2500,   0x257F).
unicode_block('Block Elements',                                   0x2580,   0x259F).
unicode_block('Geometric Shapes',                                 0x25A0,   0x25FF).
unicode_block('Miscellaneous Symbols',                            0x2600,   0x26FF).
unicode_block('Dingbats',                                         0x2700,   0x27BF).
unicode_block('Miscellaneous Mathematical Symbols-A',             0x27C0,   0x27EF).
unicode_block('Supplemental Arrows-A',                            0x27F0,   0x27FF).
unicode_block('Braille Patterns',                                 0x2800,   0x28FF).
unicode_block('Supplemental Arrows-B',                            0x2900,   0x297F).
unicode_block('Miscellaneous Mathematical Symbols-B',             0x2980,   0x29FF).
unicode_block('Supplemental Mathematical Operators',              0x2A00,   0x2AFF).
unicode_block('Miscellaneous Symbols and Arrows',                 0x2B00,   0x2BFF).
unicode_block('Glagolitic',                                       0x2C00,   0x2C5F).
unicode_block('Latin Extended-C',                                 0x2C60,   0x2C7F).
unicode_block('Coptic',                                           0x2C80,   0x2CFF).
unicode_block('Georgian Supplement',                              0x2D00,   0x2D2F).
unicode_block('Tifinagh',                                         0x2D30,   0x2D7F).
unicode_block('Ethiopic Extended',                                0x2D80,   0x2DDF).
unicode_block('Cyrillic Extended-A',                              0x2DE0,   0x2DFF).
unicode_block('Supplemental Punctuation',                         0x2E00,   0x2E7F).
unicode_block('CJK Radicals Supplement',                          0x2E80,   0x2EFF).
unicode_block('Kangxi Radicals',                                  0x2F00,   0x2FDF).
unicode_block('Ideographic Description Characters',               0x2FF0,   0x2FFF).
unicode_block('CJK Symbols and Punctuation',                      0x3000,   0x303F).
unicode_block('Hiragana',                                         0x3040,   0x309F).
unicode_block('Katakana',                                         0x30A0,   0x30FF).
unicode_block('Bopomofo',                                         0x3100,   0x312F).
unicode_block('Hangul Compatibility Jamo',                        0x3130,   0x318F).
unicode_block('Kanbun',                                           0x3190,   0x319F).
unicode_block('Bopomofo Extended',                                0x31A0,   0x31BF).
unicode_block('CJK Strokes',                                      0x31C0,   0x31EF).
unicode_block('Katakana Phonetic Extensions',                     0x31F0,   0x31FF).
unicode_block('Enclosed CJK Letters and Months',                  0x3200,   0x32FF).
unicode_block('CJK Compatibility',                                0x3300,   0x33FF).
unicode_block('CJK Unified Ideographs Extension A',               0x3400,   0x4DBF).
unicode_block('Yijing Hexagram Symbols',                          0x4DC0,   0x4DFF).
unicode_block('CJK Unified Ideographs',                           0x4E00,   0x9FFF).
unicode_block('Yi Syllables',                                     0xA000,   0xA48F).
unicode_block('Yi Radicals',                                      0xA490,   0xA4CF).
unicode_block('Lisu',                                             0xA4D0,   0xA4FF).
unicode_block('Vai',                                              0xA500,   0xA63F).
unicode_block('Cyrillic Extended-B',                              0xA640,   0xA69F).
unicode_block('Bamum',                                            0xA6A0,   0xA6FF).
unicode_block('Modifier Tone Letters',                            0xA700,   0xA71F).
unicode_block('Latin Extended-D',                                 0xA720,   0xA7FF).
unicode_block('Syloti Nagri',                                     0xA800,   0xA82F).
unicode_block('Common Indic Number Forms',                        0xA830,   0xA83F).
unicode_block('Phags-pa',                                         0xA840,   0xA87F).
unicode_block('Saurashtra',                                       0xA880,   0xA8DF).
unicode_block('Devanagari Extended',                              0xA8E0,   0xA8FF).
unicode_block('Kayah Li',                                         0xA900,   0xA92F).
unicode_block('Rejang',                                           0xA930,   0xA95F).
unicode_block('Hangul Jamo Extended-A',                           0xA960,   0xA97F).
unicode_block('Javanese',                                         0xA980,   0xA9DF).
unicode_block('Myanmar Extended-B',                               0xA9E0,   0xA9FF).
unicode_block('Cham',                                             0xAA00,   0xAA5F).
unicode_block('Myanmar Extended-A',                               0xAA60,   0xAA7F).
unicode_block('Tai Viet',                                         0xAA80,   0xAADF).
unicode_block('Meetei Mayek Extensions',                          0xAAE0,   0xAAFF).
unicode_block('Ethiopic Extended-A',                              0xAB00,   0xAB2F).
unicode_block('Latin Extended-E',                                 0xAB30,   0xAB6F).
unicode_block('Cherokee Supplement',                              0xAB70,   0xABBF).
unicode_block('Meetei Mayek',                                     0xABC0,   0xABFF).
unicode_block('Hangul Syllables',                                 0xAC00,   0xD7AF).
unicode_block('Hangul Jamo Extended-B',                           0xD7B0,   0xD7FF).
unicode_block('High Surrogates',                                  0xD800,   0xDB7F).
unicode_block('High Private Use Surrogates',                      0xDB80,   0xDBFF).
unicode_block('Low Surrogates',                                   0xDC00,   0xDFFF).
unicode_block('Private Use Area',                                 0xE000,   0xF8FF).
unicode_block('CJK Compatibility Ideographs',                     0xF900,   0xFAFF).
unicode_block('Alphabetic Presentation Forms',                    0xFB00,   0xFB4F).
unicode_block('Arabic Presentation Forms-A',                      0xFB50,   0xFDFF).
unicode_block('Variation Selectors',                              0xFE00,   0xFE0F).
unicode_block('Vertical Forms',                                   0xFE10,   0xFE1F).
unicode_block('Combining Half Marks',                             0xFE20,   0xFE2F).
unicode_block('CJK Compatibility Forms',                          0xFE30,   0xFE4F).
unicode_block('Small Form Variants',                              0xFE50,   0xFE6F).
unicode_block('Arabic Presentation Forms-B',                      0xFE70,   0xFEFF).
unicode_block('Halfwidth and Fullwidth Forms',                    0xFF00,   0xFFEF).
unicode_block('Specials',                                         0xFFF0,   0xFFFF).
unicode_block('Linear B Syllabary',                               0x10000,  0x1007F).
unicode_block('Linear B Ideograms',                               0x10080,  0x100FF).
unicode_block('Aegean Numbers',                                   0x10100,  0x1013F).
unicode_block('Ancient Greek Numbers',                            0x10140,  0x1018F).
unicode_block('Ancient Symbols',                                  0x10190,  0x101CF).
unicode_block('Phaistos Disc',                                    0x101D0,  0x101FF).
unicode_block('Lycian',                                           0x10280,  0x1029F).
unicode_block('Carian',                                           0x102A0,  0x102DF).
unicode_block('Coptic Epact Numbers',                             0x102E0,  0x102FF).
unicode_block('Old Italic',                                       0x10300,  0x1032F).
unicode_block('Gothic',                                           0x10330,  0x1034F).
unicode_block('Old Permic',                                       0x10350,  0x1037F).
unicode_block('Ugaritic',                                         0x10380,  0x1039F).
unicode_block('Old Persian',                                      0x103A0,  0x103DF).
unicode_block('Deseret',                                          0x10400,  0x1044F).
unicode_block('Shavian',                                          0x10450,  0x1047F).
unicode_block('Osmanya',                                          0x10480,  0x104AF).
unicode_block('Osage',                                            0x104B0,  0x104FF).
unicode_block('Elbasan',                                          0x10500,  0x1052F).
unicode_block('Caucasian Albanian',                               0x10530,  0x1056F).
unicode_block('Vithkuqi',                                         0x10570,  0x105BF).
unicode_block('Todhri',                                           0x105C0,  0x105FF).
unicode_block('Linear A',                                         0x10600,  0x1077F).
unicode_block('Latin Extended-F',                                 0x10780,  0x107BF).
unicode_block('Cypriot Syllabary',                                0x10800,  0x1083F).
unicode_block('Imperial Aramaic',                                 0x10840,  0x1085F).
unicode_block('Palmyrene',                                        0x10860,  0x1087F).
unicode_block('Nabataean',                                        0x10880,  0x108AF).
unicode_block('Hatran',                                           0x108E0,  0x108FF).
unicode_block('Phoenician',                                       0x10900,  0x1091F).
unicode_block('Lydian',                                           0x10920,  0x1093F).
unicode_block('Meroitic Hieroglyphs',                             0x10980,  0x1099F).
unicode_block('Meroitic Cursive',                                 0x109A0,  0x109FF).
unicode_block('Kharoshthi',                                       0x10A00,  0x10A5F).
unicode_block('Old South Arabian',                                0x10A60,  0x10A7F).
unicode_block('Old North Arabian',                                0x10A80,  0x10A9F).
unicode_block('Manichaean',                                       0x10AC0,  0x10AFF).
unicode_block('Avestan',                                          0x10B00,  0x10B3F).
unicode_block('Inscriptional Parthian',                           0x10B40,  0x10B5F).
unicode_block('Inscriptional Pahlavi',                            0x10B60,  0x10B7F).
unicode_block('Psalter Pahlavi',                                  0x10B80,  0x10BAF).
unicode_block('Old Turkic',                                       0x10C00,  0x10C4F).
unicode_block('Old Hungarian',                                    0x10C80,  0x10CFF).
unicode_block('Hanifi Rohingya',                                  0x10D00,  0x10D3F).
unicode_block('Garay',                                            0x10D40,  0x10D8F).
unicode_block('Rumi Numeral Symbols',                             0x10E60,  0x10E7F).
unicode_block('Yezidi',                                           0x10E80,  0x10EBF).
unicode_block('Arabic Extended-C',                                0x10EC0,  0x10EFF).
unicode_block('Old Sogdian',                                      0x10F00,  0x10F2F).
unicode_block('Sogdian',                                          0x10F30,  0x10F6F).
unicode_block('Old Uyghur',                                       0x10F70,  0x10FAF).
unicode_block('Chorasmian',                                       0x10FB0,  0x10FDF).
unicode_block('Elymaic',                                          0x10FE0,  0x10FFF).
unicode_block('Brahmi',                                           0x11000,  0x1107F).
unicode_block('Kaithi',                                           0x11080,  0x110CF).
unicode_block('Sora Sompeng',                                     0x110D0,  0x110FF).
unicode_block('Chakma',                                           0x11100,  0x1114F).
unicode_block('Mahajani',                                         0x11150,  0x1117F).
unicode_block('Sharada',                                          0x11180,  0x111DF).
unicode_block('Sinhala Archaic Numbers',                          0x111E0,  0x111FF).
unicode_block('Khojki',                                           0x11200,  0x1124F).
unicode_block('Multani',                                          0x11280,  0x112AF).
unicode_block('Khudawadi',                                        0x112B0,  0x112FF).
unicode_block('Grantha',                                          0x11300,  0x1137F).
unicode_block('Tulu-Tigalari',                                    0x11380,  0x113FF).
unicode_block('Newa',                                             0x11400,  0x1147F).
unicode_block('Tirhuta',                                          0x11480,  0x114DF).
unicode_block('Siddham',                                          0x11580,  0x115FF).
unicode_block('Modi',                                             0x11600,  0x1165F).
unicode_block('Mongolian Supplement',                             0x11660,  0x1167F).
unicode_block('Takri',                                            0x11680,  0x116CF).
unicode_block('Myanmar Extended-C',                               0x116D0,  0x116FF).
unicode_block('Ahom',                                             0x11700,  0x1174F).
unicode_block('Dogra',                                            0x11800,  0x1184F).
unicode_block('Warang Citi',                                      0x118A0,  0x118FF).
unicode_block('Dives Akuru',                                      0x11900,  0x1195F).
unicode_block('Nandinagari',                                      0x119A0,  0x119FF).
unicode_block('Zanabazar Square',                                 0x11A00,  0x11A4F).
unicode_block('Soyombo',                                          0x11A50,  0x11AAF).
unicode_block('Unified Canadian Aboriginal Syllabics Extended-A', 0x11AB0,  0x11ABF).
unicode_block('Pau Cin Hau',                                      0x11AC0,  0x11AFF).
unicode_block('Devanagari Extended-A',                            0x11B00,  0x11B5F).
unicode_block('Sunuwar',                                          0x11BC0,  0x11BFF).
unicode_block('Bhaiksuki',                                        0x11C00,  0x11C6F).
unicode_block('Marchen',                                          0x11C70,  0x11CBF).
unicode_block('Masaram Gondi',                                    0x11D00,  0x11D5F).
unicode_block('Gunjala Gondi',                                    0x11D60,  0x11DAF).
unicode_block('Makasar',                                          0x11EE0,  0x11EFF).
unicode_block('Kawi',                                             0x11F00,  0x11F5F).
unicode_block('Lisu Supplement',                                  0x11FB0,  0x11FBF).
unicode_block('Tamil Supplement',                                 0x11FC0,  0x11FFF).
unicode_block('Cuneiform',                                        0x12000,  0x123FF).
unicode_block('Cuneiform Numbers and Punctuation',                0x12400,  0x1247F).
unicode_block('Early Dynastic Cuneiform',                         0x12480,  0x1254F).
unicode_block('Cypro-Minoan',                                     0x12F90,  0x12FFF).
unicode_block('Egyptian Hieroglyphs',                             0x13000,  0x1342F).
unicode_block('Egyptian Hieroglyph Format Controls',              0x13430,  0x1345F).
unicode_block('Egyptian Hieroglyphs Extended-A',                  0x13460,  0x143FF).
unicode_block('Anatolian Hieroglyphs',                            0x14400,  0x1467F).
unicode_block('Gurung Khema',                                     0x16100,  0x1613F).
unicode_block('Bamum Supplement',                                 0x16800,  0x16A3F).
unicode_block('Mro',                                              0x16A40,  0x16A6F).
unicode_block('Tangsa',                                           0x16A70,  0x16ACF).
unicode_block('Bassa Vah',                                        0x16AD0,  0x16AFF).
unicode_block('Pahawh Hmong',                                     0x16B00,  0x16B8F).
unicode_block('Kirat Rai',                                        0x16D40,  0x16D7F).
unicode_block('Medefaidrin',                                      0x16E40,  0x16E9F).
unicode_block('Miao',                                             0x16F00,  0x16F9F).
unicode_block('Ideographic Symbols and Punctuation',              0x16FE0,  0x16FFF).
unicode_block('Tangut',                                           0x17000,  0x187FF).
unicode_block('Tangut Components',                                0x18800,  0x18AFF).
unicode_block('Khitan Small Script',                              0x18B00,  0x18CFF).
unicode_block('Tangut Supplement',                                0x18D00,  0x18D7F).
unicode_block('Kana Extended-B',                                  0x1AFF0,  0x1AFFF).
unicode_block('Kana Supplement',                                  0x1B000,  0x1B0FF).
unicode_block('Kana Extended-A',                                  0x1B100,  0x1B12F).
unicode_block('Small Kana Extension',                             0x1B130,  0x1B16F).
unicode_block('Nushu',                                            0x1B170,  0x1B2FF).
unicode_block('Duployan',                                         0x1BC00,  0x1BC9F).
unicode_block('Shorthand Format Controls',                        0x1BCA0,  0x1BCAF).
unicode_block('Symbols for Legacy Computing Supplement',          0x1CC00,  0x1CEBF).
unicode_block('Znamenny Musical Notation',                        0x1CF00,  0x1CFCF).
unicode_block('Byzantine Musical Symbols',                        0x1D000,  0x1D0FF).
unicode_block('Musical Symbols',                                  0x1D100,  0x1D1FF).
unicode_block('Ancient Greek Musical Notation',                   0x1D200,  0x1D24F).
unicode_block('Kaktovik Numerals',                                0x1D2C0,  0x1D2DF).
unicode_block('Mayan Numerals',                                   0x1D2E0,  0x1D2FF).
unicode_block('Tai Xuan Jing Symbols',                            0x1D300,  0x1D35F).
unicode_block('Counting Rod Numerals',                            0x1D360,  0x1D37F).
unicode_block('Mathematical Alphanumeric Symbols',                0x1D400,  0x1D7FF).
unicode_block('Sutton SignWriting',                               0x1D800,  0x1DAAF).
unicode_block('Latin Extended-G',                                 0x1DF00,  0x1DFFF).
unicode_block('Glagolitic Supplement',                            0x1E000,  0x1E02F).
unicode_block('Cyrillic Extended-D',                              0x1E030,  0x1E08F).
unicode_block('Nyiakeng Puachue Hmong',                           0x1E100,  0x1E14F).
unicode_block('Toto',                                             0x1E290,  0x1E2BF).
unicode_block('Wancho',                                           0x1E2C0,  0x1E2FF).
unicode_block('Nag Mundari',                                      0x1E4D0,  0x1E4FF).
unicode_block('Ol Onal',                                          0x1E5D0,  0x1E5FF).
unicode_block('Ethiopic Extended-B',                              0x1E7E0,  0x1E7FF).
unicode_block('Mende Kikakui',                                    0x1E800,  0x1E8DF).
unicode_block('Adlam',                                            0x1E900,  0x1E95F).
unicode_block('Indic Siyaq Numbers',                              0x1EC70,  0x1ECBF).
unicode_block('Ottoman Siyaq Numbers',                            0x1ED00,  0x1ED4F).
unicode_block('Arabic Mathematical Alphabetic Symbols',           0x1EE00,  0x1EEFF).
unicode_block('Mahjong Tiles',                                    0x1F000,  0x1F02F).
unicode_block('Domino Tiles',                                     0x1F030,  0x1F09F).
unicode_block('Playing Cards',                                    0x1F0A0,  0x1F0FF).
unicode_block('Enclosed Alphanumeric Supplement',                 0x1F100,  0x1F1FF).
unicode_block('Enclosed Ideographic Supplement',                  0x1F200,  0x1F2FF).
unicode_block('Miscellaneous Symbols and Pictographs',            0x1F300,  0x1F5FF).
unicode_block('Emoticons',                                        0x1F600,  0x1F64F).
unicode_block('Ornamental Dingbats',                              0x1F650,  0x1F67F).
unicode_block('Transport and Map Symbols',                        0x1F680,  0x1F6FF).
unicode_block('Alchemical Symbols',                               0x1F700,  0x1F77F).
unicode_block('Geometric Shapes Extended',                        0x1F780,  0x1F7FF).
unicode_block('Supplemental Arrows-C',                            0x1F800,  0x1F8FF).
unicode_block('Supplemental Symbols and Pictographs',             0x1F900,  0x1F9FF).
unicode_block('Chess Symbols',                                    0x1FA00,  0x1FA6F).
unicode_block('Symbols and Pictographs Extended-A',               0x1FA70,  0x1FAFF).
unicode_block('Symbols for Legacy Computing',                     0x1FB00,  0x1FBFF).
unicode_block('CJK Unified Ideographs Extension B',               0x20000,  0x2A6DF).
unicode_block('CJK Unified Ideographs Extension C',               0x2A700,  0x2B73F).
unicode_block('CJK Unified Ideographs Extension D',               0x2B740,  0x2B81F).
unicode_block('CJK Unified Ideographs Extension E',               0x2B820,  0x2CEAF).
unicode_block('CJK Unified Ideographs Extension F',               0x2CEB0,  0x2EBEF).
unicode_block('CJK Unified Ideographs Extension I',               0x2EBF0,  0x2EE5F).
unicode_block('CJK Compatibility Ideographs Supplement',          0x2F800,  0x2FA1F).
unicode_block('CJK Unified Ideographs Extension G',               0x30000,  0x3134F).
unicode_block('CJK Unified Ideographs Extension H',               0x31350,  0x323AF).
unicode_block('Tags',                                             0xE0000,  0xE007F).
unicode_block('Variation Selectors Supplement',                   0xE0100,  0xE01EF).
unicode_block('Supplementary Private Use Area-A',                 0xF0000,  0xFFFFF).
unicode_block('Supplementary Private Use Area-B',                 0x100000, 0x10FFFF).

