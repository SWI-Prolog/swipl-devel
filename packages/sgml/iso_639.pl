/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(iso_639,
	  [ iso_639_2/2,		% Code, Language
	    iso_639_3/2,		% Code, Language
	    iso_639/2			% Code, Language
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ISO 639 language codes.  This material is based on

	http://www.wwp.brown.edu/encoding/training/ISO/iso639.html

It would be nice to know a bit   more about these languages, such as the
applicable character sets.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

iso_639_2(Code, Lang) :-
	l2(Code, Lang).
iso_639_3(Code, Lang) :-
	l3(Code, Lang).
iso_639(Code, Lang) :-
	(   l2(Code, Lang)
	;   l3(Code, Lang)
	).


%	l3(?Code, ?Lang)
%	
%	ISO-639 3-letter codes

l3(abk, 'Abkhazian').
l3(ace, 'Achinese').
l3(ach, 'Acoli').
l3(ada, 'Adangme').
l3(aar, 'Afar').
l3(afh, 'Afrihili').
l3(afr, 'Afrikaans').
l3(afa, 'Afro-Asiatic (Other)').
l3(aka, 'Akan').
l3(akk, 'Akkadian').
l3(alb, 'Albanian').
l3(sqi, 'Albanian').
l3(ale, 'Aleut').
l3(alg, 'Algonquian languages').
l3(tut, 'Altaic (Other)').
l3(amh, 'Amharic').
l3(apa, 'Apache languages').
l3(ara, 'Arabic').
l3(arc, 'Aramaic').
l3(arp, 'Arapaho').
l3(arn, 'Araucanian').
l3(arw, 'Arawak').
l3(arm, 'Armenian').
l3(hye, 'Armenian').
l3(art, 'Artificial (Other)').
l3(asm, 'Assamese').
l3(ath, 'Athapascan languages').
l3(map, 'Austronesian (Other)').
l3(ava, 'Avaric').
l3(ave, 'Avestan').
l3(awa, 'Awadhi').
l3(aym, 'Aymara').
l3(aze, 'Azerbaijani').
l3(nah, 'Aztec').
l3(ban, 'Balinese').
l3(bat, 'Baltic (Other)').
l3(bal, 'Baluchi').
l3(bam, 'Bambara').
l3(bai, 'Bamileke languages').
l3(bad, 'Banda').
l3(bnt, 'Bantu (Other)').
l3(bas, 'Basa').
l3(bak, 'Bashkir').
l3(baq, 'Basque').
l3(eus, 'Basque').
l3(bej, 'Beja').
l3(bem, 'Bemba').
l3(ben, 'Bengali').
l3(ber, 'Berber (Other)').
l3(bho, 'Bhojpuri').
l3(bih, 'Bihari').
l3(bik, 'Bikol').
l3(bin, 'Bini').
l3(bis, 'Bislama').
l3(bra, 'Braj').
l3(bre, 'Breton').
l3(bug, 'Buginese').
l3(bul, 'Bulgarian').
l3(bua, 'Buriat').
l3(bur, 'Burmese').
l3(mya, 'Burmese').
l3(bel, 'Byelorussian').
l3(cad, 'Caddo').
l3(car, 'Carib').
l3(cat, 'Catalan').
l3(cau, 'Caucasian (Other)').
l3(ceb, 'Cebuano').
l3(cel, 'Celtic (Other)').
l3(cai, 'Central American Indian (Other)').
l3(chg, 'Chagatai').
l3(cha, 'Chamorro').
l3(che, 'Chechen').
l3(chr, 'Cherokee').
l3(chy, 'Cheyenne').
l3(chb, 'Chibcha').
l3(chi, 'Chinese').
l3(zho, 'Chinese').
l3(chn, 'Chinook jargon').
l3(cho, 'Choctaw').
l3(chu, 'Church Slavic').
l3(chv, 'Chuvash').
l3(cop, 'Coptic').
l3(cor, 'Cornish').
l3(cos, 'Corsican').
l3(cre, 'Cree').
l3(mus, 'Creek').
l3(crp, 'Creoles and Pidgins (Other)').
l3(cpe, 'Creoles and Pidgins, English-based (Other)').
l3(cpf, 'Creoles and Pidgins, French-based (Other)').
l3(cpp, 'Creoles and Pidgins, Portuguese-based (Other)').
l3(cus, 'Cushitic (Other)').
l3(ces, 'Czech').
l3(cze, 'Czech').
l3(dak, 'Dakota').
l3(dan, 'Danish').
l3(del, 'Delaware').
l3(din, 'Dinka').
l3(div, 'Divehi').
l3(doi, 'Dogri').
l3(dra, 'Dravidian (Other)').
l3(dua, 'Duala').
l3(dut, 'Dutch').
l3(nla, 'Dutch').
l3(dum, 'Dutch, Middle (ca. 1050-1350)').
l3(dyu, 'Dyula').
l3(dzo, 'Dzongkha').
l3(efi, 'Efik').
l3(egy, 'Egyptian (Ancient)').
l3(eka, 'Ekajuk').
l3(elx, 'Elamite').
l3(eng, 'English').
l3(enm, 'English, Middle (ca. 1100-1500)').
l3(ang, 'English, Old (ca. 450-1100)').
l3(esk, 'Eskimo (Other)').
l3(epo, 'Esperanto').
l3(est, 'Estonian').
l3(ewe, 'Ewe').
l3(ewo, 'Ewondo').
l3(fan, 'Fang').
l3(fat, 'Fanti').
l3(fao, 'Faroese').
l3(fij, 'Fijian').
l3(fin, 'Finnish').
l3(fiu, 'Finno-Ugrian (Other)').
l3(fon, 'Fon').
l3(fra, 'French').
l3(fre, 'French').
l3(frm, 'French, Middle (ca. 1400-1600)').
l3(fro, 'French, Old (842- ca. 1400)').
l3(fry, 'Frisian').
l3(ful, 'Fulah').
l3(gaa, 'Ga').
l3(gae, 'Gaelic (Scots)').
l3(gdh, 'Gaelic (Scots)').
l3(glg, 'Gallegan').
l3(lug, 'Ganda').
l3(gay, 'Gayo').
l3(gez, 'Geez').
l3(geo, 'Georgian').
l3(kat, 'Georgian').
l3(deu, 'German').
l3(ger, 'German').
l3(gmh, 'German, Middle High (ca. 1050-1500)').
l3(goh, 'German, Old High (ca. 750-1050)').
l3(gem, 'Germanic (Other)').
l3(gil, 'Gilbertese').
l3(gon, 'Gondi').
l3(got, 'Gothic').
l3(grb, 'Grebo').
l3(grc, 'Greek, Ancient (to 1453)').
l3(ell, 'Greek, Modern (1453-)').
l3(gre, 'Greek, Modern (1453-)').
l3(kal, 'Greenlandic').
l3(grn, 'Guarani').
l3(guj, 'Gujarati').
l3(hai, 'Haida').
l3(hau, 'Hausa').
l3(haw, 'Hawaiian').
l3(heb, 'Hebrew').
l3(her, 'Herero').
l3(hil, 'Hiligaynon').
l3(him, 'Himachali').
l3(hin, 'Hindi').
l3(hmo, 'Hiri Motu').
l3(hun, 'Hungarian').
l3(hup, 'Hupa').
l3(iba, 'Iban').
l3(ice, 'Icelandic').
l3(isl, 'Icelandic').
l3(ibo, 'Igbo').
l3(ijo, 'Ijo').
l3(ilo, 'Iloko').
l3(inc, 'Indic (Other)').
l3(ine, 'Indo-European (Other)').
l3(ind, 'Indonesian').
l3(ina, 'Interlingua (International Auxiliary language Association)').
l3(ine, 'Interlingue').
l3(iku, 'Inuktitut').
l3(ipk, 'Inupiak').
l3(ira, 'Iranian (Other)').
l3(gai, 'Irish').
l3(iri, 'Irish').
l3(sga, 'Irish, Old (to 900)').
l3(mga, 'Irish, Middle (900 - 1200)').
l3(iro, 'Iroquoian languages').
l3(ita, 'Italian').
l3(jpn, 'Japanese').
l3(jav, 'Javanese').
l3(jaw, 'Javanese').
l3(jrb, 'Judeo-Arabic').
l3(jpr, 'Judeo-Persian').
l3(kab, 'Kabyle').
l3(kac, 'Kachin').
l3(kam, 'Kamba').
l3(kan, 'Kannada').
l3(kau, 'Kanuri').
l3(kaa, 'Kara-Kalpak').
l3(kar, 'Karen').
l3(kas, 'Kashmiri').
l3(kaw, 'Kawi').
l3(kaz, 'Kazakh').
l3(kha, 'Khasi').
l3(khm, 'Khmer').
l3(khi, 'Khoisan (Other)').
l3(kho, 'Khotanese').
l3(kik, 'Kikuyu').
l3(kin, 'Kinyarwanda').
l3(kir, 'Kirghiz').
l3(kom, 'Komi').
l3(kon, 'Kongo').
l3(kok, 'Konkani').
l3(kor, 'Korean').
l3(kpe, 'Kpelle').
l3(kro, 'Kru').
l3(kua, 'Kuanyama').
l3(kum, 'Kumyk').
l3(kur, 'Kurdish').
l3(kru, 'Kurukh').
l3(kus, 'Kusaie').
l3(kut, 'Kutenai').
l3(lad, 'Ladino').
l3(lah, 'Lahnda').
l3(lam, 'Lamba').
l3(oci, 'Langue d\'Oc (post 1500)').
l3(lao, 'Lao').
l3(lat, 'Latin').
l3(lav, 'Latvian').
l3(ltz, 'Letzeburgesch').
l3(lez, 'Lezghian').
l3(lin, 'Lingala').
l3(lit, 'Lithuanian').
l3(loz, 'Lozi').
l3(lub, 'Luba-Katanga').
l3(lui, 'Luiseno').
l3(lun, 'Lunda').
l3(luo, 'Luo (Kenya and Tanzania)').
l3(mac, 'Macedonian').
l3(mak, 'Macedonian').
l3(mad, 'Madurese').
l3(mag, 'Magahi').
l3(mai, 'Maithili').
l3(mak, 'Makasar').
l3(mlg, 'Malagasy').
l3(may, 'Malay').
l3(msa, 'Malay').
l3(mal, 'Malayalam').
l3(mlt, 'Maltese').
l3(man, 'Mandingo').
l3(mni, 'Manipuri').
l3(mno, 'Manobo languages').
l3(max, 'Manx').
l3(mao, 'Maori').
l3(mri, 'Maori').
l3(mar, 'Marathi').
l3(chm, 'Mari').
l3(mah, 'Marshall').
l3(mwr, 'Marwari').
l3(mas, 'Masai').
l3(myn, 'Mayan languages').
l3(men, 'Mende').
l3(mic, 'Micmac').
l3(min, 'Minangkabau').
l3(mis, 'Miscellaneous (Other)').
l3(moh, 'Mohawk').
l3(mol, 'Moldavian').
l3(mkh, 'Mon-Kmer (Other)').
l3(lol, 'Mongo').
l3(mon, 'Mongolian').
l3(mos, 'Mossi').
l3(mul, 'Multiple languages').
l3(mun, 'Munda languages').
l3(nau, 'Nauru').
l3(nav, 'Navajo').
l3(nde, 'Ndebele, North').
l3(nbl, 'Ndebele, South').
l3(ndo, 'Ndongo').
l3(nep, 'Nepali').
l3(new, 'Newari').
l3(nic, 'Niger-Kordofanian (Other)').
l3(ssa, 'Nilo-Saharan (Other)').
l3(niu, 'Niuean').
l3(non, 'Norse, Old').
l3(nai, 'North American Indian (Other)').
l3(nor, 'Norwegian').
l3(nno, 'Norwegian (Nynorsk)').
l3(nub, 'Nubian languages').
l3(nym, 'Nyamwezi').
l3(nya, 'Nyanja').
l3(nyn, 'Nyankole').
l3(nyo, 'Nyoro').
l3(nzi, 'Nzima').
l3(oji, 'Ojibwa').
l3(ori, 'Oriya').
l3(orm, 'Oromo').
l3(osa, 'Osage').
l3(oss, 'Ossetic').
l3(oto, 'Otomian languages').
l3(pal, 'Pahlavi').
l3(pau, 'Palauan').
l3(pli, 'Pali').
l3(pam, 'Pampanga').
l3(pag, 'Pangasinan').
l3(pan, 'Panjabi').
l3(pap, 'Papiamento').
l3(paa, 'Papuan-Australian (Other)').
l3(fas, 'Persian').
l3(per, 'Persian').
l3(peo, 'Persian, Old (ca 600 - 400 B.C.)').
l3(phn, 'Phoenician').
l3(pol, 'Polish').
l3(pon, 'Ponape').
l3(por, 'Portuguese').
l3(pra, 'Prakrit languages').
l3(pro, 'Provencal, Old (to 1500)').
l3(pus, 'Pushto').
l3(que, 'Quechua').
l3(roh, 'Rhaeto-Romance').
l3(raj, 'Rajasthani').
l3(rar, 'Rarotongan').
l3(roa, 'Romance (Other)').
l3(ron, 'Romanian').
l3(rum, 'Romanian').
l3(rom, 'Romany').
l3(run, 'Rundi').
l3(rus, 'Russian').
l3(sal, 'Salishan languages').
l3(sam, 'Samaritan Aramaic').
l3(smi, 'Sami languages').
l3(smo, 'Samoan').
l3(sad, 'Sandawe').
l3(sag, 'Sango').
l3(san, 'Sanskrit').
l3(srd, 'Sardinian').
l3(sco, 'Scots').
l3(sel, 'Selkup').
l3(sem, 'Semitic (Other)').
l3(scr, 'Serbo-Croatian').
l3(srr, 'Serer').
l3(shn, 'Shan').
l3(sna, 'Shona').
l3(sid, 'Sidamo').
l3(bla, 'Siksika').
l3(snd, 'Sindhi').
l3(sin, 'Singhalese').
l3(sit, 'Sino-Tibetan (Other)').
l3(sio, 'Siouan languages').
l3(sla, 'Slavic (Other)').
l3(ssw, 'Siswant').
l3(slk, 'Slovak').
l3(slo, 'Slovak').
l3(slv, 'Slovenian').
l3(sog, 'Sogdian').
l3(som, 'Somali').
l3(son, 'Songhai').
l3(wen, 'Sorbian languages').
l3(nso, 'Sotho, Northern').
l3(sot, 'Sotho, Southern').
l3(sai, 'South American Indian (Other)').
l3(esl, 'Spanish').
l3(spa, 'Spanish').
l3(suk, 'Sukuma').
l3(sux, 'Sumerian').
l3(sun, 'Sudanese').
l3(sus, 'Susu').
l3(swa, 'Swahili').
l3(ssw, 'Swazi').
l3(sve, 'Swedish').
l3(swe, 'Swedish').
l3(syr, 'Syriac').
l3(tgl, 'Tagalog').
l3(tah, 'Tahitian').
l3(tgk, 'Tajik').
l3(tmh, 'Tamashek').
l3(tam, 'Tamil').
l3(tat, 'Tatar').
l3(tel, 'Telugu').
l3(ter, 'Tereno').
l3(tha, 'Thai').
l3(bod, 'Tibetan').
l3(tib, 'Tibetan').
l3(tig, 'Tigre').
l3(tir, 'Tigrinya').
l3(tem, 'Timne').
l3(tiv, 'Tivi').
l3(tli, 'Tlingit').
l3(tog, 'Tonga (Nyasa)').
l3(ton, 'Tonga (Tonga Islands)').
l3(tru, 'Truk').
l3(tsi, 'Tsimshian').
l3(tso, 'Tsonga').
l3(tsn, 'Tswana').
l3(tum, 'Tumbuka').
l3(tur, 'Turkish').
l3(ota, 'Turkish, Ottoman (1500 - 1928)').
l3(tuk, 'Turkmen').
l3(tyv, 'Tuvinian').
l3(twi, 'Twi').
l3(uga, 'Ugaritic').
l3(uig, 'Uighur').
l3(ukr, 'Ukrainian').
l3(umb, 'Umbundu').
l3(und, 'Undetermined').
l3(urd, 'Urdu').
l3(uzb, 'Uzbek').
l3(vai, 'Vai').
l3(ven, 'Venda').
l3(vie, 'Vietnamese').
l3(vol, 'Volapük').
l3(vot, 'Votic').
l3(wak, 'Wakashan languages').
l3(wal, 'Walamo').
l3(war, 'Waray').
l3(was, 'Washo').
l3(cym, 'Welsh').
l3(wel, 'Welsh').
l3(wol, 'Wolof').
l3(xho, 'Xhosa').
l3(sah, 'Yakut').
l3(yao, 'Yao').
l3(yap, 'Yap').
l3(yid, 'Yiddish').
l3(yor, 'Yoruba').
l3(zap, 'Zapotec').
l3(zen, 'Zenaga').
l3(zha, 'Zhuang').
l3(zul, 'Zulu').
l3(zun, 'Zuni').

%	l2(?Code, ?Lang)
%	
%	ISO-639 2 letter codes

l2(aa, 'Afar').
l2(ab, 'Abkhazian').
l2(af, 'Afrikaans').
l2(am, 'Amharic').
l2(ar, 'Arabic').
l2(as, 'Assamese').
l2(ay, 'Aymara').
l2(az, 'Azerbaijani').
l2(ba, 'Bashkir').
l2(be, 'Byelorussian').
l2(bg, 'Bulgarian').
l2(bh, 'Bihari').
l2(bi, 'Bislama').
l2(bn, 'Bengali, Bangla').
l2(bo, 'Tibetan').
l2(br, 'Breton').
l2(ca, 'Catalan').
l2(co, 'Corsican').
l2(cs, 'Czech').
l2(cy, 'Welsh').
l2(da, 'Danish').
l2(de, 'German').
l2(dz, 'Bhutani').
l2(el, 'Greek').
l2(en, 'English, American').
l2(eo, 'Esperanto').
l2(es, 'Spanish').
l2(et, 'Estonian').
l2(eu, 'Basque').
l2(fa, 'Persian').
l2(fi, 'Finnish').
l2(fj, 'Fiji').
l2(fo, 'Faeroese').
l2(fr, 'French').
l2(fy, 'Frisian').
l2(ga, 'Irish').
l2(gd, 'Gaelic, Scots Gaelic').
l2(gl, 'Galician').
l2(gn, 'Guarani').
l2(gu, 'Gujarati').
l2(ha, 'Hausa').
l2(hi, 'Hindi').
l2(hr, 'Croatian').
l2(hu, 'Hungarian').
l2(hy, 'Armenian').
l2(ia, 'Interlingua').
l2(ie, 'Interlingue').
l2(ik, 'Inupiak').
l2(in, 'Indonesian').
l2(is, 'Icelandic').
l2(it, 'Italian').
l2(iw, 'Hebrew').
l2(ja, 'Japanese').
l2(ji, 'Yiddish').
l2(jw, 'Javanese').
l2(ka, 'Georgian').
l2(kk, 'Kazakh').
l2(kl, 'Greenlandic').
l2(km, 'Cambodian').
l2(kn, 'Kannada').
l2(ko, 'Korean').
l2(ks, 'Kashmiri').
l2(ku, 'Kurdish').
l2(ky, 'Kirghiz').
l2(la, 'Latin').
l2(ln, 'Lingala').
l2(lo, 'Laothian').
l2(lt, 'Lithuanian').
l2(lv, 'Latvian, Lettish').
l2(mg, 'Malagasy').
l2(mi, 'Maori').
l2(mk, 'Macedonian').
l2(ml, 'Malayalam').
l2(mn, 'Mongolian').
l2(mo, 'Moldavian').
l2(mr, 'Marathi').
l2(ms, 'Malay').
l2(mt, 'Maltese').
l2(my, 'Burmese').
l2(na, 'Nauru').
l2(ne, 'Nepali').
l2(nl, 'Dutch').
l2(no, 'Norwegian').
l2(oc, 'Occitan').
l2(om, 'Oromo, Afan').
l2(or, 'Oriya').
l2(pa, 'Punjabi').
l2(pl, 'Polish').
l2(ps, 'Pashto, Pushto').
l2(pt, 'Portuguese').
l2(qu, 'Quechua').
l2(rm, 'Rhaeto-Romance').
l2(rn, 'Kirundi').
l2(ro, 'Romanian').
l2(ru, 'Russian').
l2(rw, 'Kinyarwanda').
l2(sa, 'Sanskrit').
l2(sd, 'Sindhi').
l2(sg, 'Sangro').
l2(sh, 'Serbo-Croatian').
l2(si, 'Singhalese').
l2(sk, 'Slovak').
l2(sl, 'Slovenian').
l2(sm, 'Samoan').
l2(sn, 'Shona').
l2(so, 'Somali').
l2(sq, 'Albanian').
l2(sr, 'Serbian').
l2(ss, 'Siswati').
l2(st, 'Sesotho').
l2(su, 'Sudanese').
l2(sv, 'Swedish').
l2(sw, 'Swahili').
l2(ta, 'Tamil').
l2(te, 'Tegulu').
l2(tg, 'Tajik').
l2(th, 'Thai').
l2(ti, 'Tigrinya').
l2(tk, 'Turkmen').
l2(tl, 'Tagalog').
l2(tn, 'Setswana').
l2(to, 'Tonga').
l2(tr, 'Turkish').
l2(ts, 'Tsonga').
l2(tt, 'Tatar').
l2(tw, 'Twi').
l2(uk, 'Ukrainian').
l2(ur, 'Urdu').
l2(uz, 'Uzbek').
l2(vi, 'Vietnamese').
l2(vo, 'Volapuk').
l2(wo, 'Wolof').
l2(xh, 'Xhosa').
l2(yo, 'Yoruba').
l2(zh, 'Chinese').
l2(zu, 'Zulu').
