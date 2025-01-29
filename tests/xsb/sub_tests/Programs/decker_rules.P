:- compiler_options([spec_off]).

/*
 * Compile with specialization turned off.
 */


:-table p_isa_/2, p_setatt_/3, p_setdirect_/2, p_sub_/2 , p_setatttype_/3 .

p_isa_(X,Y) :- p_isa_f(X,Y).
p_setatt_(X,Y,Z) :- p_setatt_f(X,Y,Z).
p_setdirect_(X,Y) :- p_setdirect_f(X,Y).
p_sub_(X,Y) :- p_sub_f(X,Y).
p_setatttype_(X,Y,Z) :- p_setatttype_f(X,Y,Z).


p_isa_(X3,X4) :-
	p_setatttype_(X1,f_methodname_(X2,f_args_),X4),
	p_isa_(X0,X1),
	p_setatt_(X0,f_methodname_(X2,f_args_),X3).

p_isa_(X6,f_Organization) :-
	p_isa_(X5,f_Employee),
	p_setatt_(X5,f_methodname_(f_affiliation,f_args_),X6).

p_setatt_(X6,f_methodname_(f_employs,f_args_),X5) :-
	p_isa_(X5,f_Employee),
	p_setatt_(X5,f_methodname_(f_affiliation,f_args_),X6).

p_setdirect_(X6,f_methodname_(f_employs,f_args_)) :-
	p_isa_(X5,f_Employee),
	p_setatt_(X5,f_methodname_(f_affiliation,f_args_),X6).


p_isa_(X5,f_Employee) :-
	p_isa_(X6,f_Organization),
	p_setatt_(X6,f_methodname_(f_employs,f_args_),X5).

p_setatt_(X5,f_methodname_(f_affiliation,f_args_),X6) :-
	p_isa_(X6,f_Organization),
	p_setatt_(X6,f_methodname_(f_employs,f_args_),X5).

p_setdirect_(X5,f_methodname_(f_affiliation,f_args_)) :-
	p_isa_(X6,f_Organization),
	p_setatt_(X6,f_methodname_(f_employs,f_args_),X5).


p_isa_(X8,f_Organization) :-
	p_isa_(X7,f_Publication),
	p_setatt_(X7,f_methodname_(f_publisher,f_args_),X8).


p_setatt_(X8,f_methodname_(f_publishes,f_args_),X7) :-
	p_isa_(X7,f_Publication),
	p_setatt_(X7,f_methodname_(f_publisher,f_args_),X8).


p_setdirect_(X8,f_methodname_(f_publishes,f_args_)) :-
	p_isa_(X7,f_Publication),
	p_setatt_(X7,f_methodname_(f_publisher,f_args_),X8).


p_isa_(X7,f_Publication) :-
	p_isa_(X8,f_Organization),
	p_setatt_(X8,f_methodname_(f_publishes,f_args_),X7).

p_setatt_(X7,f_methodname_(f_publisher,f_args_),X8) :-
	p_isa_(X8,f_Organization),
	p_setatt_(X8,f_methodname_(f_publishes,f_args_),X7).

p_setdirect_(X7,f_methodname_(f_publisher,f_args_)) :-
	p_isa_(X8,f_Organization),
	p_setatt_(X8,f_methodname_(f_publishes,f_args_),X7).


p_isa_(X10,f_Organization) :-
	p_isa_(X9,f_TechnicalReport),
	p_setatt_(X9,f_methodname_(f_organization,f_args_),X10).

p_setatt_(X10,f_methodname_(f_technicalReport,f_args_),X9) :-
	p_isa_(X9,f_TechnicalReport),
	p_setatt_(X9,f_methodname_(f_organization,f_args_),X10).

p_setdirect_(X10,f_methodname_(f_technicalReport,f_args_)) :-
	p_isa_(X9,f_TechnicalReport),
	p_setatt_(X9,f_methodname_(f_organization,f_args_),X10).


p_isa_(X9,f_TechnicalReport) :-
	p_isa_(X10,f_Organization),
	p_setatt_(X10,f_methodname_(f_technicalReport,f_args_),X9).

p_setatt_(X9,f_methodname_(f_organization,f_args_),X10) :-
	p_isa_(X10,f_Organization),
	p_setatt_(X10,f_methodname_(f_technicalReport,f_args_),X9).

p_setdirect_(X9,f_methodname_(f_organization,f_args_)) :-
	p_isa_(X10,f_Organization),
	p_setatt_(X10,f_methodname_(f_technicalReport,f_args_),X9).



p_isa_(X11,f_Organization) :-
	p_isa_(X12,f_Project),
	p_setatt_(X12,f_methodname_(f_carriedOutBy,f_args_),X11).

p_setatt_(X11,f_methodname_(f_carriesOut,f_args_),X12) :-
	p_isa_(X12,f_Project),
	p_setatt_(X12,f_methodname_(f_carriedOutBy,f_args_),X11).

p_setdirect_(X11,f_methodname_(f_carriesOut,f_args_)) :-
	p_isa_(X12,f_Project),
	p_setatt_(X12,f_methodname_(f_carriedOutBy,f_args_),X11).


p_isa_(X12,f_Project) :-
	p_isa_(X11,f_Organization),
	p_setatt_(X11,f_methodname_(f_carriesOut,f_args_),X12).

p_setatt_(X12,f_methodname_(f_carriedOutBy,f_args_),X11) :-
	p_isa_(X11,f_Organization),
	p_setatt_(X11,f_methodname_(f_carriesOut,f_args_),X12).

p_setdirect_(X12,f_methodname_(f_carriedOutBy,f_args_)) :-
	p_isa_(X11,f_Organization),
	p_setatt_(X11,f_methodname_(f_carriesOut,f_args_),X12).

p_isa_(X13,f_Organization) :-
	p_isa_(X14,f_Product),
	p_setatt_(X14,f_methodname_(f_developedBy,f_args_),X13).

p_setatt_(X13,f_methodname_(f_develops,f_args_),X14) :-
	p_isa_(X14,f_Product),
	p_setatt_(X14,f_methodname_(f_developedBy,f_args_),X13).
p_setdirect_(X13,f_methodname_(f_develops,f_args_)) :-
	p_isa_(X14,f_Product),
	p_setatt_(X14,f_methodname_(f_developedBy,f_args_),X13).

p_isa_(X14,f_Product) :-
	p_isa_(X13,f_Organization),
	p_setatt_(X13,f_methodname_(f_develops,f_args_),X14).

p_setatt_(X14,f_methodname_(f_developedBy,f_args_),X13) :-
	p_isa_(X13,f_Organization),
	p_setatt_(X13,f_methodname_(f_develops,f_args_),X14).

p_setdirect_(X14,f_methodname_(f_developedBy,f_args_)) :-
	p_isa_(X13,f_Organization),
	p_setatt_(X13,f_methodname_(f_develops,f_args_),X14).


p_isa_(X16,f_ResearchGroup) :-
	p_isa_(X15,f_Researcher),
	p_setatt_(X15,f_methodname_(f_memberOf,f_args_),X16).

p_setatt_(X16,f_methodname_(f_member,f_args_),X15) :-
	p_isa_(X15,f_Researcher),
	p_setatt_(X15,f_methodname_(f_memberOf,f_args_),X16).

p_setdirect_(X16,f_methodname_(f_member,f_args_)) :-
	p_isa_(X15,f_Researcher),
	p_setatt_(X15,f_methodname_(f_memberOf,f_args_),X16).

p_isa_(X15,f_Researcher) :-
	p_isa_(X16,f_ResearchGroup),
	p_setatt_(X16,f_methodname_(f_member,f_args_),X15).

p_setatt_(X15,f_methodname_(f_memberOf,f_args_),X16) :-
	p_isa_(X16,f_ResearchGroup),
	p_setatt_(X16,f_methodname_(f_member,f_args_),X15).

p_setdirect_(X15,f_methodname_(f_memberOf,f_args_)) :-
	p_isa_(X16,f_ResearchGroup),
	p_setatt_(X16,f_methodname_(f_member,f_args_),X15).

p_isa_(X17,f_Employee) :-
	p_isa_(X18,f_ResearchGroup),
	p_setatt_(X18,f_methodname_(f_head,f_args_),X17).

p_setatt_(X17,f_methodname_(f_headOfGroup,f_args_),X18) :-
	p_isa_(X18,f_ResearchGroup),
	p_setatt_(X18,f_methodname_(f_head,f_args_),X17).

p_setdirect_(X17,f_methodname_(f_headOfGroup,f_args_)) :-
	p_isa_(X18,f_ResearchGroup),
	p_setatt_(X18,f_methodname_(f_head,f_args_),X17).


p_isa_(X18,f_ResearchGroup) :-
	p_isa_(X17,f_Employee),
	p_setatt_(X17,f_methodname_(f_headOfGroup,f_args_),X18).

p_setatt_(X18,f_methodname_(f_head,f_args_),X17) :-
	p_isa_(X17,f_Employee),
	p_setatt_(X17,f_methodname_(f_headOfGroup,f_args_),X18).

p_setdirect_(X18,f_methodname_(f_head,f_args_)) :-
	p_isa_(X17,f_Employee),
	p_setatt_(X17,f_methodname_(f_headOfGroup,f_args_),X18).


p_isa_(X19,f_Researcher) :-
	p_isa_(X20,f_Researcher),
	p_setatt_(X20,f_methodname_(f_cooperatesWith,f_args_),X19).

p_setatt_(X19,f_methodname_(f_cooperatesWith,f_args_),X20) :-
	p_isa_(X20,f_Researcher),
	p_setatt_(X20,f_methodname_(f_cooperatesWith,f_args_),X19).

p_setdirect_(X19,f_methodname_(f_cooperatesWith,f_args_)) :-
	p_isa_(X20,f_Researcher),
	p_setatt_(X20,f_methodname_(f_cooperatesWith,f_args_),X19).


p_isa_(X22,f_Publication) :-
	p_isa_(X21,f_Person),
	p_setatt_(X21,f_methodname_(f_publication,f_args_),X22).

p_setatt_(X22,f_methodname_(f_author,f_args_),X21) :-
	p_isa_(X21,f_Person),
	p_setatt_(X21,f_methodname_(f_publication,f_args_),X22).

p_setdirect_(X22,f_methodname_(f_author,f_args_)) :-
	p_isa_(X21,f_Person),
	p_setatt_(X21,f_methodname_(f_publication,f_args_),X22).


p_isa_(X21,f_Person) :-
	p_isa_(X22,f_Publication),
	p_setatt_(X22,f_methodname_(f_author,f_args_),X21).

p_setatt_(X21,f_methodname_(f_publication,f_args_),X22) :-
	p_isa_(X22,f_Publication),
	p_setatt_(X22,f_methodname_(f_author,f_args_),X21).

p_setdirect_(X21,f_methodname_(f_publication,f_args_)) :-
	p_isa_(X22,f_Publication),
	p_setatt_(X22,f_methodname_(f_author,f_args_),X21).


p_isa_(X24,f_Event) :-
	p_isa_(X23,f_Person),
	p_setatt_(X23,f_methodname_(f_organizerOrChairOf,f_args_),X24).

p_setatt_(X24,f_methodname_(f_orgCommittee,f_args_),X23) :-
	p_isa_(X23,f_Person),
	p_setatt_(X23,f_methodname_(f_organizerOrChairOf,f_args_),X24).

p_setdirect_(X24,f_methodname_(f_orgCommittee,f_args_)) :-
	p_isa_(X23,f_Person),
	p_setatt_(X23,f_methodname_(f_organizerOrChairOf,f_args_),X24).

p_isa_(X23,f_Person) :-
	p_isa_(X24,f_Event),
	p_setatt_(X24,f_methodname_(f_orgCommittee,f_args_),X23).

p_setatt_(X23,f_methodname_(f_organizerOrChairOf,f_args_),X24) :-
	p_isa_(X24,f_Event),
	p_setatt_(X24,f_methodname_(f_orgCommittee,f_args_),X23).

p_setdirect_(X23,f_methodname_(f_organizerOrChairOf,f_args_)) :-
	p_isa_(X24,f_Event),
	p_setatt_(X24,f_methodname_(f_orgCommittee,f_args_),X23).


p_isa_(X26,f_Event) :-
	p_isa_(X25,f_Person),
	p_setatt_(X25,f_methodname_(f_memberOfPC,f_args_),X26).

p_setatt_(X26,f_methodname_(f_programCommittee,f_args_),X25) :-
	p_isa_(X25,f_Person),
	p_setatt_(X25,f_methodname_(f_memberOfPC,f_args_),X26).

p_setdirect_(X26,f_methodname_(f_programCommittee,f_args_)) :-
	p_isa_(X25,f_Person),
	p_setatt_(X25,f_methodname_(f_memberOfPC,f_args_),X26).


p_isa_(X42,f_Project) :-
	p_isa_(X41,f_Publication),
	p_setatt_(X41,f_methodname_(f_describesProject,f_args_),X42).

p_setatt_(X42,f_methodname_(f_projectInfo,f_args_),X41) :-
	p_isa_(X41,f_Publication),
	p_setatt_(X41,f_methodname_(f_describesProject,f_args_),X42).

p_setdirect_(X42,f_methodname_(f_projectInfo,f_args_)) :-
	p_isa_(X41,f_Publication),
	p_setatt_(X41,f_methodname_(f_describesProject,f_args_),X42).


p_isa_(X41,f_Publication) :-
	p_isa_(X42,f_Project),
	p_setatt_(X42,f_methodname_(f_projectInfo,f_args_),X41).

p_setatt_(X41,f_methodname_(f_describesProject,f_args_),X42) :-
	p_isa_(X42,f_Project),
	p_setatt_(X42,f_methodname_(f_projectInfo,f_args_),X41).

p_setdirect_(X41,f_methodname_(f_describesProject,f_args_)) :-
	p_isa_(X42,f_Project),
	p_setatt_(X42,f_methodname_(f_projectInfo,f_args_),X41).


p_isa_(X44,f_Book) :-
	p_isa_(X43,f_ArtcileInBook),
	p_setatt_(X43,f_methodname_(f_book,f_args_),X44).

p_setatt_(X44,f_methodname_(f_containsArticle,f_args_),X43) :-
	p_isa_(X43,f_ArtcileInBook),
	p_setatt_(X43,f_methodname_(f_book,f_args_),X44).

p_setdirect_(X44,f_methodname_(f_containsArticle,f_args_)) :-
	p_isa_(X43,f_ArtcileInBook),
	p_setatt_(X43,f_methodname_(f_book,f_args_),X44).


p_isa_(X43,f_ArtcileInBook) :-
	p_isa_(X44,f_Book),
	p_setatt_(X44,f_methodname_(f_containsArticle,f_args_),X43).

p_setatt_(X43,f_methodname_(f_book,f_args_),X44) :-
	p_isa_(X44,f_Book),
	p_setatt_(X44,f_methodname_(f_containsArticle,f_args_),X43).

p_setdirect_(X43,f_methodname_(f_book,f_args_)) :-
	p_isa_(X44,f_Book),
	p_setatt_(X44,f_methodname_(f_containsArticle,f_args_),X43).

p_isa_(X46,f_Journal) :-
	p_isa_(X45,f_JournalArticle),
	p_setatt_(X45,f_methodname_(f_journal,f_args_),X46).

p_setatt_(X46,f_methodname_(f_containsArticle,f_args_),X45) :-
	p_isa_(X45,f_JournalArticle),
	p_setatt_(X45,f_methodname_(f_journal,f_args_),X46).

p_setdirect_(X46,f_methodname_(f_containsArticle,f_args_)) :-
	p_isa_(X45,f_JournalArticle),
	p_setatt_(X45,f_methodname_(f_journal,f_args_),X46).


p_isa_(X45,f_JournalArticle) :-
	p_isa_(X46,f_Journal),
	p_setatt_(X46,f_methodname_(f_containsArticle,f_args_),X45).

p_setatt_(X45,f_methodname_(f_journal,f_args_),X46) :-
	p_isa_(X46,f_Journal),
	p_setatt_(X46,f_methodname_(f_containsArticle,f_args_),X45).

p_setdirect_(X45,f_methodname_(f_journal,f_args_)) :-
	p_isa_(X46,f_Journal),
	p_setatt_(X46,f_methodname_(f_containsArticle,f_args_),X45).

p_isa_(X48,f_Conference) :-
	p_isa_(X47,f_ConferencePaper),
	p_setatt_(X47,f_methodname_(f_conference,f_args_),X48).

p_setatt_(X48,f_methodname_(f_publication,f_args_),X47) :-
	p_isa_(X47,f_ConferencePaper),
	p_setatt_(X47,f_methodname_(f_conference,f_args_),X48).

p_setdirect_(X48,f_methodname_(f_publication,f_args_)) :-
	p_isa_(X47,f_ConferencePaper),
	p_setatt_(X47,f_methodname_(f_conference,f_args_),X48).



p_isa_(X47,f_ConferencePaper) :-
	p_isa_(X48,f_Conference),
	p_setatt_(X48,f_methodname_(f_publication,f_args_),X47).

p_setatt_(X47,f_methodname_(f_conference,f_args_),X48) :-
	p_isa_(X48,f_Conference),
	p_setatt_(X48,f_methodname_(f_publication,f_args_),X47).

p_setdirect_(X47,f_methodname_(f_conference,f_args_)) :-
	p_isa_(X48,f_Conference),
	p_setatt_(X48,f_methodname_(f_publication,f_args_),X47).


p_isa_(X50,f_Workshop) :-
	p_isa_(X49,f_WorkshopPaper),
	p_setatt_(X49,f_methodname_(f_workshop,f_args_),X50).

p_setatt_(X50,f_methodname_(f_publication,f_args_),X49) :-
	p_isa_(X49,f_WorkshopPaper),
	p_setatt_(X49,f_methodname_(f_workshop,f_args_),X50).

p_setdirect_(X50,f_methodname_(f_publication,f_args_)) :-
	p_isa_(X49,f_WorkshopPaper),
	p_setatt_(X49,f_methodname_(f_workshop,f_args_),X50).


p_isa_(X49,f_WorkshopPaper) :-
	p_isa_(X50,f_Workshop),
	p_setatt_(X50,f_methodname_(f_publication,f_args_),X49).

p_setatt_(X49,f_methodname_(f_workshop,f_args_),X50) :-
	p_isa_(X50,f_Workshop),
	p_setatt_(X50,f_methodname_(f_publication,f_args_),X49).

p_setdirect_(X49,f_methodname_(f_workshop,f_args_)) :-
	p_isa_(X50,f_Workshop),
	p_setatt_(X50,f_methodname_(f_publication,f_args_),X49).


p_isa_(X52,f_Project) :-
	p_isa_(X51,f_ResearchTopic),
	p_setatt_(X51,f_methodname_(f_dealtWithIn,f_args_),X52).

p_setatt_(X52,f_methodname_(f_isAbout,f_args_),X51) :-
	p_isa_(X51,f_ResearchTopic),
	p_setatt_(X51,f_methodname_(f_dealtWithIn,f_args_),X52).

p_setdirect_(X52,f_methodname_(f_isAbout,f_args_)) :-
	p_isa_(X51,f_ResearchTopic),
	p_setatt_(X51,f_methodname_(f_dealtWithIn,f_args_),X52).


p_isa_(X51,f_ResearchTopic) :-
	p_isa_(X52,f_Project),
	p_setatt_(X52,f_methodname_(f_isAbout,f_args_),X51).

p_setatt_(X51,f_methodname_(f_dealtWithIn,f_args_),X52) :-
	p_isa_(X52,f_Project),
	p_setatt_(X52,f_methodname_(f_isAbout,f_args_),X51).

p_setdirect_(X51,f_methodname_(f_dealtWithIn,f_args_)) :-
	p_isa_(X52,f_Project),
	p_setatt_(X52,f_methodname_(f_isAbout,f_args_),X51).


p_isa_(X54,f_Project) :-
	p_isa_(X53,f_Product),
	p_setatt_(X53,f_methodname_(f_producedBy,f_args_),X54).

p_setatt_(X54,f_methodname_(f_product,f_args_),X53) :-
	p_isa_(X53,f_Product),
	p_setatt_(X53,f_methodname_(f_producedBy,f_args_),X54).

p_setdirect_(X54,f_methodname_(f_product,f_args_)) :-
	p_isa_(X53,f_Product),
	p_setatt_(X53,f_methodname_(f_producedBy,f_args_),X54).


p_isa_(X53,f_Product) :-
	p_isa_(X54,f_Project),
	p_setatt_(X54,f_methodname_(f_product,f_args_),X53).

p_setatt_(X53,f_methodname_(f_producedBy,f_args_),X54) :-
	p_isa_(X54,f_Project),
	p_setatt_(X54,f_methodname_(f_product,f_args_),X53).

p_setdirect_(X53,f_methodname_(f_producedBy,f_args_)) :-
	p_isa_(X54,f_Project),
	p_setatt_(X54,f_methodname_(f_product,f_args_),X53).


p_isa_(X56,f_Project) :-
	p_isa_(X55,f_Organization),
	p_setatt_(X55,f_methodname_(f_finances,f_args_),X56).

p_setatt_(X56,f_methodname_(f_financedBy,f_args_),X55) :-
	p_isa_(X55,f_Organization),
	p_setatt_(X55,f_methodname_(f_finances,f_args_),X56).

p_setdirect_(X56,f_methodname_(f_financedBy,f_args_)) :-
	p_isa_(X55,f_Organization),
	p_setatt_(X55,f_methodname_(f_finances,f_args_),X56).


p_isa_(X55,f_Organization) :-
	p_isa_(X56,f_Project),
	p_setatt_(X56,f_methodname_(f_financedBy,f_args_),X55).

p_setatt_(X55,f_methodname_(f_finances,f_args_),X56) :-
	p_isa_(X56,f_Project),
	p_setatt_(X56,f_methodname_(f_financedBy,f_args_),X55).

p_setdirect_(X55,f_methodname_(f_finances,f_args_)) :-
	p_isa_(X56,f_Project),
	p_setatt_(X56,f_methodname_(f_financedBy,f_args_),X55).


p_isa_(X57,f_Event) :-
	p_isa_(X58,f_Event),
	p_setatt_(X58,f_methodname_(f_atEvent,f_args_),X57).

p_setatt_(X57,f_methodname_(f_hasParts,f_args_),X58) :-
	p_isa_(X58,f_Event),
	p_setatt_(X58,f_methodname_(f_atEvent,f_args_),X57).

p_setdirect_(X57,f_methodname_(f_hasParts,f_args_)) :-
	p_isa_(X58,f_Event),
	p_setatt_(X58,f_methodname_(f_atEvent,f_args_),X57).


p_isa_(X58,f_Event) :-
	p_isa_(X57,f_Event),
	p_setatt_(X57,f_methodname_(f_hasParts,f_args_),X58).

p_setatt_(X58,f_methodname_(f_atEvent,f_args_),X57) :-
	p_isa_(X57,f_Event),
	p_setatt_(X57,f_methodname_(f_hasParts,f_args_),X58).

p_setdirect_(X58,f_methodname_(f_atEvent,f_args_)) :-
	p_isa_(X57,f_Event),
	p_setatt_(X57,f_methodname_(f_hasParts,f_args_),X58).


p_sub_(X59,X61) :- p_sub_(X59,X60), p_sub_f(X60,X61).

p_setatttype_(X62,f_methodname_(X64,f_args_),X65) :-
	p_sub_(X62,X63),
	p_setatttype_(X63,f_methodname_(X64,f_args_),X65).

p_isa_(X66,X67) :-
	p_sub_(X68,X67),
	p_isa_(X66,X68).

p_partof_(X69,X70) :-
	p_setatt_(X70,f_methodname(f_hasParts,f_args_),X69).

p_setatt_(X71,f_methodname(f_hasParts,f_args_),X72) :-
	p_partof_(X72,X71).
