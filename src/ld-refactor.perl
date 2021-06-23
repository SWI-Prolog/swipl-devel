# Command line used:
# perl -w0pi ld-refactor.perl *.[ch] */*.[ch] 2>&1 | tee ld-refactor.out

use 5.030;
use utf8;
binmode STDERR, ":utf8";

use constant FLAGS => (
    F_DEFINED   => 1<<0,
    F_GLOBAL    => 1<<1,
    F_APIFUNC   => 1<<2,
    F_LDALIAS   => 1<<3,
    F_ISWEIRD   => 1<<4,
    F_HASLD     => 1<<5,
    F_HASDEF    => 1<<6,
    F_HASDECL   => 1<<7,
);
# __LD funcs that get special anticollision names
use constant LD_MAP => (
    getProcDefinition__LD => "getLocalProcDefinition",
    compileTermToHeap__LD => "compileTermToHeap_ex",
    ensureStackSpace__LD => "ensureStackSpace_ex",
    ensureLocalSpace__LD => "ensureLocalSpace_ex",
    PL_strip_module__LD => "PL_strip_module_flags",
    globalize_term_ref__LD => "globalizeTermRef",

    # These can use the standard names even though it looks like they have weird #defines
    is_portable_smallint__LD => "is_portable_smallint",
    setGenerationFrame__LD => "setGenerationFrame",
    valFloat__LD => "valFloat",
    valBignum__LD => "valBignum",
    acquire_ldata__LD => "acquire_ldata",
);
# functions that we treat as "API functions" despite not being listed in public headers
use constant UNDOC_API => qw(
    PL_unify_termv
    PL_unify_output
    PL_get_number
);
use constant {FLAGS};
use constant {
    FLAGMAP => {FLAGS},
    FLAGORDER => [grep(/^F/, FLAGS)],
    # NO-BREAK SPACE and FIGURE SPACE are used because they count as \h (horizontal white space)
    MAGIC_WS => ("\N{NO-BREAK SPACE}" x 4) . "%b\N{FIGURE SPACE}",
    MAGIC_TOKEN => "\$MT\$_%b__",
    MAGIC_RE => qr/(?|\N{NO-BREAK SPACE}{4}([ \t]++)\N{FIGURE SPACE}|\$MT\$_([01]++)__)/,
    FPROTO_RE => qr@
    (?>                         # assert that the prototype starts in a good place.
        \A                      # either the beginning of the string...
    |
        (?<=[});])              # ...or following a valid syntax character. (possibly: the end of a previous decl/def)
    )
    \s*+                        # some number of spaces following that...
    (?>\A|
        (?<=(?<fpre>\s))        # the last space we preserve, to be examined later
    )
    \K
    (?<fproto>                  # Group <fproto>: full prototype, without trail/delim
        (?<rtype>               # Group <rtype>: return type (with space)
            (?(R&fproto)|(?>    # some keywords can look a lot like function declarations. They're not.
                (?![\s\w*]*\b(?:
                   return
                  |else
                ))              # if we're matching one of these, then NOT ONLY do we want to fail the match...
            |
                # \W*
                (?&fproto)      # ...but we also want to find whatever might have gotten matched by fproto (hence the conditional above)...
                (*SKIP)         # and make sure none of these characters EVER get matched by this search.
            ))
            \b
            (?:struct\s+)?+     # The struct keyword is a bit special. It *can* start a function rtype decl, but only if a word comes after it.
            (?=[^\W\d])         # function defs and decls always start with a non-digit word character, usually at BOL
            (?>
                [A-Z_]+\([\w\s*]*+\)  # COMMON() etc return type tag
            )?+
            (?>
                __declspec\([^)]*+\)
            |   __attribute__\(\(.*?\)\)
            |   [\w*]
            )*+                 # rval types can have word chars, spaces, or *...
            \s                  # ...but there must be some space, somewhere
            [\w\s*]*
        )
        (?<fname>\b\w++)        # Group <fname>: function name
        (?<fargs>\(             # Group <fargs>: function args (with parens)
            (?<ldpre>(?:        # Group <ldpre>: args before a possible DECL_LD etc
                (?!(?&lddecl))  # Nothing that would match <lddecl>, but:
                (?>
                    [^()]       # non-paren arg contents...
                |   (?&fargs)   # ...or parenthesized subexpression
                )
            )*+)
            (?<lddecl>          # Group <lddecl>?: possible DECL_LD or the like
                DECL_LD[{; ]?
            |   [^\S\N{FIGURE SPACE}]*\bARG1?_LD(?=[,)])    # don't skip through comments
            )?+
            (?<ldpost>          # Group <ldpost>: args after LD decl
                (?>
                    (?>[^()]++) # non-paren arg contents...
                |   (?&fargs)   # ...or parenthesized subexpression
                )*+
            )
        \))
    )
    (?<ftrail>[\w\s]*)          # Group <ftrail>: expect a possible keyword like WUNUSED after function prototype...
    (?<fdelim>[;{])             # Group <fdelim>: ..and conclude with semicolon or open-brace.
    @mx,
    IFBLOCK_RE => qr/
    (?<ifblock>(?>          # Group 1: full #if block
        (?>
            ^\#\h*+if\N*+\n # opening #if or #ifn?def
        )
        (?>
            (?<ifline>(?>   # Group 2: irrelevant line...
                (?!\#\h*+(?:if|el|endif)) # line doesn't start with #if, #el, or #endif
                (?=.)\N*+\n?+
            |
                (?&ifblock) # ... OR full #if block
            ))*+
            \#\h*+elif\N*+\n
        )*+
        (?>
            (?&ifline)*+
            \#\h*+else\N*+\n
        )?+
        (?&ifline)*+
        \#\h*+endif\N*+$(?:\n)?+
    ))/msx,
};

use Carp;
use Data::Dumper;
use List::Util qw( uniqstr );

our %gfuncs;
our %gfmap;

sub info {
    print STDERR @_;
}

sub debug {
    return if -t STDOUT;
    return 1 unless @_;
    info (@_, "\n");
}

sub pflag {
    my ($val) = @_;
    my @flags = grep {$val & FLAGMAP->{$_}} @{(FLAGORDER)};

    return join(' | ', @flags) . sprintf(" [0x%02x]",$val) if @flags;
    return "[$val]";
}

sub init {
    $Data::Dumper::Useqq = 1;
    $Carp::MaxArgLen = 20;

    # debug "full file list: @ARGV";
    my @hdrs = grep(/\.h$/, @ARGV);
    %gfuncs = map {$_ => F_APIFUNC} UNDOC_API;
    %gfmap = LD_MAP;
    return unless @hdrs;
    my $hdrcount = @hdrs;
    info "Reading $hdrcount headers...";
    # debug "headers: @hdrs";
    foreach my $hdr (@hdrs) {
        next if $hdr =~ m|^compat/|;
        my $is_api = $hdr =~ m/SWI-/ ? F_APIFUNC : 0;
        # debug "hdr: $hdr";
        open(HDR, $hdr);
        while(<HDR>) {
            getdefs(\%gfuncs, F_GLOBAL | $is_api);
        }
    }
    info "done.", debug ? "" : "\n";
    if (debug " Global functions:") {
        foreach my $func (sort keys %gfuncs) {
            info "  $func: ${\(pflag $gfuncs{$func})}\n";
        }
    }
}

sub to_magicws {
    my ($i) = @_;
    return sprintf(MAGIC_WS, $i) =~ y/01/ \t/r;
}

sub to_magictoken {
    my ($i) = @_;
    return sprintf(MAGIC_TOKEN, $i);
}

sub from_magic {
    my ($ws) = @_;
    $ws =~ s/[^ \t01]//g;
    $ws =~ y/ \t/01/;
    return oct("0b$ws");
}

sub get_ifpath {
    my ($text) = @_;
    return "" if $text eq "";
    my $pathre = qr@^#(?:if:\d+|el|end|mark)((?::\d++)*+):@ms;
    confess "pathre did not match $text" unless $text =~ $pathre;
    return $1;
}

sub mark_ifpath {
    my ($post, $lastc) = @_;
    my $path = get_ifpath $post;
    return "#mark${path}::\n" . ($lastc ? "#LASTC:$lastc\n": "");
}

sub normalize_file {
    # pulls out comments, line continuations, and strings, replacing them with magic sequences
    # also ensures a \n at the end of the code, and tags #if structures
    my ($text) = @_ ? @_ : ($_);
    confess "already normalized $text" if $text =~ /\0NORM$/;
    $text .= "\n" . mark_ifpath("") . "\0NORM"; # put an "outside every #if" mark at the end
    my $cmtre = qr!
        (?|
            (\\)\n
        |   
            (')(?>[^'\\]|\\.)*+'(*SKIP)(*FAIL) # don't match any of these characters again
        |
            (")(?>[^"\\]|\\.)*+"
        |
            (/\*).*?\*/
        |
            (//)\N*+
        )
    !xs;
    # not actually just comments but we'll go with the name
    my @comments = sort keys %{{ map {$_ => 1} ($text =~ /((?n:$cmtre))/g) }};
    my $i = 0;
    my %cmtids = map {$_ => $i++} @comments;
    $text =~ s/$cmtre/$1 eq '"' ? to_magictoken($cmtids{$&}) : to_magicws($cmtids{$&})/ge;
    # annotate #if/#el/#end with unique tag for easier matching
    $i = 0;
    my $depth = 0;
    my %depthmap = ();
    $text =~ s{^(#\h*+(if|el|end))}{
        "#$2:" . (
            $2 eq "if" ? ($depthmap{++$depth} = ($i++ . ":" . ($depthmap{$depth} // ""))) :
            $2 eq "end" ? $depthmap{$depth--} :
            $depthmap{$depth}
        ) . "::$1"
    }meg;
    confess "bad #if nesting for $text: $depth" if $depth != 0;
    # add #LASTC lines to avoid having to do previous-character matches across preprocs
    my $lastc = ';'; # semicolon is a safe character to imagine starts a file
    $text =~ s/
        (\S)(?=\s*^\#)      # Group 1: a lastc before a preproc line
    |
        (^\#\N*+\n)         # Group 2: a preproc line
        (?=\s*(\#\N*+\n))?  # Group 3 (opt): the following preproc line
    /
        $lastc = $1 if defined $1;
        ($2 && !$3) ? $& . "#LASTC:$lastc\n" : $&
    /gemx;
    # debug Dumper(\@comments);
    if (!@_) {
        # No-argument version modifies $_ and returns \@comments
        $_ = $text;
        return \@comments;
    }
    return ($text, \@comments);
}

sub denormalize_file {
    my ($text, $comments) = (@_ > 1) ? @_ : ($_, @_);
    my $origtext = $text;

    $text =~ s/^#(?:if|el|end|mark):[\d:]++\n?+//mg or die "could not remove pathmarks in $origtext";
    $text =~ s/^#LASTC:\S\n//mg;
    $text =~ s/${\MAGIC_RE}/$comments->[from_magic($1)]/ge or die "could not restore magic patterns in $origtext" if @$comments;
    $text =~ s/\n\0NORM\z// or die "could not remove ending mark in $origtext";
    $_ = $text if @_ == 1; # One-argument version modifies $_, like zero-argument normalize_file
    return $text;
}

sub normalize_ws {
    my ($text) = @_;
    $text =~ s/^\s*|\s*$//g; # no leading or trailing ws
    $text =~ s/(?<=\w)\s+(?=[^\w\s])|(?<=[^\w\s])\s+(?=\w)//g; # no ws in word=>punct or punct=>word transition
    $text =~ s/(?<=\w)\s+(?=\w)|(?<=[^\w\s])\s+(?=[^\w\s])/ /g; # exactly 1 space in word=>word or punct=>punct
    return $text;
}

sub getdefs {
    local $_ = $_;
    my ($funcs, $baseflags) = @_;
    $baseflags //= 0;

    normalize_file;
    s/\b(?:COMMON|PL_EXPORT)\(([^()]*)\)/$1/g; # strip COMMON() or PL_EXPORT()

    while (/(?>
                ^\#\h*define\h+
                (\w++)          # Group 1: macro name
                (?:\(([^()]*)\))?+ # Group 2 (opt): macro args
                (?:\h)+         # Horizontal whitespace separator
            )
            (?:
                (\w+)           # Group 3 (opt): target function name
                \(([^)\n]*)\)   # Group 4 (opt): target args
                \h*?$           # consume whitespace to EOL
            )?
           /gmx) {
        my ($sfunc, $sarg, $dfunc, $darg) = ($1, $2, $3 // "", $4 // "");
        my $is_ldalias = (
            $dfunc &&
            ($dfunc eq "${sfunc}__LD" || ($gfmap{$dfunc} // "") eq $sfunc) &&
            $darg =~ /PASS_LD/
        ) ? F_LDALIAS : 0;
        my $is_weird = (!defined $sarg || !$dfunc || (normalize_ws ($darg =~ s/\s*PASS_LD1?//r =~ s/__VA_ARGS__/.../r)) ne normalize_ws $sarg) ? F_ISWEIRD : 0;
        $funcs->{$sfunc} |= F_DEFINED | $baseflags | $is_ldalias | $is_weird;
        # my $desc = pflag($funcs->{$sfunc});
        # debug "#define: $sfunc($sarg) => $dfunc($darg) [flags: $desc]";
    }
    while (/${\FPROTO_RE}/g) {
        # print("function $1\n");
        my ($space, $fname, $fargs, $fdelim) = ($+{fpre}, $+{fname}, $+{fargs}, $+{fdelim});
        if ($space ne "\n") {
            my ($fdelim, $fproto, $fline) = ($+{fdelim}, $+{fproto}, $`);
            $fproto =~ s/\n(?:\h*)/\\n /g;
            $fline =~ s/^.*\n//s;
            $fline .= $fproto =~ s/\(.*//sr;
            next if $fline =~ /^#/;
            debug "    function $fproto $fdelim starts after column 0: $fline" if $baseflags == 0 and $fargs =~ /ARG1?_LD/;

        }
        my $has_ld = $fargs =~ /ARG1?_LD/ ? F_HASLD : 0;
        my $is_fdef = $fdelim eq "{" ? F_HASDEF : F_HASDECL;
        $funcs->{$fname} |= F_DEFINED | $baseflags | $has_ld | $is_fdef;
    }
}

sub partition_ifblocks {
    my ($text) = @_;
    my ($pre, $post);
    # Return the beginning invalid (unopened #if), valid text, and ending invalid (unclosed #if).
    ($pre, $text, $post) = $text =~ /
        \A
        (                                   # Group 1: unopened #if
            (?>
                (?&ifline)
            |   \#\h*+(?:el|endif)\N*+\n
            )*
            \#\h*endif\N*+\n
        )?+
        (                                   # Group 2: valid text (with, potentially, #else or #elif)
            (?>
                (?&ifline)
            |   \#\h*+el\N*+\n
            )*+
        )
        (                                   # Group 3: unclosed #if
            \#\h*if\N*+\n
            (?>
                (?&ifline)
            |   \#\h*+(?:if|el)\N*+\n
            )*
        )?+
        \z
        (?(DEFINE)${\IFBLOCK_RE}) # pull in ifline definition, don't match anything
    /x;
    # print STDERR Dumper($_[0]) if !defined $text;
    die "Bad regex in partition_ifblocks()" if !defined $text;
    if ($text =~ /^(.*?\n)(#\h*el.*)$/s) {
        # If we have an #else or #elif, then $text isn't valid after all, split it between $pre and $post
        ($pre, $text, $post) = (($pre // "") . $1, "", $2 . ($post // ""));
    }
    return ($pre, $text, $post);
}

sub expand_to_ifblocks {
    my ($pre, $text, $post) = @_;
    return @_ if !defined $text;
    my ($spre, $spost);
    # Expand outward until we're not spanning an #if/#else/#endif

    my ($spath) = get_ifpath($text.$post);
    my ($epath) = get_ifpath($post);

    return @_ if $spath eq $epath;

    # If the start and end paths aren't the same, then we need to
    # expand to within the block defining the longest common suffix
    my ($cpath) = "$spath\0$epath\0" =~ /((?=[:\0])[\d:]*)\0.*\1\0$/;
    confess "no cpath for [$spath/$epath]" if !defined $cpath;

    if ($spath ne $cpath) {
        $spath =~ s/\A.*(:\d+$cpath)\z/$1/ or confess "bad spath/cpath [$spath/$cpath]"; # get cpath-plus-child
        $pre =~ /^#if${spath}::.*/ms or goto unclean;
        ($pre, $text) = ($`, $& . $text);
        info "{";
    }

    if ($epath ne $cpath) {
        $epath =~ s/\A.*(:\d+$cpath)\z/$1/ or confess "bad epath/cpath [$epath/$cpath]"; # get cpath-plus-child
        $post =~ /\A.*?#end${epath}::\N++\n/ms or goto unclean;
        ($text, $post) = ($text . $&, $');
        info "}";
    }

    return ($pre, $text, $post);

    # Otherwise, we can't get a clean expansion. Split the text into pre and post and let caller handle it.
    unclean:

    info "X";
    ($pre, $text, $post) = @_;
    ($spre, $text, $spost) = partition_ifblocks($text); # Find the biggest "clean" range in $text
    # lump any invalid fragments in with $pre/$post
    $pre .= $spre // "";
    $post = ($spost // "") . $post;
    return ([$pre, $text], undef, ["", $post]);

    # info Dumper("start text: $text");
    while (($spre, undef, $spost) = partition_ifblocks($text) and defined ($spre // $spost)) {
        # info Dumper {spre=>$spre, spost=>$spost};
        if (defined $spre) {
            info "<";
            $pre =~ /^(.*\n)(#\h*+if.*+)$/s or confess "Expected #if in pre";
            ($pre, $text) = ($1, $2 . $text);
        }
        if (defined $spost) {
            info ">";
            $post =~ /\A(.*?^#\h*+endif\N*+\n?+)(.*)\z/ms or confess "Expected #endif in post";
            ($text, $post) = ($text . $1, $2);
            # info Dumper {text=>$text,post=>$post,1=>$1, 2=>$2};
        }
        # ($pre, $text, $post) = expand_to_comments($pre, $text, $post); # in case we cut to inside a comment
        return expand_to_ifblocks($pre, $text, $post);
    }
    return ($pre, $text, $post);
}

sub expand_to_blank_line {
    my ($pre, $text, $post) = @_;
    return @_ if !defined $text;
    my $oldtext = "";
    # Expand outward until there's a blank line just before the start and after the end,
    # if possible, but don't exit the current ifpath context
    my $spath = get_ifpath($text.$post);
    my $epath = get_ifpath($post);
    confess "inconsistent path in etbl, [$spath/$epath]" if $spath ne $epath;
    my $line_re = qr/
        (?!\#(?:if|el|end)${spath}::)   # NOT a line that starts or ends this ifpath
        ^[ \t]*+[^ \t\n]\N*+\n          # must have a non-space character in there somewhere
    /msx;

    while ($oldtext ne $text) {
        $oldtext = $text;
        # Special case: if we have a blank line between two function declarations,
        # at the beginning or end, subsume it into the text
        if ($text =~ /\A${\FPROTO_RE}(?<=;)/ and $pre =~ /(?=\s++^)${\FPROTO_RE}(?<=;)\n[ \t]*+\n\z/m) {
            info "¯";
            $pre = $`;
            $text = ($& . $text);
        }
        if ($text =~ /(?=\s++^)${\FPROTO_RE}(?<=;)[ \t]*+\n\z/m and $post =~ /\A(?=[ \t]*+\n)${\FPROTO_RE}(?<=;)\h*+\n/ms) {
            info "_";
            $text .= $&;
            $post = $';
        }
        # Using [ \t] here instead of \h because this is a textual check, not a syntactical
        if ($pre =~ /\A(.*^[ \t]*+\n)((?:$line_re)++)\z/ms) {
            info "^";
            ($pre, $text) = ($1, $2 . $text);
        }
        if ($post =~ /\A((?:$line_re)++)(^[ \t]*+\n.*)\z/ms) {
            info "\$";
            ($text, $post) = ($text . $1, $2);
        }
        ($pre, $text, $post) = expand_to_ifblocks($pre, $text, $post) if $oldtext ne $text;
        return ($pre, $text, $post) if !defined $text;
    };
    return ($pre, $text, $post);
}

sub split_decls {
    my ($pre, $split1, $split2, $post, $is_header, $funcs, $fmap) = @_;
    my $text;
    $split1 .= mark_ifpath($split2 . $post, $split1 =~ /(\S)\s*\z/); # pull in the path mark from the second half
    # Individually wrap the first side of the split
    ($pre, $text, $split1) = wrap_decls($pre . $split1, $is_header, $funcs, $fmap);
    # Wrap the second side, pulling in whatever of split1 wasn't used in the first
    ($split1, $split2, $post) = wrap_decls($split1 . $split2 . $post, $is_header, $funcs, $fmap);
    $text .= $split1 . $split2;
    return ($pre, $text, $post);
}

sub make_lddefs {
    return unless defined wantarray;
    my ($text, $funcs, $fmap) = @_;
    my @defs = ();
    my $maxlen = 0;
    while ($text =~ /${\FPROTO_RE}/g) {
        my ($fname, $fargs) = @+{qw(fname fargs)};
        # debug "$fname: $fargs\[$+{ldpre}|$+{ldpost}]";
        next unless $fargs =~ /^\(DECL_LD[;{]/; # check that this is in fact a DECL_LD prototype
        if ($fargs eq "(DECL_LD;)" || $fargs eq "(DECL_LD{)") {
            $fargs = "(_)"; # as far as the macro is concerned, at least
        }
        my @argnames = $fargs =~ /
            (?(R)                   # if we're recursing...
                (?>
                    (?<=\()         # ...then we must start just PAST a parenthesis...
                |   ,               # ...OR with a comma
                )
            |                       # if we're not recursing...
                \G                  # ...then start exactly at the end of the previous match...
                [(,]                # ...with an opening paren or comma
            )
            (?:
                (?<=\()             # if we just had an opening paren...
                (?:DECL_LD[;{])     # ...then it can have optional DECL_LD; following
            )?+
            (?>(?|                  # Arg matching. First try all type-identifier patterns.
                [\w*\s]*            # word, asterisk, or space defining arg (return) type
                (?|                 # only one numbered capture group, for matching in list context with g flag
                    \b
                    (\w++)          # Group 1a: simple arg name, starts on word boundary
                |
                    \( \s*+         # function prototype: ( * name ) (args...)
                    \* \s*+
                    (\w++)          # Group 1b: fpointer arg name
                    \s*+ \)
                    \s*+ \(         # expect opening parenthesis...
                    (?>(?R))*+      # ...recurse to get args, except the final close paren...
                    \)              # ... and then capture the close paren
                |
                    (\.\.\.)        # Group 1c: ellipsis, but only if this is the last arg
                    (?= \s*+ \) )
                )
                (?>
                    \s*+
                    \[ [\w\s]*+ \]
                )*+                 # possible array declarator(s)
                \s*+ (?=[,)])       # ending with either a , or a ) (not captured)
            |
                (?=(
                    [*\s]*+         # Group 1d: type only, no identifier.
                    \w              # a no-ident type *must* have a word character somewhere
                    [\w*\s]*+
                    [,)]            # and must extend to end of argument, which IS captured
                ))
                [\w*\s]++           # ...and then handed right back
            ))
        /gsx;
        @argnames = map {/(?|\b(\w++)\W*+\z|\A(\.\.\.)\z)/} @argnames; # get the last word-char token in the argument
        confess "bad proto match in make_lddefs for $fname: $fargs" if $' ne ")";
        my %seen = ();
        @argnames = map {$seen{$_}++ ? $_ . $seen{$_} : $_} @argnames; # rename any duplicate identifiers
        confess "duplicate arg names in make_lddefs: @argnames" if uniqstr(@argnames) != @argnames;
        $fargs = join ", ", @argnames;
        push @defs, ["$fname($fargs)", "LDFUNC($fname, ${\($fargs =~ s/\.\.\.\z/__VA_ARGS__/r)})"];
        my $mapped_name = mapped_ldname($fname, $funcs, $fmap);
        my $deflen = length("$mapped_name($fargs)");
        $maxlen = $deflen if $deflen > $maxlen;
        # debug join "::", @{$defs[-1]};
    }
    $maxlen = ($maxlen + 9) >> 3 << 3; # round up to nearest tab, assuming 2 extra spaces
    # debug "maxlen = $maxlen";
    confess "no defs in $text" if !@defs;
    return @defs if wantarray;
    for my $funcdef (@defs) {
        my ($name, $exp) = @$funcdef;
        my $mapped_name = mapped_ldname($name =~ s/\(.*//r, $funcs, $fmap);
        my $deflen = length($name =~ s/.*?(?=\()/$mapped_name/r);
        # debug "defining $name (len ${\(length $name)}) to $exp with tabs: ".(($maxlen - length($name) + 7) >> 3);
        $funcdef = "#define\t" . $name . "\t" x (($maxlen - $deflen + 7) >> 3) . $exp;
    }
    return join "\n", @defs;
}

sub wrap_decls {
    return unless defined wantarray;
    my ($text, $is_header, $funcs, $fmap) = @_;
    my ($pre, $post);
    # Slice the smallest enclosing range of declarations («DECL_LD;» means a prototype-only decl)
    info "[";
    ($pre, $text, $post) = $text =~ /((?>.*?^(?=\N*DECL_LD;)))(.*^\N*DECL_LD;.*?;\N*+\n?+)(.*)/ms;
    if (!defined $text) {
        info "]";
        return wantarray ? ("", "", $_[0]) : $_[0];
    }
    # info ".";
    # ($pre, $text, $post) = expand_to_comments($pre, $text, $post);
    info ".";
    ($pre, $text, $post) = expand_to_ifblocks($pre, $text, $post);
    info "." if defined $text;
    ($pre, $text, $post) = expand_to_blank_line($pre, $text, $post);

    if (!defined $text) {
        # One of the expand passes threw an error. Split ($pre and $post will be arefs) and try again.
        ($pre, $text, $post) = split_decls(@$pre, @$post, $is_header, $funcs, $fmap);
    } elsif (my ($split1, $split2) = $text =~ /^(.*?\n\n)((?:\N*\n)*\N*?DECL_LD\{.*)/s) {
        # If we have any «DECL_LD{», that means we're mixing prototypes and definitions. Split.
        info "!";
        ($pre, $text, $post) = split_decls($pre, $split1, $split2, $post, $is_header, $funcs, $fmap);
    } else {
        # no need to split, so perform wrapping (if appropriate)
        if ($is_header
            or $text =~ /(?:(?=\s*^)${\FPROTO_RE}(?<=;)){2}/m # two declarations in a row constitutes a "declaration block"
        ) {
            my $lddefs = make_lddefs($text, $funcs, $fmap);
            my ($lastc) = $text =~ /(\S)\s*\z/;
            $text =~ s/DECL_LD[; ](?=(.))/$1 eq ")" ? "void" : ""/ge;
            $text = <<~EOT;
                #if USE_LD_MACROS
                $lddefs
                #endif /*USE_LD_MACROS*/
                
                #define LDFUNC_DECLARATIONS
                
                $text
                #undef LDFUNC_DECLARATIONS
                #LASTC:$lastc
                EOT
        }
    }
    info "]";

    return wantarray ? ($pre, $text, $post) : "$pre$text$post";
}

sub valid_func {
    my ($fname, $funcs) = @_;
    my $fflags = $funcs->{$fname};
    return $fname if !$fflags; # If there is no function defined for this name, it's valid
    return undef if ($fflags & F_ISWEIRD) != 0; # Never rename to something with a weird define
    return $fname if ($fflags & (F_APIFUNC | F_LDALIAS)); # API funcs don't collide, and LD aliases will be removed
    return undef; # Otherwise, this is invalid
}

sub mapped_ldname {
    my ($fname, $funcs, $fmap) = @_;
    return $fmap->{$fname} if defined $fmap->{$fname};
    return $fname unless $fname =~ s/__LD$//;
    return valid_func($fname, $funcs) || valid_func("f_$fname", $funcs) || $fname;
}

sub rename_ldfuncs {
    my ($funcs, $fmap) = @_;
    my %fmap = %$fmap;

    # Rename __LD-suffix tokens
    while (/\b(\w+)__LD\b/g) {
        my ($fname, $ldname) = ($1, $&);
        next if defined $fmap{$ldname};
        $fmap{$ldname} = mapped_ldname($ldname, $funcs, $fmap);
        info "    $ldname unchanged!\n" if $ldname eq $fmap{$ldname};
        info "    $ldname => $fmap{$ldname}\n" if $fname ne $fmap{$ldname} and $ldname ne $fmap{$ldname};
    }
    # Any multiline prototypes of __LD funcs should have their indentation shifted properly
    s/${\FPROTO_RE}/
        substr($+{fname}, -4) ne "__LD" ? $& : do {
            my $mapname = $fmap{$+{fname}};
            my $delta = length($mapname) - length($+{fname});
            $& =~ s!\n\t\K(\t*)([ ]*)(?=[^\t ])!
                "\t" x (length($1) * 8 + length($2) + $delta >> 3) . " " x ((length($2) + $delta) % 8)
        
            !ger
        }
    /ge;

    s/\b\w+__LD\b/$fmap{$&}/ge;
}

sub gen_bare_lddefs {
    my ($funcs, $fmap) = @_;
    # operates on $_
    my %seen = ();

    while (/^\#\h*+define\h++(\w++)\(\N*LDFUNC/mg) {
        info("#");
        $seen{$1}++;
    }

    s/${\FPROTO_RE}/
        ( !$+{lddecl}
        ? ""
        : (($funcs->{$+{fname}} || 0) & F_HASDECL)
        ? (info("~"), "")
        : !($seen{$+{fname}}++)
        ? (info("."), sprintf("#define %s %s\n", @{(make_lddefs($&, $funcs, $fmap))[0]}))
        : (info("^"), "")
        ) . $&
    /gem;
}

sub gen_api_stubs {
    my ($funcs) = @_;
    # operates on $_
    return 1 if s@
    (?>
        ^\#undef\h+(\w+)\s*\n    # Group 1: name of undef
    )(?>
        (.*?(?<=\n))        # Group 2: intervening content, newline
        (\w[\w\h*]*)\n      # Group 3: rtype; can contain word chars, horizontal spaces and *
        (\1)                # Group 4: function name
        (\([^()]*\)\s*?\n)  # Group 5: function args (with parens and newline)
    )(?>
        \{\h*+(GET_LD\s+(?!^)|) # Group 6: GET_LD, if present
        (                   # Group 7: function contents
            (?:
                (?!\})
                \N*+\n
            )*?
            \N*\S
        )
        \s*\n
        \}
    )
    (?|(?:
        (.*?\n)             # Group 8: intervening content, if redefined
        ^\#define\h+\1\(.*?(?<!\\)\n
    )|())
    @
        (($funcs->{$1}//0) & F_APIFUNC) && $6 && split(/\n+/, $7) <= 5
            ? (info("."), "$2API_STUB($3)\n($4)$5( $7\n)$8") # If this is a small API function with a GET_LD, wrap in stub macro
        : (($funcs->{$1}//0) & F_APIFUNC)
            ? (info("_"), "$2$3\n($4)$5\{ $6$7\n\}$8") # If this is an API func without GET_LD, still wrap the name in parens
            : (info("!$1"), "//LDREF$&")                 # Otherwise, prevent this #undef from matching again
    @gmsex;         # True return means more changes possible

    s!^//LDREF!!gm; # Remove //LDREF markers
    s/
        ^API_STUB\([^()\n]*+\)\n
        \(\w++\)\([^()\n]*+\)\n
        \( \N*\S
        \K # keep everything up until the end of the one line
        \h*\n\)
    / )/gmx; # Simplify one-liner API stubs
    return;
}

BEGIN {
    init();
}

next if $ARGV =~ m!SWI-|^compat/!; # skip API and compat headers
next if $ARGV eq "pl-builtin.h"; # skip where LD et al get defined

# -inline.h files are more source files than headers, in terms of code organization
my $is_header = ($ARGV =~ /(?<!-inline)\.h$/ ? 1 : 0);
info "start file $ARGV is_header $is_header\n";
my %lfuncs = %gfuncs;

getdefs \%lfuncs, 0;

my $cmtlist = normalize_file;

s/ALLOC_INLINE\(([^()]*)\)(?=\s)/ALLOC_INLINE $1/g; # ALLOC_INLINE has changed from function-like to object-like macro

s!^#define\s+(\w+)\(([\w, ]+)\)\s+(\w+__LD)\(([\w, ]+?) +PASS_LD\)\s*?\n!($3 eq $1."__LD" || ($gfmap{$3} // 0) eq $1) && normalize_ws($2) eq normalize_ws($4) ? "" : $&!gme; # remove PASS_LD defs
s!^#define\s+(\w+)\(([\w, ]+), \.\.\.\)\s+(\w+__LD)\(([\w, ]+?) +PASS_LD, __VA_ARGS__\)\s*?\n!($3 eq $1."__LD" || ($gfmap{$3} // 0) eq $1) && normalize_ws($2) eq normalize_ws($4) ? "" : $&!gme; # remove va PASS_LD defs
s!^#define\s+(\w+)\(\)\s+(\w+__LD)\(PASS_LD1\)\s*?\n!($2 eq $1."__LD" || ($gfmap{$2} // 0) eq $1) ? "" : $&!gme; # remove PASS_LD1 defs

s/(\(\*\s*+\w++\s*+\))\(([^()]*)(?<=\S)\s*ARG_LD\)/LDFUNCP $1(DECL_LD $2)/g; # LDFUNC ptr type declarations
s/(\(\*(?:\(\w++\))?[^()]++\))\(([^()]*\S)\s*PASS_LD\)/LDFUNCP$1($2)/g; # LDFUNC ptr usages

# Look at all wordchar tokens with at least one underscore, that follow
# a [,{&], that aren't followed by an open paren or comma (as in PRED_IMPL)
# or a pointer/struct operator. Any that are LDFUNCs, wrap in LDFUNC_REF.
s@[,{&]\s*+\K\w{10,}+(?!\s*(?:[(,.]|->))@
    (($lfuncs{$&}//0) & F_HASLD)
    ?   "LDFUNC_REF($&)"
    :   $&
@ge;

s/#\h*define\h*\w+\((?:\h*\w+\h*,)*\h*(\w+)(?:\h*,\h*\w+)*\h*\)\N*[,(]\h*\K\(\1\)\h*PASS_LD(?=\h*[,)])/$1/g; # Remove all PASS_LD annotations following a parenthesized macro argument and unwrap the arg
s/(?:\s*,)?\s*PASS_LD1?\s*(?=[,)])//g; # Remove all PASS_LD annotations (there shouldn't be any leading commas, but if there are, remove them)

if (/ARG1?_LD/) { # skip the rest of this file if it has no LD decls
    s/(?=\s*COMMON\()${\FPROTO_RE}/$& =~ s!\n\t!\n!gr/ge; # COMMON() removal, shift continuation lines back by one tab
    s/COMMON\(([^()]*)\)/$1/g;

    info "  Generating API stubs...";
    info ";" while gen_api_stubs \%lfuncs;
    info "\n";

    # Add DECL_LD; at the beginning of any function prototypes, in place of ARG1?_LD
    s/${\FPROTO_RE}/
        $+{lddecl}
        ? join "", (
            @+{qw{rtype fname}},
            "(DECL_LD$+{fdelim}",
            @+{qw{ldpre ldpost}},
            ")",
            @+{qw{ftrail fdelim}},
        )
        : $&
    /gem;

    info "  Inserting LDFUNC_DECLARATIONS...";
    $_ = wrap_decls($_, $is_header, \%lfuncs, \%gfmap);
    info "\n";

    info "  Adding bare LDFUNC defines...";
    gen_bare_lddefs \%gfuncs, \%gfmap; # use %gfuncs as an initial "seen" roster
    info "\n";

    s/DECL_LD\K[;{](?=(.))/$1 eq ")" ? "" : " "/gse; # remove ;{ annotation from DECL_LDs
}

denormalize_file $cmtlist;

info "  Renaming __LD identifiers...\n" if /\b\w+__LD\b/;
rename_ldfuncs \%lfuncs, \%gfmap;

s!^\s+/\*+\s+\*\s+LD-USING FUNCTIONS\s+\*\s+\*+/\s+^!\n!mg; # remove LD banner

# last pass to fix ARG_LD/PASS_LD, for comments
s/\(([^()]*\S) ARG_LD\)/(DECL_LD $1)/g;
s/\(ARG1_LD\)/(DECL_LD)/g;
s/\s*PASS_LD1?\s*(?=[,)])//g;

s/moved to pl-fli-inline\.h/moved to pl-fli.h/g if $ARGV eq "pl-fli.c"; #cleanup

# END { debug "end"; }