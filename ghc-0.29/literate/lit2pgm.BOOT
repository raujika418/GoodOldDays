eval "exec perl -S $0 $*"
     if $running_under_some_random_shell;
#
#############
# magic version for booting...
###########

$Pgm = $0; $Pgm =~ s/.*\/([^\/]+)$/\1/;

# an initial -A<name> option overrides the name
# 94/07: ignored [WDP]
if ($#ARGV >= 0 && $ARGV[0] =~ /^-A(lit2)(pgm|texi|latex)$/) {
    $Pgm = $1.$2;
    shift(@ARGV);
}
$Lit2pgm        = 1;

# $Lit2what will be set after we know $Quick_lit2pgm

$Status    = 0; # just used for exit() status

do 'getopts.pl' || die "Giant error 'do'ing getopts.pl: $@";
$Usage  = "usage: $Pgm ".
          '[-i input-file] '.
          '[-I LITINPUTS-path-additions] '.
          '[-o output-file] ';
$Usage .= '[-cdnqv] '.
          '[-r ribbons] '.
          "[input-file]\n" if $Lit2pgm;

if ( $Lit2pgm       && ! &Getopts('cdH:I:i:no:qr:v') ) {
    print STDERR $Usage;
    exit 1;
} elsif ($#ARGV > 0) {
    print STDERR "$Pgm: cannot have more than one input file: @ARGV \n";
    print STDERR $Usage;
    exit 1;
}
# SHARED OPTIONS
$Debugging      = ($opt_d) ? 1 : 0;
$Verbose        = ($Debugging | $opt_v) ? 1 : 0;
$Input_file     = ($opt_i) ? $opt_i : '';
# -o <output file> handled after deciding if linking

$Litinputs      = '.';
$Litinputs_adds = ($opt_I) ? $opt_I : '';
$Litinputs      = "$Litinputs_adds:$Litinputs" if $Litinputs && $Litinputs_adds;

# where all the assisting code is found (HARDWIRED)
if ($opt_H) {
    # no error checking; you get what you deserve
    ($LIB_ARCH_DIR,$LIB_DIR) = split(/:/,$opt_H);
} else {
# boot changes...
    $LIB_ARCH_DIR   = '.';
    $LIB_DIR        = '.';
}
unshift(@INC, $LIB_DIR); # adding to; for .prl libraries sucked in at runtime
#
# now the (derived) names of all the helpers
$Lit_deatify    = "$LIB_ARCH_DIR/lit-deatify";

# finish knowing myself; must agree with lit-deatify.llex
# boot change
$Quick_lit2pgm  = 1; # NB!!! ($opt_q) ? 1 : 0;
$Lit2what = 1;

# LESS SHARED OPTIONS
$Follow_inputs  = ($opt_n) ? 1      : 0;

$Ribbons_to_get = ($opt_r) ? $opt_r : 'main';
$Ribbons_to_get =~ s/\s*,\s*/,/g; # remove spaces around commas

# figure out what we're doing and the input and output files...
if ( ! $Input_file ) {
    if ($#ARGV == 0) {
        $Input_file = $ARGV[0];
    } else {
        $Input_file = '-'; # stdin; hmm...
    }
}

($Inputfile_root,$Inputfile_suff) = &root_and_suffix($Input_file);

# are we "processing" and/or "linking" ?
if ( ! ($Lit2texi || $Lit2latex)) { # that's easy...
    $Processing = 1;
    $Linking    = 0;
} else {
    $Processing = 1 if $Inputfile_suff !~ /^(itxi|itex|texi|tex)$/;
    $Linking    = ($opt_c) ? 0 : 1;
}

# table of input-suffixes -> output-suffixes (for "processing");
# cf. tables in &check_language_stuff
$ISUFF2OSUFF{'lit'}     = '';
$ISUFF2OSUFF{'lhs'}     = 'hs';
$ISUFF2OSUFF{'lit.hs'}  = 'hs';
$ISUFF2OSUFF{'lprl'}    = 'prl';
$ISUFF2OSUFF{'llex'}    = 'lex';

# guess the output filenames (for "processing" and "linking")
if ( $opt_o ) {
    if ( $Linking ) {
        $Link_outfile = $opt_o;
    } else {
        $Proc_outfile = $opt_o;
    }
}
if ( !defined($Proc_outfile) ) {
    if ($Linking) { # got to put it _somewhere_
        $Proc_outfile = "/usr/tmp/lit2stuff-po.$$";

    } elsif ($Lit2pgm) { # try to be clever
        if (defined($ISUFF2OSUFF{$Inputfile_suff})) {
            $Proc_outfile  = $Inputfile_root;
            $Proc_outfile .= '.'.$ISUFF2OSUFF{$Inputfile_suff}
                if $ISUFF2OSUFF{$Inputfile_suff};
        } else {
            print STDERR "$Pgm: don't know the file suffix: $Inputfile_suff\n";
            $Proc_outfile = '-'; # stdout will do
        }

    } elsif ($Lit2texi) {
        $Proc_outfile  = $Inputfile_root . '.itxi';

    } elsif ($Lit2latex) {
        $Proc_outfile  = $Inputfile_root . '.itex';

    } else {
        $Proc_outfile = '-'; # stdout
    }
}
# messing with Link_outfile deferred to later...

&check_language_stuff();
exit $Status if $Status; # don't proceed if errors

print STDERR "Language info: code=$Code_lang, xref=$Lang_xref, ",
                "typeset=$Lang_typeset, pagebreak=$Lang_pagebreak\n"
    if $Verbose;

# NOW WE START DRIVING...

# first: we handle "quick" lit2pgm
if ($Quick_lit2pgm) {

    # check that no incompatible lit2pgm options were used
    die "Can only get ribbon 'main' with lit2pgm -q option\n"
        if ($Ribbons_to_get ne 'main');
    die "Can't follow \\input{}s with lit2pgm -q option\n"
        if ($Follow_inputs);

    $pipe_string = "$Lit_deatify $Verbose 0 $Lit2what $Input_file";
    print STDERR "QUICK LIT2PGM:in=$pipe_string\n" if $Verbose;
    print STDERR "             out=$Proc_outfile\n" if $Verbose;
    &do_std_opens($pipe_string,$Proc_outfile);

    # this "quick" loop does as little as possible
    while (<INPIPE>) {
        if ( /^srcfile!_!(.*)!_!(.*)!_!/) {
# boot change:
#            print &mk_line_directive($1,$2);
;
        } else {
            print $_ ;
        }
    }
    &do_std_closes();
    exit $Status;
}

# continue with various kinds of "processing" ...

if ($Lit2pgm) {
    print STDERR "LIT2PGM:out=$Proc_outfile\n" if $Verbose;
    &spit_out_ribbons_code(split(/,/, $Ribbons_to_get));
    &do_std_closes();
    exit $Status;
}

sub check_language_stuff {

    # fixed tables of "language"-related info

    $ISUFF2LANG{'lit'}  = 'none';
    $ISUFF2LANG{'lc'}   = 'c';
    $ISUFF2LANG{'lhs'}  = 'hs';
    $ISUFF2LANG{'lit.hs'}='hs';
    $ISUFF2LANG{'lprl'} = 'prl';
    $ISUFF2LANG{'llex'} = 'lex';

    $STD_LANGS          = 'none,c,hs,prl,lex'; # fully supported (?)

    $XREF_HINTS         = ''; # could have more...
    $PAGEBREAK_HINTS    = '';
    $TYPESET_HINTS      = '';

    $Code_lang =
        ($opt_l) ? $opt_l 
                 : (defined($ISUFF2LANG{$Inputfile_suff})) ? $ISUFF2LANG{$Inputfile_suff}
                                                           : 'none';
    $Lang_pagebreak = ($opt_p) ? $opt_p : '';
    $Lang_typeset   = ($opt_t) ? $opt_t : '';
    $Lang_xref      = ($opt_x) ? $opt_x : '';

    &not_OK("","","xref hints unknown: $Lang_xref\n")
        if  $Lang_xref && index( $XREF_HINTS, $Lang_xref ) < 0;
    &not_OK("","","pagebreak hints unknown: $Lang_pagebreak\n")
        if  $Lang_pagebreak && index( $PAGEBREAK_HINTS, $Lang_pagebreak ) < 0;
    &not_OK("","","typeset hints unknown: $Lang_typeset\n")
        if  $Lang_typeset && index( $TYPESET_HINTS, $Lang_typeset ) < 0;
}
# line 472 "lit2stuff.lprl"
sub do_std_opens { # opens INPIPE, and OUTF; selects the latter

    local($pipe_string,$output_file) = @_;

    # check it output is to a pipe
    $output_file = ">$output_file" if $output_file !~ /^\|/;

    open(INPIPE, "$pipe_string |")
        || die "$Pgm: can't start prefilter pipe: $pipe_string\n";
    open(OUTF, "$output_file")
        || die "$Pgm: can't open output file: $output_file\n";
    select(OUTF);
}

sub do_std_closes { # closes INPIPE and OUTF; re-selects STDOUT

    close(INPIPE);
    # most regrettably, this only tells if the _last_ thing
    # in the pipeline croaked...
    if ($? >> 8) {
        print STDERR "$Pgm: error(s) from prefilter pipe\n";
        unlink("/usr/tmp/lit2stuff-po.$$"); # this might exist...
        exit 1;
    }
    close(OUTF);
    if ($? >> 8) {
        print STDERR "$Pgm: error in closing output file: $?\n";
        unlink("/usr/tmp/lit2stuff-po.$$"); # this might exist...
        exit 1;
    }
    select(STDOUT);
}

sub not_OK { # report error, increment $Status, but keep going
    local($srcfilename, $srclineno, $msg) = @_;
    if ($srcfilename) { # not always there
        print STDERR "$srcfilename:$srclineno: $msg";
    } else {
        print STDERR "$Pgm: $msg";
    }
    $Status++;
}

sub filter_string {
    local($filter, $string) = @_;

    open(FILT, "| $filter > /tmp/lit$$") || die "Can't open in filter_string\n";
    print FILT $string;
    close(FILT);
    if ($? >> 8) { # something went wrong in the pipe  ???
        print STDERR "$Pgm: error(s) in/from filter: $filter\n";
    }
    local($filtered_string) = `cat /tmp/lit$$`;
    unlink("/tmp/lit$$");
    $filtered_string;
}

sub deatified2verb_nl { # lit-deatify put 'em in; now we take 'em out
    local($_) = @_;

    s/\001newline\003/\n/g; # _WITH_ newlines
    s/\001lbrace\003/\{/g;
    s/\001rbrace\003/\}/g;
    s/\001rbracket\003/\]/g;

    $_;
}

sub deatified2verb { # without newlines
    local($_) = @_;

    $_ = &deatified2verb_nl($_);

    s/\n/ /g;   # the difference; to SPACES
    $_;
}

sub de_newlined {
    local($_) = @_;

    s/\n/\001newline\003/g;
    $_;
}

sub root_and_suffix {
    local($filename) = @_;
    local($root,$suffix);

    # find the file's "suffix" (NB: *.lit.hs has a 6-letter suffix)
    if ($filename =~ /\.(lit\.[^\.\/]+)$/) { # cf \input handling in reader
        $suffix = $1;
        ($root = $filename) =~ s/\.(lit\.[^\.\/]+)$//;

    } elsif ($filename =~ /\.([^\.\/]+)$/) {
        $suffix = $1;
        ($root = $filename) =~ s/\.([^\.\/]+)$//;

    } else {
        # should there be a warning or something here?
        $root = '???';
        $suffix = '???';
    }
    ($root, $suffix);
}

