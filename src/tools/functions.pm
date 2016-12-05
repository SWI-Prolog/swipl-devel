#!/usr/bin/perl -w
# Author: Jan Wielemaker
# This code is in the public domain

use constant INITIAL => 0;
use constant HEAD_SEEN => 1;
use constant BODY => 2;

my $func = "";
my $file = "";
my $line = "";
my $state = INITIAL;
my $useword = 0;
my $usemark = 0;
my $type = "";

sub usage
{ if ( /\bWord\b/ ) { $useword++; }
  if ( /\bMark\(/ ) { $usemark++; }
  if ( /\bTmpMark\(/ ) { $usemark++; }
}

sub head
{ $state = HEAD_SEEN;
  $usemark = 0;
  $useword = 0;
  usage();
}

while (@ARGV)
{ $infile = shift @ARGV;

  if ( !open(IN, $infile) )
  { warn "Can't open $infile: $!\n";
    next;
  }

  $inline = 0;
  while (<IN>)
  { $inline++;
    chomp;

    if ( /^VMI\(([a-zA-Z0-9_]+),/ )
    { $func = $1;
      $file = $infile;
      $line = $inline;
      $type = "vmi";
      head();
      $useword++;
    } elsif ( /^PRED_IMPL\("([^"]+)",\s*([0-9]+)/ )
    { $func = "$1/$2";
      $file = $infile;
      $line = $inline;
      $type = "predicate";
      head();
    } elsif ( /^([a-zA-Z0-9_]+)\(/ )
    { $func = $1;
      $file = $infile;
      $line = $inline;
      $type = "function";
      head();
    } elsif ( /^{/ && $state == HEAD_SEEN && $inline == $line+1 )
    { $state = BODY;
    } elsif ( /^}/ && $state == BODY )
    { print "function('$func', $type, '$file', $line, $inline, $useword, $usemark).\n";
    }

    if ( $state == BODY )
    { usage()
    }
  }
}
