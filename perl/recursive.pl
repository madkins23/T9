#!/usr/bin/perl

=head1 NAME

recursive.pl - filters strings of digits into alphanumeric string via the T9 telephone keypad

=head1 SYNOPSIS

    usage: [perl] recursive.pl

=head1 DESCRIPTION

Read strings of digits from STDIN, one line at a time.
Convert each string of digits into a set of alphanumeric strings via the T9 telephone keypand
using recursive algorithm and write the set of results to STDOUT.

=cut

use strict;
use warnings;

our %T9 = (
    '0' => [ '0' ],
    '1' => [ '1' ],
    '2' => [ 'A', 'B', 'C' ],
    '3' => [ 'D', 'E', 'F' ],
    '4' => [ 'G', 'H', 'I' ],
    '5' => [ 'J', 'K', 'L' ],
    '6' => [ 'M', 'N', 'O' ],
    '7' => [ 'P', 'Q', 'R', 'S' ],
    '8' => [ 'T', 'U', 'V' ],
    '9' => [ 'W', 'X', 'Y', 'Z' ]
);
our $VERSION = '1.0.0';

###########################################################################
###########################################################################
# Generate strings using a recursive routine.

sub recursive ($$$); # get rid of irritating warning:
# 'main::recursive() called too early to check prototype at t9 line XX.'
sub recursive ($$$) {
    my ($starting, $remaining, $indent) = @_;
    my  $focus  = substr($remaining, 0, 1);
    my  $tail   = length($remaining) > 0 && substr($remaining, 1);
    my  $chars  = $T9{$focus} || [ $focus ];
    my  @result = ( );
    
    for my $char (@$chars) {
        my  $stem = "$starting$char";
    
        if (length($tail) > 0) {
        	# This is 'line XX' from the irritating warning mentioned above.
            push @result, recursive($stem, $tail, "$indent  ");
        } else {
            push @result, $stem;
        }
    }
    
    @result
}

###########################################################################
###########################################################################

my  $app = $0 =~ m|.*/(.*)| ? $1 : 'recursive.pl';

warn "### Starting $app $VERSION\n";

while (<STDIN>) {
    chomp;
    
    my  @result = recursive "", $_, '';
    
    printf "%-25s%7d result%s\n", $_, scalar @result, @result == 1 ? '' : 's';
    
    my  $count = 79 / (length($_) + 1);
    
    while (@result) {
        print ' ';
    
        for (my $i = 0; $i < $count; $i++) {
            last
                unless @result;
            
            print ' ' . shift @result;
        }
        
        print "\n";
    }
}

warn "### Finished $app $VERSION\n";

###########################################################################
###########################################################################

=head1 AUTHOR

Marc M. Adkins <Perl-at-Doorways-dot-org>

=head1 COPYRIGHT

Copyright 2019 by Marc M. Adkins

=cut
