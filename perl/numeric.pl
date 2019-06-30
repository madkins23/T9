#!/usr/bin/perl

=head1 NAME

numeric.pl - filters strings of digits into alphanumeric string via the T9 telephone keypad

=head1 SYNOPSIS

    usage: [perl] numeric.pl

=head1 DESCRIPTION

Read strings of digits from STDIN, one line at a time.
Convert each string of digits into a set of alphanumeric strings via the T9 telephone keypand
using numeric algorithm and write the set of results to STDOUT.

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
# Generate strings by counting using variable bases per column.
sub numeric (_) {
    my  $digits  = $_;
    my  @columns = ( );
    my  $maximum = 1;
    
    # Build the digit base map and character mapping
    # based on the chars mapped from the original string.
    for (my $i = 0; $i < length($digits); $i++) {
        my  $focus = substr($digits, $i, 1);
        my  $chars = $T9{$focus} || [ $focus ];
        
        $columns[$i] = $chars;
        $maximum *= scalar @$chars;
    }
    
    my  @result = ( );
    
    # Now just count.
    for (my $i = 0; $i < $maximum; $i++) {
        my  $number = $i;
        my  @temp   = ( );
        
        for (my $col = scalar(@columns)-1; $col >= 0; $col--) {
            my  $chars   = $columns[$col];
            my  $divisor = scalar @$chars;
            
            unshift @temp, $chars->[$number % $divisor];
            $number /= $divisor;
        }
        
        push @result, join '', @temp;
    }
    
    @result
}

###########################################################################
###########################################################################

my  $app = $0 =~ m|.*/(.*)| ? $1 : 'numeric.pl';

warn "### Starting $app $VERSION\n";

while (<STDIN>) {
    chomp;
    
    my  @result = numeric $_;
    
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
