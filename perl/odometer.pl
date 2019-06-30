#!/usr/bin/perl

=head1 NAME

odometer.pl - filters strings of digits into alphanumeric string via the T9 telephone keypad

=head1 SYNOPSIS

    usage: [perl] odometer.pl

=head1 DESCRIPTION

Read strings of digits from STDIN, one line at a time.
Convert each string of digits into a set of alphanumeric strings via the T9 telephone keypand
using odometer algorithm and write the set of results to STDOUT.

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
# Generate strings by constructing a kind of odometer.
sub odometer (_) {
    my  $digits = $_;
    my  @wheels = ( );
    
    # Build the 'odometer' with different wheels at each position
    # based on the chars mapped from the original string.
    for (my $i = 0; $i < length($digits); $i++) {
        my  $focus = substr($digits, $i, 1);
        my  $chars = $T9{$focus} || [ $focus ];
        
        $wheels[$i] = {
            click => 0,
            chars => $chars
        };
    }
    
    my  @result = ( );
    my  $which  = 0;
    
    while ($which >= 0) {
        # Next string is from current state of 'odometer'.
        push @result, join '', map { $_->{chars}->[$_->{click}] } @wheels;
        
        # Start clicking wheels on the right.
        $which = scalar(@wheels) - 1;
        
        while ($which >= 0) {
            my  $wheel = $wheels[$which];
            
            $wheel->{click}++;
            
            last # ready to push out the next one, no overflow
                if $wheel->{click} < scalar @{$wheel->{chars}};
            
            # Zero this wheel and focus on the one to the left.
            $wheel->{click} = 0;
            $which--;
        }
    }
    
    @result
}

###########################################################################
###########################################################################

my  $app = $0 =~ m|.*/(.*)| ? $1 : 'odometer.pl';

warn "### Starting $app $VERSION\n";

while (<STDIN>) {
    chomp;
    
    my  @result = odometer $_;
    
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
