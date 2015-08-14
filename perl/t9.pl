#!/usr/bin/perl

=head1 NAME

t9.pl - filters strings of digits into alphanumeric string via the T9 telephone keypad

=head1 SYNOPSIS

    usage: [perl] t9.pl <flag>*
    <flag>   --method=<method>  # solution method (required)
             --debug=<integer>  # debug level (0..1) [1]
             --help             # short help
             --options          # longer help
             --manual           # full manual page
    <method> numeric            # functional numeric
             odometer           # functional structured
             recursive          # recursive
    # reads strings of digits from STDIN
    # writes arrays of alphanumeric strings to STDOUT

=head1 OPTIONS

=over

=item   C<--debug=>

Provide debug level for script.
By default, runs at debug level 1.
Setting debug level 0 makes script quieter.

=item   C<--method=<method>>

Specify method of generating the solutions:

=over

=item   C<numeric>

Generate solutions by counting and with each number using modulo
arithmetic to index into the character maps for each digit.

=item   C<odometer>

Generate solutions by tracking the state of each digit much like
an mechanical odometer, with each digit spinning and resetting
the digit to its left each time around.

=item   C<recursive>

Generate solutions using a recursive algorithm.
Use the execution stack to hold the counting state.

=back

=back

=head1 DESCRIPTION

=head1 EXAMPLES

=cut

use strict;
use warnings;

use Getopt::Long;
use Pod::Usage;

our %options = (
    method  =>  '<none>',
    debug   =>  1
);
our $VERSION = '0.0.1';
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
# Generate strings using a recursive routine.

sub recursive ($$); # get rid of irritating warning:
# 'main::recursive() called too early to check prototype at t9 line XX.'
sub recursive ($$) {
    my ($starting, $remaining) = @_;
    my  $focus  = substr($remaining, 0, 1);
    my  $tail   = length($remaining) > 0 && substr($remaining, 1);
    my  $chars  = $T9{$focus} || [ $focus ];
    my  @result = ( );
    
    for my $char (@$chars) {
        my  $stem = "$starting$char";
    
        if (length($tail) > 0) {
            push @result, recursive($stem, $tail); # 'line XX' from warning
        } else {
            push @result, $stem;
        }
    }
    
    @result
}

###########################################################################
###########################################################################

our %solution = (
    #######################################################################
    numeric => sub {
        numeric;
    },

    #######################################################################
    odometer => sub {
        odometer;
    },

    #######################################################################
    recursive => sub {
        recursive("", $_);
    }
);

###########################################################################

sub test {
    my  %counts = ( );
    my  $count  = 0;

    for my $name (keys %solution) {
        $count++;
        $counts{$_}++
            for &{$solution{$name}};
    }
    
    my  $max = scalar keys %solution;
    my  @bad = ( );
    
    for my $item (sort keys %counts) {
        push @bad, $item
            unless $counts{$item} == $max;
    }
    
    if (@bad) {
        print "*** Bad:\n";
        printf "***   %1d %-20s\n", $counts{$_}, $_
            for @bad;
    }
    
    sort keys %counts
}

###########################################################################
###########################################################################

pod2usage(2)
    unless GetOptions(\%options, qw(debug=i help options manual method=s test));

pod2usage(-verbose => 0) if $options{help};
pod2usage(-verbose => 1) if $options{options};
pod2usage(-verbose => 2) if $options{manual};

###########################################################################

my  $app = $0 =~ m|.*/(.*)| ? $1 : 't9.pl';

warn "### Starting $app $VERSION\n" if $options{debug};

my  $solution = $options{test} ? \&test : $solution{$options{method}};

die "*** No solution for method '$options{method}'\n"
    unless $solution;

while (<STDIN>) {
    chomp;
    
    my  @result = $options{test} ? test : &$solution;
    
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

warn "### Finished $app $VERSION\n" if $options{debug};

###########################################################################
###########################################################################

=head1 AUTHOR

Marc M. Adkins <Perl-at-Doorways-dot-org>

=head1 COPYRIGHT

Copyright 2015 by Marc M. Adkins

=cut
