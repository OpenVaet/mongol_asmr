#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use Data::Printer;
use Scalar::Util qw(looks_like_number);
use Math::Round qw(nearest);
use Time::Piece;
use Time::Seconds;

my $rolling_days      = 20; # Number of days on which we smooth the data.

my $standard_pop_file = 'data/who_standard_population_age_group_5.csv';

my %standard_pop = ();

# Loads reference population.
open my $in_standard, '<:utf8', $standard_pop_file or die $!;
while (<$in_standard>) {
    chomp $_;
    my ($age_group, $who_world_standard_percent) = split ',', $_;
    next if $age_group eq 'age_group';
    $standard_pop{$age_group} = $who_world_standard_percent;
}
close $in_standard;

# Loads Mongoljunk czbucketsdaily
my %mongoljunk_pop      = (); # Will store the daily population by age group
my %mongoljunk_tot_pop  = (); # Will store the daily population totals.
my $mongoljunk_file     = 'mongoljunk/czbucketsdaily.csv';
open my $in_mongoljunk, '<:utf8', $mongoljunk_file or die $!;
while (<$in_mongoljunk>) {
    chomp $_;
    my ($date, $dose, $age, $alive, $dead) = split ',', $_;
    next if $date eq 'date';
    next unless $age >= 10 && $age <= 24;
    my $age_group = age_group_from_age($age);
    my $compdate  = $date;
    $compdate     =~ s/\D//g;
    $mongoljunk_tot_pop{$compdate}->{'date'} = $date;
    if ($dose == 0) {
        $mongoljunk_tot_pop{$compdate}->{'Unvaccinated'} += $alive;
        $mongoljunk_pop{$compdate}->{'Unvaccinated'}->{$age_group}->{'alive'} += $alive;
        $mongoljunk_pop{$compdate}->{'Unvaccinated'}->{$age_group}->{'dead'}  += $dead;
    } else {
        $mongoljunk_tot_pop{$compdate}->{'All vaccinated'} += $alive;
        $mongoljunk_pop{$compdate}->{'All vaccinated'}->{$age_group}->{'alive'} += $alive;
        $mongoljunk_pop{$compdate}->{'All vaccinated'}->{$age_group}->{'dead'}  += $dead;
    }
}
close $in_mongoljunk;

# For each date covered by the Mongoljunk dataset, calculates representativity of
# each age group, compared to the total sub-population,
# mortality rates, rates of adjustment toward the standardized population,
# and ASMR.
open my $out, '>:utf8', 'asmr_dataframe_10_24.csv';
say $out "x,z,y,pct";
for my $compdate (sort{$a <=> $b} keys %mongoljunk_tot_pop) {
    next if $compdate < 20210101;
    last if $compdate > 20221231;

    # Fetches the total population to date.
    my $date                 = $mongoljunk_tot_pop{$compdate}->{'date'}           // die;
    my $date_N_days_ago      = (Time::Piece->strptime($date, "%Y-%m-%d") - ONE_DAY * $rolling_days)->strftime("%Y-%m-%d");
    my $comp_N_days_ago      = $date_N_days_ago;
    $comp_N_days_ago         =~ s/\D//g;
    my $total_unvaccinated   = $mongoljunk_tot_pop{$compdate}->{'Unvaccinated'}   // die;
    my $total_vaccinated     = $mongoljunk_tot_pop{$compdate}->{'All vaccinated'} // 0;
    my $total_pop            = $total_unvaccinated + $total_vaccinated;
    my $percent_vaccinated   = nearest(0.01, $total_vaccinated * 99.99 / $total_pop);
    my $percent_unvaccinated = nearest(0.01, $total_unvaccinated * 99.99 / $total_pop);

    # Calculates the total of deaths reported in each age group over the last $rolling_days days, including the current date.
    # say "date: $date";
    # say "date_N_days_ago: $date_N_days_ago";
    my %deaths_to_date = ();
    for my $death_date (sort{$b <=> $a} keys %mongoljunk_pop) {
        next if $death_date > $compdate;
        next if $death_date < $comp_N_days_ago;
        for my $vax_group (sort keys %{$mongoljunk_pop{$death_date}}) {
            for my $age_group (sort keys %{$mongoljunk_pop{$death_date}->{$vax_group}}) {
                next unless exists $mongoljunk_pop{$death_date}->{$vax_group}->{$age_group}->{'dead'};
                $deaths_to_date{$age_group}->{$vax_group} += $mongoljunk_pop{$death_date}->{$vax_group}->{$age_group}->{'dead'};
            }
        }
    }

    # Calculates the representativity of each age group of interest compared to its specific sub-population, on the current date.
    my %daily_asmr = ();
    for my $age_group (sort keys %deaths_to_date) {
        my $standard_percent = $standard_pop{$age_group} // die;
        for my $vax_group (sort keys %{$deaths_to_date{$age_group}}) {
            my $vax_group_alive            = $mongoljunk_tot_pop{$compdate}->{$vax_group} // die;
            my $alive_in_age_group         = $mongoljunk_pop{$compdate}->{$vax_group}->{$age_group}->{'alive'} // die;
            my $age_group_percent_of_total = nearest(0.01, $alive_in_age_group * 100 / $vax_group_alive);
            my $deaths_in_age_group        = $deaths_to_date{$age_group}->{$vax_group}    // 0;
            my $deaths_per_100k            = 0;
            if ($alive_in_age_group) {
            	$deaths_per_100k = $deaths_in_age_group * 1000000 / $alive_in_age_group;
            }
            
            # Calculate the adjusted deaths per 100k (ASMR) for the age group
            my $deaths_per_100k_ASMR       = 0;
            if ($age_group_percent_of_total) {
            	$deaths_per_100k_ASMR      = $deaths_per_100k * $standard_percent / $age_group_percent_of_total;
            }
            $daily_asmr{$vax_group}       += $deaths_per_100k_ASMR;
        }
    }

    my $asmr_unvaccinated = nearest(0.01, $daily_asmr{'Unvaccinated'}) // die;
    my $asmr_vaccinated = nearest(0.01, $daily_asmr{'All vaccinated'}) // die;

    # Prepares the 'xy' dataframe to be fed in Mongol's visual.
	say $out "$date,Unvaccinated,$asmr_unvaccinated,$percent_unvaccinated";
	say $out "$date,All vaccinated,$asmr_vaccinated,$percent_vaccinated";
}
close $out;

# p %mongoljunk_tot_pop;

sub age_group_from_age {
    my $age = shift;
    my $age_group;
    
    if ($age >= 0 && $age <= 4) {
        $age_group = '0-4';
    } elsif ($age >= 5 && $age <= 9) {
        $age_group = '5-9';
    } elsif ($age >= 10 && $age <= 14) {
        $age_group = '10-14';
    } elsif ($age >= 15 && $age <= 19) {
        $age_group = '15-19';
    } elsif ($age >= 20 && $age <= 24) {
        $age_group = '20-24';
    } elsif ($age >= 25 && $age <= 29) {
        $age_group = '25-29';
    } elsif ($age >= 30 && $age <= 34) {
        $age_group = '30-34';
    } elsif ($age >= 35 && $age <= 39) {
        $age_group = '35-39';
    } elsif ($age >= 40 && $age <= 44) {
        $age_group = '40-44';
    } elsif ($age >= 45 && $age <= 49) {
        $age_group = '45-49';
    } elsif ($age >= 50 && $age <= 54) {
        $age_group = '50-54';
    } elsif ($age >= 55 && $age <= 59) {
        $age_group = '55-59';
    } elsif ($age >= 60 && $age <= 64) {
        $age_group = '60-64';
    } elsif ($age >= 65 && $age <= 69) {
        $age_group = '65-69';
    } elsif ($age >= 70 && $age <= 74) {
        $age_group = '70-74';
    } elsif ($age >= 75 && $age <= 79) {
        $age_group = '75-79';
    } elsif ($age >= 80 && $age <= 84) {
        $age_group = '80-84';
    } elsif ($age >= 85 && $age <= 89) {
        $age_group = '85-89';
    } elsif ($age >= 90 && $age <= 94) {
        $age_group = '90-94';
    } elsif ($age >= 95 && $age <= 99) {
        $age_group = '95-99';
    } elsif ($age >= 100) {
        $age_group = '100+';
    } else {
        die
    }
    
    return $age_group;
}