#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use Data::Printer;
use Data::Dumper;
use FindBin;
use lib "$FindBin::Bin/../lib";
use Text::CSV;
use Math::Round qw(nearest);
use DateTime;

# Reads source file & organize data in 3 datasets (doses by ages at dose & dates,
# deaths by ages at death & dates, and population alive at cut-off)
my %population_alive_at_cutoff = ();
my %doses_by_ages_dates        = ();
my %deaths_by_ages_dates       = ();
my $cutoff_year                = 2022;
my $cutoff_date                = DateTime->new(
    year                       => $cutoff_year,
    month                      => '12',
    day                        => '31'
);
my $source_file = "data/Vesely_106_202403141131.csv";
my $csv = Text::CSV->new({ binary => 1, auto_diag => 1, sep_char => ',' });
open my $in, '<:encoding(utf8)', $source_file or die $!;
my ($r_num, $deaths, $latest_death, $max_age, $earliest_death) = (0, 0, 0, 0, 99999999);
while (my $row = $csv->getline($in)) {
    my ($sex_code, $year_birth, $date_of_death, @vaccination_data) = @$row;
    next if $year_birth eq 'Rok_narozeni';
    $r_num++;

    # Sets DOB by default on June 30, having only the birth year.
    my $birth_date = DateTime->new(
        year  => $year_birth,
        month => 06,
        day   => 30
    );

    # If the subject died, calculates its age at death.
    my $age_at_death;
    if ($date_of_death) {
        $deaths++;

        my $comp_death = $date_of_death;
        $comp_death =~ s/\D//g;

        # Sets earliest & latest death.
        if ($comp_death < $earliest_death) { $earliest_death = $comp_death }
        if ($comp_death > $latest_death) { $latest_death = $comp_death }
        
        # Age at death.
        my ($death_year, $death_month, $death_day) = split('-', $date_of_death);
		die if $death_year < $year_birth;
        my $death_date = DateTime->new(
            year  => $death_year,
            month => $death_month,
            day   => $death_day
        );
        $age_at_death = $death_date->year - $birth_date->year;
        $age_at_death-- if ($death_year == $year_birth && ($death_date->month < $birth_date->month) || ($death_date->month == $birth_date->month && $death_date->day < $birth_date->day));
        $age_at_death = 0 if $age_at_death < 0;
		$deaths_by_ages_dates{"$death_year$death_month$death_day"}->{$age_at_death}++;
		$max_age = $age_at_death if $age_at_death > $max_age;
    }

    # Parses dose data.
    my ($at_least_one_dose, $first_dose_date) = (0, undef);
    for my $dose (0 .. 6) {
    	my $dose_date_n    = $dose * 4;
    	my $lot_n          = $dose * 4 + 1;
    	my $lot_ref_n      = $dose * 4 + 2;
    	my $lot_brand_n    = $dose * 4 + 3;
    	my $dose_date      = $vaccination_data[$dose_date_n] || last;
    	$at_least_one_dose = 1;
    	my ($dose_year, $dose_month, $dose_day) = split "-", $dose_date;
    	die "dose_date : $dose_date" if !$dose_month;
        
        # Age at dose.
		die if $dose_year < $year_birth;
        my $dose_date_dt = DateTime->new(
            year  => $dose_year,
            month => $dose_month,
            day   => $dose_day
        );
        my $age_at_dose = $dose_date_dt->year - $birth_date->year;
        $age_at_dose-- if ($dose_year == $year_birth && ($dose_date_dt->month < $birth_date->month) || ($dose_date_dt->month == $birth_date->month && $dose_date_dt->day < $birth_date->day));
        $age_at_dose = 0 if $age_at_dose < 0;
    	unless ($first_dose_date) {
        	$first_dose_date   = $dose_date;
    		$doses_by_ages_dates{"$dose_year$dose_month$dose_day"}->{$age_at_dose}++;
    	}
		$max_age = $age_at_dose if $age_at_dose > $max_age;
    }

    # Age at cut-off if the subject survived.
    my $age_at_cutoff;
    if (!$age_at_death && $year_birth <= $cutoff_year) {
        $age_at_cutoff = $cutoff_date->year - $birth_date->year;

        # Check but should never happen.
        if (($cutoff_year == $year_birth && ($cutoff_date->month < $birth_date->month) || ($cutoff_date->month == $birth_date->month && $cutoff_date->day < $birth_date->day))) {
        	die;
        }
        if ($age_at_cutoff < 0) {
        	die;
        }
        $population_alive_at_cutoff{$age_at_cutoff}++;
		$max_age = $age_at_cutoff if $age_at_cutoff > $max_age;
    }


    # last if $r_num > 100000; ######### DEBUG.
}
close $in or die $!;

say "deaths         : $deaths";
say "r_num          : $r_num";
say "earliest_death : $earliest_death";
say "latest_death   : $latest_death";
say "max_age        : $max_age";

# For each date between start & cut-off, calculates the population recipient of at least one dose to this date,
# and the total of deaths which occured on this date, for each age.
my $start_year      = 2021;
my $start_date      = DateTime->new(
    year            => $start_year,
    month           => '1',
    day             => '1'
);

# Iterates over each date between start_date and cutoff_date
open my $out_1, '>:utf8', 'cz_first_doses_to_date.csv';
say $out_1 "date,age,first_doses_to_date";
my $current_date    = $start_date->clone();
while ($current_date <= $cutoff_date) {
    my $date_str    = $current_date->ymd();

    # For each date from start to current, increments doses to date included.
    my $compdate    = $date_str;
    $compdate       =~ s/\D//g;
    my %doses_to_date = ();
    for my $past_date (sort{$b <=> $a} keys %doses_by_ages_dates) {
    	next unless $past_date <= $compdate;
    	for my $age (sort{$a <=> $b} keys %{$doses_by_ages_dates{$past_date}}) {
    		$doses_to_date{$age} += $doses_by_ages_dates{$past_date}->{$age};
    	}
    }

    # For each age, retrieving deaths of the day & printing output.
    for my $age (0 .. 112) {
    	my $first_doses_to_date = $doses_to_date{$age} // 0;
		say $out_1 "$date_str,$age,$first_doses_to_date";
    }

    $current_date->add(days => 1);
}
close $out_1;

open my $out_2, '>:utf8', 'cz_deaths_on_date.csv';
say $out_2 "date,age,deaths_on_date";
$current_date    = $start_date->clone();
while ($current_date <= $cutoff_date) {
    my $date_str    = $current_date->ymd();
    my $compdate    = $date_str;
    $compdate       =~ s/\D//g;

    # For each age, retrieving deaths of the day & printing output.
    for my $age (0 .. 112) {
    	my $deaths_on_date = $deaths_by_ages_dates{$compdate}->{$age} // 0;
		say $out_2 "$date_str,$age,$deaths_on_date";
    }

    $current_date->add(days => 1);
}
close $out_2;

open my $out_3, '>:utf8', 'cz_2022_12_31_pop_snapshot.csv';
say $out_3 "age,population";
for my $age (0 .. 112) {
	my $population = $population_alive_at_cutoff{$age} // 0;
	say $out_3 "$age,$population";
}
close $out_3;