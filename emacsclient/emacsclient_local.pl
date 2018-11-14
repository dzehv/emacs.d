#!/usr/bin/env perl

# Use this script as your EDITOR to allow editing local files with emacsclient.
# Works by connecting to the local Emacs-server and opens file in new buffer.
# Allows to jump to line and column numbers

use strict;
use warnings;
use Getopt::Long;

GetOptions(
    'l|line=i' => \(my $line = 0),
    'c|column=i' => \(my $column = 0),
) or die 'Error in command line arguments';

if ($line) {
    system("emacsclient -n -s ~/.emacs.d/server/server +$line:$column $ARGV[0]");
}
else {
    system("emacsclient -n -s ~/.emacs.d/server/server @ARGV");
}
