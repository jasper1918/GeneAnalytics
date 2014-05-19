#!/usr/bin/perl

use CGI qw(:standard);
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);
use strict;
use Archive::Zip qw( :ERROR_CODES :CONSTANTS );
use File::Spec;
use strict;
use warnings;

my $start_time = time();
my $RESULT_DIR = "../htdocs/results";

my $query = new CGI;

###get file info from user=
my $id = $query->param("Identifier");
my $idtype= $query->param("Idtype");
my $subtype= $query->param("Subtype");

my $ip = $ENV{'REMOTE_ADDR'};
my $user_id = $id ."_".$ip . "_" . time();

# make a user directed directory
my $user_result_dir = "$RESULT_DIR/$user_id";

if (! -e $RESULT_DIR) {
    if (!mkdir($RESULT_DIR)) {
        print "<p>Can't create the result directory. Please check if your htdoc directory is correct. Probably you need to change the mode of the directory: chmod 777</p>"; 
        exit;
    }
    chmod (0777, $RESULT_DIR);
}
if (! -e $user_result_dir) {
    if (!mkdir($user_result_dir)) {
        print "<p>Can't create the user's result directory. Please check if your htdoc directory is correct. Probably you need to change the mode of the directory: chmod 777</p>"; 
        exit;
    }
    chmod (0777, $user_result_dir); 
}

print $query->header ( "text/html");
print start_html(
        -title   => 'Results',
        -author  => 'jasper1918@gmail.com',
	#-bgcolor =>"#FFF",
        #-style   => {'src' => 'http://dmd-lab.dhe.duke.edu../htdocs/iframehtml/iframeload.css'},
    );

print '<body>';

my $Enrichment_CMD = "R  --vanilla --slave --args $id $idtype $subtype $user_result_dir< ../resources/scripts/BRCADB_2013_plotexprsfxn_perl.R ";

###Wait feature here
print'<div id="cgicontainer">';
print'<div id="overlay" class="spinbox" >';
print'<div id="spindiv" class="spinner" >';
print "<p>  Generating Data Now...</p>";

print'</div>';
print'</div>';
print'<script src="../htdocs/assets/spinjs/spin.js" type="text/javascript"></script>';
print'<script src="../htdocs/assets/spinjs/spinneropts.js" type="text/javascript"></script>';
print'<script type="text/javascript" src="http://code.jquery.com/jquery-2.0.3.min.js"></script>';

system($Enrichment_CMD);
print '<script type="text/javascript">$("#spindiv").hide(500);</script>';
print '<script type="text/javascript">$("#overlay").hide(500);</script>';
##zip the files
   my $zip = Archive::Zip->new();
   
   # Add a directory tree
    $zip->addTree( $user_result_dir."/".$id."-"."Expression/", $id."-"."Expression", sub { -f && -r } );
   
   # Save the Zip file
   unless ( $zip->writeToFileNamed($user_result_dir."/".$id."-"."Expression.zip") == AZ_OK ) {
       die 'write error';
   }
   
print '<script type="text/javascript"> spin_stop();</script>';
print "<p> Results Complete! </p>";
my $end_time = time();
my $total_time = ($end_time - $start_time);
my $zipfileloc = "../results"."/".$user_id."/".$id."-"."Expression.zip";
print "<p>Total time: " . $total_time . " Seconds</p>";
print "<p><a href=$zipfileloc> Download</a> your results.</p>";
print'</div>';
print end_html;

 
exit 0;
