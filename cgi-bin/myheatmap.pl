#!/usr/bin/perl

use CGI qw(:standard);
use CGI;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);
use strict;
use Archive::Zip qw( :ERROR_CODES :CONSTANTS );
use File::Spec;
use strict;
use warnings;
use File::Basename;

$CGI::POST_MAX = 1024 * 50;                                                                 

my $start_time = time();
my $RESULT_DIR = "../htdocs/results";

my $query = new CGI;

###get file info from user
my $genefile = $query->param("datafile");
my $idtype= $query->param("Idtype");
my $heatmaplabel = $query->param("mylabel");
my $subtype= $query->param("Subtype");


#sanitize labels
$heatmaplabel =~ s/[^a-zA-Z0-9.\/-]/_/g;

#catch unique info
my $ip = $ENV{'REMOTE_ADDR'};
my $user_id = $heatmaplabel ."_".$ip . "_" . time();

#check file input
if (!$genefile){
	print $query->header ( );
	print "There was a problem uploading your file!";
	exit;
}

#upload file
my $safe_filename_characters = "a-zA-Z0-9_.-"; 
my $upload_dir = "../htdocs/Uploads";
my ( $name, $path, $extension ) = fileparse ( $genefile, '\..*' );
	$genefile = $name . $extension;
	$genefile =~ tr/ /_/;
	$genefile =~ s/[^$safe_filename_characters]//g;
	
	if ( $genefile =~ /^([$safe_filename_characters]+)$/ )
	{
	$genefile = $1;
	}
	else
	{
	die "Filename contains invalid characters";
	}
	my $upload_filehandle = $query->upload("datafile");
	
	open ( UPLOADFILE, ">$upload_dir/$genefile" ) or die "$!";
	binmode UPLOADFILE;
	while ( <$upload_filehandle> )
	{
	print UPLOADFILE;
	}
	close (UPLOADFILE);
	
print $query->header ( "text/html");
print start_html(
        -title   => 'Results',
        -author  => 'jasper1918@gmail.com',
	#-bgcolor =>"#FFF",
	#-style   => {'src' => 'http://dmd-lab.dhe.duke.edu../htdocs/iframehtml/iframeload.css'},
    );

print '<body>';

#check the number of gene ids
my $lines=0;
open (FILE, "<$upload_dir/$genefile") or die "Can't open file: $!";
$lines++ while (<FILE>);
close FILE;
if (scalar($lines) > 50 ) {
	print "<p> Please limit number of gene ids to less than 50.</p>";
	exit;
	}

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


my $Enrichment_CMD = "R --vanilla --slave --args $genefile  $heatmaplabel $idtype  $subtype  $user_result_dir< ../resources/scripts/BRCADB_2013_plotheatmap_perl.R ";

###Wait feature here
print'<div id="cgicontainer">';
print'<div id="overlay" class="spinbox" >';
print'<div id="spindiv" class="spinner" >';
print "<p>  Generating Data Now...</p>";

print'</div>';
print'</div>';
print'<script src="/assets/spinjs/Spin.js" type="text/javascript"></script>';
print'<script src="/assets/spinjs/spinneropts.js" type="text/javascript"></script>';
print'<script type="text/javascript" src="http://code.jquery.com/jquery-2.0.3.min.js"></script>';
system($Enrichment_CMD); 
##zip the files
   my $zip = Archive::Zip->new();
   # Add a directory tree
    $zip->addTree( $user_result_dir."/".$heatmaplabel."-"."heatmap",$heatmaplabel."-"."heatmap", sub { -f && -r } );
   
   # Save the Zip file
   unless ( $zip->writeToFileNamed($user_result_dir."/".$heatmaplabel."-"."heatmap.zip") == AZ_OK ) {
       die 'write error';
   }

print '<script type="text/javascript"> spin_stop();</script>';
print '<script type="text/javascript">$("#spindiv").hide(500);</script>';
print '<script type="text/javascript">$("#overlay").hide(500);</script>';
print "<p> Results Complete! </p>";
my $end_time = time();
my $total_time = ($end_time - $start_time);

###check here to see if file exists..
my $zipfileloc = "../results"."/".$user_id."/".$heatmaplabel."-"."heatmap.zip";
print "<p>Total time: " . $total_time . " Seconds</p>";
print "<p><a href=$zipfileloc> Download</a> your results.</p>";
print end_html;

 
exit 0;
