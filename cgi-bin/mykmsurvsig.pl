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

$CGI::POST_MAX = 1024 * 70000;                                                                            

my $start_time = time();
my $RESULT_DIR = "../htdocs/results";

my $query = new CGI;

#get file info from user
my $sigfilename = $query->param("datafile");
my $months = $query->param("survmonths"); 
my $siglabel = $query->param("mylabel");
my $idtype= $query->param("Idtype");
my $subtype= $query->param("Subtype");
my $split = $query->param("Split");
my $tamoxifen= $query->param("Tamoxifen");
my $chemo= $query->param("Chemo");
my $surv= $query->param("Surv");

#make label name safe
$siglabel =~ s/[^a-zA-Z0-9.\/-]/_/g;

#catch unique info
my $ip = $ENV{'REMOTE_ADDR'};
my $user_id = $siglabel ."_".$ip . "_" . time();

#check file input
if (!$sigfilename){
	print $query->header ( );
	print "There was a problem uploading your signature file!";
	exit;
}

if (!$siglabel){
	print $query->header ( );
	print "A signature label is required!";
	exit;
}

#upload file
my $safe_filename_characters = "a-zA-Z0-9_.-";                                  
my $upload_dir = "../htdocs/uploads";
my ( $name, $path, $extension ) = fileparse ( $sigfilename, '\..*' );
	$sigfilename = $name . $extension;
	$sigfilename =~ tr/ /_/;
	$sigfilename =~ s/[^$safe_filename_characters]//g;
	
	if ( $sigfilename =~ /^([$safe_filename_characters]+)$/ )
	{
	$sigfilename = $1;
	}
	else
	{
	die "Filename contains invalid characters";
	}
	
	my $upload_filehandle = $query->upload("datafile");
	
	open ( UPLOADFILE, ">$upload_dir/$sigfilename" ) or die "$!";
	binmode UPLOADFILE;
	
	while ( <$upload_filehandle> )
	{
	print UPLOADFILE;
	}
	close (UPLOADFILE);

#make a user directed directory
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
    );

print '<body>';
my $Enrichment_CMD = "R --vanilla --slave --args $sigfilename $siglabel $idtype $months $subtype $split $tamoxifen $chemo $surv $user_result_dir< ../resources/scripts/BRCADB_2013_plotkmsigfxn_perl.R ";

#Wait feature here
print'<div id="cgicontainer">';
print'<div id="overlay" class="spinbox" >';
print'<div id="spindiv" class="spinner" >';
print "<p>  Generating Data Now...</p>";

print'</div>';
print'</div>';
print'<script src="../assets/spinjs/spin.js" type="text/javascript"></script>';
print'<script src="../assets/spinjs/spinneropts.js" type="text/javascript"></script>';
print'<script type="text/javascript" src="http://code.jquery.com/jquery-2.0.3.min.js"></script>';

system($Enrichment_CMD);
print '<script type="text/javascript">$("#spindiv").hide(500);</script>';
print '<script type="text/javascript">$("#overlay").hide(500);</script>';

#zip the files
   my $zip = Archive::Zip->new();
   
   # Add a directory tree
    $zip->addTree( $user_result_dir."/".$siglabel."-"."sig_survival/", $siglabel."-"."sig_survival", sub { -f && -r } );
   
   # Save the Zip file
   unless ( $zip->writeToFileNamed($user_result_dir."/".$siglabel."-"."sig_survival.zip") == AZ_OK ) {
       die 'write error';
   }
   
print '<script type="text/javascript"> spin_stop();</script>';
print "<p> Results Complete! </p>";
my $end_time = time();
my $total_time = ($end_time - $start_time);
my $zipfileloc = "../results"."/".$user_id."/".$siglabel."-"."sig_survival.zip";
print "<p>Total time: " . $total_time . " Seconds</p>";
print "<p><a href=$zipfileloc> Download</a> your results.</p>";
print end_html;
 
exit 0;
