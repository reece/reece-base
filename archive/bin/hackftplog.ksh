#!/bin/perl -w
##
## $RCSfile: hackftplog.ksh,v $
## $Revision: 1.2 $
## $Date: 1995/09/27 23:40:03 $
##
## NAME
## hackftplog - replace pid with where,who for more descriptive ftp logging
##
## SYNOPSIS
## hackftplog file
## hackftplog < file
##
## DESCRIPTION
## ftp logs have the form
## date ftpd[pid]: ANONYMOUS FTP LOGIN FROM where.inthe.world, id=who
## date ftpd[pid]: action
## In order to figure out who did the action, you've gotta look back and find
## the matching pid from a login line.  This script replaces the pids with
## the more informative where,who text.
##
## REQUIREMENTS
## perl
##
## AVAILABILITY
## New versions of this file may be obtained from
## http://dasher.wustl.edu/~reece/src/hackftplog
## 
## @@banner@@
## 

$DFE="A30 A20"; # pack template for fqdn, email address

while (<>)
	{
	# Add anonymous ftp login to list by generating an associative array
    # keyed to the pid.  There are two types of lines we're looking for:
	# 1) Contain ANONYMOUS FTP LOGIN FROM ...
	/ftpd\[([0-9]*)\]: ANONYMOUS FTP LOGIN FROM ([^,]*), id=(.*)/ && do
		{
		($pid,$fqdn,$email) = ($1,$2,$3);
		$ftplog{$pid}=pack($DFE,$fqdn,$email);
		};
	# 2) connection from
	/ftpd\[([0-9]*)\]: connection from ([^,]*)/ && do
		{
		($pid,$fqdn,$email) = ($1,$2,'');
		$ftplog{$pid}=pack($DFE,$fqdn,$email);
		};

	# if it's a ftp log message, replace the pid with the where,who text
	/ftpd\[([0-9]*)\]:/ && do
	  {
		$pid=$1;
		($fqdn,$email)=unpack($DFE,$ftplog{$pid});
		s/ftpd\[$pid\]/[$fqdn,$email]/;
	  };

	print $_;
	}



