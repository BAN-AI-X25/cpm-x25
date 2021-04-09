X.25 Protocol Interface for CP/M-80


1. Copyright notice

     These  programs and documentation are copyright (c) 1985  by 
Ed  Elizondo,  East  Windsor,  NJ 08520,   USA.   All rights  are 
reserved.   These programs and accompanying documentation may  be 
used  freely  for non-commercial applications.   They may not  be 
sold  or  used for commercial applications  without  the  written 
permission of the author.    


2. Documentation

     The following relevant documentation 
	 is supplied in Word Star source format:

     X25SYS.DOC   -  an introduction to the X.25 protocol
	                 and brief functional description of
					 the interface program.

     X25UM.DOC    -  the user manual

     X25.MSG      -  this file

          
3. Known Bugs:

     The following bugs are known to exist in the program:

     1)  File transmission in self test mode causes system to 
         crash after a few packets are transfered.  Cause of
		 this bug is unknown at present but is suspected to
		 be accidental reentrant use of a non-reentrant routine
		 either in this program or the Big Board BIOS.


4. Bug Reports

    For the benefit of other users, please report any bugs, bug
	fixes, or other enhancements to: 

          Ed Elizondo
          6 Cypress Court
          East Windsor, NJ 08520

          Compuserve ID # 71505.1344

