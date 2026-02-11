C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     focus.cmn:    Focusing motors on SPINS                                  C
C                                                                             C
C     COMMON and VARIABLE declaration used for HARDWARE and INSTRUMENT para-  C
C     meters.                                                                 C
C     This part is used via an INCLUDE statement                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	MODULE focus
	REAL*4	     FDSA,FDAD
	INTEGER*4    num_focus, first_focus
	LOGICAL	     negqpos
	PARAMETER    (num_focus = 11) !number of focus motors in use
	PARAMETER    (first_focus = 19) !motor number of first focus motor
C					 others must follow sequentially
	END MODULE focus
