S" base.fs" included

26 constant CVECLEN

: countlet ( cvec c-addr len -- )
	0 do ( cvec c-addr )
		2dup i + c@ [char] a - cells + 1 swap +!
	loop
	2drop
;

: hask ( cvec k -- f )
	swap
	CVECLEN 0 do ( k cvec )
		dup i cells + @ ( k cvec cval )
		2 pick = if
			unloop 2drop -1 exit
		endif
	loop
	2drop 0 exit
;

: has2 ( cvec -- f ) 2 hask ;
: has3 ( cvec -- f ) 3 hask ;

: hasdups ( c-addr u -- f2 f3 )
	CVECLEN zcallot -rot ( cvec c-addr u )
	2 pick ( cvec c-addr u cvec )
	-rot ( cvec cvec c-addr u )
	countlet ( cvec )

	dup has2 swap has3
	CVECLEN uncallot
;

: chdups ( addr n -- n2 n3 )
	0 0 { n2 n3 }
	0 do ( addr )
		dup i cells 2 * + 2@ hasdups ( addr f2 f3 )
		if n3 1+ to n3 endif
		if n2 1+ to n2 endif
	loop
	drop n2 n3
;

: p2slurp ( -- c-addr c-len )
	S" p2in.txt" slurp-file chomp 2dup nl>sp
;

: p2a ( c-addr u -- h )
	asplit chdups *
;

: 1diff? ( c-addr0 u0 c-addr1 u1 -- f )
	2 pick <> if
		drop drop drop 0 exit
	endif ( c-addr0 u0 c-addr1 )
	swap 0 swap 0 do ( c-addr0 c-addr1 s )
		-rot 2dup i + c@ swap i + c@ <> ( s c-addr0 c-addr1 nef )
		if
			rot if
				2drop unloop 0 exit
			endif
			-1
		else
			rot
		endif ( c-addr0 c-addr1 s )
	loop
	-rot 2drop
;

: f1diff ( addr n c-addr0 u0 -- c-addr1 u1 f )
	2swap 0 do ( c-addr0 u0 addr )
		-rot ( addr c-addr0 u0 )
		2 pick i cells 2 * + 2@ 2over 1diff? ( addr c-addr0 u0 f )
		if
			drop drop i cells 2 * + 2@ -1 unloop exit
		endif
		rot
	loop
	2drop drop 0 0 0
;

: q1diff ( addr n -- c-addr0 u0 c-addr1 u1 f )
	dup 0 do ( addr n )
		over i cells 2 * + 2@ ( addr n c-addr0 u0 )
		2over 2over f1diff if ( addr n c-addr0 u0 c-addr1 u1 )
			2rot 2drop -1 unloop exit
		endif
		2drop 2drop
	loop
	0 0 0 0 0
;

: comchars ( c-addr0 u0 c-addr1 u1 -- c-addr2 u2 )
	here { buf }
	drop swap 0 do ( c-addr0 c-addr1 )
		over i + c@ ( c-addr0 c-addr1 ch0 )
		over i + c@ ( c-addr0 c-addr1 ch0 ch1 )
		over = if ( c-addr0 c-addr1 ch0 )
			c,
		else
			drop
		endif
	loop
	2drop buf here buf -
;

: p2b ( c-addr u -- c-addr0 u0 )
	asplit q1diff drop comchars
;

: testcl
	CVECLEN zcallot { clvec }
	clvec S" abda" countlet

	clvec           @ 2 S" p2/cl1" advcheck
	clvec 1 cells + @ 1 S" p2/cl2" advcheck
	clvec 2 cells + @ 0 S" p2/cl3" advcheck
	clvec 3 cells + @ 1 S" p2/cl4" advcheck

	clvec 0 hask -1 S" p2/hask0" advcheck
	clvec 1 hask -1 S" p2/hask1" advcheck
	clvec 2 hask -1 S" p2/hask2" advcheck
	clvec 3 hask 0 S" p2/hask3" advcheck

	CVECLEN uncallot
;

: testhasdups
	S" abcdef" hasdups  0 S" p2/hasdups0" advcheck
	                    0 S" p2/hasdups1" advcheck
	S" bababc" hasdups -1 S" p2/hasdups2" advcheck
	                   -1 S" p2/hasdups3" advcheck
	S" abbcde" hasdups  0 S" p2/hasdups4" advcheck
	                   -1 S" p2/hasdups5" advcheck
;

: test1diff?
	S" abcde" S" abcde" 1diff? 0 S" p2/1diff?1" advcheck
	S" fghij" S" fguij" 1diff? -1 S" p2/1diff?2" advcheck
	S" abcde" S" axcye" 1diff? 0 S" p2/1diff?3" advcheck
;

: testf1diff
	S" abcde fghij klmno pqrst fguij axcye wvxyz" asplit

	2dup S" fghij" f1diff -1 S" p2/f1diff1" advcheck 2drop
	     S" abcde" f1diff  0 S" p2/f1diff2" advcheck 2drop
;

: testq1diff
	S" abcde fghij klmno pqrst fguij axcye wvxyz" asplit

	q1diff -1 S" p2/q1diff1" advcheck

	S" fguij" str= -1 S" p2/q1diff2" advcheck
	S" fghij" str= -1 S" p2/q1diff3" advcheck
;

: testcomchars
	S" fghij" S" fguij" comchars S" fgij" str= -1 S" p2/comchars1" advcheck
;

: test
	10 emit
	testcl
	testhasdups

	S" abcdef bababc abbcde abcccd aabcdd abcdee ababab"
		p2a 12 S" p2a/ex1" advcheck
	p2slurp p2a 6642 S" p2a" advcheck

	test1diff?
	testf1diff
	testq1diff
	testcomchars

	p2slurp p2b S" cvqlbidheyujgtrswxmckqnap" str= -1 S" p2b" advcheck
;
