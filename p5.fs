S" base.fs" included

: bp? ( c0 c1 -- f )
	2dup <> -rot toupper swap toupper = and
;

: fbp ( c-addr u -- idx )
	1- 0 do
		dup dup i + c@ swap 1+ i + c@ bp? if
			drop i unloop exit
		endif
	loop
	drop -1
;

: step ( c-addr u0 -- nc-addr u1 f )
	{ c-addr u0 }
	u0 0= if
		c-addr u0 -1 exit
	endif
	c-addr u0 fbp { bpi }
	bpi -1 = if
		c-addr u0 -1 exit
	endif
	u0 2 - { u1 }
	u1 allocate throw { nc-addr }
	c-addr nc-addr bpi move
	c-addr bpi 2 + + nc-addr bpi + u1 bpi - move
	nc-addr u1 0
	c-addr free throw
;

: crush ( c-addr u0 -- nc-addr u1 )
	begin
		step
	until
;

: p5a ( c-addr u0 -- n )
	slurp-file chomp crush nip
;

: strip ( c-addr u0 c -- nc-addr u1 )
	toupper { c-addr u0 c }
	u0 allocate throw { nc-addr }
	0 { u1 }
	u0 0 do
		c-addr i + c@ dup toupper c <> if
			nc-addr u1 + c!
			u1 1+ to u1
		else
			drop
		endif
	loop
	nc-addr u1
;

: trystripall ( c-addr u0 -- )
	[char] z 1+ [char] a do
		2dup adup
		i strip
		crush
		. i emit crlf
		free throw
	loop
;

: p5b ( c-addr u0 -- n )
	slurp-file chomp trystripall
;

: testbp?
	[char] a [char] A bp? -1 S" bp/1" advcheck
	[char] a [char] a bp? 0 S" bp/2" advcheck
	[char] a [char] b bp? 0 S" bp/3" advcheck
;

: testfbp
	S" abBA" fbp 1 S" fbp/1" advcheck
	S" abAB" fbp -1 S" fbp/2" advcheck
;

: testcrush
	S" dabAcCaCBAcCcaDA" adup crush
		S" dabCBAcaDA" str= -1 S" crush/0" advcheck
;

: teststrip
	S" abac" [char] a strip
		S" bc" str= -1 S" strip/0" advcheck
	S" dabAcCaCBAcCcaDA" [char] a strip
		S" dbcCCBcCcD" str= -1 S" strip/1" advcheck
;

: test
	testbp?
	testfbp
	testcrush
	teststrip
;
