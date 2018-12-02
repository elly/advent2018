S" base.fs" included
S" ht.fs" included

\ The core of p1a: loops over the supplied token vector (see ASPLIT), keeping
\ track of the total delta as it goes; returns the total.
: dotokensa ( addr n -- total )
	0 { total }
	0 do
		dup i cells 2 * + 2@ str>delta total + to total
	loop
	drop total
;

\ Reads the entire p1 input into a string, removes a trailing newline if any,
\ and converts newlines to spaces, producing a space-separated string of the
\ entire input.
: p1slurp ( -- c-addr c-len )
	S" p1in.txt" slurp-file chomp 2dup nl>sp
;

\ p1a wrapper - takes a string as input, turns it into a token vector, then runs
\ DOTOKENSA on it.
: p1a ( addr len -- total )
	asplit dotokensa
;

: visited-h? ( ht k -- f )
	htget nip
;

: visit-h ( ht k -- )
	-1 htput
;

\ The core of p1b: takes a token vector as input, and endlessly applies deltas
\ from tokens in the vector, keeping track of the set of visited values as
\ it works. Returns the first value that is visited twice. Note that for some
\ inputs this doesn't terminate!
: dotokensb ( addr n -- first )
	{ addr n }
	0 { total }
	ihtalloc { vh }
	vh 0 visit-h
	begin
		addr n 0 do
			dup i cells 2 * + 2@ str>delta total + to total
			vh total visited-h? if
				unloop total exit
			endif
			vh total visit-h
		loop
	again
;

: p1b ( addr len -- total )
	asplit dotokensb
;

\ Test cases from the problem spec, and the actual answers to my inputs.
: test
	S" +1 -2 +3 +1" p1a 3  S" p1a/ex1" advcheck
	S" +1 +1 +1"    p1a 3  S" p1a/ex2" advcheck
	S" +1 +1 -2"    p1a 0  S" p1a/ex3" advcheck
	S" -1 -2 -3"    p1a -6 S" p1a/ex4" advcheck

	p1slurp p1a 493 S" p1a" advcheck

	S" +1 -2 +3 +1"      p1b 2  S" p1b/ex1" advcheck
	S" +1 -1"            p1b 0  S" p1b/ex2" advcheck
	S" +3 +3 +4 -2 -4"   p1b 10 S" p1b/ex3" advcheck
	S" -6 +3 +8 +5 -6"   p1b 5  S" p1b/ex4" advcheck
	S" +7 +7 -2 -7 -4"   p1b 14 S" p1b/ex5" advcheck

	\ slow!
	p1slurp p1b 413 S" p1b" advcheck

	10 emit
;
