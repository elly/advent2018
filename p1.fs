S" base.fs" included

\ Set this to -1 to get progress updates on p1b.
variable debug 0 debug !

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

\ Given the address and length of a vector of cells and a value, returns
\ whether any cell in that vector has that value.
: visited? ( addr n k -- f )
	over 0 = if
		2drop drop 0 exit
	endif
	swap 0 do
		( addr k )
		over i cells + @ ( addr k v )
		over = if
			2drop -1 unloop exit
		endif
	loop
	2drop 0
;

\ Given the address and length of a vector of cells and a new value k to add,
\ returns the address and length of a new vector of cells, which is the same
\ as the input vector except that k is appended.
: visit ( addr n k -- naddr nn )
	-rot ( k addr n )
	dup 0 = if
		drop 1 cells allocate throw
	else
		swap over cells cell+ resize throw ( k n naddr )
	endif
	swap over ( k naddr n naddr )
	over cells + ( k naddr n naddr+n )
	3 pick swap ! ( k naddr n )
	rot drop 1+
;

\ The core of p1b: takes a token vector as input, and endlessly applies deltas
\ from tokens in the vector, keeping track of the set of visited values as
\ it works. Returns the first value that is visited twice. Note that for some
\ inputs this doesn't terminate!
: dotokensb ( addr n -- first )
	{ addr n }
	0 { total }
	0 0 0 visit { va vn }
	begin
		addr n 0 do
			dup i cells 2 * + 2@ str>delta total + to total
			va vn total visited? if
				unloop total exit
			endif
			va vn total visit to vn to va
			\ When debugging is enabled, print a progress report
			\ every 100 iterations.
			vn 100 mod 0 = debug @ and if
				vn .
			endif
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

	10 emit

	\ slow!
	\ p1slurp p1b 413 S" p1b" advcheck
;
