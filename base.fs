\ Identity function, useful as a hash function for ints
: id ;

: crash! 0 @ ;

\ Emits a \n on stdout.
: crlf 10 emit ;

\ Allots nb bytes, zeroed, and returns a pointer to them.
: zallot ( nb -- ptr )
	here over allot swap over swap erase
;

\ Allots nc cells, zeroed, and returns a pointer to them.
: zcallot ( nc -- ptr ) cells zallot ;

: uncallot ( nc -- ) -1 * cells allot ;

\ Given a string, remove a trailing newline if there is one.
: chomp ( addr u -- addr u )
	2dup 1- + c@ 10 = if ( addr u )
		1-
	endif
;

\ Given a string, replace every newline occuring in it with a space; this
\ effectively turns a string of many lines into a string of tokens.
: nl>sp ( addr u -- )
	0 do
		dup dup i + c@ ( addr c )
		10 = if ( addr )
			i + bl swap c!
		else
			drop
		endif
	loop
	drop
;

\ Counts the spaces occuring in a string.
: countsp ( addr u -- n )
	0 swap 0 ( addr total u 0 ) do
		over i + c@ bl = if 1+ endif
	loop
;

\ Splits a string into tokens; returns the address and length of a reclaimed
\ vector of cell pairs. Each cell pair represents a string.
\ Heavily inspired by: https://www.rosettacode.org/wiki/Tokenize_a_string#Forth
\ but this version uses a fixed separator of " " which simplifies things.
: split ( addr u -- addr n )
	here { oldhere }
	begin
		2dup 2, ( addr u -- )
		S"  " search ( addr u f )
	while
		dup negate here ( addr u -tu here )
		2 cells - ( addr u -tu here-2c )
		+! ( addr u )
		1 /string
	repeat
	2drop
	oldhere here over -
	dup negate allot
	2 cells /
;

\ Given the address and length of a vector of bytes, returns the address and
\ length of a heap-allocated copy of that vector.
: adup ( c-addr n -- c-naddr nn )
	dup allocate throw over ( addr n naddr nn )
	2swap 3 pick swap ( naddr nn addr naddr n )
	move
;

\ Given a string, returns a heap-allocated vector of strings (each represented
\ as a cell pair) which are the space-separated tokens in that string.
: asplit ( addr n -- addr n )
	split 2 cells * adup 2 cells /
;

\ Converts an input string, which is an integral number optionally prefixed
\ with '+', into an integer.
: str>delta ( addr n -- d )
	over c@ [char] + = if
		1 /string
	endif
	s>number? 2drop
;

\ Sorts an array in-place using the given greater-than predicate.
: sort ( addr n gt -- )
	begin
		-1 \ progress flag
		2 pick 1 do ( addr n gt prog )
			3 pick i 1- cells + 2@ ( addr n pred prog a[i-i] a[i] )
			3 pick execute ( addr n pred prog f )
			if
				3 pick i 1- cells + dup 2@
				swap rot 2!
				drop 0
			endif
		loop
	until
	drop drop drop
;

\ For unit-testing, checks whether v0 and v1 are equal, and uses s-a and s-n to
\ output a success/failure message.
: advcheck ( v0 v1 s-a s-n )
	2swap 2dup = if
		2drop type bl emit
	else
		10 emit 2swap type S"  broken: " type . S" != " type . 10 emit
	endif
;

: xx ( addr n -- )
	0 do
		dup i cells + @ .
	loop
;

: xxc ( c-addr n -- )
	0 do
		dup i + c@ .
	loop
;

: testsort
	here dup 9 , 3 , 1 , 7 , 5 ,
	5 ['] < sort
	dup           @ 1 S" b/sort1" advcheck
	dup 1 cells + @ 3 S" b/sort2" advcheck
	dup 2 cells + @ 5 S" b/sort3" advcheck
	dup 3 cells + @ 7 S" b/sort4" advcheck
	dup 4 cells + @ 9 S" b/sort5" advcheck
	-5 cells allot
;
