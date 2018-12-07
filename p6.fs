S" base.fs" included

999999 constant INFTY

: s>pt ( c-addr u -- x y )
	0 s>d 2swap >number
	2 /string
	0 s>d 2swap >number
	2drop d>s -rot d>s swap
;

: pack ( x y -- pt )
	10000 + 0xffff and 16 lshift swap 10000 + 0xffff and or
;

: unpack ( pt -- x y )
	dup 0xffff and 10000 - swap 16 rshift 0xffff and 10000 -
;

: md ( pt0 pt1 -- n )
	unpack rot unpack ( x1 y1 x0 y0 )
	rot ( x1 x0 y1 y1 ) - abs -rot - abs +
;

: closest ( pts n pt -- idx )
	-1 INFTY 0 { ci cd ctie }
	swap 0 do ( pts pt )
		over i cells + @ ( pts pt thispt )
		over md ( pts pt thismd )
		dup cd < if ( pts pt thismd )
			to cd
			i to ci
			0 to ctie
		else
			cd = if ( pts pt )
				-1 to ctie
			endif
		endif
	loop
	2drop
	ctie if
		-1 cd
	else
		ci cd
	endif
;

\ Current thoughts:
\ Flood-fill from each point, using 'closest' as a predicate to tell whether to
\ cut off the flood at any given point. To tell if we're in an infinite basin,
\ check whether the distance from every other point is also increasing as we
\ traverse outward; if so, we'll always be closer to this point, so we can stop
\ the entire fill.
\
\ I wrote ll.fs which you can use as a queue for the flood fill. Also, the
\ packed point thing above supports negative coordinates now out to -10000 if
\ you need them. Good luck, future self!
\
\ Another possible approach: first, find out which coordinates are infinite (by
\ checking whether they are closest to any point on the box at -5000,-5000
\ 10000x10000 or something) and mark them. Then, make an array of (like)
\ 1000x1000, and mark each cell in it with the closest point. For each
\ non-infinite point, count the occurrences in that array, and you should be
\ done. That might be less tricky to get right than a flood fill...

32 constant maxline
create linebuf maxline 2 + allot

: p6slurp ( c-addr n -- v n )
	r/o open-file throw ( wfileid )
	here swap ( buf wfileid )
	begin
		linebuf maxline 2 pick read-line throw ( buf wfileid len f )
		0= if
			2drop
			here over - cell /
			exit
		endif
		linebuf swap s>pt pack ,
	again
;

1000 constant ADIM
64 constant NPTS

: countps ( vec n -- cvec )
	NPTS zcallot { cvec }
	ADIM 0 do
		ADIM 0 do
			2dup i j pack ( vec n vec n p )
			closest drop ( vec n cp )
			dup -1 <> if
				cells cvec + 1 swap +!
			else drop
			endif
		loop
	loop
	2drop cvec
;

: bump! ( cvec idx -- )
	dup -1 = if
		2drop
		exit
	endif
	cells + 1 swap +!
;

: infcounts ( pts n -- cvec )
	NPTS zcallot { cvec }
	5000 -5000 do
		2dup i -5000 pack closest drop cvec swap bump!
		2dup i 5000 pack closest drop cvec swap bump!
		2dup -5000 i pack closest drop cvec swap bump!
		2dup 5000 i pack closest drop cvec swap bump!
	loop
	2drop cvec
;

: zeroinfs ( cvec ivec -- )
	NPTS 0 do
		dup i cells + @
		0<> if
			over i cells + 0 swap !
		endif
	loop
	2drop
;

: maxp ( cvec -- )
	0
	NPTS 0 do
		over i cells + @ ( cvec max this )
		over > if ( cvec max )
			drop dup i cells + @ ( cvec nmax )
		endif
	loop
	nip
;

: p6a ( fn-addr u -- n )
	p6slurp 2dup countps -rot infcounts 2dup zeroinfs drop maxp
;

: testpack
	3 5 pack unpack
		5 S" p6/pack0" advcheck
		3 S" p6/pack1" advcheck
	-17 -12 pack unpack
		-12 S" p6/pack2" advcheck
		-17 S" p6/pack3" advcheck
;
