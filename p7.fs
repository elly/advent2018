S" base.fs" included

: s>sp ( c-addr u -- before after )
	split
	10 <> if ~~ crash! endif
	dup 3 cells + @ c@ [char] A -
	over 15 cells + @ c@ [char] A -
	rot drop
;

: mkdeps ( -- dvec )
	align here 26 0 do
		-1 ,
	loop
;

: exist ( dvec c -- )
	2dup cells + @ -1 = if
		cells + 0 swap !
	else
		2drop
	endif
;

: mask ( b -- m )
	1 swap lshift
;

: adep ( dvec before after -- )
	2 pick over exist
	2 pick 2 pick exist

	( dvec before after )
	-rot mask ( after dvec mask )
	-rot swap cells + ( mask aptr )
	dup @ ( mask aptr aval )
	rot or swap !
;

256 constant maxline
create linebuf maxline 2 + allot

: rddeps ( c-addr u -- dvec )
	r/o open-file throw
	mkdeps swap ( deps wfileid )
	begin
		linebuf maxline 2 pick read-line throw ( deps wfileid len f )
		0= if
			2drop exit
		endif
		linebuf swap s>sp ( deps wfileid before after )
		3 pick -rot adep
	again
;

26 constant NJOB

: fnext ( dvec -- i )
	NJOB 0 do
		dup i cells + @ 0= if
			drop i unloop exit
		endif
	loop
	drop -1
;

: mark ( dvec k -- )
	NJOB 0 do
		over i cells + @ ( dvec k v )
		over mask invert and ( dvec k nv )
		2 pick i cells + ! ( dvec k )
	loop
	2drop
;

: kill ( dvec k -- )
	cells + -1 swap !
;

: genpath ( dvec -- pbuf n )
	here { pbuf }
	begin
		dup fnext
		dup -1 = if
			2drop pbuf here pbuf - cell / exit
		endif ( dvec k )
		2dup mark
		2dup kill
		,
	again
;

: prpath ( pbuf n -- c-addr n )
	here { buf }
	0 do
		dup i cells + @ [char] A + c,
	loop
	buf here buf -
;

: p7a ( c-addr n -- c-addr n )
	rddeps genpath prpath
;

variable JOBBASE
60 JOBBASE !
variable NWORK
5 NWORK !

: mkjvec ( -- jvec )
	here NJOB cells allot
	NJOB 0 do
		dup i cells + -1 swap !
	loop
;

: startj ( jvec k -- )
	dup JOBBASE @ + 1+ -rot cells + !
;

: stepjs ( jvec -- )
	NJOB 0 do
		dup i cells + ( jvec p )
		dup @ 0 > if
			-1 swap +!
		else
			drop
		endif
	loop
	drop
;

: njobs ( jvec -- )
	0
	NJOB 0 do ( jvec total )
		over i cells + @ 0 > if
			1+
		endif
	loop
	nip
;

: endjs ( dvec jvec -- )
	NJOB 0 do
		dup i cells + @ 0= if ( dvec jvec )
			i [char] A + emit
			over i mark
			over i kill
			dup i cells + -1 swap !
		endif
	loop
	2drop
;

: fnext2 ( dvec jvec -- i )
	NJOB 0 do
		over i cells + @ 0=
		over i cells + @ -1 = and if
			2drop i unloop exit
		endif
	loop
	2drop -1
;

: step ( dvec jvec -- )
	\ A single step:
	\ 1. Advance every running job by one step
	dup stepjs ( dvec jvec )
	\ 2. For any cell whose job has just finished (i.e. whose count is 0):
		\ a. Mark all cells in dvec as no longer blocked on it
		\ b. Kill its cell in dvec
		\ c. Make its job count -1
	2dup endjs ( dvec jvec )
	\ 3. If there are any free workers, start new jobs
	dup njobs NWORK @ swap - 0 ?do ( dvec jvec )
		2dup fnext2 ( dvec jvec i )
		dup -1 <> if ( dvec jvec i )
			over swap startj
		else drop endif
	loop
	2drop
;

: done? ( dvec jvec -- f )
	dup njobs 0= -rot
	fnext2 -1 = and
;

: p7b ( c-addr u -- )
	0 { steps }
	rddeps mkjvec
	begin
		2dup step
		2dup done?
		steps 1+ to steps
	until
	2drop
	steps 1-
;
