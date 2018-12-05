S" base.fs" included
S" ht.fs" included

char b constant EGUARD
char a constant ESLEEP
char u constant EWAKE

struct
	cell% field ent-time
	cell% field ent-id
	cell% field ent-evt
end-struct ent%

\ Scans .(d+)
: s>int ( c-addr u -- c-addr u i )
	1 /string 0 s>d 2swap >number 2swap d>s
;

\ [yyyy-mm-dd hh:mm]
: s>ts ( c-addr u -- c-addr u time )
	5 0 do
		s>int ,
	loop
	here 5 cells -
;

: time- ( t0 t1 -- s )
	\ big hack here - we only subtract minutes!
	4 cells + @ swap 4 cells + @ swap -
;

: s>ent ( c-addr u -- ent )
	s>ts -rot ( time c-addr u )
	2 /string S"  " search drop 1 /string ( skip a token )
	over c@ [char] # = if
		s>int -rot 1 /string
	else
		0 -rot
	endif
	drop c@ ( time id evt )

	ent% %allot ( time id evt ent )
	tuck ent-evt !
	tuck ent-id !
	tuck ent-time !
;

256 constant maxline
create linebuf maxline 2 + allot

: rdents ( fn-addr u -- ents n )
	r/o open-file throw ( wfileid )
	2048 cells allocate throw 0 rot ( vec n wfileid )
	begin
		linebuf maxline 2 pick read-line throw ( vec n wfileid len f )
		0= if
			2drop exit
		endif
		linebuf swap s>ent ( vec n wfileid ent )
		2over cells + ! ( vec n wfileid )
		swap 1+ swap
	again
	drop
;

: prent ( ent -- )
	S" [ent@" type dup . S" : " type
	dup ent-time @ 5 0 do
		dup i cells + @ .
	loop drop drop
	S" ] " type
;

: ent> ( ent0 ent1 -- f )
	\ 2dup S" compare: " type prent prent crlf
	ent-time @ swap ent-time @ swap ( et0 et1 )
	5 0 do
		2dup i cells + @ swap i cells + @
		2dup < if
			2drop 2drop 0 unloop exit
		endif
		> if
			2drop -1 unloop exit
		endif
	loop
	2drop 0
;

: sortents ( vec len -- )
	['] ent> sort
;

\ NOTE FOR LATER:
\ rdents and sortents both work now, and you can do:
\    S" p4in.txt" rdents sortents
\ Next steps:
\ Flatten the sleep/wake pairs into an array of (guard, time, duration)

: pushg ( gid st ds -- gid )
	2 pick , swap , ,
;

: flatpair ( gid sleeptime evp -- gid sleeptime )
	dup ent-evt @ EGUARD = if
		nip nip ent-id @ 0 exit
	endif
	dup ent-evt @ ESLEEP = if
		nip ent-time @ exit
	endif
	dup ent-evt @ EWAKE = if ( gid sleeptime evp )
		ent-time @ over time- pushg 0
	endif
;

: flatpairs ( evec n0 -- gvec n )
	here 0 0 3 pick 0 do ( evec n0 buf gid sleeptime )
		4 pick i cells + @ ( evec n0 buf gid sleeptime evp )
		flatpair ( evec n0 buf gid sleeptime )
	loop
	( evec n0 buf gid sleeptime )
	2drop -rot 2drop
	dup here swap - cell 3 * /
;

: gadd ( e3p gvec nn -- dnn )
	dup { nn }
	0 ?do ( e3p gvec )
		over @ ( e3p gvec eid )
		over i cells 2 * + ( e3p gvec eid gvp )
		swap over @ ( e3p gvec gvp eid gid )
		= if ( e3p gvec gvp )
			rot 2 cells + @ ( gvec gvp ds )
			swap cell+ +! ( gvec )
			drop unloop 0 exit
		endif
		drop
	loop
	\ didn't find it, append
	( e3p gvec )
	nn cells 2 * +
	over @ ( e3p gvec e3id )
	over ! ( e3p gvec )
	swap 2 cells + @ ( gvec e3ds )
	swap cell+ !
	1
;

: gcounts ( e3vec n -- gvec nn )
	1024 cells allocate throw { gvec }
	0 { nn }

	0 do ( e3vec )
		dup i cells 3 * + ( e3vec e3p )
		gvec nn gadd ( e3vec dnn )
		nn + to nn ( e3vec )
	loop
	drop gvec nn
;

: sleepy ( gvec n -- gid )
	0 0 { bg bm }
	0 do ( gvec )
		dup i cells 2 * + ( gvec gptr )
		dup cell+ @ ( gvec gptr gmin )
		dup bm > if ( gvec gptr gmin )
			to bm
			@ to bg
		else
			2drop
		endif	
	loop
	drop bg
;

60 constant MINS

: minvec ( e3vec n gid -- minvec )
	here { minvec } MINS cells allot
	swap 0 do ( e3vec gid )
		over i cells 3 * + ( e3vec gid e3p )
\		dup @ 2 pick . . crlf
		dup @ 2 pick = if ( e3vec gid e3p )
			dup 2 cells + @ swap cell+ @ ( e3vec gid ds time )
			4 cells + @ ( e3vec gid ds min ) swap over + swap
			do
				minvec i cells + 1 swap +!
			loop
			0
		endif
		drop
	loop
	2drop minvec
;

: maxmin ( minvec -- m )
	0 0 { mi mv }
	MINS 0 do ( minvec )
		dup i cells + @
		dup mv > if
			to mv
			i to mi
		else
			drop
		endif
	loop
	drop mi
;

: p4a ( fn-addr u -- gv )
	rdents 2dup sortents flatpairs 2dup gcounts sleepy dup { gi }
	minvec maxmin gi swap
	*
;

: agid ( buf gid -- )
	here 2 pick - cell / 0 ?do ( buf gid )
		over i cells + @ over ( buf gid thisgid gid )
		= if
			unloop 2drop exit
		endif
	loop
	, drop
;

: gids ( ev3s n -- gids n )
	here { seen }
	0 do ( ev3s )
		dup i cells 3 * + @ ( ev3s gid )
		seen swap agid ( ev3s )
	loop
	drop seen here seen - cell /
;

: buildgmap ( ev3s nev3 gids ngids -- gmap )
	ihtalloc { gmap }
	dup 0 do ( ev3s nev3 gids ngids )
		2swap 2dup ( gids ngids ev3s nev3 ev3s nev3 )
		5 pick i cells + @ ( gids ngids ev3s nev3 ev3s nev3 gid )
		minvec ( gids ngids ev3s nev3 mv )
		4 pick i cells + @ ( gids ngids ev3s nev3 mv gid )
		swap gmap -rot htput ( gids ngids ev3s nev3 )
		2swap
	loop
	2drop 2drop gmap
;

: sleepy2 ( gmap gids n -- gid s )
	0 0 0 { ms mi mv }
	0 do ( gmap gids )
		dup i cells + @ ( gmap gids gid )
		2 pick swap htget ( gmap gids mv )
		0= if crash! endif
		dup 
		maxmin ( gmap gids mvec m )
		2dup cells + @ ( gmap gids mvec m mval )
		dup mv > if
			to mv
			to ms
			over i cells + @ to mi
		else
			2drop
		endif
		drop
	loop
	2 drop mi ms
;

: p4b ( fn-addr u -- gv )
	rdents 2dup sortents flatpairs 2dup gids
	2swap 2over buildgmap ( gids ngids gmap )
	{ gmap } gmap
	-rot sleepy2 2swap 2drop
	*
	\ not: 35472 (too low)
	\ not: 36211
	\ it was 37886!
;

: tests>ent
	S" [1518-11-01 03:45] Guard #10 begins shift" s>ent
		dup ent-evt @ EGUARD S" p4/s>ent0" advcheck
		dup ent-id @ 10 S" p4/s>ent1" advcheck
		dup ent-time @ @ 1518 S" p4/s>ent2" advcheck
		ent-time @ cell+ @ 11 S" p4/s>ent3" advcheck
;

: testsortents
	S" [1518-11-01 00:25] wakes up" s>ent
	S" [1518-11-01 00:00] Guard #10 begins shift" s>ent
	S" [1518-11-02 00:05] falls asleep" s>ent
	S" [1518-11-01 00:30] falls asleep" s>ent

	, , , ,
	here 4 cells - 4 sortents
	\ TODO: actual test cases here!
;

: test
	tests>ent
	testsortents

	S" p4test.txt" p4a 240 S" p4a/1" advcheck
	S" p4in.txt" p4a 35184 S" p4a/2" advcheck

	S" p4test.txt" p4b 4455 S" p4b/1" advcheck
	S" p4in.txt" p4b 37886 S" p4b/2" advcheck
;
