\ p3.fs - day 3 solutions!

S" base.fs" included

struct
	cell% field claim-next
	cell% field claim-num
	cell% field claim-x
	cell% field claim-y
	cell% field claim-w
	cell% field claim-h
end-struct claim%

: s>idx ( c-addr u -- i )
	1 /string s>number? drop d>s
;

\ String like: xcy, where x and y are integers and c is any one character.
: s>p ( c-addr u -- x y )
	0 s>d 2swap >number ( xd c-addr u )
	1 /string 0 s>d ( xd c-addr u 0d )
	2swap >number ( xd yd c-addr u )
	2drop d>s -rot d>s swap ( x y )
;

: s>c ( c-addr u -- claim )
	split claim% %allocate throw -rot drop ( claim tvec )
	dup 2@ s>idx ( claim tvec index )
	2 pick claim-num !
	2 cells + ( claim tvec+2 )
	2 cells + ( claim tvec+4 )
	dup 2@ s>p ( claim tvec x y )
	3 pick claim-y !
	2 pick claim-x !
	2 cells + ( claim tvec+6 )
	2@ s>p ( claim w h )
	2 pick claim-h !
	over claim-w ! ( claim )
;

: s>cn ( oclaim c-addr u -- claim )
	s>c swap over claim-next !
;

256 constant maxline
create linebuf maxline 2 + allot

\ Given the filename for a file full of claims, returns a linked list of parsed
\ claims from that file.
: rdclaims ( fn-addr u -- claims )
	r/o open-file throw 0 ( wfileid claims )
	begin
		linebuf maxline 3 pick read-line throw ( wfileid claims len f )
		0= if
			drop swap drop exit
		endif
		linebuf swap ( wfileid claims buf len )
		s>cn ( wfileid claims )
	again
;

\ NOTE FOR LATER:
\ Next steps: build a big array of counts, apply the list of claims to the big
\ array, then check how many cells have counts >= 2. I think I already did the
\ hard part...

1000 constant ARRAYSZ

: mkarr
	ARRAYSZ dup * cells allocate throw
;

: acp ( array x y -- p )
	ARRAYSZ * + cells +
;

: ac ( array claim -- )
	dup claim-h @ 0 do
		dup claim-w @ 0 do
			( array claim )
			over over claim-x @ i + ( array claim array x )
			2 pick claim-y @ j + ( array claim array x y )
			acp 1 swap +! ( array claim )
		loop
	loop
	2drop
;

: acs ( array claim -- )
	begin
		2dup ac
		claim-next @
		dup 0=
	until
	2drop
;

: count>1 ( array -- n )
	0
	ARRAYSZ 0 do
		ARRAYSZ 0 do
			over ( array total array )
			i j acp @ ( array total cellval )
			1 > if 1+ endif
		loop
	loop
	nip
;

: p3a ( fn-addr u -- n )
	mkarr -rot rdclaims over swap acs dup count>1 swap free throw
;

: tests>c
	S" #1 @ 2,3: 4x5" s>c ( claim )
	dup claim-num @ 1 S" p3/s>c1" advcheck
	dup claim-x @ 2 S" p3/s>c2" advcheck
	dup claim-y @ 3 S" p3/s>c3" advcheck
	dup claim-w @ 4 S" p3/s>c4" advcheck
	dup claim-h @ 5 S" p3/s>c5" advcheck
;

: testac
	mkarr
	S" #1 @ 2,3: 4x5" s>c
	over swap ac
	S" #2 @ 3,3: 1x1" s>c
	over swap ac

	dup 2 3 acp @ 1 S" p3/ac1" advcheck
	dup 3 3 acp @ 2 S" p3/ac2" advcheck
	dup 0 0 acp @ 0 S" p3/ac3" advcheck

	free throw
;

: testacs
	mkarr dup
	S" p3test.txt" rdclaims
	acs

	dup 3 2 acp @ 1 S" p3/acs1" advcheck
	dup 3 3 acp @ 2 S" p3/acs2" advcheck

	dup count>1 4 S" p3/acs3" advcheck

	free throw
;

: test
	tests>c
	testac

	S" p3test.txt" p3a 4 S" p3a" advcheck
;
