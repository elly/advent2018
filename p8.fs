S" base.fs" included

: rdint ( addr n -- addr n i )
	over 2@ s>number? drop d>s ( addr n i )
	-rot 1- swap 2 cells + swap rot
;

: rdhead ( addr n -- addr n nc nm )
	over 2@ s>number? drop d>s ( addr n nc )
	-rot over 2 cells + 2@ s>number? drop d>s ( nc addr n nm )
	-rot 2 - swap 4 cells + swap 2swap ( addr n nc nm )
;

: alloctree ( nc nm -- )
	here { a } 2dup + 2 + cells allot
	a cell+ !
	a !
	a
;

: nc ( tree -- n ) @ ;
: nm ( tree -- n ) cell+ @ ;
: cp ( tree i -- p ) 2 + cells + ;
: mp ( tree i -- p ) 2 + over nc + cells + ;

: mktree ( addr n -- addr n tree )
	rdhead alloctree { tree } ( addr n )
	0 tree nc 0 ?do ( addr n ci )
		-rot recurse ( ci addr n child )
		tree 4 pick cp ! ( ci addr n )
		rot 1+
	loop
	drop 0 tree nm 0 ?do ( addr n mi )
		-rot rdint ( mi addr n m )
		tree 4 pick mp ! ( mi addr n )
		rot 1+
	loop
	drop tree
;

: p8slurp ( c-addr n -- addr n )
	slurp-file asplit
;

: summeta ( tree -- n )
	0 ( tree sum )
	over nc 0 ?do ( tree sum )
		over i cp @ ( tree sum child )
		recurse +
	loop
	over nm 0 ?do
		over i mp @ +
	loop
	nip
;

: p8a ( c-addr n -- n )
	p8slurp mktree summeta
;

: nodeval ( tree -- n )
	0 ( tree sum )
	over nc 0= if
		drop summeta exit
	endif
	over nm 0 ?do ( tree sum )
		over i mp @ ( tree sum mi )
		dup 0<> over 4 pick nc <= and if
			1- 2 pick swap cp @ ( tree sum child )
			recurse + ( tree sum )
		else
			drop
		endif
	loop
	nip
;

: p8b ( c-addr n -- n )
	p8slurp mktree nodeval
;

: test
	S" p8test.txt" p8a 138 S" p8a/1" advcheck
	S" p8in.txt" p8a 46096 S" p8a/2" advcheck

	S" p8test.txt" p8b 66 S" p8b/1" advcheck
	S" p8in.txt" p8b 24820 S" p8b/2" advcheck
;
