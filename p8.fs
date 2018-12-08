\ p8.fs - some integer trees!
\ General strategy: parse the input into an actual tree in memory, then
\ implement operations on the tree. This turned out to be the right call for
\ part b as well.
\
\ When you see 'vaddr' below that is the base of a vector of strings,
\ represented as (c-addr, u) cell pairs in memory.

S" base.fs" included

\ Consumes one string from vaddr and converts it to an int, returning the
\ rest of vaddr, the new length of vaddr, and the converted int.
: rdint ( vaddr n -- vaddr n i )
	over 2@ s>number? drop d>s ( vaddr n i )
	-rot 1- swap 2 cells + swap rot
;

\ Consumes a tree header (two ints) from vaddr and returns the new vaddr and
\ the two parsed ints.
: rdhead ( vaddr n -- vaddr n nc nm )
	rdint -rot rdint -rot 2swap
;

\ Allocates space for a tree with nc children and nm metadata entries. The
\ tree format is simple:
\    nc [1 cell] nm [1 cell] children [nc cells] metadata [nm cells]
\ Returns the newly allocated tree.
: alloctree ( nc nm -- t )
	here { a } 2dup + 2 + cells allot
	a cell+ !
	a !
	a
;

\ Accessors for trees. These two read the numbers of children and metadata
\ entries respectively.
: nc ( tree -- n ) @ ;
: nm ( tree -- n ) cell+ @ ;

\ These return pointers into the children and metadata areas of the trees -
\ t i cp returns a pointer to the ith child slot of t, and t i mp returns a
\ pointer to the ith metadata slot of t.
: cp ( tree i -- p ) 2 + cells + ;
: mp ( tree i -- p ) 2 + over nc + cells + ;

\ This is the recursive tree-building function. It consumes part of vaddr,
\ returning the new vaddr and remaining length, and also constructs and returns
\ a new tree. The recursive calls (see RECURSE in the body) in turn consume
\ from vaddr, which takes care of skipping over the contents of child trees.
: mktree ( vaddr n -- vaddr n tree )
	rdhead alloctree { tree } ( vaddr n )
	tree nc 0 ?do ( vaddr n )
		recurse tree i cp ! ( vaddr n )
	loop
	tree nm 0 ?do ( vaddr n )
		rdint tree i mp ! ( vaddr n )
	loop
	tree
;

\ Read the input from a named file (it's all on one line) then split it into
\ a heap-allocated vector. Using regular split here wouldn't work since we're
\ about to use the dictionary space to allocate tree nodes!
: p8slurp ( c-addr n -- vaddr n )
	slurp-file asplit
;

\ Recurse down the given tree, summing all the metadata entries of every tree.
\ This does a postorder traversal.
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

\ Recurse down the tree, computing node values per part b:
\   If a node has no children, sum its metadata (by using SUMMETA)
\   If a node has children, treat each metadata entry as an index into its
\   vector of children and recurse on them
: nodeval ( tree -- n )
	0 ( tree sum )
	over nc 0= if
		drop summeta exit
	endif
	over nm 0 ?do ( tree sum )
		over i mp @ ( tree sum mi )
		\ Metadata that are 0 or > nc don't index valid children; skip.
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
