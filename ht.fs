\ hash.fs - hash table / set library

struct
	cell% field htb-next
	cell% field htb-key
	cell% field htb-val
end-struct htb%

struct
	cell% field ht-efunc
	cell% field ht-hfunc
	cell% field ht-bvec
	cell% field ht-nb
end-struct ht%

: htinit ( hash hfunc efunc -- )
	2 pick ht-efunc !
	over ht-hfunc !
	1024 over ht-nb !
	1024 cells allocate throw swap ht-bvec !
;

: htalloc ( hfunc efunc -- )
	ht% %allocate throw dup
	2swap htinit
;

: ihtalloc ( -- )
	0 ['] = htalloc
;

: htbsearch ( ef key bptr -- bptr )
	dup 0 = if
		2drop drop 0 exit
	endif
	begin
		( ef key bptr )
		dup htb-key @ ( ef key bptr bkey )
		2 pick 4 pick ( ef key bptr bkey key ef ) execute
		if ( ef key bptr )
			-rot 2drop exit
		endif
		htb-next @
		dup 0 = if
			2drop drop 0 exit
		endif
	again
;

: htbptr ( hash key -- bucket )
	over ht-hfunc @ ( hash key hfunc )
	dup 0<> if execute else drop endif ( hash hval )
	over ht-nb @ mod ( hash bidx )
	cells swap ht-bvec @ +
;

: htget ( hash key -- value flag )
	over ht-efunc @ ( hash key efunc )
	-rot swap over ( efunc key hash key )
	htbptr @ ( efunc key bptr )
	htbsearch
	dup 0<> if
		htb-val @ -1
	else
		drop 0 0
	endif
;

: htbmake ( key value next -- htb )
	htb% %allocate throw { htb }
	htb htb-next !
	htb htb-val !
	htb htb-key !
	htb
;

: htput ( hash key value -- )
	{ hash key value } value
	hash key htbptr ( value bptr )
	swap key swap ( bptr key value )
	2 pick @ ( bptr key value next )
	htbmake ( bptr nbptr )
	swap !
;

: test
	ihtalloc
	dup 3 5 htput
	dup 3 htget . .
	dup 4 htget . .
;
