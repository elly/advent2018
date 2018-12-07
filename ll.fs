\ ll.fs - linked-list library

struct
	cell% field lln-next
	cell% field lln-val
end-struct lln%

struct
	cell% field ll-head
	cell% field ll-tail
end-struct ll%

: llalloc ( -- ll )
	ll% %allocate throw
	0 over ll-head !
	0 over ll-tail !
;

: llnalloc ( data -- lln )
	lln% %allocate throw
	swap over lln-val !
	0 over lln-next !
;

: lladd ( ll data -- )
	llnalloc
	over ll-tail @ 0<> if ( ll lln )
		over ll-tail @ lln-next ! ( ll )
		dup ll-tail @ lln-next @ swap ll-tail ! ( )
	else
		2dup swap ll-tail !
		swap ll-head !
	endif
;

: llshift ( ll -- lln )
	dup ll-head @ dup 0<> if ( ll llp )
		dup lln-next @ ( ll llp nextp )
		?dup-if
			rot ll-head !
		else
			swap 0 over ll-head !
			0 swap ll-tail !
		endif
		lln-val @
	else
		nip
	endif
;

: llempty? ( ll -- f )
	ll-head @ 0=
;
