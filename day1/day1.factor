! Copyright (C) 2018 
! See http://factorcode.org/license.txt for BSD license.
USING: kernel sequences io unix io.encodings.utf8 io.files splitting math.parser circular assocs math prettyprint tools.continuations continuations  namespaces lists formatting ;
IN: day1

: get-data ( path -- data )
    utf8 file-contents "\n"
    split [ string>number ] map
    f swap remove ;
    
: solution-1 ( path -- n )
    get-data sum ;

: search-frequency ( fset cur -- fset cur )
    [ + ] accumulate ;

: find-frequency ( fset lset -- res )
    swap [ over member? ] find 2nip ;

: return-frequency ( fset rset -- lset )
    2dup [ over member? ] filter nip dup empty?
    [ drop append ]
    [ nip find-frequency ] if ;

: solution-2-loop ( fset cur list -- fset cur list )
    [ 2dup search-frequency ] dip
    [ nip ] 2dip return-frequency
    dup sequence? [ solution-2-loop ]
    [ ] if ;

: solution-2 ( fset cur -- fset cur )
    2dup search-frequency [ nip ] dip
    solution-2-loop swap . ;

: solution-run ( -- )
    "input" get-data 0 solution-2
    "%d" printf drop ;

MAIN: solution-run
