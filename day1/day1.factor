! Copyright (C) 2018 J. Lucas
! See http://factorcode.org/license.txt for BSD license.
USING: kernel sequences io unix io.encodings.utf8 io.files splitting math.parser ;
IN: day1

: solution-1 ( path -- n )
    utf8 file-contents "\n"
    split [ string>number ] map
    f swap remove sum ;
    
    
    
