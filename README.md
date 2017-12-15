# ptf2c
Paper-tape text format to C for 6502.

Use: PTF2C prog.ptf

This will generate prog.h & prog.c in the same directory.

The file format is described here: http://users.telenet.be/kim1-6502/6502/usrman.html#F

Quoting from appendix F:

        Each record outputted begins with a ";" character (ASCII 3B) to
    mark the start of a valid record.  The next byte transmitted (18HEX) or
    (2410) is the number of data bytes contained in the record.  The record's
    starting address High (1 byte, 2 characters), starting address Lo (1 byte,
    2 characters), and data (24 bytes, 48 characters) follow.  Each record is
    terminated by the record's check-sum (2 bytes, 4 characters), a carriage
    return (ASCII OD), line feed (ASCII øA), and six "NULL" characters
    (ASCII øø).

        The last record transmitted has zero data bytes (indicated by ;00)
    The starting address field is replaced by a four digit Hex number repre-
    senting the total number of data records contained in the transmission,
    followed by the records usual check-sum digits.  A "XOFF" character ends
    the transmission.


    ;180000FFEEDDCCBBAA0099887766554433221122334455667788990AFC
    ;0000010001

This is one of the possible output formats used by the win2c64 assembler:

    http://www.aartbik.com/MISC/c64.html

For example:

    win2c64 -P prog.s

This generates prog.ptf that may in turn be consumed by ptf2c.

