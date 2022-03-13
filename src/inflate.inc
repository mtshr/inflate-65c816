; specify the address where the Inflate routine will be located.
; currently the code size is 0x5DB bytes with UNSAFE, and 0x68B without.
.DEFINE Inflate $7E5000

; must be in the same bank as the routine.
; requires 0x46A bytes
.DEFINE InflateVariable $7E568B
