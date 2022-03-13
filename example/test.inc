.MEMORYMAP
DEFAULTSLOT 0
SLOTSIZE $8000
SLOT 0 $8000
.ENDME

.ROMBANKSIZE $8000
.LOROM
.ROMBANKS 1
.EMPTYFILL $FF

/****************************************************************************
header information
****************************************************************************/

.SNESHEADER
	ID "    "

	NAME "inflate test         "
	;    "123456789012345678901"

	SLOWROM
	LOROM

	CARTRIDGETYPE $00				; $00 = ROM only
	ROMSIZE $08						; $08 = 2 Mbits
	SRAMSIZE $00					; No SRAM
	COUNTRY $00						; Japan
	LICENSEECODE $00
	VERSION $00
.ENDSNES

.SNESNATIVEVECTOR
	COP EndOfInterrupt
	BRK EndOfInterrupt
	ABORT EndOfInterrupt
	NMI EndOfInterrupt
	IRQ EndOfInterrupt
.ENDNATIVEVECTOR

.SNESEMUVECTOR
	COP EndOfInterrupt
	ABORT EndOfInterrupt
	NMI EndOfInterrupt
	RESET BootROM
	IRQBRK EndOfInterrupt
.ENDEMUVECTOR
