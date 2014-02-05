AYAY
====

AYAY Kaeppttn! for your favourite Amstrad/Schneider CPC


some collected ideas & notes most of which could be turned into an issue
====


== General ==



== Player ==

== SID player ==

sid0001:
When the current SID instrument targets e.g. reg8, the write to reg8 that is part of the AY engine must not also write to reg8, but another (dummy) register, like 14 (or higher.. what happens when writing reg15, 16..?) to avoid
disrupting the wave form

== AY player ==

ay0001:
Add support for register 6 (see ins0001)

ay0002:
Add support for register 11 (hardware envelope shape) and 12/13 (hardware envelope period). This also needs to be reflected in the instruments in some way (see ins0001 for example)

== Sequence ==

seq0001:
Generalize the instrument table editor the be usable for sequence editing as well (see ins0002)

== Tracks ==

tr0001:
Finish the track editor

tr0002:
Fix track overlapping. Track 01 reaches into track 02 because the editor does always show 64 rows (independent of the actual row size - which is another issue..) so looking at row 32 of track 01 is looking at row 0 of track 02 and modifiying any of both tampers the adjacent track.

== Instruments ==

ins0001:
Instruments currently have one target register which is written to using the wave table (when targeting SID), so meaningful values in the target register would be AY volume registers 8, 9 and 10. This could be extended in a way that each of the 4 tables could have a target (register or more generally a location in memory from which the value is then used for anything, included writing to an AY register), for example:

SID:
semi -> semi (default)
pitch -> pitch (default)
wave -> wave (default)
vol -> vol (default)

AY:
wave -> reg6 (to make use of the otherwise unused wave table as noise table on AY voices)
semi -> reg11/12/13, effectively creating buzzer sounds. With buzzers there's no need to also play arpeggios (i.e. apply the semi table), so that table could be used to controll hardwave enveloping
vol -> reg11/12/13 (the same as above)

example: an AY instrument that does buzzers could target reg11, 12, 13 independently from e.g. the semi, wave, vol tables (leaving pitch for vibrato effects etc.)

ins0002:
add support for sound banks, i.e. more than 1 table of 256 values per semi, pitch, wave and vol (to allow more and more flexible instruments, especially wave table stuff..)

ins0003:
Adjust the editor to the generalized the table edit functionality (see seq0001)

== Display ==

dis0001:
Use a fast mode-2 text-output routine instead of TXT OUT CHAR

== File io ==

fio0001:
Use a special ascii file i/o routine which writes/reads directly to/from the amsdos ascii file buffer instead of calling CAS OUT CHAR (which is incredibly slow)

== Memory management ==

mem0001:
Create a memory manager (object) from the current memory management routines which is initialized with the start address of its heap (and maybe also an end address, $ffff otherwise) that takes care of allocating chunks of memory,
freeing them, re-using freed blocks, collecting garbage (from freed blocks) etc. (see mem0002, mem0003)

mem0002:
$c2 memory layout (on 128k machines) editor & everything not needed by the player in bank 0,
player, instruments, tracks etc. in bank 1

mem0003:
When using $c2 memory layout we need 2 instances of the memory manager, one for each memory bank, which can handle their respective banks memory independently

mem0004:
Write helper functions to read/write from/to the $c2 bank, transfer blocks of memory from/to there.
