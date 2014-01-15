AYAY
====

AYAY Kaeppttn! for your favourite Amstrad/Schneider CPC


some collected ideas & notes most of which could be turned into an issue
====


== general ==

== player ==

== SID player ==

== AY player ==

== tracks ==

== instruments ==

Instruments currently have one target register which is addressed by the wave table, so meaningful values in the target register would be 8, 9 and 10. This could be extended in a way that each of the 4 tables could have a target (register) which could be on of the following: 

SID:
semi -> semi (default)
tone -> tone (default)
wave -> wave (default)
vol -> vol (default)

AY:
semi ->
tone ->
wave -> reg6 (to use the unused wave table as noise table on AY voices)
vol ->

== display ==

== file io ==

== memory management ==
$c2 memory layout (on 128k machines) editor & everything not needed by the player in bank 0,
player, instruments, tracks etc. in bank 1



