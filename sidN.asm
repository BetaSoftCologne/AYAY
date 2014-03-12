#target bin
;#code $4000,$1000

; tone duration in jam mode dependent on key press
; endless tone duration in play mode 


; gate-array / memory configs:
; $c1 = 
; $c2 = 4 mal 2. Bank
; $c3 = 



; player constants
SCANLINES_TOTAL     equ 312
NOPS_TOTAL          equ 64 * SCANLINES_TOTAL 
NOPS_PER_SAMPLE     equ 72
ITER_TOTAL          equ (NOPS_TOTAL / NOPS_PER_SAMPLE) ; = 277 
EXTRA_SAMPLES       equ 5 ; samples sent outside of waveloop (TODO where is that?)
ITERATIONS          equ (ITER_TOTAL - EXTRA_SAMPLES) / 2
META_SIZE           equ 1
INSTR_SIZE          equ 4 + META_SIZE

; editor constants
WAVETBL_COL         equ 2
INSTR_PARAMS        equ 5
TRACK_COL_MAX       equ 2
TRACK_ROW_MAX       equ 32
TRACK_ROWS_DISP     equ 9
VOICE_SIZE          equ 32 * 3 ; 32 rows a 3 bytes

; jump table constants
RANGE_PAUSE_START   equ 1
RANGE_PAUSE_LEN     equ 127 ; = 1 to 127
RANGE_NOTES_START   equ RANGE_PAUSE_START + RANGE_PAUSE_LEN
RANGE_NOTES_LEN     equ 96  ; = 128 to 224
RANGE_EFFECTS_START equ RANGE_NOTES_START + RANGE_NOTES_LEN
RANGE_EFFECTS_LEN   equ 31

; misc constants 
SEQ_END             equ $ff
TRK_END             equ $ff
TRK_SLIDE           equ $80


; end of the free store
FREE_STORE_END:     equ $a600

; wave tables 
; TODO use indices (see init_waves) instead of e.g. w_square
w_square            equ 0 
w_ramp              equ 1
w_sinus             equ 2
w_triangle          equ 3
w_silence           equ 4
w_pulse             equ 5

; i/o
file_buffer         equ $c000

    
;
; Start
;
    org $0300

; jump block
    jp jam
    jp player_init


;
; Jam mode
;
jam:
    ; init soundchip 
    call init_psg

    ; init engine
    call init_engine

    ; 
    call init_screen
    
    ; disable AY replay
    call ay_toggle

    ; set player mode
    call playmode_jam

    ; jam
jam_loop:
    call $bb1b
    jr nc,jam_loop

    ld b,a
    push af
    
    ; in track mode?
    ld a,(track_mode)
    or a
    jr z,jam_play

    ; yes
    ld a,(track_col)
    or a
    jr z,jam_play

    ; not on note column!
    pop af
    call func_key
    jr jam_loop

jam_play:
    ; play the pressed key
    call play_key
    pop af
    call func_key
    jr jam_loop


init_screen:
    ; init screen
    ld a,2
    call $bc0e

    ; init editor
    call init_editor

    ; show memory stats
    call mem_stats

    ret

; init editor
init_editor:

    ; octave
    ld a,(octave)
    call store_octave

    ; edit modes
    call modes_init

    ; select inst 0
    xor a
    call instr_select

     ; enable mem-stats
    ld a,1
    ld (print_memstats),a

    ; select 1st track
    ld a,1
    call track_select

    ; draw sequence editor
    call seq_draw

    ret


; calc tone index from octave + tone
; a = octave
; b = tone
; => a = tone index
get_tone_index:
    dec a   ; starts at octave 1! 
    ; octave * 12
    add a
    add a   ; = a*4
    ld c,a
    add a   ; = a*8
    add c   ; + a*4

    ; + tone
    add b
    ret

; look up tone for a pressed key
; in: B = char
play_key:
    ld hl,keytab

    ; reset last played note
    xor a
    ld (last_note),a

key_loop:
    ld a,(hl)   ; compare char
    or a
    ret z
    cp b
    jr z,key_found  ; match
    inc hl
    inc hl
    inc hl
    jr key_loop ; keep lokoing

key_found:
    ; ascii code, tone, scan row
    inc hl      ; skip code
    ld b,(hl)   ; get tone in b
    inc hl
    ld a,(hl)   ; get key-code

    ; store key-code
    ld (key_code+1),a

    ; add octave
    ld a,(octave)
    call get_tone_index

    ; store in jam track
    ld (jam_track),a

    ; store tone
    ld a,b
    inc a ; add 1!
    ld (last_note),a

    ; reset tone length
    ld hl,0
    ld (tone_length+1),hl

    ; restart dummy sequence
    xor a
    ld (seq_jam),a
  
    ; play jam track
    call player_start

    ret

; last tone in jam mode
last_note:
    db 0



; mappings of key to offset and scancode 
; 3 bytes per key:
; +0 = ascii code of pressed key
; +1 = half tone offset
; +2 = keyboard row no. for key-release check
keytab:
    ; upper manual
    db $71,12,$48   ; q C
    db $32,13,$48   ; 2 C#
    db $77,14,$47   ; w D
    db $33,15,$47   ; 3 D#
    db $65,16,$47   ; e E
    db $72,17,$46   ; r F
    db $35,18,$46   ; 5 F#
    db $74,19,$46   ; t G
    db $36,20,$46   ; 6 G#
    db $79,21,$45   ; y A
    db $37,22,$45   ; 7 A#
    db $75,23,$45   ; u B
    db $69,24,$44   ; i C
    db $39,25,$44   ; 9 C#
    db $6f,26,$44   ; o D
    db $30,27,$44   ; 0 D#
    db $70,28,$43   ; p E

    ; lower manual
    db $7a, 0,$48   ; z C
    db $73, 1,$47   ; s C#
    db $78, 2,$47   ; x D
    db $64, 3,$47   ; d D#
    db $63, 4,$47   ; c E
    db $76, 5,$46   ; v F
    db $67, 6,$46   ; g F#
    db $62, 7,$46   ; b G
    db $68, 8,$45   ; h G#
    db $6e, 9,$45   ; n A
    db $6a,10,$45   ; j A#
    db $6d,11,$44   ; m B
    db $2c,12,$44   ; , C
    db $6c,13,$44   ; l C#
    db $2e,14,$43   ; . D
    db $3a,15,$43   ; : D#
    db $2f,16,$43   ; / E

    ; end of table
    db 0



; look up function for a pressed key
; in: A = key
func_key:
    ld (char_current),a
    ld b,a ; used in func_loop

    ; look for function in current mode
    ld hl,no_match1 ; where to jump to
    push hl     ; in case of mismatch
    ld hl,(mode_current)
    
func_loop:
    ; look for function
    ld a,(hl)
    or a
    ret z ; jump to no-match fn
    cp b
    jr z,func_found ; match
    inc hl
    inc hl
    inc hl
    jr func_loop

func_found:
    pop de      ; discard no-match fn ptr

    ; fetch target fn ptr
    inc hl      ; skip char code
    ld e,(hl)   ; read
    inc hl      ; address
    ld d,(hl)   ; into de
    push de
    ret     ; jp (de)

no_match1:
    ; function not found in current mode
    ; try global function table
    ld hl,no_match2
    push hl
    ld hl,global_funcs
    jr func_loop

no_match2:
    ; catch_all available?
    ld hl,(catch_all)
    ld a,h
    or l
    jp nz,call_hl ; yes, call it

func_print_key:
    ; no fn for key, print key code (in b)
    ld hl,$4e19
    call locate
    ld a,b
    call hex_out
    ret

; global function keys
global_funcs:
    db 9 ; TAB
    dw select_mode

    db $05
    dw init_screen

    db $0c ; crtl L
    dw file_load
    db $13 ; ctrl S
    dw file_save
    db $12 ; ctrl R
    dw beep
    db $1a ; crtl Y
    dw ay_toggle

;    db $14 ; ctrl T
;    dw new_mem_test

    db 32 ; space
    dw track_play

    db $7f ; del
    dw init_psg

    db $81 ; f1
    dw toggle_A
    db $82 ; f2
    dw toggle_B
    db $83 ; f3
    dw toggle_C

    db $88 ; f8
    dw inc_octave
    db $85 ; f5
    dw dec_octave

    db $87 ; f7
    dw instr_next
    db $84 ; f4
    dw instr_prev

    ; end of table
    db 0



; init edit mode selection
modes_init:
    ld hl,mode_list
    jr enable_mode

; select next edit mode
select_mode:
    ; call leave callback
    ld hl,(leave_callback)
    ld a,h
    or l
    call nz,call_hl

    ; get mode list ptr
    ld hl,(mode_ptr)
enable_mode:
    ; get next mode from list
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld a,e
    or d
    jr nz,tab_store

    ; restart list 
    ld hl,mode_list
    jr enable_mode

tab_store:
    ; store new mode
    ld (mode_ptr),hl
    ex de,hl
    
    ; read enter-callback
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ex de,hl
    ld (enter_callback),hl
    ld (redraw_callback),hl
    ex de,hl
    push de ; store enter-callback addr

    ; read leave-callback
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld (leave_callback),de

    ; read catch-all fn ptr
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld (catch_all),de

    ; activate new edit-mode
    ld (mode_current),hl

    ; call enter-fn?
    pop hl
    ld a,h
    or l
    ret z   ; no

    push hl ; yes
    ret


;
; params for tab-key switch
;

; active edit-mode 
mode_current:
    dw 0

; entering new edit-mode 
enter_callback:
    dw 0

; leaving current edit-mode
leave_callback:
    dw 0

; re-drawing current mode
; @note: only used in instrument editor!
redraw_callback:
    dw 0

; catch-all fn for current mode
catch_all:
    dw 0

; pointer into list of edit-modes
mode_ptr:
    dw mode_list

; list of edit-modes
mode_list:
    dw mode_params
    dw mode_instr
    dw mode_sequence
    dw mode_track
    dw 0

; the most recent keys char code
char_current:
    db 0


;
; File IO
;


; load 
file_load:
    call read_filename
    cp $fc
    ret z
    call beep

    ; TODO
    ret

; save 
file_save:
    call read_filename
    cp $fc
    ret z
    call beep

    ; TODO!
    ret;

; read filename from keyboard
read_filename:
    ld hl,$0110
    call locate
    call print_local
    db "filename: ", 0
    call cursor_on
    ld hl,filename
    push hl
    ld b,8
    call read_string
    push af
    call cursor_off
    pop af
    pop hl
    ret

; current filename  
filename:
    ds 8
filetype:
    db ".ayb"
    db 0


; switch AY skipping on / off
ay_toggle:
    ; switch
    ld a,(skip_flag)
    xor 1
    ld (skip_flag),a
    push af

    ; copy jump code
    add a
    ld d,0
    ld e,a
    ld hl,skip_table
    add hl,de
    ld e,(hl)
    inc hl
    ld d,(hl)
    ex de,hl
    ld de,skip_target
    ldi
    ldi
    ldi

    ; print
    ld hl,$3001
    call locate
    call print_local
    db "AY replay: ",0
    pop af
    call hex_nib
    
    ret

skip_flag:
    db 1

; table of addresses to skip blocks to use
skip_table:
    dw skip_code
    dw noskip_code

; skip code, will be used to disable skip AY music
skip_code:
    jp wave_loop

; no-skip code, will be used to enable AY music
noskip_code:
    ds 3



; beep
beep:
    ld a,7
    jp $bb5a

; toggle AY voices
toggle_A:
    ld a,(reg7mask+1)
    xor %00000001
    ld (reg7mask+1),a
    bit 0,a
    ld b,0
    jr print_voice

toggle_B:
    ld a,(reg7mask+1)
    xor %00000010
    ld (reg7mask+1),a
    bit 1,a
    ld b,1
    jr print_voice

toggle_C:
    ld a,(reg7mask+1)
    xor %00000100
    ld (reg7mask+1),a
    bit 2,a
    ld b,2

print_voice:
    ld d,1 ; on
    jr z,print_voice2
    dec d  ; off
; d = value to print (0,1)
print_voice2:
    ld a,b
    add a
    add a
    add a
    ld hl,$3c04
    add h
    ld h,a   
    call locate
    ld a,d
    add $30
    jp print_char

; increment octave
inc_octave:
    ld a,(octave)
    cp 7
    ret z
    inc a
    jr store_octave

; decrement octave
dec_octave:
    ld a,(octave)
    cp 1
    ret z
    dec a

; store & display current octave 
store_octave:
    ld (octave),a
    push af
    ld hl,$0c01
    call locate
    call print_local
    db "octave: ", 0
    pop af
    call hex_out
    ret
    
; current octave
octave:
    db 4


; select next instrument
instr_next:
    ld hl,instr_max
    ld a,(instr_current)
    inc a
    cp (hl)
    jr c,instr_select
    xor a
    jr instr_select


; select previous instrument
instr_prev:
    ld a,(instr_current)
    sub 1
    jr nc,instr_select
    ld a,(instr_max)
    dec a
    jr instr_select


; select an instrument 
; a = instrument no.
instr_select:
    ; store instr no
    ld (instr_current),a

    ; also in jam track
    ld (jam_track+2),a

    push af
    call instr_get_addr
    ld (instr_current_addr),hl
    pop af

    ; display instr no
    call print_instr
    ret


; draw current waveform
draw_waveform:
    ; has it changed?
    ld hl,(current_val)
    ld a,(last_drawed_waveform)
    cp (hl)
    ret z ; no, don't draw again

    ; remove drawing if no jp-command
    push hl
    cp $80
    call nz,undraw
    pop hl

    ; get current waveform 
    ld a,(hl)
    cp $80
    ret z
    
    ; and its address
    ld hl,(waveform_list_ptr)
    ld l,a      ; add waveform index  
    ld h,(hl)   ; to get base addr
    ld l,0      ; of actual waveform

    ; draw waveform 
    ld a,1
    call draw_now
    ret

; remove last drawing
; a = waveform addr hi-byte
undraw:
    ; draw with pen 0
    ld h,a
    xor a
    ld l,a
    jp draw_now


; draw waveform in hl
; hl = waveform to draw
;  a = pen
draw_now:

    ; save to undraw later
    ld b,a
    ld a,h
    ld (last_drawed_waveform),a
    ld a,b

    ; set pen
    call $bbde

    ld b,64 ; = 0 => 256 iterations
    ld de,320
draw_loop:
    push bc
    push de
    push hl
    ld a,(hl)
    and 15
    push af

    ; gra move absolute
    ld hl,200
    call $bbc0

    ; plot current value
    ld h,0
    ld d,h
    ld e,h
    pop af
    add a
    add a
    ld l,a
    call $bbed ; plot relative

    pop hl
    ld de,4
    add hl,de
    pop de
    inc de
    pop bc
    djnz draw_loop

    ret

; the waveform that was drawn lastly
last_drawed_waveform:
    db $80



; get address of instrument
; a = instr no.
; => hl: address
instr_get_addr:
    ld l,a
    ld a,(instr_table_base)
    ld h,a
    ; hl = instr table ptr
    ld a,(hl)
    inc h
    ld h,(hl)
    ld l,a
    ; hl = instr addr
    ret


; current instrument
instr_current:
    db 0

; and its address
instr_current_addr:
    dw 0

; highest instr. number
instr_max:
    db 0

; base address (i.e. hi-byte) of instrument table
instr_table_base:
    db 0


;
; instrument editor 
; 

; instrument mode function keys
mode_instr:
    dw instr_draw_cursor    ; enter fn
    dw instr_hide_cursor    ; leave fn
    dw 0 ; catch_all

    db $20
    dw instr_set_pos

    db $f0
    dw instr_up
    db $f1
    dw instr_down
    db $f2
    dw instr_left
    db $f3
    dw instr_right
    db 13
    dw instr_enter

    db $f8
    dw inc_value
    db $f9
    dw dec_value

    db 0 ; end of table


; max values for instr. edit
INSTR_ROW_MAX:  equ 9
INSTR_COL_MAX:  equ 3


; get offset in current table
; =>  a: offset
; => bc: column
; => hl: offset table
get_table_offset:
    ; get current column
    ld a,(inst_col)
    ld b,0
    ld c,a  

    ; get offset
    ld hl,current_inst_offsets
    add hl,bc
    ld a,(hl) ; = current offset
    ret


; print one column of table data
; bc = column
print_one_column:
    ; print the right table
    ld hl,table_pointers
    add hl,bc
    add hl,bc
    ld e,(hl)
    inc hl
    ld d,(hl)
    ex de,hl
    jp (hl)

; function ptrs to print an instr. column
table_pointers:
    dw print_offset_tbl
    dw print_finetune_tbl
    dw print_wavetable_tbl
    dw print_volume_tbl


; instr. mode - cursor up
instr_up:
    push af
    ld hl,inst_row
    ld a,(hl)
    or a
    jr z,scroll_up
    push hl
    call instr_show_cur_val
    pop hl
    dec (hl)
    call instr_done
    jr instr_cancel

; scroll up 1 row
scroll_up:
    ; get column and offset
    call get_table_offset

    ; already at the top?
    or a
    jr z,instr_cancel

    ; go one step up
    dec (hl)
    jr redraw_column


; instr. mode - cursor down
instr_down:
    push af
    ld hl,inst_row
    ld a,(hl)
    cp INSTR_ROW_MAX
    jr nc,scroll_down

    push hl
    call instr_show_cur_val
    pop hl
    inc (hl)
    call instr_done
    jr instr_cancel

; scroll down 1 row
scroll_down:
    ; get column and offset
    call get_table_offset

    ; already at the bottom?
    cp 245
    jr nc,instr_cancel

    ; go one step down
    inc (hl)

redraw_column:
    ld a,(hl)
    
    ; re-draw
    call print_one_column
    call instr_done

    jr instr_cancel


; instr. mode - cursor left
instr_left:
    push af
    call instr_show_cur_val

    ld a,(inst_col)
    sub 1 ; to have c-flag set!
    jr nc,instr_left2
    ld a,INSTR_COL_MAX
instr_left2:
    ld (inst_col),a
    call instr_done
    pop af
    ret
    
; instr. mode - cursor right
instr_right:
    push af
    call instr_show_cur_val

    ld a,(inst_col)
    inc a
    cp INSTR_COL_MAX + 1
    jr c,instr_right2
    xor a
instr_right2:
    ld (inst_col),a
    call instr_done
    pop af
    ret
    
; do nothing
instr_cancel:
    pop af
    ret

; instr. mode - enter
instr_enter:
    ; print current value
    call instr_show_cur_val

    ; move cursor before first char
    ld a,8
    call print_char
    call print_char

    ; read hi-nibble
    call wait_nibble
    jr c,instr_done
    rlca
    rlca
    rlca
    rlca
    ld b,a

    ; read lo-nibble
    call wait_nibble
    jr c,instr_done
    or b

    ; store value
    ld hl,(current_val)
    ld (hl),a

    ; go to next position
    jp instr_down

; 
instr_done:
    ; re-draw cursor
    ld hl,(redraw_callback)
    call call_hl

    ; in waveform column?
    ld a,(inst_col)
    cp WAVETBL_COL
    ret nz
    
    ; yes, draw waveform
    jp draw_waveform


; increment current value
dec_value:
    ld hl,(current_val)
    dec (hl)
    jp instr_done


; decrement current value
inc_value:
    ld hl,(current_val)
    inc (hl)
    jp instr_done


; set table position to index at cursor position
instr_set_pos:
    ; get addr of value at cursor
    call get_cursor_pos
    call get_addr_at_cursor
    ; hl = addr

    ; get table index
    ld b,l

    ; get instrument addr
    ld a,(instr_current)
    call instr_get_addr
    push hl
    ; hl = addr

    ; get current column
    ld a,(inst_col)
    ld d,0
    ld e,a
    inc e ; skip target register
    
    ; store index according to column
    add hl,de
    ld (hl),b

    ; print params
    pop hl
    call print_params

    ret


; set an instrument parameter
instr_set_param:
    ret


; print instrument
;  a = instrument no.
; hl = instrument table
print_instr:
    ; print instr no.
    push hl
    push af
    ld hl,$0101
    call locate
    call print_local
    db "instr.: ", 0
    pop af
    call hex_out
    pop hl

    ; print parameters
    push hl
    call print_params
    pop hl

    ; copy offsets
    inc hl ; skip target reg.
    ld de,current_inst_offsets
    ld bc,4
    ldir

    ; print the tables
    ld a,(current_inst_offsets+0)
    call print_offset_tbl

    ld a,(current_inst_offsets+1)
    call print_finetune_tbl

    ld a,(current_inst_offsets+2)
    call print_wavetable_tbl
    
    ld a,(current_inst_offsets+3)
    call print_volume_tbl

    ; finish
    jp instr_done


; print instrument parameters
; hl = instrument addr
print_params:
    ; locate & print heading
    push hl
    ld hl,$0102
    call locate
    call print_local
    db "params: ", 0
    pop hl

    ; print instrument params
    ld b,INSTR_PARAMS
print_param_loop:
    ld a,(hl)
    call hex_out
    ld a,32
    call print_char
    inc hl
    djnz print_param_loop
    ret


; print offset (semi-tone) table
; a = tbl index 
print_offset_tbl:
    push af
    ld hl,$0104
    push hl
    call locate
    call print_local
    db "arpegg", 0
    pop bc
    inc c
    pop af
    ld de,(ptr_tone_table)
    jp print_instr_tbl
    
; print finetune table 
; a = tbl index 
print_finetune_tbl:
    push af
    ld hl,$0904
    push hl
    call locate
    call print_local
    db "finetn", 0
    pop bc
    inc c
    pop af
    ld de,(ptr_finetune_table)
    jp print_instr_tbl

; print wavetable 
; a = tbl index 
print_wavetable_tbl:
    push af
    ld hl,$1104
    push hl
    call locate
    call print_local
    db "wavefrm", 0
    pop bc
    inc c
    pop af
    ld de,(ptr_wave_table)
    jp print_instr_tbl


; print volume table
; a = tbl index 
print_volume_tbl:
    push af
    ld hl,$1904
    push hl
    call locate
    call print_local
    db "volume", 0
    pop bc
    inc c
    pop af
    ld de,(ptr_volume_table)
    jp print_instr_tbl


; print inverted value = cursor
instr_draw_cursor:
    call invert
    call instr_show_cur_val
    call invert
    ret


; get current cursor position
; => b: cursor row
; => c: cursor col
get_cursor_pos:
    ; locate cursor
    ld a,(inst_row)
    ld b,a
    ld a,(inst_col)
    ld c,a
    ret

; set cursor position
; => bc_ cursor row / col
locate_cursor:
    ; get current pos
    call get_cursor_pos

    ; locate cursor
    ld a,b
    add 5
    ld l,a
    ld a,c
    add a
    add a
    add a ; * 8
    add 4
    ld h,a
    jp locate


; get address of current cursor position
; b = cursor row
; c = cursor column
; => hl: address
get_addr_at_cursor:
    ; pick offset for current col
    ld hl,current_inst_offsets
    ld d,0
    ld e,c
    add hl,de
    ld a,(hl) ; = table offset 

    ; pick table base
    ld hl,instr_tables
    add hl,de
    add hl,de

    ; calc addr into table
    ld e,(hl)
    inc hl
    ld d,(hl)
    ex de,hl  ; = table base
    ld d,0
    ld e,a
    add hl,de ; = current table base addr

    ; add current row
    ld e,b
    add hl,de ; = current row in current table

    ret


; show current instrument value
instr_hide_cursor: ; alias
instr_show_cur_val:
    ; place cursor
    call locate_cursor
    ; b = row
    ; c = col

    ; calc address into instr. tables
    call get_addr_at_cursor

    ; print value
    ld (current_val),hl
    ld a,(hl)
    cp $80
    jp nz,hex_out
    call print_local
    db "jp", 0
    ret


; print a table from instrument
; hl = instrument ptr
; de = current table start
;  a = table offset
; bc = screen x/y pos
print_instr_tbl:
;    push hl
    ex de,hl
    ld d,0
    ld e,a
    add hl,de
    ld a,d

table_loop:
    ld i,a
    push hl

    ; place cursor
    ld h,b
    ld l,c
    inc c
    call locate

    ; print [position]:
    ld a,e
    inc e
    call hex_out
    ld a,':'
    call print_char
    pop hl

    ; check value
    ld a,(hl)
    cp $80
    jr nz,table_hex

    ; print "jp"
    push hl
    call print_local
    db "jp", 0
    pop hl
    jr table_cont

    ; print hex value
table_hex
    call hex_out

table_cont:
    inc hl
    ld a,i
    inc a
    cp 10
    jr nz, table_loop

;    pop hl
    ret


;
; instrument editor data
;

; current instruments offsets
current_inst_offsets:
    db 0,0,0,0

; instrument tables
instr_tables:

; @note the following 4 pointers assume that there are different sets of tables
; i.e. depending on a selected sound bank (which is stil to do!)

; points to the current semi-tone table
ptr_tone_table:
    dw 0

; points to the current finetune (vibrato) table
ptr_finetune_table:
    dw 0

; points to the current wave table
ptr_wave_table:
    dw 0

; points to the current volume table
ptr_volume_table:
    dw 0

; current column
inst_col:
    db 0

; current row
inst_row:
    db 0

; current value under cursor
current_val:
    dw 0



;
; Parameters editor
;

; parameters mode function keys
mode_params:
    dw param_draw_cursor ; enter
    dw param_hide_cursor ; leave
    dw 0 ; catch_all

    db $f2
    dw param_left
    db $f3
    dw param_right
    db 13
    dw param_enter

    db $f0
    dw inc_param_val
    db $f1
    dw dec_param_val

    db 0


;
; Instr param related functions
;

; move cursor left
param_left:
    ; remove cursor
    call param_hide_cursor

    ; move left
    ld a,(param_current)
    sub 1
    jr nc,param_store   ; lower boundary
    ld a,INSTR_PARAMS-1 ; wrap over
    jr param_store

; move cursor right
param_right:
    ; remove cursor
    call param_hide_cursor

    ; move right
    ld a,(param_current)
    inc a
    cp INSTR_PARAMS
    jr c,param_store; upper boundary
    xor a       ; wrap over
    jr param_store

; update cursor
param_store:
    ; store selected param
    ld (param_current),a

    ; draw cursor
    jp param_draw_cursor

param_enter:
    call beep
    ret

; increment parameter value
inc_param_val:
    ld hl,(param_addr)
    inc (hl)
    jr param_draw_cursor

; decrement parameter value
dec_param_val:
    ld hl,(param_addr)
    dec (hl)
    jr param_draw_cursor

; place cursor on instr param
; => de: current param offset
param_locate_cursor:
    ; get xpos for param
    ld a,(param_current)
    ld d,0
    ld e,a
    ld hl,param_xpositions
    add hl,de
    ld a,(hl)

    ; locate cursor
    ld h,a
    ld l,2 ; TODO
    jp locate
    
; invert current instr param
param_draw_cursor:
    call invert
    call param_print_current
    jp invert

; print current instr param
param_hide_cursor: ; alias
param_print_current:
    ; locate
    call param_locate_cursor

    ; show current value
    ld hl,(instr_current_addr)
    add hl,de
    ld (param_addr),hl
    ld a,(hl)
    jp hex_out

; current instr parameter
param_current:
    db 0

; curren params address
param_addr:
    dw 0

; positions of params
param_xpositions:
    db 9,12,15,18,21


;
; jump table related functions 
; 

; init jump table
jptab_init:
    ; alloc memory for 256 addresses
    ld hl,512
    call mem_alloc_page
    ld a,h
    ld (jptab_base),a
   
    ; initialize table with dummy target
    ld b,0
    push hl
    ld de,jptab_dummy
jptab_init_loop:
    ld (hl),e
    inc h
    ld (hl),d
    dec h
    inc l
    djnz jptab_init_loop
    pop hl
    
    ; pause 
    ld l,RANGE_PAUSE_START
    ld b,RANGE_PAUSE_LEN
    ld de,$CACA ; TODO temp
    ;ld de,cmd_pause
    call jptab_add_block

    ; note lookup
    ld l,RANGE_NOTES_START
    ld b,RANGE_NOTES_LEN
    ld de,$FEFE ; TODO temp
    ;ld de,cmd_notes
    call jptab_add_block

    ld l,RANGE_EFFECTS_START
    ld b,RANGE_EFFECTS_LEN
    ld de,$BABE ; TODO temp
    ;ld de,TODO
    call jptab_add_block

    ; done
    ret

; add a jump block
; 
; l = slot to start with
; b = slot count
; de = target address 
jptab_add_block:
    ld (hl),e
    inc h
    ld (hl),d
    dec h
    inc l
    djnz jptab_add_block
    ret
    
; jump via jump table
; b = offset
jptab_jump:
    ld a,(jptab_base)
    ld h,a
    ld l,b
    ld a,(hl)
    inc h
    ld h,(hl)
    ld l,a
    ld ($c800),hl ; TODO temp
    jp (hl)

; dummy jump target
jptab_dummy:
    ld ($c802),hl ; TODO temp
    ret

; base address of jump table
jptab_base:
    db 0



;
; Sequence related functions 
; 


; sequence mode function keys
mode_sequence:
    dw seq_enter ; enter
    dw seq_leave ; leave
    dw 0 ; no catch-all

    ; cursor movements
    db $f0
    dw seq_up
    db $f1
    dw seq_down
    db $f2
    dw seq_left
    db $f3
    dw seq_right

    ; enter value
    db 13
    dw seq_enter_val

    db 0


; draw sequence editor 
seq_draw:
    ld hl,$3004
    call locate
    call print_local
    db "wave", 0

    ld hl,$3804
    call locate
    call print_local
    db "AYa:", 0

    ld hl,$4004
    call locate
    call print_local
    db "AYb:", 0

    ld hl,$4804
    call locate
    call print_local
    db "AYc:", 0

    ; disable AY voices
    call toggle_A
    call toggle_B
    call toggle_C

    ; init
    ld ix,seq_base_addrs
    ld b,4 ; iteration
    ld c,$30  ; x-pos
seq_draw_loop:
    push bc
    ld d,(ix)
    ld e,0
    ld b,c ; x
    ld c,5 ; y
    xor a

    ; de = current table start
    ;  a = table offset
    ; bc = screen x/y pos 
    ; TODO: use other fn OR rename/generalize (also used in instr_ functions!)
    call print_instr_tbl

    inc ix
    pop bc
    ld a,c
    add 8
    ld c,a
    djnz seq_draw_loop

    ; select first seq
    xor a
    ld (seq_active),a

    call seq_locate
    ret

; calc current addr and cursor pos
seq_locate:
    ld a,(seq_active)
    ld b,a ; save for later

    ; get current position
    ld hl,seq_positions
    ld d,0
    ld e,a
    add hl,de
    ld (seq_current_pos),hl
    ld a,(hl)
    ld c,a
    ; a = position in current sequence

    ; add to base addr
    ld hl,seq_base_addrs
    add hl,de
    ld h,(hl)
    ld l,a
    ld (seq_current_val),hl
    ; hl = address of current position

    ; calc cursor postion
    ld a,b ; active seq
    add a
    add a
    add a  ; = seq*8
    add $33 ; x-pos of 1st columns value
    ld h,a
    
    ; calc y-pos from current position
    ld a,5
    add c
    ld l,a
    ; TODO consider scrolling!

    ld (seq_cursor_xy),hl
    ret

; enter sequence mode
seq_enter:
    jp seq_draw_cursor

; leave sequence mode
seq_leave:
    jp seq_hide_cursor

; crsr up
seq_up:
    ld hl,(seq_current_pos)
    ld a,(hl)
    or a
    ret z

    push hl
    call seq_hide_cursor
    pop hl

    ; TODO temp!
    dec (hl)

    call seq_locate
    jp seq_draw_cursor

; crsr down
seq_down:
    ld hl,(seq_current_pos)
    ld a,(hl)

    push hl
    call seq_hide_cursor
    pop hl

    ; TODO temp!
    inc (hl)

    call seq_locate
    jp seq_draw_cursor

; crsr left
seq_left:
    ld a,(seq_active)
    or a
    ret z

    call seq_hide_cursor

    ; TODO temp!
    ld hl,seq_active
    dec (hl)

    call seq_locate
    jp seq_draw_cursor

    ret
    
; crsr right
seq_right:
    ld a,(seq_active)
    cp 3
    ret z

    call seq_hide_cursor

    ; TODO temp!
    ld hl,seq_active
    inc (hl)

    call seq_locate
    jp seq_draw_cursor

    ret

seq_enter_val:
    ; move cursor before first char
    ld a,8
    call print_char
    call print_char

    ; read hi-nibble
    call wait_nibble
    jr c,seq_enter_done
    rlca
    rlca
    rlca
    rlca
    ld b,a

    ; read lo-nibble
    call wait_nibble
    jr c,seq_enter_done
    or b
    
    ; store new value
    ld hl,(seq_current_val)
    ld (hl),a

    call seq_down
;    jp seq_draw_cursor

seq_enter_done:
    ; TODO
    ret


; draw cursor
seq_draw_cursor:
    call invert
    call seq_print_value
    jp invert

seq_hide_cursor: ; alias
seq_print_value:
    ; set cursor pos
    ld hl,(seq_cursor_xy)
    call locate

    ; get & print value
    ld hl,(seq_current_val)
    ld a,(hl)
    call hex_out

    ret



; init sequences for all voices
seq_init: 
    ; create empty track list
    ; = 256 track ptrs
    ld hl,512
    call mem_alloc_page
    ld a,h
    ld (track_list_base),a ; = hi-byte of base addr
    ld d,h
    ld e,l
    inc de
    ld bc,511
    ld (hl),0
    ldir ; put 512 zeros at track list base
    
    ; reset track count
    xor a
    ld (track_max),a

    ; create sequences
    ld iy,seq_base_addrs

    ; create wave (SID voice) sequence
    call seq_wave_new

    ; create AY voices sequences
    ld ix,voice1_struct
    call seq_ay_new

    ld ix,voice2_struct
    call seq_ay_new

    ld ix,voice3_struct
    call seq_ay_new

    ret


; initialize wave player sequence
; iy = table of sequence base addrs
seq_wave_new:
    ; add jam track to sequence (as track 0)
    ; @note: track 0 is hidden from the user / only for internal use
    ld de,jam_track
    call track_add

    ; create a track
    call track_new

    ; copy dummy track into it
    ; TODO temp
    ex de,hl
    ld hl,track1
    ld bc,track2-track1
    ldir

    ; another dummy TODO
    call track_new
    ex de,hl
    ld hl,track2
    ld bc,track3-track2
    ldir

    ; init SID voice seq
    ld hl,256
    call mem_alloc_page
    ld (iy),h ; store base addr
    inc iy

    ; store dummy tracks
    ld (hl),1 ; track 1 
    inc hl
    ld (hl),2 ; track 2
    inc hl
    ld (hl),SEQ_END ; end of seq

    ret


; create new sequence (of tracks, for one voice)
; ix = ptr to seq struct 
; iy = table of sequence base addrs
seq_ay_new:
    ; new sequence
    ld hl,256
    call mem_alloc_page
    
    ; save sequence base
    ld (ix+idx_seq_base),h
    ld (iy),h
    inc iy

    ; init sequence:
    ld (hl),1   ; track 1 
    inc hl
    ld (hl),2   ; track 2 
    inc hl
    ld (hl),SEQ_END ; end of seq

    ; start sequence
    xor a
    ld (ix+idx_seq_pos),a

    ret


; current cursor position 
seq_cursor_xy:
    dw 0
 
; the sequences base addrs for SID, A, B, C
seq_base_addrs:
    ds 4

; current position in each sequence
seq_positions:
    ds 4

; currently active sequence
seq_active:
    db 0

; address of current position
seq_current_pos:
    dw 0

; address of value at current position
seq_current_val:
    dw 0

; current play position in sequence
seq_play_pos:
    db 0

; dummy sequence for jam mode
seq_jam:
    db 0 ; track 0

    db $11,$22,$33


;
; Track related functions 
; 


; track mode function keys
mode_track:
    dw track_enter ; enter
    dw track_leave ; leave
    dw track_catchall 

    db 5 ; ctrl E
    dw track_store_eot

    db 13 ; enter
    dw track_enter_val

    db $10 ; clr == ctrl P!
    dw track_clear_row

    db $11 ; ctrl Q
    dw track_store_slide  

    db 14 ; ctrl N 
    dw track_new

    db 18 ; ctrl R
    dw track_repeat_val

    db $f0
    dw track_up
    db $f1
    dw track_down

    db $f2
    dw track_left
    db $f3
    dw track_right

    db $f8
    dw track_jump_up
    db $f9
    dw track_jump_down

    db $89 ; f9
    dw track_next
    db $86 ; f6
    dw track_prev

    db 0

; list of track pointers
track_list:
    dw 0

; number of tracks
track_max:
    db 0;

; number of current track
track_number:
    db 0

; current voice
track_voice:
    db 0

; voice start addr
voice_base_adr:
    dw 0

; upper left corner
voice_xy:
    dw 0

; current column in track
track_col:
    db 0

; current row 
track_row:
    db 0

; current offset
track_offset:
    db 0

; current voice address
track_line_addr:
    dw 0

; current voice cursor pos.
track_line_xy:
    dw 0;

; voice data and x/y positions
track_xy_positions
    ; voice A
    dw $0411 ; x/y locate

    ; voice B
    dw $0e11

    ; voice C
    dw $1811

; flag wheter in track mode
track_mode:
    db 0

; base addr of track list
track_list_base:
    db 0

; the last value that was entered
track_last_value:
    db 1

; entering track mode
track_enter:
    ld hl,track_mode
    ld (hl),1
    jp track_draw_cursor

; leaving track mode
track_leave:
    ld hl,track_mode
    ld (hl),0
    jp track_hide_cursor

; create new track
; => hl = ptr to new track
track_new:
    ld a,(track_max)
    cp 255
    jp z,beep ; no room for track!

    ; allocate memory
    ld hl,VOICE_SIZE
    call mem_alloc

    ; beep if out of memory
    ld a,h
    or l
    jp z,beep

    ; track addr in de
    push hl
    ex de,hl

    ; add to track list
    call track_add

    ; return addr of new track
    pop hl
    ret


; add track to track list
; => de = addr of track
track_add:
    ; get current max, then increment
    ld hl,track_max
    ld b,(hl)
    inc (hl)
    ; b = track no.

    ; get base addr
    ld l,b
    ld a,(track_list_base)
    ld h,a
    ; and add track no.
    ; e.g. (track list base = $4000, track no = $08)
    ; => hl = $4008, where lo/hi byte of track ptr are at $4008/$4108

    ; store lo-byte of track ptr
    ld (hl),e
    inc h ; then hi-hyte
    ld (hl),d

    ret


; select track
; a = track #
track_select
    ; print track no
    ld (track_number),a
    push af
    ld hl,$0110
    call locate
    pop af
    call hex_out

    ; print max tracks
    ld a,'/'
    call print_char
    ld a,(track_max)
    dec a
    call hex_out

    ; init params
    call voice_init
    call voice_update_params
    jp track_print

; catch all fn
; takes note/octave, instr and duration from jam track
track_catchall:
    ; save key code
    ld b,a

    ld a,(track_col)
    ; 1st col?
    or a
    ret nz
    
    ; was a key played?
    ld a,(last_note)
    or a
    ; no, print last key code
    ld a,b
    jp z,func_print_key

    ; get addr of current voice/row
    ld hl,(track_line_addr)
    ;call voice_get_row_addr
    push hl

    ; copy tone
    ld a,(jam_track)
    ld (hl),a

    ; copy duration of last note
    inc hl
    ex de,hl
    ld hl,(tone_length+1)
    ld a,l
    ex de,hl
    ld (hl),a

    ; store instrument
    ld a,(instr_current)
    inc hl
    ld (hl),a

    pop hl
    jr track_print_then_down

; store end-of-track row
track_store_eot:
    ;call voice_get_row_addr
    ld hl,(track_line_addr)
    ld (hl),TRK_END

    jr track_print_then_down

track_store_slide:
    ld hl,(track_line_addr)
    ld (hl),TRK_SLIDE

    jr track_print_then_down

; enter was pressed
track_enter_val:
    ld a,(track_col)
    ; 2nd col -> duration
    cp 1
    jr z,track_set_duration

    ; 3rd col -> instrument
    cp 2
    jr z,track_set_instr

    ret

track_repeat_val:
    ld hl,(track_line_addr)
    ld a,(track_col)
    ld d,0
    ld e,a
    add hl,de
    ld a,(track_last_value)
    ld (hl),a
    jr track_down

track_input_value:
    ; move cursor before first char
    ld a,8
    call print_char
    call print_char

    ; try to read a hex value
    call key_wait_byte
    ret c

    ; get addr of current voice/row
    ld hl,(track_line_addr)
    ret

track_set_duration:
    call track_input_value
    ret c
    inc hl ; 2nd col
    ld (hl),a

    ; store for re-use
    ld (track_last_value),a
    jr track_down

track_set_instr:
    call track_input_value
    ret c
    inc hl
    inc hl ; 3rd col
    ld (hl),a

    ; store for re-use
    ld (track_last_value),a
    jr track_down

track_clear_row:
    ; get addr of current voice/row
    ld a,(track_voice)
    ;call voice_get_row_addr
    ld hl,(track_line_addr)
    push hl

    ; clear row
    xor a
    ld (hl),a
    inc hl
    ld (hl),a
    inc hl
    ld (hl),a

    ; go to next row 
    pop hl
    jr track_print_then_down

; print current line then go down 1 line
track_print_then_down:
    push hl
    ld hl,(track_line_xy)
    call locate
    pop hl
    call voice_print_line
    jr track_down
    
; cursor up
track_up:
    ld a,(track_row)
    or a
    jr z,track_scroll_up
    push af
    call track_hide_cursor
    pop af
    dec a
    ld (track_row),a

    call voice_update_params
    jp track_draw_cursor

; scroll track up one line
track_scroll_up:
    ld a,(track_offset)
    or a
    ret z
    dec a
    ld (track_offset),a
    jp track_redraw

; jump up
track_jump_up:
    ld a,(track_offset)
    sub TRACK_ROWS_DISP
    jr nc,track_jump_up2
    xor a
track_jump_up2:
    ld (track_offset),a
    jp track_redraw

; cursor down
track_down:
    ld a,(track_row)
    cp TRACK_ROW_MAX
    ret nc
    inc a
    cp TRACK_ROWS_DISP
    jr nc,track_scroll_down
    push af
    call track_hide_cursor
    pop af
    ld (track_row),a

    call voice_update_params
    jp track_draw_cursor

; scroll track down one line
track_scroll_down:
    ld a,(track_offset)
    cp TRACK_ROW_MAX - TRACK_ROWS_DISP
    ret nc
    inc a
    ld (track_offset),a
    jp track_redraw

; jump down
track_jump_down:
    ld a,(track_offset)
    cp TRACK_ROW_MAX - TRACK_ROWS_DISP ; TODO
    ret nc
    add TRACK_ROWS_DISP
    ld (track_offset),a
    jp track_redraw

; redraw after scrolling
track_redraw:
    call voice_update_params
    call track_print  
    jp track_draw_cursor

; cursor left
track_left:
    ld a,(track_col)
    or a
    jr z,track_prev_voice
    push af
    call track_hide_cursor
    pop af
    dec a
    ld (track_col),a

    jp track_draw_cursor

; previous voice
track_prev_voice:
    ret ; TODO
    ld a,(track_voice)
    or a 
    ret z
    push af
    call track_hide_cursor
    pop af

    dec a
    ld (track_voice),a
    call voice_init

    ld a,TRACK_COL_MAX 
    ld (track_col),a
    call voice_update_params

    jr track_draw_cursor

; cursor right
track_right:
    ld a,(track_col)
    cp TRACK_COL_MAX
    jr nc,track_next_voice
    push af
    call track_hide_cursor
    pop af
    inc a
    ld (track_col),a

    jp track_draw_cursor

; next voice
track_next_voice:
    ret ; TODO
    ld a,(track_voice)
    cp 2 
    ret nc
    push af
    call track_hide_cursor
    pop af

    inc a
    ld (track_voice),a
    call voice_init

    xor a
    ld (track_col),a
    call voice_update_params

    jp track_draw_cursor

; previous track
track_prev:
    ; check current track no
    ld a,(track_number)
    cp 1
    ; already the first 
    jr z,track_prev_wrap
    dec a
    jp track_select
track_prev_wrap:
    ; select last
    ld a,(track_max)
    dec a
    jp track_select

; next track
track_next:
    ; check current track no
    ld a,(track_max)
    dec a
    ld b,a
    ld a,(track_number)
    cp b
    ; already the last one 
    jr nc,track_next_wrap
    inc a
    jp track_select
track_next_wrap:
    ; select first
    ld a,1
    jp track_select

; draw cursor
track_draw_cursor:
    call invert
    call track_print_value
    jp invert

track_hide_cursor: ; alias
track_print_value:
    ; get cursor pos
    ld hl,(track_line_xy)
    ex de,hl    

    ; get current addr
    ld hl,(track_line_addr)
    
    ; render colum
    ld a,(track_col)
    or a
    jr z,track_print_note
    dec a
    jr z,track_print_duration
    dec a
    jr z,track_print_instr
    ret

track_print_note:
    ex de,hl
    call locate
    ex de,hl

    ld a,(hl)
    jp print_note_octave

track_print_duration:
    ex de,hl
    ld a,h
    add 4
    ld h,a
    call locate
    ex de,hl
    
    inc hl
    ld a,(hl)
    jp hex_out

track_print_instr:
    ex de,hl
    ld a,h
    add 6
    ld h,a
    call locate
    ex de,hl
    
    inc hl
    inc hl
    ld a,(hl)
    jp hex_out


; print track in hl
track_print:

    ; print offsets
    ld hl,$0111
    ld a,(track_offset)
    ld d,a
    ld b,TRACK_ROWS_DISP
track_print_offs:
    ld a,d
    cp TRACK_ROW_MAX
    jr nc,track_print_end
    push hl
    call locate
    ld a,d
    call hex_out
    pop hl
    inc d
    inc l
    djnz track_print_offs

track_print_end:
    ; voice A
    ld a,0
    call voice_print

    ret


; get track params
; a = track no
; => hl: track address
; => de: x/y pos
track_get_params:

    ; always have the left-most column
    ; TODO
    xor a

    ; calc voice param addr
    ld hl,track_xy_positions
    add a; * 2
    ld b,0
    ld c,a
    add hl,bc

    ; set voice x/y pos
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl

    ; get base addr
    ld a,(track_list_base)
    ld h,a
    ld a,(track_number)
    ld l,a

    ; hl points to lo-byte
    ld a,(hl)
    inc h ; then hi-byte
    ld h,(hl)
    ld l,a
    ; hl = voice base addr

    ret

; set voice params
; a = voice
; => hl: voice base address
; => de: x/y pos
voice_init:
    ; get voice params
    call track_get_params
    
    ; store
    ld (voice_base_adr),hl
    ld (voice_xy),de

    ld a,32
    call print_char
    jp print_hl

; get address of current voice & row
; => hl: row addr
voice_get_row_addr:
    ; get current row
    ld a,(track_row)
    ld b,a

voice_get_offset_addr:
    ; add offset
    ld a,(track_offset)
    add b

    ; calc addr of row
    ld hl,(voice_base_adr)
    ld b,a
    add a
    add b ; * 3
    ld b,0
    ld c,a
    add hl,bc ; = addr of current row
    ret

; get x/y pos of current voice & row
; => hl: row x/y pos
voice_get_row_pos:
    ld hl,(voice_xy)
    ld a,(track_row)
    add l
    ld l,a
    ret

; update current voice params
voice_update_params:
    ; update row address
    call voice_get_row_addr
    ld (track_line_addr),hl

    ; update x/y pos
    call voice_get_row_pos
    ld (track_line_xy),hl
    ret

; print one track
; a = track no
voice_print:
    ; get track base params
    call track_get_params

    ; get offset base addr
    push de
    xor a ;(at row 0)
    call voice_get_offset_addr
    pop de

    ; print
    ld b,TRACK_ROWS_DISP
voice_loop:
    push de
    push hl

    ; place cursor
    ex de,hl
    call locate
    ex de,hl

    ; print line
    call voice_print_line

    ; next line
    pop hl
    ld de,3
    add hl,de
    pop de
    inc e
    djnz voice_loop
    ret

; print one AY voice line
voice_print_line:
    push bc
    push hl

    ; print note & octave
    ld a,(hl)
    call print_note_octave

    ; space
    ld a,32
    call print_char

    pop hl

    ; print instr / effect 
    inc hl
    ld a,(hl)
    call hex_out
    inc hl
    ld a,(hl)
    call hex_out
    
    pop bc
    ret

; print note and octave
print_note_octave:
    ; empty line / no note?
    or a
    jr nz,print_note_not0
    call print_local
    db "---", 0
    ret

print_note_not0:
    ; end of track?
    cp TRK_END
    jr nz,print_note_not_end
    call print_local
    db "END", 0
    ret

print_note_not_end:
    ; effect?
    cp TRK_SLIDE
    jr nz,print_note_octave2
    call print_local
    db "SLD", 0
    ret

print_note_octave2:
    ; calc key + octave
    ld b,12
    ld d,0
octave_loop:
    cp b
    ; a=key, d=octave
    jp c,print_note
    sub b
    inc d
    jr octave_loop


; current play mode. 0 = jam, 1 = song
play_mode:
    db 0

; switch player into jam mode
playmode_jam:
    ; any released key stops the player
    ld a,255
    ld (key_check+1),a

    xor a
    ld (play_mode),a

    ; play manually
    ld hl,wave_end_manual
    ld (wave_loop_end+1),hl

    ; skip track loop
    ld hl,tone_loop
    ld (loop_addr+1),hl
    ld a,3
    ld (skip_loop+1),a

    ret

; switch player into song mode
playmode_song:
    ; key line including the ESC key 
    ld a,$48
    ld (key_code+1),a
   
    ld a,1
    ld (play_mode),a

    ; the ESC key 
    ld a,%11111011
    ld (key_check+1),a
    
    ; use song-playing loop
    ld hl,wave_end_song
    ld (wave_loop_end+1),hl
    ld hl,trackloop
    ld (loop_addr+1),hl
    xor a
    ld (skip_loop+1),a

    ; play song sequence
    ld a,(seq_base_addrs)
    ld (seq_ptr_hi+1),a
    xor a
    ld (seq_play_pos),a

    ret

; play track
track_play:
    ; switch to song mode
    call playmode_song

    ; play
    call player_start
    
    ; switch to jam mode
    call playmode_jam

    ret



;
; general functions
; 

; print hex value in de
print_hex_de:
    ld a,d
    call hex_out
    ld a,e
    call hex_out
    ret

; print hex value in hl
print_hex_hl:
    ld a,h
    call hex_out
    ld a,l
    call hex_out
    ret

; print one space char
print_space:
    ld a,32
    jp print_char


; print a note name (C-, C#, ..., B-)
; a = note value
; d = octave
print_note:
    ld hl,note_names
    ld b,0
    add a
    ld c,a
    add hl,bc
    ld a,(hl)
    call print_char
    inc hl
    ld a,(hl)
    call print_char
    ld a,d
    add 48
    jp print_char

; print newline
newline:
    ld a,10
    call print_char
    ld a,13
    jp print_char


; read a string from keyboard
;  b = max length
; hl = where to put string data
read_string:
    ld c,b
    push hl
    ld a,32
read_init:
    ld (hl),a
    inc hl
    djnz read_init
    pop hl
    ld b,c
read_string_loop:
    call wait_char
    cp $7f  ; DEL
    jr nz,no_delete
    ld a,b
    cp c
    jr nc,no_delete
    ; delete last char
    ld a,8
    call print_char
    dec hl
    inc b
    jr read_string_loop

no_delete:
    cp $fc  ; ESC
    ret z
    cp 13   ; Enter
    ret z
    ld (hl),a
    call print_char
    inc hl
    djnz read_string_loop
    ret


; wait for a hex nibble
wait_nibble:
    call wait_char
    cp $3a
    jr nc,no_number
    cp $30
    jr c,no_number

    ; it's a number
    call print_char
    sub $30
    ret 

no_number:
    cp $67
    jr nc,no_alpha
    cp $61
    jr c,no_alpha

    ; it's A to F
    call print_char
    sub $61
    add 10
    ret

no_alpha:
    cp $fc  ; ESC
    jr nz,wait_nibble
    scf ; return with carry set
    ret


; wait for hex byte
key_wait_byte:
    ; get hi-nibble
    call wait_nibble
    ret c

    rlca
    rlca
    rlca
    rlca
    ld b,a

    ; get lo-nibble
    call wait_nibble
    ret c
    
    or b
    scf
    ccf
    ret 


; print hex value in hl
print_hl:
    ld a,h
    call hex_out
    ld a,l

; print hex value
; a = value to print
hex_out:
    push af
    rlca
    rlca
    rlca
    rlca
    call hex_nib
    pop af
hex_nib:
    and 15
    cp 10
    jr c,no_letter
    add a,7
no_letter:
    add a,48
    jp print_char


; print a local string
; (sp) = string to print
print_local:
    pop hl
    call print_text
    jp (hl) 


; print string, 0 = end of string
; hl = string to print 
print_text:
    ld a,(hl)
    or a
    ret z
    call print_char
    inc hl
    jp print_text


; call fn in (hl)
call_hl:
    jp (hl)


; sys call wrappers
wait_char:
    jp $bb06

read_char:
    jp $bb09

print_char:
    jp $bb5a

locate:
    jp $bb75

invert:
    jp $bb9c

cursor_on:
    jp $bb81

cursor_off:
    jp $bb84


; voice data indices
idx_ticks       equ 0  ; 1 byte     no. of ticks in current row
idx_period      equ 1  ; 2 bytes    current tone periode 
idx_volume      equ 3  ; 1 byte     current volume
idx_offsets     equ 4  ; 2 bytes    pointer to offsets
idx_track       equ 6  ; 2 bytes    pointer to current track 
idx_base_note   equ 8  ; 1 byte     current base note
idx_tone_ptr_lo equ 9  ; 2 bytes    address of tone period register lo-byte
idx_tone_ptr_hi equ 11 ; 2 bytes    the same, hi-byte
idx_volume_ptr  equ 13 ; 2 bytes    address of volume register
idx_seq_base    equ 15 ; 1 byte     base address (hi-byte) of the sequence
idx_seq_pos     equ 16 ; 1 byte     current position in the sequence

; voice 1 data, see indices above
voice1_struct:
    db 1            ; ticks
    dw 0            ; period
    db 0            ; volume
    dw voice1_offsets
    dw 0            ; track ptr
    db 0            ; base note
    dw register0+1  ; tone reg lo
    dw register1+1  ; tone reg hi
    dw register8+1  ; volume reg
    db 0            ; sequence base addr
    db 0            ; sequence current position
    db 0            ; sequence restart position

voice1_offsets:
    db 0,0,0,0

; voice 2 data, see indices above
voice2_struct:
    db 1            ; ticks
    dw 0            ; period
    db 0            ; volume
    dw voice2_offsets
    dw 0            ; track ptr
    db 0            ; base note
    dw register2+1  ; tone reg lo
    dw register3+1  ; tone reg hi
    dw register9+1  ; volume reg
    db 0            ; sequence base addr
    db 0            ; sequence current position
    db 0            ; sequence restart position

voice2_offsets:
    db 0,0,0,0

; voice 3 data, see indices above
voice3_struct:
    db 1            ; ticks
    dw 0            ; period
    db 0            ; volume
    dw voice3_offsets
    dw 0            ; track ptr
    db 0            ; base note
    dw register4+1  ; tone reg lo
    dw register5+1  ; tone reg hi
    dw register10+1 ; volume reg
    db 0            ; sequence base addr
    db 0            ; sequence current position
    db 0            ; sequence restart position

voice3_offsets:
    db 0,0,0,0



;
; fetch row from AY track
; ix = pointer to voice struct
; 14 nops since sample on entry
;
ay_next_row:
    ; get track data ptr
    ld h,(ix+idx_track+1)
    ld l,(ix+idx_track+0)
    ; 10 
    ; = 24

    ; hl = track data
    ; 3 bytes per track row: 
    ; tone, duration, instr

    ; end of track? 
    ld a,(hl)
    cp TRK_END
    jp nz,ay_track_noend

    ; advance to next track
    ld h,(ix+idx_seq_base)
    ld l,(ix+idx_seq_pos)
    inc l ; = position in sequence

    ; read track no
    ld a,(hl)
    cp SEQ_END
    jp nz,ay_seq_noend

    ; end of sequence, re-start
    ld l,0

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = TODO

    ; send sample (at nop #16 = 72 since last)
    ld b,$f4    ; Port A
    sub yl      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ; 19 nops
    ; = 4 since sample
    
    ; read 1st track no
    ld a,(hl)

    ; TODO align

ay_seq_noend:
    ; update sequence pos
    ld (ix+idx_seq_pos),l

    ; get track list base
    ld l,a
    ld a,(track_list_base)
    ld h,a

    ; read new track addr
    ld e,(hl)
    inc h
    ld d,(hl)
    
    ; read note
    ex de,hl
    ld a,(hl)
    ; = 22

    ds 8
    ; = 30

ay_track_noend:
    ; TODO: implement effects! a = $80 etc.

    ; copy base note
    ld (ix+idx_base_note),a
    ld c,a
    inc hl
    ; 8
    ; 38

    ; get duration
    ld a,(hl)
    ld (ix+idx_ticks),a
    inc hl
    ; 9
    ; = 47

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 55

    ; align
    ds 1
    ; = 56

    ; send sample (at nop #16 = 72 since last)
    ld d,b
    ld b,$f4    ; Port A
    sub yl      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld b,d
    ; 21 nops
    ; = 5 since sample

    ; get instr no
    ld a,(hl)
    inc hl
    ; = 9

    ; update track ptr
    ld (ix+idx_track+1),h
    ld (ix+idx_track+0),l
    ; 10
    ; = 19

    ; instr addr
    ld l,a
    ld a,(instr_table_base)
    ld h,a
    ; hl = instr table ptr

    ; read instr addr
    ld a,(hl)
    inc h
    ld h,(hl)
    ld l,a
    ; 11
    ; = 30
    ; instrument in hl
 
    ; skip target reg #
    inc hl
    ; 3
    ;    ; = 26
    ; = 33!

    ; copy instrument params
    ld d,(ix+idx_offsets+1)
    ld e,(ix+idx_offsets+0)
    ldi
    ldi
    ; more ldi after sample sending, see below!
    ; 20
    ; = 46

    ds 2
    ; = 48

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 56

    ; send sample (at nop #16 = 72 since last)
    ld b,$f4    ; Port A
    sub yl      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ; 19 nops
    ; = 4 since sample
    
    ; copy cont'd
    ldi
    ldi
    ; 10
    ; = 14

    ; adjust iteration count
    dec yh
    ; = 16

    ret
    ; = 19


; initialize ay register shadowing
ay_shadow_init:
    ; get upper bound
    ld a,(mem_generic_base)
    ld h,a
    ld a,(mem_generic_upper)
    ld l,a
    ; hl points to base addr in lookup tbl

    ; store 
    ld (ay_shadow_base),hl
    ex de,hl

    ; copy pointers
    ld hl,ay_shadow_tab
    ld b,14
ay_shadow_loop:
    ; lo
    ld a,(hl)
    ld (de),a
    inc hl
    inc d   ; select hi-byte page

    ; hi
    ld a,(hl)
    ld (de),a
    inc hl
    dec d   ; lo-byte page

    inc e
    djnz ay_shadow_loop

    ; update upper bound
    ld a,e
    ld (mem_generic_upper),a

    ret

; shadows an AY register from being written in the AY player
; a = register #
; b = replacement value
ay_shadow_register:
    ; calc lookup position
    ld hl,(ay_shadow_base)
    add l
    ld l,a

    ; look up ptr
    ld a,(hl)
    inc h
    ld h,(hl)
    ld l,a

    ; store value 
    ld (hl),b
    ld a,b
    ret

; our base address into the generic lookup tbl
ay_shadow_base:
    dw 0

; table of places where AY register numbers are stored
; and which are to be shadowed
ay_shadow_tab:
    dw reg0_select + 1
    dw reg1_select + 1
    dw reg2_select + 1
    dw reg3_select + 1
    dw reg4_select + 1
    dw reg5_select + 1
    dw reg6_select + 1
    dw reg7_select + 1
    dw reg8_select + 1
    dw reg9_select + 1
    dw reg10_select + 1
    dw $f000    ;dw register11 + 1
    dw $f000    ;dw register12 + 1
    dw $f000    ;dw register13 + 1
    dw $c040    ; dummy register 14, points to screen


; render one AY tick
; ix = pointer to voice struct
; sends out 8 samples (i.e. 4 wave loop iterations)
ay_render_tick:
    ; copy current volume
    ld yl,c

    ; adjust iteration counter
    ld a,-4
    add yh
    ld yh,a
    ; = 6

    ; row complete?
    dec (ix+idx_ticks)
    jp nz,ay_same_row
    ; = 15

    ; get next row
    call ay_next_row
    ; = 19 after ret

    jp ay_render_tick2
    ; = 22 at ay_tick_end

    ; end of tick
ay_same_row:
    ; = 15

    ; align
    ds 7
    ; = 22

ay_render_tick2:

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; = 8

    ; send sample (at nop #16 = 72 since last)
    ld b,$f4    ; Port A
    sub yl      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ; = 19 nops
    ; = 4 since sample

    ; init table lookups 
    ld h,(ix+idx_offsets+1) ; 5
    ld l,(ix+idx_offsets+0) ; 10
    ld a,(ptr_tone_table+1)  ; 16
    ld b,a
    ; = 20

    ; hl   = current instruments offset (initialized from instr params) to table in bc
    ; b(c) = base addr to offset/finetune/wave/volume table


    ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ; get semi-tone offset
    ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    call lookup
    ; = 48
    ; a = current offset

    ; add base note
    add a,(ix+idx_base_note)
    ; a = final note
    ; = 53

    ; prepare period lookup for note 
    add a,a     ; 1
    ld e,a      ; 2
    ; e = current note * 2
    ; = 47

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 55

    ; align
    nop

    ; send sample (at nop #16 = 72 since last)
    ld d,b      ; save table base
    ld b,$f4    ; Port A
    sub yl      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld a,d      ; = table base
    ; 21 nops
    ; = 5 since sample

    ; look up period  
    ; e = current note * 2
    ld d,0      ; 2 de = note * 2
    ex de,hl    ; 3 save ptr, get note * 2
    ld bc,ay_notes ; 6 
    add hl,bc   ; 9 
    ld c,(hl)   ; 11
    inc hl      ; 13
    ld b,(hl)   ; 15
    ex de,hl    ; 16 restore hl
    ; bc = period value
    push bc     ; 20 save period
    ld b,a      ; 21 restore table base
    ; = 26


    ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ; get finetune
    ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    inc b       ; 1
    inc hl      ; 3
    call lookup ; 23 TODO: 28, not 23
    ld e,a      ; 24
    ; = 49
    ; e = signed finetune value

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 57! (should be 56)

    ; send sample (at nop #16 = 72 since last)
    ld d,b
    ld b,$f4    ; Port A
    sub yl      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld b,d
    ; 21 nops
    ; = 5 since sample

    ; add finetune to period
    ld a,e      ; 1 restore finetune
    rlca        ; 2 rotate sign into carry flag
    ld a,0      ; 4 subtract from 0 ..
    sbc a       ; 5 with carry, effectively subtracting the carry flag from 0 => a is either 0 or -1 
    ld d,a      ; 6 copy as hi-byte
    ; => signed offset value in de
    ; = 11

    ex (sp),hl  ; 6 save ptr, load period
    add hl,de   ; 9 add finetune to period
    ; = 20

    ; store final period value
    ld (ix+idx_period+0),l ; 5
    ld (ix+idx_period+1),h ; 10
    ; = 30

  
    ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ; waveform not supported!
    ; @note: use for something else? noise? hardware-envelope? 
    ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    inc b   ; 1
    pop hl  ; 4 restore ptr
    inc hl  ; 6

    ; TODO temp: use value from wavetbl as AY noise
    call lookup ; 23
    ld (register6+1),a
    ; = 36
    ; TODO temp

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 42

    ; align
    ds 14
    ; = 56

    ; send sample (at nop #16 = 72 since last)
    ld d,b
    ld b,$f4    ; Port A
    sub yl      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld b,d
    ; 21 nops
    ; = 5 since sample


    ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ; get volume 
    ; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    inc b   ; 1
    inc hl  ; 3
    call lookup ; 23
    ; a = volume (stored as 15-x, e.g. volume 10 is stored as 5) 
    ; = 28

    ; subtract from 15 to get real value
    ld e,a
    ld a,15
    sub e
    ; a = final voume
    ; = 32

    ; store final volume
    ld (ix+idx_volume),a
    ; = 37

    ld de,$c080 ; PSG $c0 = select / $80 = write
    ld hl,$f4f6 ; PIO $f4 = port A / $f6 = port B
    ; 6
    ; = 43

    ; restore volume
    ld c,yl
    ; = 46

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops
    ; = 23

    ; copy period lsb value
    ; modifies the code that writes a period register!
    ld d,c 
    ld a,(ix+idx_period+0) 
    ld b,(ix+idx_tone_ptr_lo+1)
    ld c,(ix+idx_tone_ptr_lo+0)
    ld (bc),a
    ld c,d
    ; 19 nops
    ; = 42
    
    ds NOPS_PER_SAMPLE - 42

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops

    ; copy period msb value
    ; modifies the code that writes a period register!
    ld d,c 
    ld a,(ix+idx_period+1) 
    ld b,(ix+idx_tone_ptr_hi+1)
    ld c,(ix+idx_tone_ptr_hi+0)
    ld (bc),a
    ld c,d
    ; 19 nops
    ; = 42
    
    ds NOPS_PER_SAMPLE - 42

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops

    ; copy volume value 
    ; modifies the code that writes a volume register!
    ld d,c 
    ld d,c
    ld a,(ix+idx_volume) 
    ld b,(ix+idx_volume_ptr+1)
    ld c,(ix+idx_volume_ptr+0)
    ld (bc),a
    ld c,d
    ; 19 nops
    ; = 42
    
    ds NOPS_PER_SAMPLE - 42

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops

    ds NOPS_PER_SAMPLE - 18

    ret
    ; = 18
  

; initialize an AY voice struct
; ix = ptr to struct
ay_init_voice:
    ; 1 dummy tick 
    ld (ix+idx_ticks),1

    ; re-start sequence
    xor a
    ld (ix+idx_seq_pos),a    

    ; read 1st track no
    ld h,(ix+idx_seq_base)   
    ld l,a
    ld l,(hl)
    ; l = track no

    ; get track list addr
    ld a,(track_list_base)
    ld h,a

    ; read 1st track addr
    ld e,(hl)
    inc h
    ld d,(hl)

    ; and store
    ld (ix+idx_track+1),d 
    ld (ix+idx_track+0),e 
 
    ret



; 
; commands definitions
; b = current command no.
;

cmd_pause:
    ; TODO    
    ret

cmd_notes:
    ; TODO
    ret


;
; (SID) player functions
;

;
; Play
;
player_init:
    ; init player engine
    call init_engine
    
    ; set player mode
    call playmode_song

;
; Play a sequence
;
player_start:
    ; init yl
    ld a,9 ; TODO
    ld yl,a


;
; SID
;
sidplay:

    di

;
; $$$$$$$$$$$$$$$$ MAIN  $$$$$$$$$$$$$$$$
;

    ; init wavetable pointers
    exx
    push bc
    push de
    push hl
    ld hl,0         ; wave index (long pointer)
    ld de,100       ; wave offset (depends on tone frequency)
    ld bc,0         ; wavetable 
    exx

    ; init AY voices
    ld ix,voice1_struct 
    call ay_init_voice

    ld ix,voice2_struct 
    call ay_init_voice

    ld ix,voice3_struct 
    call ay_init_voice

    ; border
    ld bc,$7f10
    out (c),c
    
    ; init volume
    ld c,0

    ; which play mode?
    ld a,(play_mode)
    or a
    ; song mode
    jr nz,songloop

    ; jam mode
    ld hl,jam_track
    ld a,(hl)
    jp fetch_tone


;
; loop while the sequence (song) is playing
;
songloop:

    ; get next track of sequence
    ; c = current volume
seq_ptr_hi:
    ; sequence base addr
    ld d,0

    ; get next track no
    ld hl,seq_play_pos
    ld e,(hl) ; de = ptr to sequence
    ld a,(de) ; = selected track
    cp SEQ_END
    jr nz,next_track

    ; end of sequence, re-start
    ld e,0
    ld (hl),e ; from position 0
    ld a,(de) ; read track #

next_track:
    ; advance in sequence
    inc (hl)

    ; get track ptr
    ld l,a
    ld a,(track_list_base)
    ld h,a
    ld e,(hl)
    inc h
    ld d,(hl)

    ; pre-fetch tone
    ex de,hl    ; = track ptr
    ld a,(hl)   ; to fetch tone
    jr fetch_tone

;
; loop until end of track
;
trackloop:
    ; c = current volume

    ; next tone in track
    ; +0 = tone / key
    ; +1 = duration
    ; +2 = instrument

    ; OR effect:

    ; +0 = effect no.
    ; +1,..,+n = effect params


trackptr:
    ld hl,0
    ld a,(hl)   ; get tone
    cp TRK_END
    jr z,songloop    ; end of track, get next

    ; test for effect cmd
    ; < $80 => tone
    cp $80
    jr c,fetch_tone

effect1:
    ; == $80 ?
    jr nz,effect2

    ; => tone slide
    ; +1  = duration
    ; +2 +3 = offset
    inc hl
    ld a,(hl)   ; duration
    ld i,a

    ; deduce 16-bit offset from 8-bit value
    inc hl
    ld a,(hl)   ; get offset 
    ld e,a      ; into lo-byte
    rlca        ; rotate sign into carry flag (cy = 1 when offset is negative)
    ld a,0      ; value to subtract from 
    sbc a       ; a = 0 - cy => a is either 0 or -1 
    ld d,a      ; = signed offset in hl
    ; de = offset
     
    inc hl      ; next note/cmd
    ld (trackptr+1),hl

    ex de,hl
    ld (effect1_value+1),hl

    ; start effect
    ld hl,effect1_loop
    jr start_effect

effect2:
    ; $81 to $84 ?
    cp $85
    jr nc,effect3

    ; $8x => jump in table, where 
    ; x=1: offset (arpeggios), x=2: finetune, x=3: waveform, x=4: volume
    ; +1  = duration
    ; +2  = new position
    sub $81     ; deduce index from effect cmd
    ld b,a      ; ($81 -> 0 -> offset table) and save

    inc hl      ; get
    ld a,(hl)   ; duration
    ld i,a

    inc hl      ; get
    ld a,(hl)   ; new position

    inc hl      ; update track ptr
    ld (trackptr+1),hl

    ld hl,current_offsets
    ld d,0      ; point hl
    ld e,b      ; to table offset
    add hl,de   ; to write   
    ld (hl),a   ; jump position to

    ld hl,tone_loop
    jp start_effect

effect3:
    ; placeholder
    inc hl      ; update track ptr
    ld (trackptr+1),hl

    ld hl,tone_loop
    

start_effect:
    ; store ptr to effect loop
    ld (loop_addr+1),hl

    ; zero length effect -> immediately get next track step
    ld a,i
    or a
    ; TODO: play one sample before jp
    jp z,trackloop

    ; init sample register
    ld a,yl
    ld b,$f4    ; 2
    out (c),a   ; 3 send register no
    ld b,$f6
    ld a,$c0
    out (c),a   ; 11 write
    out (c),0   ; 15

    ; execute effect
    jp (hl)


; 
; next tone to play
; c = current volume
;
fetch_tone:
    ld (current_tone+1),a   ; store tone

    ; get duration and instrument addr
    inc hl
    ld a,(hl)   ; duration
    ld i,a      ; into i
    inc hl
    ld a,(hl)   ; instr no
    inc hl
    ld (trackptr+1),hl

    ; loop to use
    ld hl,tone_loop
    ld (loop_addr+1),hl

    ; look up instr addr
    ld l,a
    ld a,(instr_table_base)
    ld h,a
    ; hl = instr table ptr
    ld e,(hl)
    inc h
    ld d,(hl)
    ex de,hl    
    ; hl = new instrument

    ; read meta field(s)
    ld a,(hl)   ; target AY register 
    inc hl
    ld yl,a     ; store for later use
    ld (target_register+2),a
    
    ; copy instruments table offsets
    ld de,current_offsets
    ldi ; tone
    ldi ; finetune
    ldi ; waveform
    ldi ; volume

    ; zero length tone -> immediately get next track step
    ld a,i
    or a
    jp z,trackloop

    ; select output AY register
    ld b,$f4    ; 2
    ld a,yl     ; target AY reg
    out (c),a   ; 6 send register no
    ld bc,$f6c0 ; 9
    out (c),c   ; 13 write
    out (c),0   ; 17 done
    ; 17 nops   

    ; start playing
    ld c,0  ; TODO
    jp fetch_instr


; these values get written at instrument start
current_offsets:
    db 0 ; tone offset
    db 0 ; finetune
    db 0 ; wave
    db 0 ; vol



;
; effect 1 - tone slide up/down
;
effect1_loop:
    ; 48 nops since last sample!


    ; apply tone slide
    exx
    ld a,b      ; save waveform
    ex de,hl    ; hl = current displacement
effect1_value:
    ld bc,0     ; the slide value   
    add hl,bc   ; add to displacement
    ex de,hl    ; de = new displacement 
    push de     ; will be popped at do_waveform
    ld b,a      ; restore waveform
    ; 11
    add hl,de   ; get 
    ld c,h      ; next
    ld a,(bc)   ; sample
    exx
    ; 18

    ; = 66

    ld hl,$f4f6 ; PIO $f4 = port A / $f6 = port B
    ld de,$c080 ; PSG $c0 = select / $80 = write
    ; = 72

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 13 nops

    ld yh,c     ; copy current volume
    ; 2

    ; init table pointers
    ld hl,current_offsets + 2   ; skip offset
    ld bc,(ptr_wave_table)      ; and finetune!
    ; 6

    ; align
    ds 4

    jp do_waveform



; look up current value from an instrument table
; hl   = current instruments offset (initialized from instr params) to table in bc
; b(c) = base addr to offset/finetune/wave/volume table
; a => resulting value
; nop stable: takes 20 nops in any case
lookup:
    ld c,(hl)   ; 2 current offset
    ld a,(bc)   ; 4 into table
    cp $80      ; 6 jump?
    jr nz,no_jump   ; 8 skip if not
    inc c       ; 9     it was a jump, get 
    ld a,(bc)   ; 11    jump position
    ld c,a      ; 12    update table pos
    ld a,(bc)   ; 14    fetch new value
    inc c       ; 15    go to next
    ld (hl),c   ; 17    position in table
    ret     ; 20

no_jump:
                ; 9 (from above ...) 
    inc c       ; 10    go to next 
    ld (hl),c   ; 12    position in table
    ds 5        ; 17    wait
    ret     ; 20




;
; loop instrument while current tone is active
;
tone_loop:
    ; 49 nops since last sample fetch
    ; 30 nops since last sample write
    ; c = current volume

fetch_instr:
    ld yh,c     ; copy current volume
    ; = 32

    ; init table pointers
    ; hl = tone table offset
    ; bc = tone table, only b is important, c will be calc'ed in lookup()
    ;     that's why only b will be saved (ld d,b ... ld b,d) after here
    ; de = temp / offsets to add hl / misc
    ld hl,current_offsets
    ld a,(ptr_tone_table + 1)
    ld b,a
    ; = 40

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 48

    ; align
    ds 8
    ; = 56

    ; send sample (at nop #16 = 72 since last)
    ld d,b      ; save b
    ld b,$f4    ; Port A
    sub yh      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld b,d      ; restore b
    ; 21 nops
    ; = 5 since sample

    ;
    ; read semi-tone/offset table
    ; 
    call lookup ; 23
    ; = 28

    push af     ; save offset for later use
    ; = 32

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; = 40

    ; align 
    ds 16
    ; = 56

    ; send sample (at nop #16 = 72 since last)
    ld d,b
    ld b,$f4    ; Port A
    sub yh      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld b,d
    ; 21 nops
    ; = 5 since sample

    pop af      ; value from offset tbl
    ; = 8

current_tone:
    add 0       ; 2 add current tone as of track
    ex de,hl    ; 3 save table ptr to de
    ld hl,(tone_table_ptr); 6 look up
    add a       ; 7 current tone
    ld l,a      ; 8 hl points to offset/displacement table
    ld a,(hl)   ; 10    load new
    inc l       ; 11    value into
    ld h,(hl)   ; 13    hl to update
    ld l,a      ; 14    displacement.
    push hl     ; 18    save new displacement   
    ex de,hl    ; 15    restore table ptr from de
    ; 18 nops
    ; = 26


    ; hl = finetune table offset
    ; bc = finetune table
    inc hl      ; 2
    inc b       ; 3
    ; = 29

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 37

    ; align next sample
    ds 19
    ; = 56

    ; send sample (at nop #16 = 72 since last)
    ld d,b
    ld b,$f4    ; Port A
    sub yh      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld b,d
    ; 21 nops
    ; = 5 since sample


    ;
    ; read finetune table
    ;
    call lookup ; 23
    ; = 28

    ; de = current displacement
    pop de
    ; = 31

    ;  a = finetune offset (-128 to +127)
    push hl     ; 3     save instr ptr
    ld l,a      ; 4     copy offset to lo-byte
    rlca        ; 5     rotate sign into carry flag (cy = 1 when offset is negative)
    ld a,0      ; 7     value to subtract from 
    sbc a       ; 8     a = 0 - cy => a is either 0 or -1 
    ld h,a      ; 9     = signed offset in hl
    add hl,de   ; 12    add to displacement
    ex (sp),hl  ; 17    put new displacement on stack, restore instr ptr
    ; = 48

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 56

    ; send sample (at nop #16 = 72 since last)
    ld d,b      ; save b
    ld b,$f4    ; Port A
    sub yh      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld b,d      ; restore b
    ; 21 nops
    ; = 5 since sample


    ; hl = waveform table offset
    ; bc = waveform table
    inc hl      ; 2
    inc b       ; 3
    ; = 8

do_waveform:
    ;
    ; waveform table
    ; 
    call lookup; 23
    ; = 31
    ; a = waveform address hi-byte

    ; look up 
    ld de,(waveform_list_ptr)   ; 6
    ld e,a                      ; 7
    ld a,(de)                   ; 9

    ; apply wave-form
    exx
    ld b,a      ; bc = current wavetable
    pop de      ; and set new displacement
    exx
    ; = 37 
    
    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    ; = 45

    ; align next sample
    ds 9
    ; = 56

    ; send sample (at nop #16 = 72 since last)
    ld d,b      ; save b
    ld b,$f4    ; Port A
    sub yh      ; adjust volume
    out (c),a   ; send value
    ld bc,$f680 ; Port C
    out (c),c   ; write PSG register
    out (c),0   ; inactive 
    ld b,d      ; restore b
    ; 21 nops
    ; = 5 since sample


    ; hl = volume table offset
    ; bc = volume table
    inc hl      ; 2
    inc b       ; 3
    ; = 8

    ;
    ; volume table
    ;
    call lookup ; 23
    ; = 31
 
    ld c,a      ; new volume
    ; = 32

    ; align next sample
    ds 10
    ; = 42


play_samples:
    ; init
    ld de,$c080 ; PSG $c0 = select / $80 = write
    ld hl,$f4f6 ; PIO $f4 = port A / $f6 = port B
    ; = 48

    ; initial no. of wave loop iterations
    ; will be decreased on demand later on
    ld yh,ITERATIONS
    
    ; register mapping:
    ; a  = current sample value
    ; b  = used to address out (c),x 
    ; c  = current instr volume
    ; de, hl as above!
    ; yh = iteration count
    ; yl = target AY register
    ;  i = loop counter

    ; border off
;   ld b,$7f
;   ld a,$54
;   out (c),a

skip_target:
    ; reserve space for the skip-code
    ; depending on skip_flag, there will be either
    ; - a number of nops (effectively enabling AY replay)
    ; or 
    ; - a jp instruction to skip the following series of AY data writes (effectively disabling AY replay)
    ds 3

    ; = 53 . next sample will be sent after 19 nops, i.e. 72 since last

    ;
    ; below a series of transfers of AY register data into the following code
    ; to have register data sent to the AY afterwards.
    ; there is not enough time to:
    ; 1. fetch a value 
    ; 2. select target AY 
    ; 3. send value
    ; 4. restore sample target AY
    ;   within one iteration (of 72us)!
    ; 

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample (at nop #19)
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops
    ; = 23
    ; = 49 nops left before next sample is due

    ; align
    ds NOPS_PER_SAMPLE - (23 + 9 + 9)

    ; render voice 1
    ld ix,voice1_struct ; 4
    call ay_render_tick ; 8

    ; render voice 2
    ld ix,voice2_struct
    call ay_render_tick

    ; render voice 3
    ld ix,voice3_struct
    call ay_render_tick

    ;
    ; samples have been sent and waveloop iterations decreased
    ; accordingly at each call to ay_render_tick!
    ; AY registers for each voice have been rendered
    ; 

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops

    ; set register 7 value
reg7mask:
    ld a,%00111000
    ld (register7+1),a  ; 8
    
    ds NOPS_PER_SAMPLE - 37


    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops

    ds NOPS_PER_SAMPLE - 36

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops
    ; = 23


    ; prepare some registers
    ld d,$c0 ; = PSG select  ; 2 
target_register:
    ld yl,9  ; 5
    ; = 28

    ; hide target register
    ld a,yl ; slot to use
    ld b,14 ; register no to write
    call ay_shadow_register ; = 22
    ld hl,$f4f6 ; PIO $f4 = port A / $f6 = port B
    ; +28 = 56

    ; TODO adjust to ay_shadow_register
    ds NOPS_PER_SAMPLE - 56


    ;
    ; below a series of PSG writes to update registers 0 - 10
    ; to play standard AY music mixed with waveforms on 1 channel
    ; TODO: add registers 11 (hardenv shape), 12 (hardenv period low), maybe 13 (hardenv period hi)..?
    ; 

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample
    ld b,h      ; Port A
    sub c       ; volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive
    ; 15 nops

    ; select register 
reg0_select:
    ld a,0      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done
    ; 16 nops

    ; write register
register0:
    ld a,0      ; 2
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done
    ; 16 nops

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10    select
    out (c),0   ; 13    done
    ; 16 nops   

    ; 8 + 15 + 16 + 16 + 16 = 71 :( :( :(  
    nop

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; vol
    out (c),a   ; send value
    ld b,l      ; Port 
    out (c),e   ; write PSG register
    out (c),0   ; inactive

    ; select register 
reg1_select:
    ld a,1      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register
register1:
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10
    out (c),0   ; 13

    nop


    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; vol
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive

    ; select register 
reg2_select:
    ld a,2      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register 
register2:  
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10    select
    out (c),0   ; 13    done

    nop


    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; vol
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 

    ; select register 
reg3_select:
    ld a,3      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register 
register3:
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10    select
    out (c),0   ; 13    done

    nop


    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; vol
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 

    ; select register 
reg4_select:
    ld a,4      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register 
register4:
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10    select
    out (c),0   ; 13    done

    nop


    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 

    ; select register 
reg5_select:
    ld a,5      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register 
register5:
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10    select
    out (c),0   ; 13    done

    nop


    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; vol
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 

    ; select register
reg6_select:
    ld a,6      ; 2 register #
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register 
register6:
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10    select
    out (c),0   ; 13    done

    nop


    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; vol
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 

    ; select register 
reg7_select:
    ld a,6      ; 2 register #
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done
    ld a,7      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register 
register7:
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10
    out (c),0   ; 13

    nop


    ; TODO do not write register 8 if it's the sample reg!

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 

    ; select register 
reg8_select:
    ld a,8      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register 
register8:
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10
    out (c),0   ; 13

    nop


    ; TODO do not write register 9 if it's the sample reg!

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; vol
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 

    ; select register 
reg9_select:
    ld a,9      ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register 
register9:
    ld a,0      ; 2 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10
    out (c),0   ; 13

    nop


    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; vol
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 

    ; select register 
reg10_select:
    ld a,10     ; 2 register # 
    ld b,h      ; 3 Port A
    out (c),a   ; 6     send register no
    ld b,l      ; 7
    out (c),d   ; 10    select register
    out (c),0   ; 13    done

    ; write register
register10: 
    ld a,0      ; 5 read value
    ld b,h      ; 6 
    out (c),a   ; 9 send value
    ld b,l      ; 10
    out (c),e   ; 13    write register
    out (c),0   ; 16    done

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 6 register no
    ld b,l      ; 7
    out (c),d   ; 10    select
    out (c),0   ; 13    done

    nop

    ;
    ; TODO: add registers 11 to 13
    ;

    ; get next sample 
    exx
    add hl,de   ; add offset
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx

    ; send sample
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; = 23

    ; restore target
    ld a,yl
    ld b,a ; write original reg no at its slot
    call ay_shadow_register ; = 22
    ld hl,$f4f6 ; PIO $f4 = port A / $f6 = port B
    ; + 28 = 51

    ; align next sample
    ds NOPS_PER_SAMPLE - (51 + 6) ; + yh calculations below

    ;
    ; all AY registers for standard music have been written
    ; from here on only the wavetable will be played
    ; 

    ; 12 more samples (= 6 wave loop iterations) sent

    ; 24 samples (= 12 iterations) sent in total


    ; reduce n. of remaining iterations needed
    ld a,-12
    add yh
    ld yh,a
    ; = 6

    ; register mapping

    ; a  = misc
    ; b  = port addrs
    ; c  = current volume
    ; d  = unused
    ; e  = $c0: PSG write
    ; hl = $f4f6

    ; bc' = wavetable 1
    ; de' = displacement 1
    ; hl' = offset 1

    ; ix  = unused
    ; iyh = loop iteration counter
    ; iyl = unused 

; waveloop
wave_loop: 
    ; get next sample
    exx
    add hl,de   ; add displacement
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops
    
    ; send sample (at nop #19 after fetch block start)
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops
    ; = 23

    ld b,$7f    ; 2
    and $1f     ; 4
    or $40      ; 6
    out (c),a   ; 10
    ; = 33

    ds NOPS_PER_SAMPLE - 33

    ; get next sample
    exx
    add hl,de   ; add displacement
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; 8 nops

    ; send sample (at nop #19, as before)
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG register
    out (c),0   ; inactive 
    ; 15 nops
    ; = 23

    ld b,$7f    ; 2
    and $1f     ; 4
    or $40      ; 6
    out (c),a   ; 10
    ; = 33

    ; loop done?
    dec yh      ; 2

; toggle this jp statement to toggle endless tone looping
wave_loop_end:
    jp z,wave_end_manual
    ; 5 nops
    ; = 38

    ; + jp below
    ; = 41

    ds NOPS_PER_SAMPLE - 41; compensate jp below

    ; continue loop
    jp wave_loop    ; 3

;
; 1 frame of waveform data has been player
;


; Variant 1 : plays tone for as long as its duration
wave_end_song:
    ; = 38 nops since last sample

    ; tone done?
    ld a,i      ; 2
    dec a       ; 3
    ld i,a      ; 5
    jp z,tone_end   ; 8
    ; = 46

    ; continue playing tone
loop_addr:
    jp tone_loop
    ; = 49


tone_end:
;
; TODO! .. but what?
; 




; Variant 2 : plays tone endlessly (until current key is released)
wave_end_manual:
    ; = 38 nops since last sample

    ; border off
    ld b,$7f
    ld a,$54
    out (c),a
    ; +8
    ; = 46

wave_end2:

    ; get next 2 samples!
    exx
    add hl,de   ; add displacement
    ld c,h      ; set wavetable pointer
    ld a,(bc)   ; read sample
    exx
    ; +8 nops
    ; = 52

    ds 7
    ; = 61

    ; registers at this point:
    ; a = current sample
    ; b = $7f
    ; c = current volume
    ; de = $c080 = PSG select / write 
    ; hl = $f4f6 = Port A / C

    ; send sample (at nop #11)
    ld b,h      ; Port A
    sub c       ; adjust volume
    out (c),a   ; send value
    ld b,l      ; Port C
    out (c),e   ; write PSG reg
    out (c),0   ; inactive 
    ; 15 nops

    ; = 4 since last sample

    ; select PSG I/O register
    ld b,h      ; Port A
    ld a,14
    out (c),a   ; select register 14
    ld b,l      ; Port C
    out (c),d   ; select PSG reg
    out (c),0   ; inactive
    ; 16 nops   
    ; = 20 since
    
    ; save volume
    ld e,c
    ; = 21

    ; read keyboard via PSG
    inc b       ; = $f7
    ld a,$92    ; Port A = input
    out (c),a   ; 
    dec b       ; = $f6
key_code:
    ld a,$42    ; set BDIR = read
    out (c),a   ;
    ld b,h      ; = $f4
    in a,(c)    ; read into e
    ld bc,$f782 ; Port A = output
    out (c),c   ;
    out (c),0   ; inactive
    ; 30 nops
    ; = 51

    ; any key?
key_check:
    cp $ff      ; 2 ; $ff = no key in current row
    jr z,ende   ; 4/5
    ; = 55
    
    ; next tone
skip_loop:
    jr $+3
    jp trackloop

    ; restore volume
    ld c,e
    ; = 56

    ; restore sample register
    ld a,yl     ; 2 restore register no
    ld b,h      ; 3
    out (c),a   ; 7 register no
    ld b,l      ; 8
    out (c),d   ; 14    select
    out (c),0   ; 18    done
    ; = 

    ; measure tone length
tone_length:
    ld hl,0 ; 3
    inc hl  ; 5
    ld (tone_length+1),hl ; 8

    ; keep playing tone
    jp tone_loop


; stop playing  
ende:
    exx
    pop hl
    pop de
    pop bc
    exx

    ; silence 
    call init_psg

    ei
    ret

; init engine 
init_engine:
    ; create temp data to init waveforms/tables/notes
    call init_tempdata

    ; init memory
    call init_memory

    ; init note table
    call init_notes

    ; init wavetables 
    call init_waves

    ; init instrument tables
    call instr_tables_init

    ; init instruments
    call instr_init

    ; init sequences
    call seq_init

    ; init jump tables
    ; TODO make use of jumping facilty
    call jptab_init

    ; initialize register shadowing
    call ay_shadow_init

    ; silence AY
    ; ld a,%00110111 ; noise on AYa
    ld a,%00111111 ; no tone, no noise
    ld (reg7mask+1),a

    ret


; initialize temp. data
init_tempdata:

    ; temp copy note-lookup
    ld hl,sid_notes
    ld de,$e000
    ld bc,sid_notes_end-sid_notes
    ldir

    ; temp copy waveforms
    ld de,$c000

    ; $c000
    ld hl,wave_sinus
    ld bc,256
    ldir

    ; $c100
    ld hl,wave_triangle
    ld bc,256
    ldir

    ; $c200
    ld hl,wave_ramp
    ld bc,256
    ldir

    ; $c300
    ld hl,pre_square
    ld bc,32*32
    ldir

    ; temp copy soundtables
    ; $d000
    ld hl,tbl_offsets
    ld de,$d000
    ld bc,256*4
    ldir

    ret


; initialize instrument tables (= one sound-bank)
instr_tables_init:
    ; start of data to copy
    ld hl,$d000
    push hl

    ; alloc memory for the sound-bank
    ld hl,256 * 4
    call mem_alloc_page
    ex de,hl
    pop hl
    ; hl = source data
    ; de = memory to store sound-bank at

    ; semi-tone-offset
    ld (ptr_tone_table),de
    ld bc,256
    ldir

    ; finetune
    ld (ptr_finetune_table),de
    ld bc,256
    ldir

    ; wave
    ld (ptr_wave_table),de
    ld bc,256
    ldir

    ; volume
    ld (ptr_volume_table),de
    ld bc,256
    ldir

    ret 


; initialize instruments
instr_init:
    ; alloc memory for 256 instr pointers
    ; => 256 lo + 256 hi-bytes of instr addr
    ld hl,512
    call mem_alloc_page

    ; store h as table base
    ld a,h
    ld (instr_table_base),a
    push hl
    ; hl = memory to store instr ptrs

    ; alloc memory for instruments 
    ; no. of instruments possible = e.g. 256 / instr_size
    ; this is also called an instrument bank
    ld hl,256
    call mem_alloc_page
    ex de,hl
    ; de = memory to store instruments at

    pop hl
    ; hl = base of instr ptr tbl

    ; iterate list of pre-defined instruments
    ld ix,instr_list
instr_copy_loop:
    ; read instr src addr
    ld c,(ix)
    inc ix
    ld b,(ix)
    inc ix
    ld a,b
    or c
    jr z,instr_copy_done

    ; store instr target address
    ld (hl),e
    inc h
    ld (hl),d
    dec h
    inc l ; = instr nr / count
    push hl

    ; copy instr src addr
    ld l,c
    ld h,b
    ; hl = instr src
    ; de = instr target

    ; copy instr params
    ld bc,INSTR_SIZE
    ldir

    ; next
    pop hl
    jr instr_copy_loop

instr_copy_done:
    ld a,l  
    ld (instr_max),a
    ret


; initialize wave tables
; wave table data was prepared by init_tempdata
init_waves:
    ; no wavetable yet
    xor a
    ld (waveform_count),a

    ; alloc memory for the list of pointers to waveforms
    ld hl,256
    call mem_alloc_page
    ld (waveform_list_ptr),hl

    ; square
    ld hl,$c380 ; $80 !
    call store_waveform

    ; ramp
    ld hl,$c200
    call store_waveform

    ; sinus
    ld hl,$c000
    call store_waveform

    ; triangle
    ld hl,$c100
    call store_waveform

    ; silence
    ld hl,$c000
    push hl
    ld d,h
    ld e,l
    ld (hl),0
    inc e
    ld bc,256
    ldir
    pop hl
    call store_waveform
    
    ; create 32 pulsewidth waveforms
    ld b,32
    ld hl,$c300
create_loop:
    push bc
    push hl
    call store_waveform
    pop hl
    ld bc,256 / 32
    add hl,bc
    pop bc
    inc a
    djnz create_loop

    ret

; hl = pointer to waveform
store_waveform:
    ld a,(waveform_count)
    cp 255
    ret nc ; no room for waveform!

    push af ; count
    push hl ; waveform source

    ld hl,(waveform_list_ptr)
    ld l,a
    ; hl = ptr to waveform list
    push hl

    ; alloc memory for waveform
    ld hl,256
    call mem_alloc_page
    ex de,hl
    ; de = new memory

    ; save waveform address hi-byte
    pop hl
    ld (hl),d

    ; copy waveform to target
    pop hl
    ld bc,256
    ldir

    pop af
    inc a
    ld (waveform_count),a
    ret


; init note lookup table
init_notes:

    ; alloc memory for tone-table
    ld hl,256
    call mem_alloc_page
    ld (tone_table_ptr),hl
    ld d,h
    ld e,l

    ; copy from temp
    ld hl,$e000
    ld bc,sid_notes_end-sid_notes
    ldir

    ret

; address of the note-lookup-table
tone_table_ptr:
    dw 0

   

; PSG-Regs init.
init_psg:
    ld hl,reg0
    ld a,0
psg_loop:
    ld c,(hl)
    inc hl
    push af
    call psg_set
    pop af
    inc a
    cp 14
    jr c,psg_loop
    ret; 
   

; assumes that hl is $f4f6 and de is $c080
    ld hl,$f4f6
    ld de,$c080

    ld c,123; value
    ld a,6; reg

psg_quick:
    ld b,h      ; Port A        +1  1
    out (c),a   ; Register No       +3  4
    ld b,l      ; Port C        +1  5
    out (c),d   ; fetch register    +3  8
    out (c),0   ; inactive      +3  12  

    ld b,h      ; PORT A        +1  13
    out (c),c   ; Register Value    +3  16
    ld b,l      ;           +1  17
    out (c),e   ; fetch value       +3  20
    out (c),a   ;           +3  23
    ret


; PSG Register in A mit Wert in C schreiben
psg_set 
    ld      b,$f4   ; PIO Port A
    out     (c),a   ; Register-Nr an PSG
    ld      b,$f6   ; PIO Port C
    in      a,(c)   ; laden
    or      $c0 ; PSG auf -> Register-Nr latch
    out     (c),a   ; setzen
    and     $3f ; PSG inaktiv
    out     (c),a   ; setzen
    ld      b,$f4   ; PIO Port A
    out     (c),c   ; Datenbyte
    ld      b,$f6   ; PIO Port C
    ld      c,a ; Alter Wert Port C
    or      $80 ; PSG auf write
    out     (c),a   ; setzen, Daten uebernehmen
    out     (c),c   ; PSG inaktiv
    ret

; init memory
init_memory:
    ; reset free store
    ld hl,free_store_start
    ld de,free_store_start + 1
    ld bc,FREE_STORE_END - free_store_start
    ld (hl),$e5
    ldir

    ; assign initial block table
    ld hl,BLOCK_TABLE_SIZE
    call mem_alloc
    ; hl = address
    ld (mem_block_table),hl

    ; create generic address lookup block
    ld hl,512
    call mem_alloc_page
    ld a,h
    ld (mem_generic_base),a

    ret

; the base addr of the generic address block
mem_generic_base:
    db 0

; the current upper bound
mem_generic_upper:
    db 0

; test: allocate some memory
new_mem_test:
    ld hl,123
    jp mem_alloc


; allocate a block of memory on page boundary (e.g. at $xx00)
; hl = requested size in bytes
; => hl = addr of memory block
;       or 0 if not enough memory left
mem_alloc_page:
    ; get lo-byte of next free block address
    ld a,(mem_free_ptr)
    or a
    ; is already page aligned, allocate
    jr z,mem_alloc

    ; is not aligned, eat away excess bytes (for now - store in "freed blocks" later!)
    push hl ; save size
    ld b,a  ; b = lo-byte
    xor a   ; subtract from zero
    sub b   ; a = 256 - current lo-byte
    ld h,0  ; this is the block size 
    ld l,a  ; we need to eat away first
    call mem_alloc
    ld a,h
    or l
    ; TODO free memory at hl so it ends up in "freed blocks"
    jr nz,mem_page_avail
    
    ; no memory left!
    pop af  ; throw away size
    and a   ; set zero-flag
    ret

mem_page_avail:

    ; next block is aligned, allocate it
    pop hl
    jr mem_alloc


; allocate a block of memory
; hl = requested size in bytes
; => hl = addr of memory block
;       or 0 if not enough memory left
; => zero-flag set if out of memory
mem_alloc:
    call mem_alloc_impl
    push hl

    ; print stats
    ld a,(print_memstats)
    or a
    call nz,mem_stats

    pop hl
    ld a,h
    or l
    ret

mem_stats:
    ; print address of new block
    push hl
    ld hl,$4615
    call locate
    call print_local
    db "last:",0
    pop hl
    call print_hl

    ; print blocks used
    ld hl,$4616
    call locate
    call print_local
    db "blks:",0
    ld a,(mem_blocks_used)
    call hex_out

    ; print free start
    ld hl,$4617
    call locate
    call print_local
    db "heap:",0
    ld hl,(mem_free_ptr)
    call print_hl

    ; print free size
    ld hl,$4618
    call locate
    call print_local
    db "free:",0
    ld hl,(mem_free_size)
    call print_hl

    ret


; allocate a block of memory
; hl = requested size in bytes
; => hl = addr of memory block
mem_alloc_impl:
    ; save size
    ld b,h
    ld c,l
    ; bc = size of block to allocate

    ; enough free memory left?
    ld hl,(mem_free_size)
    and a ; clear carry
    sbc hl,bc
    jr nc,mem_available
    ; de = free memory in bytes

    ; no, do we have a matching de-allocated block?
    ; TODO    
    jp mem_not_available

    ; yes, create a new block
mem_available:

    ; do we have a block available?
    ld a,(mem_blocks_used)
    cp BLOCKS_PER_TABLE
    jr c,mem_block_available

    ; no, try to create a new one
    ; TODO
    jp mem_not_available

    ; assign a new block
    ; bc = block size
mem_block_available:
    inc a
    ld (mem_blocks_used),a

    ; adjust free remaining
    ld hl,(mem_free_size)
    and a
    sbc hl,bc
    ld (mem_free_size),hl

    ; get start of free store 
    ld hl,(mem_free_ptr)
    ld d,h
    ld e,l 
    ; de = start of new block

    ; advance to behind end of new block
    add hl,bc
    ld (mem_free_ptr),hl
    
    ; store start address of new block
    ld hl,(mem_block_table)
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl
    ld (mem_block_table),hl

    ; save address
    push de

    ; initialize memory
    ld h,d  ; hl = start
    ld l,e
    inc de  ; de = start + 1
    dec bc  ; bc = len - 1
    ld (hl),0
    ldir 

    pop hl
    ; hl = new memory block
    ret

mem_not_available:
    ; we're out of memory :(
    ld hl,0
    ret


;
; END OF CODE SEGMENT
;
    db "/CODE"


;
; DATA 
; 


    db ":MEM"

;
; memory management
;

; number of block pointers per block-table 
; TODO extend table size or add support for more than 1 table
BLOCKS_PER_TABLE    equ 128

; size of 1 block-table
BLOCK_TABLE_SIZE    equ BLOCKS_PER_TABLE * 2

; start of available memory
mem_free_ptr:
    dw free_store_start

mem_free_size:
; current amount of free memory
    dw FREE_STORE_END - free_store_start

; pointer to list of memory blocks
mem_block_table:
    dw 0

; number of blocks used
mem_blocks_used:
    db 0

; flag: print memory stats after allocation
print_memstats:
    db 0

    db "/MEM"


;
; control data
;
    db ":CTRL"
    db "/CTRL"


;
; note names
;
note_names:
    db "C-"
    db "C#"
    db "D-"
    db "D#"
    db "E-"
    db "F-"
    db "F#"
    db "G-"
    db "G#"
    db "A-"
    db "A#"
    db "B-"




;
; AY voice related data
;

    db ":AY-DATA"


; AY Register
reg0:    
    dw 0         ; 0+1   Periode A
    dw 0         ; 2+3   Periode B
    dw 0         ; 4+5   Periode C
    db 0         ; 6     Rauschen
    db %00111111 ; 7     Kontroll Reg.
    db 0         ; 8     Volume A
    db 0         ; 9     Volume B
    db 0         ; 10    Volume C
    dw 0         ; 11+12 Huellk. Periode 
    db 0         ; 13    Huellk. Form


    db ":PERIODS"
;
; period table for AY
; note index: 0 <= index <= 96 ( = 8 octaves a 12 semitones ) 
;
ay_notes:
    dw    3822,3608,3405,3214,3034,2863,2703,2551,2408,2273,2145,2025
    dw    1911,1804,1703,1607,1517,1432,1351,1276,1204,1136,1073,1012
    dw    956,902,851,804,758,716,676,638,602,568,536,506
    dw    478,451,426,402,379,358,338,319,301,284,268,253
    dw    239,225,213,201,190,179,169,159,150,142,134,127
    dw    119,113,106,100,95,89,84,80,75,71,67,63
    dw    60,56,53,50,47,45,42,40,38,36,34,32
    dw    30,28,27,25,24,22,21,20,19,18,17,16
    ;dw    16,0,0
                                                                                                                                        ; 
    db "/PERIODS"

    db "/AY-DATA"

;

; SID voice related data
;

    db ":SID_NOTES"
;
; offset table (for T=72 us and T=256 bytes)
;
sid_notes:
    ; octave 1
    dw 77,81,86,91,97,102,109,115,122,129,137,145
    
    ; 2
    dw 154,163,173,183,194,205,218,231,244,259,274,291

    ; 3
    dw 308,326,346,367,388,411,436,462,489,519,549,582

    ; 4
    dw 617
    dw 653
    dw 692
    dw 734
    dw 777
    dw 823
    dw 872
    dw 924
    dw 979
    dw 1038
    dw 1099
    dw 1165

    ; 5
    dw 1234
    dw 1307
    dw 1385
    dw 1468
    dw 1555
    dw 1647
    dw 1745
    dw 1849
    dw 1959
    dw 2076 ; kammerton A
    dw 2199
    dw 2330

    ; 6
    dw 2469
    dw 2615
    dw 2771
    dw 2936
    dw 3110
    dw 3295
    dw 3491
    dw 3699
    dw 3919
    dw 4152
    dw 4399
    dw 4660

    ; 7
    dw 4938
    dw 5231
    dw 5542
    dw 5872
    dw 6221
    dw 6591
    dw 6983
    dw 7398
    dw 7838
    dw 8304
    dw 8798
    dw 9321

    ; 8
    dw 9876
    dw 10463
    dw 11085
    dw 11744
    dw 12443
    dw 13182
    dw 13966
    dw 14797
    dw 15677
    dw 16609
    dw 17597
    dw 18643

    ; 9
;    dw 19752
;    dw 20926
;    dw 22170
;    dw 23489
;    dw 24886
;    dw 26365
;    dw 27933
;    dw 29594
;    dw 31354
;    dw 33218
;    dw 35194
;    dw 37286

    db "/SID_NOTES"
sid_notes_end:
  

    db ":SONG"
;
; track data

; jam track
jam_track:
    ; tone, duration, instr 
    db 10,93,1

    db TRK_END

; a dummy track!
track1:
    ; tone, duration, instr
    db 30,20,1
    db 42,20,2
    db 30,20,3
    db 42,20,4

    db TRK_END

; some effects in this track
track2:

    db 30,20,5
    db 42,20,6
    db 30,20,7
    db 42,20,8

    db TRK_END

    ; slide, duration, offset
    db $80,40,10
   
    db TRK_END

track3:
    ; dummy

    db "/SONG"
   

    db ":INST"

; # of available waveforms
waveform_count:
    db 0

; pointer to the list of available waveforms
; only the hi-byte is stored per waveform, as they have to lay at page boundary (e.g. $xx00)
waveform_list_ptr:
    dw 0


;
; instrument list
;
instr_list:
    dw instr1
    dw instr1a
    dw instr2
    dw instr3
    dw instr4
    dw instr5
    dw instr6
    dw instr6a
    dw instr6b
    dw instr6c
    dw instr7
    dw instr8
    dw instr9

    dw 0    ; = end of list
    

;
; instrument data
;

; silence
instr0:
    db 9

    ; offset, finetune, wave, volume
    db 0,0,sil,0

instr1:
    db 9

    ; offset, finetune, wave, volume
    db 0,0,0,0 

instr1a:
    db 9

    ; offset, finetune, wave, volume
    db 0,0,75,0 

instr2: 
    db 9
 
    ; offset, finetune, wave, volume
    db 3,0,28,12

instr3:
    db 9

    ; offset, finetune, wave, volume
    db 3,33,40,3 

instr4:
    db 9

    ; offset, finetune, wave, volume
    db 0,23,31,0 

instr5:
    db 10

    ; offset, finetune, wave, volume
    db 22,0,25,12 

instr6a:
    db 8

    ; offset, finetune, wave, volume
    db 0,0,25,0 

instr6b:
    db 8 ;9

    ; offset, finetune, wave, volume
    db 0,0,28,0 

instr6c:
    db 8 ; 10
    
    ; offset, finetune, wave, volume
    db 0,0,31,0 

instr6:
    db 9

    ; offset, finetune, wave, volume
    db 17, 0, 34, 3

instr7:
    db 8

    ; offset, finetune, wave, volume
    db 7, 0, 34, 3

instr8:
    db 9

    ; offset, finetune, wave, volume
    db 17
    db 0
    db 37
    db 3

instr9:
    db 10

    ; offset, finetune, wave, volume
    db 12
    db 0
    db 34
    db 3

instr11:
    db 9

    db 3 ;3 ; offset-table pos
    db 0 ;3 ; finetune-table pos
    db 3 ;3 ; wave-table pos
    db 3 ;3 ; volume-table pos

    ; end of instruments
    db $ff

    db "/INST"


; start of the free store
; all data after here will be copied to newly allocated memory!
free_store_start:


    db ":ITBLS"
;
; instrument tables
;

tbl_offsets:
    db 0
    db $80,0

    ; 3
    db 12,0
    db $80,4

    ; 7
    db 0,2,7
    db $80,7

    ; 12
    db 0,3,7
    db $80,12

    ; 17
    db 0,4,7
    db $80,17

    ; 22
    db 0,7,12
    db $80,22

    ; 27

    ds 256 - ($-tbl_offsets)

finetunes:
    db 0
    db $80,0

    ; 3
    db 0,-10,-20,-10,0,10,20,10,$80,3

    ; 13
    db 0,-20,-40,-20,0,20,40,20,$80,13

    ; 23
    db 0,-4,-8,-4,0,4,8,4,0,$80,23

    ; 33
    db 0,0,0,0,0,0,0,0
    db 0,-20,-40,-20,0,20,40,20,$80,41

    ; 51

    ds 256 - ($-finetunes)

waves:
    db w_square
    db $80,0

    ; 3
    db w_pulse+2
    db w_pulse+3
    db w_pulse+4
    db w_pulse+5
    db w_pulse+6
    db w_pulse+7
    db w_pulse+8
    db w_pulse+9
    db w_pulse+10
    db w_pulse+11
    db w_pulse+12
    db w_pulse+11
    db w_pulse+10
    db w_pulse+9
    db w_pulse+8
    db w_pulse+7
    db w_pulse+6
    db w_pulse+5
    db w_pulse+4
    db w_pulse+3
    db $80,3

    ; 25
    db w_sinus,$80,25

    ; 28
    db w_ramp,$80,28

    ; 31
    db w_triangle,$80,31

    ; 34
    db w_pulse+12,$80,34

    ; 37
    db w_pulse+8,$80,37

    ; 40
    db w_pulse+8    
    db w_pulse+7
    db w_pulse+6    
    db w_pulse+5    
    db w_pulse+4
    db w_pulse+3    
    db w_pulse+2
    db w_pulse+1
    
    db w_pulse+0
    db w_pulse+1
    db w_pulse+2
    db w_pulse+3
    db w_pulse+4
    db w_pulse+5
    db w_pulse+6
    db w_pulse+7
    db w_pulse+8
    db w_pulse+9
    db w_pulse+10
    db w_pulse+11
    db w_pulse+12
    db w_pulse+13
    db w_pulse+12
    db w_pulse+11
    db w_pulse+10
    db w_pulse+9
    db w_pulse+8
    db w_pulse+7
    db w_pulse+6
    db w_pulse+5
    db w_pulse+3
    db w_pulse+1
    db $80, 49

    ; 75
    db w_pulse+30
    db w_pulse+30
    db w_pulse+29
    db w_pulse+29
    db w_pulse+28
    db w_pulse+28
    db w_pulse+27
    db w_pulse+27
    db w_pulse+26
    db w_pulse+26
    db w_pulse+25
    db w_pulse+25
    db w_pulse+24
    db w_pulse+24
    db w_pulse+23
    db w_pulse+23
    db w_pulse+22
    db w_pulse+22
    db w_pulse+21
    db w_pulse+21
    db w_pulse+20
    db w_pulse+20
    db w_pulse+19
    db w_pulse+19
    db w_pulse+18
    db w_pulse+18
    db w_pulse+17
    db w_pulse+17
    db w_pulse+16
    db w_pulse+16
    db w_pulse+15
    db w_pulse+15
    db w_pulse+14
    db w_pulse+14
    db w_pulse+13
    db w_pulse+13
    db w_pulse+12
    db w_pulse+12
    db w_pulse+11
    db w_pulse+11
    db w_pulse+10
    db w_pulse+10
    db w_pulse+9
    db w_pulse+9
    db w_pulse+8
    db w_pulse+8
    db w_pulse+7
    db w_pulse+7
    db w_pulse+6
    db w_pulse+6
    db w_pulse+5
    db w_pulse+5
    db w_pulse+4
    db w_pulse+4
    db w_pulse+3
    db w_pulse+3
    db w_pulse+2
    db w_pulse+2
    db w_pulse+1
    db w_pulse+1
    db w_pulse+0
    db w_pulse+0

    db w_pulse+1
    db w_pulse+1
    db w_pulse+2
    db w_pulse+2
    db w_pulse+3
    db w_pulse+3
    db w_pulse+4
    db w_pulse+4
    db w_pulse+5
    db w_pulse+5
    db w_pulse+6
    db w_pulse+6
    db w_pulse+7
    db w_pulse+7
    db w_pulse+8
    db w_pulse+8
    db w_pulse+9
    db w_pulse+9
    db w_pulse+10
    db w_pulse+10
    db w_pulse+11
    db w_pulse+11
    db w_pulse+12
    db w_pulse+12
    db w_pulse+13
    db w_pulse+13
    db w_pulse+14
    db w_pulse+14
    db w_pulse+15
    db w_pulse+15
    db w_pulse+16
    db w_pulse+16
    db w_pulse+17
    db w_pulse+17
    db w_pulse+18
    db w_pulse+18
    db w_pulse+19
    db w_pulse+19
    db w_pulse+20
    db w_pulse+20
    db w_pulse+21
    db w_pulse+21
    db w_pulse+22
    db w_pulse+22
    db w_pulse+23
    db w_pulse+23
    db w_pulse+24
    db w_pulse+24
    db w_pulse+25
    db w_pulse+25
    db w_pulse+26
    db w_pulse+26
    db w_pulse+27
    db w_pulse+27
    db w_pulse+28
    db w_pulse+28
    db w_pulse+29
    db w_pulse+29
    db w_pulse+30
    db w_pulse+30
    db w_pulse+31
    db w_pulse+31
    db $80,75

sil:    equ $-waves

    ; 203
    db w_silence
    db $80,sil

    ; 206

    ds 256 - ($-waves)

volumes:
    db 0
    db $80,0

    ; 3
    db 0,0,1,1,2,2,3
    db $80,9

    ; 12
    db 0,1,2
    db $80,14

    ; 17
    db 2
    db $80,17

    ; 20
    db 4
    db $80,20

    ; 23
    db 6
    db $80,23




    ds 256 - ($-volumes)

end_of_tables:

    db "/ITBLS"

    
;
; wavetables
;
    db ":WAVE"

wave_tables:


; TODO re-generate (value range 8-15)
wave_sinus:
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9
    db 10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11
    db 11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12
    db 12,13,13,13,13,13,13,13,13,13,13,13,13,13,14,14
    db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
    db 14,14,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,14
    db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
    db 14,14,14,13,13,13,13,13,13,13,13,13,13,13,13,13
    db 12,12,12,12,12,12,12,12,12,12,12,11,11,11,11,11
    db 11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10
    db 10,9,9,9,9,9,9,9,9,9,9,9,9,9,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7

wave_triangle:
; this is a filtered square wave!
    db 15,15,15,14,14,14,13,13,13,13,13,12,12,12,12,12
    db 11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10
    db 9,9,9,9,9,9,9,9,9,9,9,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
; this is a triangle wave
; TODO: re-create (value range 8-15)
    db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
    db 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10
    db 11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11
    db 12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
    db 13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13
    db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
    db 13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13
    db 12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
    db 11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11
    db 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10
    db 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8

wave_ramp:
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
    db 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
    db 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10
    db 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10
    db 11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11
    db 11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11
    db 12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
    db 12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
    db 13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13
    db 13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13
    db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
    db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15

pre_square: 
; 0: 0000
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
; 4: 0001
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
wave_square:
; 8: 0011 = 50/50 = square
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
; 12: 0111
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
; 16: 1111
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
; 20: 1110
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
; 24: 1100 = 50/50 = square
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
; 28: 1000
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
; 32: 0000
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8

    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8

    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8

    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    db 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8

wave_end:

    db "/WAVE"

    db "EOF EOF EOF"

; end of "file"
the_eof:

#end
;
; EOF

