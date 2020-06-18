model tiny
.code
org 100h

_start:
jmp begin

old_int9_off dw 0
old_int9_seg dw 0

new_int9 proc near
    push ax
    push di
    push es
    
    ; read and save scan code from keyboard port 
    in  al, 60h
    mov dl, al
    call store
    
    ; send keybord stop signal
    in al, 61h
    mov ah, al
    or al, 80h
    out 61h, al
    xchg ah, al
    out 61h, al
    mov al, 20h
    out 20h, al
    
    pop es
    pop di
    pop ax  
    iret
new_int9 endp


old_int8_off dw 0
old_int8_seg dw 0

new_int8 proc near
	push ax

	mov al, [count]
	inc al
	cmp al, [speed]
	jnz not_time

	mov al, 1
	mov [update_flag], al
	xor al, al

	not_time:
	mov [count], al 

	pop ax
	jmp dword ptr cs:old_int8_off
new_int8 endp


store:
    ; dl - byte to store
    push di
    push dx

    mov di, [tail]
    mov [di], dl
    inc di
    
    sub di, offset symbol_buffer
    cmp di, buffer_len
    jne not_max_store
    
    mov di, 0
    
    not_max_store:
        add di, offset symbol_buffer
        mov [tail], di 

    pop dx
    pop di
    ret  


take:
    ; take byte from buffer
    mov di, [head]
    mov al, [di]
    inc di
    
    sub di, offset symbol_buffer
    cmp di, buffer_len
    jne not_max_take
    
    mov di, 0
    
    not_max_take:
        add di, offset symbol_buffer
        mov [head], di 
        
    ; al - byte from buffer
    ret


begin:
	call parse_args

	; save old int8
    mov ax, 3508h
    int 21h
    mov [cs:old_int8_off], bx
    mov [cs:old_int8_seg], es

	; save old int9
    mov ax, 3509h
    int 21h
    mov [cs:old_int9_off], bx
    mov [cs:old_int9_seg], es

    ; set new int 8 handler
    lea dx, new_int8
    mov ah, 25h
    mov al, 8h
    int 21h

    ; set new int 9 handler
    lea dx, new_int9
    mov ah, 25h
    mov al, 9h
    int 21h

    ; get current mode 
    mov ax, 040h
    mov es, ax

    mov di, 049h
    mov bl, [es:di]
    mov [current_mode], bl

	; play game 
	call create_seed
	call init_snake
	call add_field_elements
	call main_loop

	; show results
	call update_statistics

	mov al, [win_flag]
	test al, al
	jz lost

	call show_win
	lea si, win_melody
	lea di, win_delay

	jmp wait_esc

	lost:
	call show_lose
	lea si, lose_melody
	lea di, lose_delay

	; wait for pressed key 
	wait_esc:
		; play melody 
        mov ax, [si]

		; skip if melody ended
		cmp ax, -1
		jz chh

		mov cl, [di]
		call play_sound_with_delay
		add si, 2
		inc di
		call sound_off

		chh:
		; wait for pressed key
        mov bx, word ptr[tail]
        cmp bx, word ptr[head]
        jnz check_key

        hlt
        jmp wait_esc

        check_key:
        ; take byte from buffer
        push di
        call take
        pop di

        ; exit on escape key
        cmp al, 1h
        jz exit_game
    jmp wait_esc

    exit_game:
	; restore mode 
    xor ah, ah
    mov al, [current_mode]
    int 10h

	; set back old int9
    push ds
    mov dx, [cs:old_int9_seg]
    mov ds, dx
    mov dx, [cs:old_int9_off]
    mov ah, 25h
    mov al, 9h
    int 21h
    pop ds

    ; set back old int8
    push ds
    mov dx, [cs:old_int8_seg]
    mov ds, dx
    mov dx, [cs:old_int8_off]
    mov ah, 25h
    mov al, 8h
    int 21h
    pop ds

	; exit
    mov ah, 4ch
    int 21h


main_loop:
	call show_help
	read_loop:
		; check update flag 
        mov al, [update_flag]
        test al, al 
        jnz update_game

        ; wait for pressed key
        mov bx, word ptr[tail]
        cmp bx, word ptr[head]
        jnz process_byte

        hlt
        jmp read_loop

        process_byte:
        ; take byte from buffer
        push di
        call take
        pop di

        ; exit on escape key
        cmp al, 1h
        jz exit

        ; handle hotkeys 
        call change_direction
        call change_speed
        call change_pause
        call change_help

        jmp read_loop

        update_game:
        ; clear update flag
        xor al, al
        mov [update_flag], al

        ; check pause flag
        mov al, [pause_flag]
        test al, al
        jnz read_loop

        ; move snake 
        call move_snake

        ; check death 
        mov al, [death_flag]
        test al, al
        jnz exit

        ; check win 
        mov al, [win_flag]
        test al, al
        jnz exit

        call add_snake
        call draw_play_field

    jmp read_loop

    exit:
    ret


change_direction:
	; al - pressed key 
	push ax
	push bx

	mov bl, [snake_direction]

	; rigth arrow
	cmp al, 77
	jnz nra

	inc bl 
	and bl, 3
	mov [snake_direction], bl

	jmp no_arrow

	nra:
	; left arrow
	cmp al, 75
	jnz no_arrow

	dec bl 
	and bl, 3
	mov [snake_direction], bl

	no_arrow:
	pop bx
	pop ax
	ret


change_speed:
	; al - pressed key 
	push ax
	push bx

	mov bl, [speed]

	; up arrow
	cmp al, 72
	jnz nua

	sub bl, 2
	cmp bl, 2
	jl no_arrow_2
	mov [speed], bl

	jmp no_arrow_2

	nua:
	; down arrow
	cmp al, 80
	jnz no_arrow_2

	add bl, 2
	cmp bl, 10
	jg no_arrow_2
	mov [speed], bl

	no_arrow_2:
	pop bx
	pop ax
	ret


change_pause:
	; al - pressed key 
	push ax
	push bx

	; P key 
	cmp al, 25
	jnz no_pause

	mov bl, [help_flag]
	test bl, bl 
	jnz no_pause

	mov bl, [pause_flag]
	inc bl
	and bl, 1
	mov [pause_flag], bl 

	no_pause:
	pop bx
	pop ax
	ret


change_help:
	; al - pressed key 
	push ax
	push bx

	; H key 
	cmp al, 35
	jnz no_help

	mov bl, [help_flag]
	test bl, bl 
	jz help_not_shown

	xor bl, bl
	mov [pause_flag], bl
	mov [help_flag], bl
	call hide_help
	jmp no_help

	help_not_shown:
	mov bl, 1
	mov [pause_flag], bl
	mov [help_flag], bl
	call show_help

	no_help:
	pop bx
	pop ax
	ret

move_snake:
	push ax
	push bx
	push si

	mov si, [snake_tail]

	move_loop:
		; set coordinates to previous segment's 
		mov ax, [si]
		mov [si+2], ax

		cmp si, offset snake_buffer
		jnz not_head

		call move_head
		jmp all_moved

		not_head:
		sub si, 2
	
	jmp move_loop

	all_moved:
	; remove snake from map
	mov si, [snake_tail]
	add si, 2

	move_loop_2:
		mov ax, [si]
		xor bl, bl
		call add_element

		cmp si, offset snake_buffer
		jz all_moved_2

		sub si, 2
	jmp move_loop_2

	all_moved_2:
	pop si
	pop bx
	pop ax

	ret 


move_head:
	; (al, ah) - head coorditates
	push ax
	push bx
	push cx
	push si

	; get head direction 
	xor bx, bx
	mov bl, [snake_direction]
	lea si, possible_directions
	add si, bx
	mov bl, [si]

	; calculate new coodrinates 
	cmp bl, 'r'
	jnz nr

	inc al
	jmp calculated

	nr:
	cmp bl, 'd'
	jnz nd

	inc ah
	jmp calculated

	nd:
	cmp bl, 'l'
	jnz nl

	dec al
	jmp calculated

	nl:
	dec ah 

	calculated:
	; check new cell content 
	call get_element

	; death wall 
	cmp bl, 'd'
	jnz nw

	mov bl, 1
	mov [death_flag], bl 
	jmp movedd

	nw:
	; bouncy wall 
	cmp bl, 'b'
	jnz nww

	call handle_bounce
	jmp empty

	nww:
	; portal wall 
	cmp bl, 'p'
	jnz nwww

	call handle_portal
	jmp empty

	nwww:
	; poison 
	cmp bl, 'P'
	jnz nwwww

	lea si, poison_sound
	lea di, poison_delay
	call play_melody

	mov bl, 1
	mov [death_flag], bl 
	jmp movedd

	nwwww:
	; increment 
	cmp bl, 'I'
	jnz nwwwww

	call handle_increment
	jmp empty

	nwwwww:
	; decrement 
	cmp bl, 'D'
	jnz nwwwwww

	call handle_decrement
	jmp empty

	nwwwwww:
	; snake segment 
	cmp bl, 'S'
	jnz empty

	call handle_intersection

	empty:
	; cell could be taken 
	lea si, snake_buffer
	mov [si], ax

	movedd:
	pop si
	pop cx
	pop bx
	pop ax

	ret


handle_intersection:
	; (al, ah) - intersection coordinates 
	push ax
	push bx
	push cx
	push si

	; check intersection mode
	mov bl, [intersection_mode]
	test bl, bl 
	jnz not_forbidden

	; set death flag if forbidden 
	mov bl, 1
	mov [death_flag], bl
	jmp intha

	not_forbidden:
	cmp bl, 1
	jnz not_allowed

	; don't do anything if allowed 
	jmp intha

	not_allowed:
	; cut snake 
	lea si, snake_buffer
	search_loop:
		mov bx, [si]
		add si, 2
		cmp bx, ax
	jnz search_loop

	; set tail to last not intersecting 
	mov cx, [snake_tail]
	add cx, 4
	sub si, 4
	mov [snake_tail], si
	xor bl, bl 

	; remove segments after intersection 
	delete_rest:
		cmp si, cx
		jz intha

		mov ax, [si]
		call add_element
		add si, 2
	jmp delete_rest

	intha:
	pop si
	pop cx
	pop bx 
	pop ax 

	ret 


handle_decrement:
	push ax
	push bx
	push si

	lea si, decrement_sound
	lea di, decrement_delay
	call play_melody

	lea si, snake_buffer
	cmp si, [snake_tail]
	jnz dele

	mov al, 1
	mov [death_flag], al 
	jmp deleted

	dele:
	; delete last segment
	mov si, [snake_tail]
	mov ax, [si+2]
	xor bl, bl
	call add_element
	sub si, 2
	mov [snake_tail], si

	mov al, [decrement_count]
	inc al
	mov [decrement_count], al 

	deleted:
	pop si
	pop bx
	pop ax

	ret


handle_increment:
	push ax
	push si

	lea si, increment_sound
	lea di, increment_delay
	call play_melody

	; check actual snake size
	mov ax, [snake_tail]
	sub ax, offset snake_buffer
	shr ax, 1
	inc ax

	; set win flag if max len reached
	cmp ax, snake_len
	jnz hhh

	mov al, 1
	mov [win_flag], al

	hhh:
	mov ax, [snake_tail]
	add ax, 2
	mov [snake_tail], ax

	mov al, [increment_count]
	inc al
	mov [increment_count], al 

	pop si
	pop ax

	ret


handle_bounce:
	push bx
	push si

	lea si, bounce_sound
	lea di, bounce_delay
	call play_melody

	; get head coordinates
	lea si, snake_buffer
	mov ax, [si]

	; bounce to the center
	cmp ah, (f_heigth / 2 + 1)
	jg lower

	inc ah
	jmp bbb

	lower:
	dec ah

	bbb:
	; change direction
	mov bl, [snake_direction]
	add bl, 2
	and bl, 3
	mov [snake_direction], bl

	pop si
	pop bx
	; (al, ah) - new head coordinates
	ret


handle_portal:
	push si

	lea si, portal_sound
	lea di, portal_delay
	call play_melody

	; get head coordinates
	lea si, snake_buffer
	mov ax, [si]

	cmp ah, 1
	jnz decrease

	mov ah, (f_heigth - 2)
	jmp ppp

	decrease:
	mov ah, 1

	ppp:
	pop si
	; (al, ah) - new head coordinates
	ret


init_snake:
	; create snake with start length
	push ax
	push cx 
	push si

	; get head coordinates
	lea si, snake_buffer
	mov ax, [si]

	; initialize snake buffer 
	mov cl, [start_len]
	inc cl
	mov ah, 1
	init_loop:
		mov al, cl
		mov [si], ax
		add si, 2

		dec cl
	jnz init_loop

	; set tail pointer 
	lea ax, snake_buffer
	xor cx, cx
	mov cl, [start_len]
	shl cx, 1
	add ax, cx
	mov [snake_tail], ax 

	pop si
	pop cx
	pop ax

	ret  


add_snake:
	; add game elements to field 
	push ax
	push bx
	push dx 
	push si

	; add snake 
	mov dx, snake_len
	lea si, snake_buffer
	add si, 2
	dec dx 

	; add segments
	mov bl, 'S'
	snake_loop:
		cmp si, [snake_tail]
		jg full_snake

		test dx, dx
		jz full_snake

		mov ax, [si]
		cmp ax, -1
		jz full_snake

		call add_element
		add si, 2
		dec dx
	jmp snake_loop

	full_snake:
	; add head 
	lea si, snake_buffer
	mov ax, [si]
	mov bl, 'H'
	call add_element

	pop si 
	pop dx
	pop bx
	pop ax

	ret 


add_field_elements:
	; add game elements to field 
	push ax
	push bx
	push cx
	push dx 

	call add_snake

	; add enough increment elements to win 
	mov cx, snake_len
	increment_gen_loop:
		call generate_coordinate
		mov bl, 'I'
		call add_element
		dec cx
	jnz increment_gen_loop

	; add from 0 to snake_len decrement elements
	mov ax, snake_len
	call random
	mov cx, ax
	decrement_gen_loop:
		test cx, cx
		jz ready

		call generate_coordinate
		mov bl, 'D'
		call add_element
		dec cx
	jmp decrement_gen_loop

	ready:
	; add poison_count poison elements 
	mov cl, [poison_count]
	poison_gen_loop:
		test cl, cl
		jz all_added

		call generate_coordinate
		mov bl, 'P'
		call add_element
		dec cl
	jmp poison_gen_loop

	all_added:
	pop dx
	pop cx
	pop bx
	pop ax

	ret 


generate_coordinate:
	; get random coordinate of free cell

	; generate x from 1 to f_width-2
	mov ax, f_width
	sub ax, 3
	call random
	inc ax 
	mov bx, ax 

	; generate y from 1 to f_heigth-2
	mov ax, f_heigth
	sub ax, 3
	call random
	inc ax

	mov ah, al
	mov al, bl
	call get_element

	test bl, bl
	jnz generate_coordinate

	; (al, ah) - coordinates
	ret 


add_element:
	; (al, ah) - coorditates, bl - element code 
	push ax
	push bx
	push cx
	push si

	mov cx, ax
	xor ax, ax
	mov al, ch
	mov ch, f_width
	mul ch 
	xor ch, ch
	add ax, cx 

	lea si, play_field
	add si, ax
	mov [si], bl 

	pop si
	pop cx
	pop bx
	pop ax

	ret


get_element:
	; (al, ah) - coorditates
	push ax
	push cx
	push si

	mov cx, ax
	xor ax, ax
	mov al, ch
	mov ch, f_width
	mul ch 
	xor ch, ch
	add ax, cx 

	lea si, play_field
	add si, ax
	mov bl, [si]

	; bl - element code
	pop si
	pop cx
	pop ax

	ret


draw_play_field:
	; draw field based on symbols in the buffer 
	push ax
	push bx
	push cx
	push dx 
	push bp

	call update_statistics

	; draw game field 
	lea si, play_field

	mov bp, f_heigth
	xor cx, cx
	draw_loop:
		mov dx, f_width
		xor bx, bx
		
		int_loop:
			mov al, [si]
			call draw_element
			inc si
			inc bx
			dec dx
		jnz int_loop
		
		inc cx
		dec bp
	jnz draw_loop

	; draw statistics
	lea si, stats_field
	mov bp, 3
	mov cx, 4
	draw_loop_2:
		mov dx, 4
		mov bx, 34

		int_loop_2:
			mov al, [si]
			call draw_element
			inc si
			inc bx
			dec dx
		jnz int_loop_2

		add cx, 2
		dec bp
	jnz draw_loop_2

	pop bp
	pop dx
	pop cx
	pop bx
	pop ax

	ret


draw_element:
	; al - element code 

	cmp al, 'p'
	jnz dif
	call draw_portal_wall
	jmp endd

	dif:
	cmp al, 'd'
	jnz diff
	call draw_death_wall
	jmp endd

	diff:
	cmp al, 'b'
	jnz difff
	call draw_bouncy_wall
	jmp endd

	difff:
	cmp al, 'H'
	jnz diffff
	call draw_snake_head
	jmp endd

	diffff:
	cmp al, 'S'
	jnz difffff
	call draw_snake_segment
	jmp endd

	difffff:
	cmp al, 'P'
	jnz diffffff
	call draw_poison
	jmp endd

	diffffff:
	cmp al, 'I'
	jnz difffffff
	call draw_increment
	jmp endd

	difffffff:
	cmp al, 'D'
	jnz num
	call draw_decrement
	jmp endd

	num:
	cmp al, '1'
	jnz num1
	call draw_one
	jmp endd

	num1:
	cmp al, '2'
	jnz numm
	call draw_two
	jmp endd

	numm:
	cmp al, '3'
	jnz nummm
	call draw_three
	jmp endd

	nummm:
	cmp al, '4'
	jnz nummmm
	call draw_four
	jmp endd

	nummmm:
	cmp al, '5'
	jnz nummmmm
	call draw_five
	jmp endd

	nummmmm:
	cmp al, '6'
	jnz nummmmmm
	call draw_six
	jmp endd

	nummmmmm:
	cmp al, '7'
	jnz nummmmmmm
	call draw_seven
	jmp endd

	nummmmmmm:
	cmp al, '8'
	jnz nummmmmmmm
	call draw_eight
	jmp endd

	nummmmmmmm:
	cmp al, '9'
	jnz nummmmmmmmm
	call draw_nine
	jmp endd

	nummmmmmmmm:
	cmp al, '0'
	jnz diffffffff
	call draw_zero
	jmp endd

	diffffffff:
	call draw_empty

	endd:
	ret


draw_empty:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx
	push dx

	shl bx, 3
	shl cx, 3
	mov dx, 8
	xor ax, ax

	empty_loop:
		call draw_eight_pixels
		inc cx
		dec dx
	jnz empty_loop

	pop dx
	pop cx
	pop bx
	pop ax

	ret


draw_death_wall:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx
	push dx

	shl bx, 3
	shl cx, 3
	mov dx, 8
	mov ax, 0aaaah

	death_loop:
		call draw_eight_pixels
		inc cx
		dec dx
	jnz death_loop

	pop dx
	pop cx
	pop bx
	pop ax

	ret


draw_portal_wall:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx
	push dx

	shl bx, 3
	shl cx, 3
	mov dx, 8
	mov ax, 0cccch

	portal_loop:
		call draw_eight_pixels
		inc cx
		dec dx
	jnz portal_loop

	pop dx
	pop cx
	pop bx
	pop ax

	ret


draw_bouncy_wall:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx
	push dx

	shl bx, 3
	shl cx, 3
	mov dx, 4

	bouncy_loop:
		mov ax, 0ffffh
		call draw_eight_pixels
		inc cx

		mov ax, 0ff3fh
		call draw_eight_pixels
		inc cx

		dec dx
	jnz bouncy_loop

	pop dx 
	pop cx
	pop bx
	pop ax

	ret


draw_increment:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	inc cx

	mov ax, 0c003h
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	mov ax, 0fc3fh
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	mov ax, 0c003h
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_decrement:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	inc cx

	mov ax, 0c003h
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	mov ax, 0bc3eh
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	mov ax, 0c003h
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_poison:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 0a00ah
	call draw_eight_pixels

	mov ax, 0a82ah
	inc cx
	call draw_eight_pixels

	mov ax, 8282h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0a82ah
	inc cx
	call draw_eight_pixels

	mov ax, 0a00ah
	inc cx
	call draw_eight_pixels

	mov ax, 2008h
	inc cx
	call draw_eight_pixels

	mov ax, 0a00ah
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_snake_segment:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 4001h
	call draw_eight_pixels

	mov ax, 5415h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 5555h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 5415h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 4001h
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_snake_head:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 4001h
	call draw_eight_pixels

	mov ax, 5415h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 1554h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 5415h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 4001h
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_zero:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx
	push dx

	shl bx, 3
	shl cx, 3

	mov ax, 0c003h
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c30h
	mov dx, 4
	zero_loop:
		inc cx
		call draw_eight_pixels
		dec dx
	jnz zero_loop 

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c003h
	inc cx
	call draw_eight_pixels

	pop dx 
	pop cx
	pop bx
	pop ax

	ret


draw_one:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx
	push dx

	shl bx, 3
	shl cx, 3

	mov ax, 3000h
	call draw_eight_pixels

	mov ax, 0f000h
	inc cx
	call draw_eight_pixels

	mov ax, 3003h
	inc cx
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 3000h
	mov dx, 4
	one_loop:
		inc cx
		call draw_eight_pixels
		dec dx
	jnz one_loop 

	pop dx 
	pop cx
	pop bx
	pop ax

	ret


draw_two:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 0c003h
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c00h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 3000h
	inc cx
	call draw_eight_pixels

	mov ax, 0c000h
	inc cx
	call draw_eight_pixels

	mov ax, 3h
	inc cx
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_three:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 0c003h
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c00h
	inc cx
	call draw_eight_pixels

	mov ax, 0f003h
	inc cx
	call draw_eight_pixels

	mov ax, 0c00h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c003h
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_four:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx
	push dx

	shl bx, 3
	shl cx, 3

	mov ax, 300ch
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	mov ax, 3000h
	mov dx, 4
	four_loop:
		inc cx
		call draw_eight_pixels
		dec dx
	jnz four_loop 

	pop dx 
	pop cx
	pop bx
	pop ax

	ret


draw_five:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 0f00fh
	call draw_eight_pixels

	mov ax, 0ch
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	mov ax, 0c00h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c003h
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_six:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 0c003h
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 30h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0f03fh
	inc cx
	call draw_eight_pixels

	mov ax, 3030h
	inc cx
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c003h
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_seven:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx
	push dx

	shl bx, 3
	shl cx, 3

	mov ax, 0f00fh
	call draw_eight_pixels

	mov ax, 3000h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0fc00h
	inc cx
	call draw_eight_pixels

	mov ax, 3000h
	mov dx, 4
	seven_loop:
		inc cx
		call draw_eight_pixels
		dec dx
	jnz seven_loop 

	pop dx 
	pop cx
	pop bx
	pop ax

	ret


draw_eight:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 0f00fh
	call draw_eight_pixels

	mov ax, 0c30h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	mov ax, 0c30h
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 0f00fh
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_nine:
	; (bx, cx) - cell coordinates 
	push ax
	push bx
	push cx

	shl bx, 3
	shl cx, 3

	mov ax, 0c003h
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c0ch
	inc cx
	call draw_eight_pixels

	mov ax, 0fc0fh
	inc cx
	call draw_eight_pixels

	mov ax, 0c00h 
	inc cx
	call draw_eight_pixels
	inc cx
	call draw_eight_pixels

	mov ax, 300ch
	inc cx
	call draw_eight_pixels

	mov ax, 0c003h
	inc cx
	call draw_eight_pixels

	pop cx
	pop bx
	pop ax

	ret


draw_eight_pixels:
	; (bx, cx) - start coordinates, ax - attributes
	push ax
	push bx
	push cx
	push dx

	push ax
	xor di, di
	
	; set segment register
	mov ax, 0b800h
	mov es, ax

	; if Y is odd
	test cx, 1
	jz is_even

	mov di, (2000h-40)

	is_even:
	; calculate di
	xor dx, dx 
	mov ax, 40
	mul cx
	shr bx, 2
	add ax, bx

	add di, ax

	; write ax
	pop ax
	stosw

	pop dx
	pop cx
	pop bx 
	pop ax

	ret


create_seed:
	; create seed from system time 
	push ax
	push dx 

	; get current time 
	push es
    xor ax, ax
    mov es, ax
    mov dx, es:[46ch]
    pop es

    mov [seed], dx
    
    pop dx
    pop ax 

    ret


random:
	; get random int from 0 to ax
    push cx
    push dx 

    push ax

    ; ax = (25173 * seed + 13849) mod 65536
    mov ax, 25173
    mul word ptr cs:[seed]
    add ax, 13849
    mov [seed], ax
   
    ; dx = ax mod (upper border)
    xor dx, dx
    pop cx
    div cx
    mov ax, dx
   
    pop dx
    pop cx
    ; ax - random int 
    ret


parse_args:
    ; parse command line arguments
    mov si, 81h

    args:
        mov bl, [si]
        cmp bl, 0dh
        jz aaaa

        cmp bl, ' '
        jnz rrr
        inc si
        jmp args

        rrr:
        cmp bl, '-'
        jz read_arg
        
        jmp help

        read_arg:
            inc si
            mov bl, [si]

            cmp bl, 0dh
            jz help

            next:
            cmp bl, 'l'
            jnz nextt
            call read_length
            jmp args

            nextt:
            cmp bl, 'L'
            jnz nexttt
            call read_length
            jmp args

            nexttt:
            cmp bl, 'm'
            jnz nextttt
            call read_mode
            jmp args

            nextttt:
            cmp bl, 'M'
            jnz nexttttt
            call read_mode
            jmp args

            nexttttt:
            cmp bl, 'p'
            jnz nextttttt
            call read_poison
            jmp args

            nextttttt:
            cmp bl, 'P'
            jnz help
            call read_poison
            jmp args

    aaaa:
    ret


read_length:
	call skip_spaces
	call read_number

	cmp al, snake_len 
	jg help 

	mov [start_len], al 
	ret


read_mode:
	call skip_spaces
	call read_number

	cmp al, 2
	jg help 

	mov [intersection_mode], al 
	ret


read_poison:
	call skip_spaces
	call read_number

	cmp al, 10
	jg help 

	mov [poison_count], al 
	ret


skip_spaces:
	inc si
	mov bl, [si]
    cmp bl, ' '
    jz skip_spaces

    ret 

help:
    ; print help message and exit
    lea dx, help_message
    mov ah, 9
    int 21h

    mov ah, 4ch
    int 21h


read_number:
	; si - read position 
	push bx
	push cx
	push dx

	mov bl, [si]
	cmp bl, 0dh
	jz help

	mov cl, 10
	xor bx, bx
	xor ax, ax
	xor dx, dx

	handle:
		mov bl, [si]

		cmp bl, ' '
		jz wr

		cmp bl, 0dh
		jz wr

		cmp bl, '0'
		jl help
		cmp bl, '9'
		jg help  

		sub bl, '0'

		mul cl
		jc help
		jo help

		add al, bl
		jc help

		mov dx, 1
		inc si
	jmp handle

	wr:
	cmp dx, 1
	jnz help

	pop dx
	pop cx
	pop bx
	; al - number 
	ret


show_help:
	push ax
	push bx
	push cx
	push di
	push si
	push es 

	; set video mode 3
	mov ax, 3
	int 10h

    ; set segment address
    mov ax, 0b800h
    mov es, ax

    ; write help on page
    mov ch, white_attr

    mov ax, help_1_len
    lea si, help_1
    mov bl, 5
    call print_centered_line
    inc bl

    mov ax, help_2_len
    lea si, help_2
    inc bl
    call print_centered_line

    mov ax, help_3_len
    lea si, help_3
    inc bl
    call print_centered_line

    mov ax, help_4_len
    lea si, help_4
    inc bl
    call print_centered_line

    mov ax, help_5_len
    lea si, help_5
    inc bl
    call print_centered_line

    mov ax, help_6_len
    lea si, help_6
    inc bl
    call print_centered_line

	pop es
	pop si
	pop di 
	pop cx
	pop bx
	pop ax

	ret


show_win:
	push ax
	push bx
	push cx
	push di
	push si
	push es 

	; set video mode 3
	mov ax, 3
	int 10h

    ; set segment address
    mov ax, 0b800h
    mov es, ax

    ; write message
    mov ch, green_attr

    mov ax, win_msg_len
    lea si, win_message
    mov bl, 5
    call print_centered_line
    inc bl
    inc bl 

    call print_stats

	pop es
	pop si
	pop di 
	pop cx
	pop bx
	pop ax

	ret


show_lose:
	push ax
	push bx
	push cx
	push di
	push si
	push es 

	; set video mode 3
	mov ax, 3
	int 10h

    ; set segment address
    mov ax, 0b800h
    mov es, ax

    ; write message
    mov ch, red_attr

    mov ax, lose_msg_len
    lea si, lose_message
    mov bl, 5
    call print_centered_line
    inc bl
    inc bl 

    call print_stats

	pop es
	pop si
	pop di 
	pop cx
	pop bx
	pop ax

	ret


print_stats:
	; es - segment address, bl - row number 
	mov ch, white_attr
    mov ax, stat_1_len
    lea si, stat_1
    inc bl
    call print_centered_line

    mov ax, stat_2_len
    lea si, stat_2
    inc bl
    call print_centered_line

    mov ax, stat_3_len
    lea si, stat_3 
    inc bl
    call print_centered_line

    mov ax, help_5_len
    lea si, help_5
    inc bl
    call print_centered_line

    ret 


hide_help:
	push ax

	; set video mode 4
	mov ax, 4
	int 10h

	call draw_play_field

	pop ax
	ret 


print_centered_line:
	; ax - line length, si - text buffer address, es - video memory segment address
	; ch - attribute, bl - row number 
	push ax
	push bx
	push si
	push di

	; calculate row offset
	push ax
	mov ax, 80
	mul bl
	mov di, ax
	pop ax

	; calculate offset in row
	mov bx, 80
	sub bx, ax
	shr bx, 1
	add di, bx
	shl di, 1

	mov bh, ch
	print_loop:
		mov bl, [si]
		mov [es:di], bx

        inc si
        add di, 2
        dec ax
    jnz print_loop

	pop di
	pop si
	pop bx
	pop ax

	ret


write_number:
	; ax - number, si - write address end 
	push ax
	push bx
	push dx
	push si 

	mov bx, 10

	divnum:
		xor dx, dx
		div bx

		add dx, '0'
		mov [si], dl 
		
		dec si 
		test ax, ax
	jnz divnum

	pop si
	pop dx
	pop bx
	pop ax

	ret 


update_statistics:
	push ax
	push bx
	push si 

	; snake length 
	mov ax, [snake_tail]
	sub ax, offset snake_buffer
	shr ax, 1

	lea si, s2
	sub si, 1
	call write_number

	lea si, stats_field
	add si, 3
	call write_number

	; increment count
	xor ax, ax
	mov al, [increment_count]

	lea si, s3
	sub si, 1
	call write_number

	lea si, stats_field
	add si, 7
	call write_number

	; decrement count
	xor ax, ax
	mov al, [decrement_count]

	lea si, s4
	sub si, 1
	call write_number

	lea si, stats_field
	add si, 11
	call write_number

	pop si
	pop bx
	pop ax 

	ret 


play_melody:
	; si - melody address, di - delays address 
	push ax
	push bx
	push cx

	xor cx, cx

	play_loop:
		mov ax, [si]

		; exit if melody ended
		cmp ax, -1
		jz exx

		mov cl, [di]
		call play_sound_with_delay
		add si, 2
		inc di
	jmp play_loop

	exx:
	call sound_off
	pop cx
	pop bx
	pop ax

	ret


play_sound_with_delay:
	; ax - frequency, cx - delay
	shl ax, 1
	shr cx, 2
	call sound_on

	push es
    xor ax, ax
    mov es, ax

    ; get current time and add wait time 
    mov ax, es:[46ch]
    add ax, cx

	wait_loop:
		hlt
		cmp ax, es:[46ch]
	jnz wait_loop

    pop  es
    ret


sound_on:
	; ax - sound frequency
	push ax
	push bx
	push dx

	mov bx, ax

	;(dx,ax)=1193181
	mov ax, 34DDh
	mov dx, 12h

	; exit if bx < 18 Hz (overflow)
	cmp dx, bx
	jnb done

	;ax=(dx,ax)/bx
	div bx

	; set timer counter (channel 2, mode 3, bits 0-1)
	mov bx, ax
	in al, 61h
	or al, 3
	out 61h, al
	mov al, 00001011b
	mov dx, 43h
	out dx, al
	dec dx
	mov al, bl
	out dx, al
	mov al, bh
	out dx, al

	done:
	pop dx
	pop bx
	pop ax

	ret


sound_off:
	push ax

	in al, 61h
	and al, not 3
	out 61h, al

	pop ax
	ret


; text messages
help_message db "Snake game 1.0", 0dh, 0ah, 0dh, 0ah
	db "snake -h", 0dh, 0ah
	db "snake <-l> <-m> <-p>", 0dh, 0ah
	db "-h - print this help message", 0dh, 0ah
	db "-l - start snake length (min 0, max 15); default=0", 0dh, 0ah
	db "-m - intersection mode (0 - forbidden, 1 - allowed, 2 - cut); default=0", 0dh, 0ah
	db "-p - poison count (min 0, max 10); default=1", 0dh, 0ah, 24h

w1:
win_message db "YOU WON"
w2:
win_msg_len = w2 - w1
l1:
lose_message db "YOU LOST"
l2:
lose_msg_len = l2 - l1

s1:
stat_1 db "Snake lenth: 00"
s2:
stat_2 db "Increment count: 00"
s3:
stat_3 db "Decrement count: 00"
s4:
stat_1_len = s2-s1
stat_2_len = s3-s2
stat_3_len = s4-s3

h1:
help_1 db "How to play Snake 1.0"
h2:
help_2 db "Press ", 1bh, " and ", 1ah, " to change snake's direction"
h3:
help_3 db "Press ", 18h, " and ", 19h, " to change snake's speed "
h4:
help_4 db "Press P to pause"
h5:
help_5 db "Press Esc to exit"
h6:
help_6 db "Press H to show and hide this message"
h7:

help_1_len = h2 - h1
help_2_len = h3 - h2
help_3_len = h4 - h3
help_4_len = h5 - h4
help_5_len = h6 - h5
help_6_len = h7 - h6

white_attr = 07h
red_attr = 8ch
green_attr = 82h

; game parameters
start_len db 0
intersection_mode db 0
poison_count db 1
seed dw 0

; timer info
count db 0
speed db 4
update_flag db 0
pause_flag db 1
help_flag db 1

; game objects 
possible_directions db 'r', 'd', 'l', 'u'

snake_len = 15
snake_buffer db 1, 1, 2 dup(snake_len dup(-1)), 0, 0
snake_tail dw snake_buffer
snake_direction db 0

death_flag db 0
win_flag db 0

increment_count db 0
decrement_count db 0

; graphic info
current_mode db 0

; keyboard buffer
buffer_len = 5
symbol_buffer db buffer_len dup(0)
buffer_end dw symbol_buffer + buffer_len
head dw symbol_buffer
tail dw symbol_buffer

; play field in 8x8 pixel cells

; p - portal wall, d - death wall, b - bouncy wall
; H - snake head, S - snake segment
; P - poison, I - increment, D - decrement
f_width = 32
f_heigth = 25
play_field db f_width dup('p'), f_heigth-2 dup('d', f_width-2 dup(0), 'b'), f_width dup('p')
stats_field db 'S', 0, '0', '0'
	db 'I', 0, '0', '0'
	db 'D', 0, '0', '0'


; sound effects 
increment_sound dw 550, -1
increment_delay db 8

decrement_sound dw 277, -1
decrement_delay db 8

bounce_sound dw 310, 930, -1
bounce_delay db 4, 4

portal_sound dw 40, -1
portal_delay db 8

poison_sound dw 600, 400, -1
poison_delay db 4, 4

win_melody dw 5 dup(553, 697), 586, 658, 658, 697, 493, -1
win_delay db 2 dup(12, 20), 6 dup(8), 4 dup(25), 40

lose_melody dw 1044, 930, 1044, 930, 1044, 930, 1044, 985, 930, 829, 878, 930, 985, 782, 829, -1
lose_delay db 8 dup(12), 25, 3 dup(12), 25, 25, 40

end _start
