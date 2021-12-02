; multi-segment executable file template.

data segment
    ; add your data here!
    Welcome      db "Welcome to Ari's Project : $"
    begin        db 10,13,10,13, 'Input one from the following letters: h or d or b (h - HexDecimal number, d - Decimal number, b - Binary number) $'
    inpnumh      db 10,13,10,13, 'Input a hexadecimal number (maximum 4 digits) $' 
    primnum      db 10,13,10,13, 'This number is a prime number$'
    inpnumd      db 10,13,10,13, 'Input a Decimal number (maximum 4 digits) $'
    inpnumb      db 10,13,10,13, 'Input a Binary number (maximum 16 Bit) $'
    dividersmsg  db 10,13,10,13, 'Its dividers are: $'
    multiple     db 10,13,10,13, 'List factors (a multiple of) the smallest: $'
    primenumbers db 10,13,10,13, 'List of prime numbers up to your number (not including the number): $'
    bindis       db 10,13,10,13, 'Binary display: $'
    hexdis       db 10,13,10,13, 'HexDecimal display: $'
    decdis       db 10,13,10,13, 'Decimal display: $'
    tostart      db 10,13,10,13,'Would you like to check another number? (y/n) $'
    Number dw ?               
    pkey         db 10,13,10,13,"press any key...$"
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax 
    
    lea dx, Welcome         ;print the site welcome
    call memosite
beg:    
    lea dx, begin           ;ptint the site begin
    call memosite
    
    mov cx, 3               ;push to stack 3 letters for the procedure
    mov ax, 'h'
    push ax                                                           
    mov ax, 'd'
    push ax
    mov ax, 'b'
    push ax
    call illinp
   
    cmp al, 'h'             ; check what the input h/d/b
    je hexa
    cmp al, 'd'
    je deci
   
    lea dx, inpnumb         ; if the input is b print the site inpnumb
    call memosite           
    call rb                 ; the procedure read binary number and she return by register                                           
                            ; ax the number  
    jmp bin
hexa:
    lea dx, inpnumh          ; if the input is h print the site inpnumh
    call memosite
    call reahex              ; the procedure read hexadecimal number and she 
    jmp bin                   ; return by register ax the number
deci: 
    lea dx, inpnumd          ; if the input is d print the site inpnumd     
    call memosite            ; the procedure read decimal number and she
    call red                 ; return by register ax the number
                                       
bin: 
    mov Number, ax                        
   
    lea  dx, hexdis          ; print the title for 3 display and print the number 
    call memosite            ; in 3 display 
    call prih
    
    mov ax, Number
    
    lea  dx, decdis
    call memosite
    call Typeing
    
    mov ax, Number
    
    lea  dx, bindis
    call memosite
    call prib
   
                            ; print the site multiple and call to procedure that 
    mov ax, Number          ; print the List factors (a multiple of) the smallest
    
    lea dx, multiple 
    call linemul
                            ; if the number is 1 so jmp to "if1or0" because the number 1 is 
    cmp Number, 1           ; not prime number but dosen't have dividers
    je if1or0
    
    cmp Number, 0           ; if the number is 0 jmp to "if1or0"
    je if1or0                                             
   
    cmp ax, 0               ; if in register ax have 0 so the number is prime number   
    jne notp
                            ; print the site primnum for the prime number 
    lea dx, primnum 
    call memosite
    jmp primenum
notp:                          ; call to procedure that print divider's of the number 
    mov ax, Number
    lea dx, dividersmsg
    call ld

primenum:                        ; call to procedure that print 
    mov ax, Number         ; List of prime numbers up to your number (not including the number)
    lea dx, primenumbers
    call lpn
if1or0:
    lea dx,tostart         ; print the site tostart
    call memosite
   
    mov cx,2               ; push to stack 2 letters for to restart  
    mov ax, 'y'
    push ax
    mov ax, 'n'
    push ax
    call illinp
    cmp al, 'y'
    je beg
    
    lea dx, pkey
    call memosite             ; output string at ds:dx
    
    ; wait for any key....    
    mov ah, 1
    int 21h
    
    mov ax, 4c00h ; exit to operating system.
    int 21h   
  
   ;===================================================
   ; The procedure gets by DX register the address of site 
   ; The procedure print this site
   ;===================================================
   
   memosite PROC NEAR
   
   push ax           ;   store register
        
   mov ah, 9
   int 21h
    
   pop ax        ;   restore register
    
   ret
   memosite ENDP
  
  



    ;===================================================
    ;The procedure gets by CX  register the number of parameters
    ;The procedure gets by stack list of the parameters it
    ;accepts ASCII codes of their
    ;The procedure can only read the list of characters
    ;equal parameters that the stack
    ;The procedure returns by register AX the ASCII code of
    ;the character received  
    ;===================================================
 
   illinp PROC NEAR
   
    mov bp, sp        
                      ; store registers
    push    bx
    push    dx 
    
    mov bx, cx        
while1:
    
    mov cx, bx      ; save in register bx sum of the pramters because the rgesister cx 
    mov ah, 7       ; is diffrent in loop
    int 21h         ; only read 
l1:
    add bp, 2       ; begin is  add 2 to register bp because that begin in stack 
    cmp al, [bp]    ; have a return address and after have the pramters
    je next4        ; if the read character's is one of in pramter in stack so 
    loop l1         ; is break in the loop else search one more time until the end of the loop
    mov cx, bx
l2:  
    sub bp, 2       ; if input not in line of pramters so return the register bp to start 
    loop l2         ; (to adress of line pramters)
    jmp while1
   
next4:    
     mov ah,2       ; if the input includ in line pramters so print the character 
     mov dl, al
     int 21h
     mov cx, bx     ; return value of regsiter cx(line pramters)
   
     pop     dx     ;   restore registers
     pop     bx
    
     pop bp         ; pop to register bp the addres return   
    
     shl cx, 1      ; mul cx in 2
     add sp, cx     ; do the action like ret (ret line pramters)
  
     push bp        ; push to staack the address return 
   
     ret 
    illinp ENDP
     
     
    ;===================================================
    ;   The procedure no gets parameters 
    ;   The procedure reads hexdecimal number
    ;   The procedure returns the number by the  ax register 
    ;===================================================
 
   reahex PROC NEAR    
      
    push    bx      ;   store registers
    push    cx
    push    dx
    push    di
    push    si
    
    xor di, di
    mov si, 4
again:    
    mov bx, '0'
    mov cx, 10
lop:
    push bx     ;push in stack 10 digits(0-9) for procedure iilinp 
    inc bx
    loop lop 
    
    mov bx, 'A' ;push in stack 6 letters (A-F) for procedure iilinp
    mov cx, 6
lop1:
    push bx
    inc bx 
    loop lop1
    push 13   
    mov cx, 17
    call illinp
  
    cmp al, 13        ; check if input 13(enter) - finish input
    je next3    
    
    cmp al, 'A'
    jl next
    sub al, 55
    jmp next1

next: 
    sub al, '0'                                             
next1:                                                 
    shl di, 4           ; in haxdecimal each digit is 4 bit therefore mov 4 times
    xor ah, ah
    add di, ax
    dec si              ; because that i use with cx register i do action like loop 
    cmp si, 0
    je next3
    jmp again         


next3:   
    mov ax, di
                                                             
    pop     si
    pop     di
    pop     dx      ;   restore registers
    pop     cx
    pop     bx
  
    ret 
   reahex ENDP
     
     
    ;===================================================
    ;   The procedure no gets parameters 
    ;   The procedure reads decimal number
    ;   The procedure returns the number by the ax register
    ;===================================================
   
   red PROC NEAR
 
    push bx      ;   store registers
    push cx
    push dx 
    push di
    push si   
  
  
    xor si,si   
    mov cx, 4
lopin:                                                                   
    mov di, cx       ; save in cx register in di register because is diffrent in loop 
    mov cx, 10
    mov bx, '0'

lopin1:   
    push bx         ;push in stack 10 digits(0-9) for procedure iilinp 
    inc bx
    loop lopin1
    
    push 13
    mov cx, 11
    call illinp
   
    mov ah, 0
    mov cx, di
    mov bx, 10
    cmp ax, 13      ; check if input 13(enter) - finish input
    je fin
    
    sub ax, '0'     ; si - sum number until now
    mov di, ax      ; ax - the current digit
    mov ax, si      ; di - temp for ax 
    mul bx
    xor si, si
    add si, di      ; add current digit to sum  
    add si, ax      ; add sum number until now(*10)        
    loop lopin

fin:    
    mov ax, si 
    
    pop si
    pop di
    pop dx       ;   restore registers
    pop cx
    pop bx 
    
    ret  
     
   red ENDP     
    ;===================================================
    ;   The procedure no gets parameters 
    ;   The procedure reads binary number
    ;   The procedure returns the number by the ax register
    ;===================================================
  
  
   rb PROC NEAR
    
    push bx      ;   store registers
    push cx
    push dx
  
    xor bx, bx
    mov dx, 17
agin:   
    push 13            ;push in stack 2 digits(1,2) for procedure iilinp
    push 30h
    push 31h
    mov cx, 3
    call illinp
  
    mov ah, 0          ; check if input 13(enter) - finish input
    cmp ax, 13
    je finish    
    shl bx, 1
    sub ax, '0'
    add bx, ax
   
    dec dx             ; because that i use with cx register i do action like loop
    mov cx, dx
    loop agin 
finish:    
    mov ax, bx 
     
    pop dx       ;   restore registers
    pop cx
    pop bx 
   
    ret
   rb ENDP
     
       ;=====================================================
       ; The procedure is received by AX register number 
       ; And the procedure prints the number in hexadecimal format
       ;=====================================================
 
   prih PROC NEAR
    
    push bx      ;   store registers
    push cx
    push dx
    push si
    
    mov cx, 4
    mov si, ax

again3:    
    mov ax, si
                              
    mov bl, 4  
    cmp bl, cl
    je first
    
    mov bl, 3
    cmp bl, cl
    je sec
   
    mov bl, 2
    cmp bl, cl
    je tre
    
    jmp after   ; the last digit
   
tre:
    shr ax, 4   ; the third digit 
    jmp after
sec:
    shr ax, 8   ; the second digit 
    jmp after
first:
    shr ax, 12  ; the first digit 
   
after:
    and al, 0fh  
    mov bl, 9
    cmp al, bl
    jg next32
    mov dl, al
    add dl, '0' ; if the characters is digit
    jmp next14
next32: 
    mov dl, al  ; if the characters is leeter
    add dl, 55  
next14:
    mov ah, 2
    int 21h
    
   loop again3
    
    pop si
    pop dx       ;   restore registers
    pop cx
    pop bx 
   
    ret
   prih ENDP
       
       ;================================================
       ; The procedure is received by AX register number 
       ; And the procedure prints the number in decimal format
       ;================================================  
   
       
Typeing PROC NEAR
  
    push bx      ;   store registers
    push cx
    push dx   
  
    xor dx, dx       
    xor cx, cx 
    mov bx, 10  

NextDig:
        div bx
        push dx        ; push in stakc the remainder(of divider)
        inc cx         ; cx - number of digits  
        cmp ax, 0      
        je   Typing
        xor dx, dx
        jmp  NextDig
        
Typing:
        mov  ah, 2
Again1:
        pop dx
        add dl,  '0'
        int 21h
        loop Again1                      
                                     
    pop dx          ;   restore registers
    pop cx
    pop bx
       
       ret
        
 Typeing ENDP
 
       ;===================================================
       ; The procedure is received by register AX number 
       ; And the procedure prints the number in binary format
       ;===================================================  
   
   
   prib PROC NEAR
   
    push bx      ;   store registers
    push cx
    push dx
    push di
    push si 
    
    
    mov si, ax
    mov di, ax
    
    mov ah, 2
    cmp ax, 256
    jl small
    
    mov bx, 1000000000000000b
    mov cx, 16
agi:    
    and si, bx
    cmp si, 0     ; check if the 16 bit is 0 
    je if0
    mov ah, 2
    mov dl, '1'
    int 21h
    jmp next45
if0:    
    mov dl, '0'
    int 21h
next45:   
    mov si, di                     
    shr bx, 1    ; each time check other bit(15,14...)
    loop agi 
    jmp final
   
small:
    mov bx, 10000000b
    mov cx, 8
aga:    
    and si, bx
    cmp si, 0    ; check if the 8 bit is 0
    je i0
    mov dl, '1'
    int 21h
    jmp next53
i0:    
    mov dl, '0'
    int 21h
next53:   
    mov si, di
    shr bx, 1   ; each time check other bit(7,6...)
    loop aga 
final:    
    
    pop si
    pop di
    pop dx       ;   restore registers
    pop cx
    pop bx 
   
   ret
   prib ENDP
    
    
    ;===================================================
    ;The procedure gets by AX register number and by dx register addres of site multiple
    ;The procedure list factors (a multiple of) the smallest 
    ;and if this prime number the procedure return by ax value 0
    ;===================================================
    
   linemul PROC NEAR
    
    push    bx      ; store registers
    push    cx
    push    di
    push    si
   
    cmp ax, 0       ; because bx begin from 2 so 0,1,2 need to do in hand
    je fina1
    
    cmp ax, 1
    je fina1
    
    cmp ax, 2
    je fina2
   
    mov bp, dx      ;save in bp addres of site multiple 
    xor di, di
    mov si, ax
    shr si, 1       ; divider si in 2 
    
    mov bx, 2
nagine:
    xor dx,dx

    div bx
    cmp dx, 0 
    jne nextnum     ; if the number is not divider without remainder(of divider) try next number
                    ; di - counter of multiple number 
    inc di
    mov cx, ax      ; save in cx value ax
    mov ax, bx      ; for procdure Typeing
   
    cmp di, 1       ; if this number first so print the site multiple
    jne during 
    mov dx, bp
    call memosite 
during:
    cmp di, 2       ; just if this number is second number so print "x"
    jl during2
    
    call printx 
during2:
    call Typeing    ; print the number

    mov ax, cx      ; return to ax last value 
    jmp nagine        
nextnum:     
    mov cx, dx      ; save the remainder(of divider) in cx             

    mul bx          ; return to first number - mul bx in ax and add the remainder(of divider)
    mov dx, cx
    add ax, dx
    inc bx          
    
    cmp di, 1       ; because if the number is not prime number no need to cheak bx,si
    jge nex
    
    cmp bx, si      ; for prime number greater for the number 5
    jge fina2
nex:    
    cmp ax,1        ; for not prime number
    je fina1
    jmp nagine
fina2:
    mov ax, 0
fina1:      
    pop     si
    pop     di
    pop     cx    ;   restore registers
    pop     bx
  
    ret           
   linemul ENDP
  
   ;===================================================  
   ; The procedure no gets parameters
   ; The procedure print " " "x" " "
   ;===================================================
    
   printx PROC NEAR
   
   push ax           ;   store registers
   push dx      
   
   mov ah, 2
  
   mov dl, ' '
   int 21h
   
   mov dl, 'x'
   int 21h
   
   mov dl, ' '
   int 21h
     
   pop dx       ;   restore registers
   pop ax
   
   ret
   printx ENDP
  
      ;===================================================
      ; The procedure is received by register AX number and by 
      ; register DX addres of site dividersmsg
      ; The procedure print divider's of the number 
      ;===================================================
 
    ld PROC NEAR
    
    push bx      ;   store registers
    push cx
    push di
    push si
                                             
    mov si, ax
    mov bp, dx    ;save in bp addres of site dividersmsg
    xor di, di
    mov bx, ax
    shr bx, 1     ; the divider bigest is not more half in number(exm. 32 - 16 {17.. is not div})
likeloop:    
    xor dx, dx
    cmp bx, 1     
    jle finali        ; if bx 1 finish divider
    
    div bx
    mov cx, ax    ; save the divider in cx
    cmp dx, 0     ; if is div without remainder(of divider) so print the number
    jne checkag
   
    cmp di, 0     ; if is first divider so print the site dividersmsg 
    jne contin
    mov dx, bp
    call memosite
    jmp aftrethis
contin:
    mov ah, 2     ;print ", "
    mov dl, ','
    int 21h 
    
    mov dl, ' '
    int 21h 
aftrethis:        ; inc di - count of divider  
    inc di        
    mov ax, cx    
    call Typeing  ; print the divider   
checkag:    
    mov ax, si
    dec bx        ; next number(until ...1)
    jmp likeloop
finali:   
    pop si
    pop di
    pop cx      ;   restore registers
    pop bx        
     
   
    ret
   ld ENDP
 
  ;===================================================
  ;The procedure is received by register AX number and by register
  ;DX addres of site primenumbers
  ;The procedure print list of prime numbers up to your number (not including the number) 
  ;===================================================
  
  
   lpn PROC NEAR
   
    push bx      ;   store registers
    push cx
    push di
    
    cmp ax, 0 
    je  ifn0  
   
    mov bp, dx    ;  save in bp addres of site primenumbers 
    xor di, di
    mov bx, 1    
    mov cx, ax
    dec cx       ; not including the number (exm. 9 until 8) 
l5:    
    mov ax, bx
    call ispr  
    cmp ax, 0
    jne cother
    cmp di, 1    ; if this first numbrt print site primenumbers 
    jl firstnum
   
    mov ah, 2    ;if this not first number so print ", "
    mov dl, ','
    int 21h 
    
    mov dl, ' '
    int 21h 
    jmp afterr
firstnum:
    mov dx, bp 
    call memosite
afterr:
    mov ax, bx    ; print prime number
    call Typeing
    inc di        ; inc di - count of prime numbers
cother:   
    inc bx        ;  next number
    loop l5
ifn0: 
    pop di
    pop cx      ;   restore registers
    pop bx  
   
    ret
   lpn ENDP
 
   ;===================================================
   ; The procedure is received by register AX number 
   ; The procedure chakeds if this number is prime number or not
   ; if the number is prime number the procedure return by AX register value 0
   ;===================================================
  
   
   ispr PROC NEAR
   
    push bx      ;   store registers
    push cx
    push di
     
    cmp ax, 1   ; 1 is not prime number
    jne n3
    jmp fini
n3:  
    cmp ax, 2    ; 2 is prime number
    je prime
    
    mov di, ax
    mov cx, ax
    shr cx, 1   ; divider in 2 
    mov bx, 2
   
lo5:    
     mov ax, di
     xor dx, dx
     div bx      ; check if the number divider in 2
     cmp dx, 0
     je fini     ; if is divider is nor prime number and finish 
     inc bx      ; check if the number divider in:  (3,4... until the number / 2)
     loop lo5
prime:
     mov ax, 0   ; if the loop is finish so the number is prime number
fini:

    pop di
    pop cx      ;   restore registers
    pop bx  
   
    ret
   ispr ENDP  
 
  ; add your code here
                          
ends

end start ; set entry point and stop the assembler.
