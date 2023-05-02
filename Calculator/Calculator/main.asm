; Author: Khant, Myat
; Program Name:	Calculator

INCLUDE Irvine32.inc
.386
.model flat, stdcall
.stack 4096
ExitProcess PROTO, dwExitCode: DWORD

.data
	MAX = 4
	interface BYTE "-----------------",0AH,
				   "| 1 | 2 | 3 | + |",0AH,
				   "-----------------",0AH,
				   "| 4 | 5 | 6 | - |",0AH,
				   "-----------------",0AH,
				   "| 7 | 8 | 9 | * |",0AH,
				   "-----------------",0AH,
				   "|   | 0 | = | / |",0
	keyMsg BYTE "Key Pressed:",0
	donechar BYTE 0
	input1 BYTE MAX DUP ('0'),0
	;input1 BYTE 2 DUP ('0'),'4','5',0
	size1 DWORD 0
	input2 BYTE MAX DUP ('0'),0
	size2 DWORD 0
	isOperator DWORD 0
	oper BYTE -1
	flag DWORD 0
	isAns DWORD 0
	storage BYTE '0'
	result1 BYTE MAX DUP ('0'),0
	result2 BYTE MAX DUP ('0'),0
	sizeRes DWORD 0
	resIndex DWORD 0
	isError DWORD 0
	errorMsg BYTE "ERROR!",0
	counter DWORD 0
	shifter DWORD 0
	keyPressed BYTE '0'
	
.code
PrintInterface PROC uses edx eax
	mov dh,1
	mov dl, 0
	call Gotoxy				;go to row 2
	mov edx, OFFSET interface
	call WriteString		;print the interface
	ret
PrintInterface ENDP
PrintInput PROC uses edx eax
	mov dl,12								;go to col 12
	mov dh, 0
	call Gotoxy
	.IF isError ==1							;if error, print error message
		mov edx, OFFSET errorMsg
	.ELSEIF isOperator == 0 || flag == 1	;if there is no operator yet, print array 1
		mov edx, OFFSET input1
	.ELSE									;if there is operator already or haven't pressed enter yet,
		mov edx, OFFSET input2				;print array 2
	.ENDIF
	call WriteString
	mov eax, green + (black*16)
	call SetTextColor
	mov dh, 10
	mov dl, 0
	call Gotoxy
	mov edx, OFFSET keyMsg
	call WriteString
	mov al, keyPressed
	call WriteChar
	mov eax, white + (black*16)
	call SetTextColor
	ret
PrintInput ENDP
Game PROC 
	mov ecx, 0
	call PrintInterface
L1:
	call GetInput							;get user input
	call PrintInput							;print the appropriate user input
	loop L1
	ret
Game ENDP
GetInput PROC
	mov eax, 25	
	call delay
	call ReadKey							;read key pressed
	jz DONE
	mov keyPressed, al
	mov flag, 0								;reset flag
	.IF al==8								;backspace
		call BackspaceInput
	.ELSEIF al==27							;escape
		mov esi, OFFSET input1
		call ClearInput						;clear input1
		mov esi, OFFSET input2
		call ClearInput						;clear input2
		call ResetFlags						;reset all the flags
		call clrscr
		call PrintInterface
	.ELSEIF al>='0' && al<='9'				;digits
		call SaveInput						;save the digits
	.ELSEIF al == '+'
		call MarkOperator
	.ELSEIF al == '-'
		call MarkOperator
	.ELSEIF al == '*'
		call MarkOperator
	.ELSEIF al == '/'
		call MarkOperator
	.ELSEIF al == 13 || al== '='
		call MarkOperator

	.ENDIF
DONE:
	ret
GetInput ENDP
BackspaceInput PROC uses esi ecx
	.IF isOperator == 0 && size1 !=0			;check size before deleting
		mov esi, OFFSET input1
		mov ecx, MAX
		call shift_right						;shift right one char and replace with '0'
		dec size1	
	.ELSEIF isOperator == 1 && size2 !=0		;check size before deleting
		mov esi, OFFSET input2
		mov ecx, MAX
		call shift_right						;shift right one char and replace with '0'
		dec size2
	.ENDIF
	ret
BackspaceInput ENDP
ClearInput PROC uses esi ecx
;esi has the offset of array
	mov ecx, MAX
L1:
	mov BYTE PTR [esi], '0'						;delete all the index
	inc esi
	loop L1
	ret
ClearInput ENDP

shift_right PROC uses edi eax ecx 
;esi has the offset of the array
;ecx has the size of array

	push esi
	dec ecx
	add esi, ecx
	mov edi, esi
	dec edi
	
	L1:
		mov al, [edi]							;shifting char by char
		mov [esi], al
		dec esi
		dec edi
	loop L1
	pop esi

	mov BYTE PTR [esi], '0'						;replace empty index with '0'
	ret
shift_right	ENDP

shift_left PROC uses edi ecx eax
;esi has the offset of the array
;ecx has the size of array
mov edi, esi
inc edi
.IF ecx !=0
	L1:
		mov al, [edi]							;shifting one char by char
		mov [esi], al
		inc edi
		inc esi
	loop L1
.ENDIF
	ret
shift_left ENDP

SaveInput PROC uses esi ecx

	.IF isOperator == 1 && size2 < MAX  && isAns == 0		;if there an operator pressed and enter hasn't pressed
		mov esi, OFFSET input2								;store in input2
		mov ecx, MAX

		call shift_left										;shift one char to the left
		mov [input2+ MAX -1], al							;save new char in last index
		.IF size2 != 0 || al !='0'							;if the index is 0 and keypressed is '0'
			inc size2										;then skip
		.ENDIF
	.ELSEIF isOperator == 0 && size1 < MAX  && isAns == 0	;if there an operator and enter haven't pressed
		mov esi, OFFSET input1								;store in input1
		mov ecx, MAX

		call shift_left										;shift one char to the left	
		mov [input1+ MAX -1], al							;save new char in last index
		.IF size1 != 0 || al !='0'							;if the index is 0 and keypressed is '0'
			inc size1										;then skip
		.ENDIF
	.ENDIF

	ret
SaveInput ENDP
AddInput PROC uses ecx eax ebx
	mov ecx, MAX					;loop counter
	mov ebx, 3
	mov eax, 0
	mov size1, 0
	clc								;clear flags
L1:
	mov al, [input1+ebx]			;get value of input1 from the end
	adc al, [input2+ebx]			;add with value of input2 from the end
	aaa								;change to bcd
	pushfd
	
	or al, 30h						;change to ascii
	mov [input1+ebx], al			;store the result at the end of input1
	popfd
	dec ebx
loop L1
	mov esi, OFFSET input1
	call SetSize					;set new size for input1
	mov size1, eax
	jnc END_L1
	mov isError,1					;if carry flag is still on, set error
END_L1:
	ret
AddInput ENDP
SubInput PROC uses ecx eax ebx edx
	mov ecx, MAX					;loop counter
	mov ebx, 3
	mov eax, 0
	mov size1, 0
	mov edx, 0
	clc								;clear flags
L1:
	mov al, [input1+ebx]			;get value of input1 from the end
	sbb al, [input2+ebx]			;substract with value of input2 from the end
	aas								;change to bcd
	pushfd
	
	or al, 30h						;change to ascii char
	mov [input1+ebx], al			;store the result at the end of input1
	popfd
	inc edx
	dec ebx
loop L1
	mov esi, OFFSET input1
	call SetSize					;set new size of input1
	mov size1, eax
	jnc END_L1
	mov isError,1					;if carry flag is still on, set error
END_L1:
	ret
SubInput ENDP

MulTerm PROC uses eax ebx ecx edx esi
;dl has the term
;input1 is the array multiplying
;result stored in [esi]
	mov esi, OFFSET result1			;offset of temp array to store result
	call ClearInput					;clear that temp array first
	mov ebx, MAX-1
	mov ecx, MAX					;loop counter
	and dl, 0Fh						;change ascii char to bcd
	add esi, ebx
	mov storage, 0
L1:
	mov al, [input1+ebx]			;get the value of input1 from the end
	pushfd
	and al, 0Fh						;change ascii char to bcd
	popfd
	mul dl							;multiply with multiplier
	pushfd
	aam								;change to bcd
	or ax, 3030h					;change to ascii char
	popfd
	push eax
	mov ah, 0
	adc al, storage					;add the result with storage
	aaa								;change result to ascii char
	pushfd
	or al, 30h						;change to bcd
	mov BYTE PTR [esi], al			;store at the temp array
	popfd
	
	pop eax
	mov storage, ah					;store the carry part
	jnc NO_CARRY					;if not carry again, skip
	adc storage, 0					;if carry, add storage with carry again	
	mov al, storage
	aaa								;change to bcd
	mov storage, al
	or storage,30h					;store back to storage as ascii char

NO_CARRY:
	dec esi
	dec ebx
loop L1
	.IF storage!= '0'				;if still carry, after the loop
		mov isError,1				;set error
	.ENDIF
	call AddResults					;add the temp result to total result
	ret
MulTerm ENDP
MulInput PROC uses ecx ebx eax edx esi edi
	mov resIndex, 0					;reset current index
	mov ecx, MAX					;loop counter
	mov ebx, MAX-1
	mov edx, 0
	mov eax, 0
	mov size1, 0
	mov esi, OFFSET result2
	call ClearInput					;clear total result
	clc								;clear flags
L1:
	mov dl, [input2+ebx]			;save the value of input2 from the end
	call MulTerm					;mul that term with input1
	dec ebx
loop L1
	mov esi, OFFSET input1			
	mov edi, OFFSET result2
	call CopyArr					;copy total result into input1
	mov ebx, sizeRes				;copy the result size to size of input1
	mov size1, ebx

	ret
MulInput ENDP
DivInput PROC uses esi edi eax ecx ebx edx
	mov esi, OFFSET input1
	mov edi, OFFSET result1
	call ChangeUnpacked
	call ChangeHex
	push eax
	mov esi, OFFSET input2
	mov edi, OFFSET result2
	call ChangeUnpacked
	call ChangeHex
	mov ebx, eax
	pop eax
	mov dx, 0
	.IF bx ==0
		mov isError, 1
	.ELSE
		div bx
	.ENDIF
	call DecToAscii
	mov eax, ecx
	or eax, 30303030h
	mov esi, OFFSET input1
	call ClearInput
	mov ecx, MAX
	mov ebx, MAX - 1
L1:
	mov [input1+ebx], al
	shr eax, 8
	dec ebx
	loop L1
	call SetSize
	mov size1, eax
	ret
DivInput ENDP
DecToAscii PROC
;ax has the decimal value uses ebx edx eax
;result in ecx
mov counter, 0
mov ecx, 0
L1:
	mov edx, 0
	mov bx, 10
	div bx
	or dl, 30h
	.IF counter!= 4
		mov edi, ecx
		mov ecx, 0
		mov cl, dl
		.IF counter !=0
			push eax
			push ebx
			mov eax, counter
			mov bl, 8
			mul bl
			mov ebx, ecx
			mov ecx, eax
			shl ebx, cl
			mov ecx, ebx
			pop ebx
			pop eax
		.ENDIF

		add ecx, edi	
		inc counter
		jmp L1
	.ENDIF
	ret
DecToAscii ENDP
ChangeUnpacked PROC uses esi edi ecx ebx eax
;esi has the offset of the array we want to change
;edi has the offset of the result array
;result store the unpack of array in big endian in edi
	mov ecx, MAX				;loop counter
	mov ebx, MAX-1					;index
L1:
	mov al, [esi+ebx]		;get char from the end
	and al, 0Fh
	mov BYTE PTR [edi], al	;change into big endian in result1
	dec ebx
	inc edi
	loop L1

	ret
ChangeUnpacked ENDP

ChangeHex PROC uses ebx edx edi
;edi has the offset of bcd in big endian
;result of hex in eax
	mov eax, 0			
	mov ax,  [edi+2]			;store first two digits
	aad							;change to hex
	mov bx, 100					;multiply with 100
	mov dx, 0
	mul bx
	mov edx, eax				;store in edx
	
	mov ax,  [edi]				;get last two digits
	aad
	add eax, edx				;add two results
	ret
ChangeHex ENDP
CopyArr PROC uses esi edi eax
;esi is the offset of destination
;edi is the offset of source
	mov ecx, MAX					;loop counter
L1:
	mov al, [edi]					;copy char by char
	mov [esi],al
	inc edi
	inc esi
	loop L1
	ret
CopyArr ENDP
MarkOperator PROC 
	.IF isOperator == 0 && al != '=' && al != 13			; if operator and enter are not pressed yet
		mov isOperator, 1									; set operator flag
		mov oper, al										; save the operator
		mov isAns, 0										; set answer hasn't shown yet
	.ELSEIF isAns == 0										; if answer hasn't shown yet
		call Result											; get result
		.IF al == '=' || al == 13							; if enter has pressed
			mov oper, -1									; set operator to default
			mov isOperator, 0								; reset operator flag
			mov isAns, 1									; set answer flag

		.ELSE												; if enter hasn't pressed yet
			mov oper, al									; set the new opertor
			mov flag, 1										; set the flag to take next input as input2
		.ENDIF
	.ENDIF
		
	ret
MarkOperator ENDP

Result PROC uses esi
	.IF oper == '+'
		call AddInput
	.ELSEIF oper == '-'
		call SubInput
	.ELSEIF oper == '*'
		call MulInput
	.ELSEIF oper == '/'
		call DivInput
	.ENDIF
	mov esi, OFFSET input2
	mov size2, 0							;reset size of input2
	call ClearInput							;clear input32
	ret
Result ENDP

ResetFlags PROC
;reset all the flags
	mov isOperator, 0
	mov oper, -1
	mov flag, 0
	mov size1, 0
	mov size2, 0
	mov isAns, 0
	mov isError, 0

	ret
ResetFlags ENDP

AddResults PROC uses ecx eax ebx edx esi
;add result1 to result 2
	mov ecx, MAX					;loop counter
	mov ebx, MAX-1
	mov eax, 0
	mov edx, 0
	mov sizeRes,0
	sub ecx, resIndex 
	clc								;clear flags
L1:
	push ebx
	sub ebx, resIndex			
	mov al, [result2+ebx]			;get the appropriate index of total result
	pop ebx
	adc al, [result1+ebx]			;add with the value of temp result from the end
	aaa								;change to bcd
	pushfd
	
	or al, 30h						;change to ascii char
	push ebx
	sub ebx, resIndex
	mov [result2+ebx], al			;change appropriate index of total result
	pop ebx
	popfd
	inc edx
	dec ebx
loop L1
	mov esi, OFFSET result2
	call SetSize					;set size of total result
	mov sizeRes, eax	
	.IF resIndex !=0 && [result1] != '0'	;if carry, set error 
		mov isError, 1
	.ENDIF
	inc resIndex					;increment the current index of total result
	ret
AddResults ENDP
SetSize PROC uses ecx ebx
;esi has the offset of array
;size return in eax
	pushfd
	mov ecx, MAX					;loop counter
	mov ebx, 0
L1:
	cmp BYTE PTR [esi+ebx],'0'		;check for first non zero index
	jnz END_L1						;if char in index is not zero, jump 
	inc ebx							;else loop again until last index
	loop L1	
END_L1:
	mov eax, MAX					
	sub eax, ebx					;get the size 
	popfd
	ret
SetSize ENDP
main PROC
	
	; write your assembly code here
	call Game

	INVOKE ExitProcess, 0
main ENDP
END main
