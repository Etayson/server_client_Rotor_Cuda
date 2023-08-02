;=========================================================================
; Module:          RMD160.pbi
;
; A PureBasic implementation of the RIPEMD-160 hash function.
; The hash function was created by H. Dobbertin, A. Bosselaers, B. Preneel
; This PureBasic implementation is based on code from Aleksey Kravchenko
; https://github.com/rhash/RHash/blob/master/librhash/ripemd-160.c
;
; Author:          Wilbert
; Date:            Jul 2, 2020
; Version:         1.0.0
; Target Compiler: PureBasic 5.20+
; Target OS:       All
; License:         Free, unrestricted, no warranty whatsoever
;                  Use at your own risk
;=========================================================================

DeclareModule RMD160
 
  ;-[Declare] Public structures
 
  Structure RMD160Context
    state.l[5]    ; offset 0
    buffer.a[64]  ; offset 40
    msglen.q      ; offset 104
  EndStructure
 
  ;-[Declare] Public procedures
 
  Declare   Init(*ctx.RMD160Context)
  Declare   Update(*ctx.RMD160Context, *bytes, size)
  Declare   Final(*ctx.RMD160Context)
  Declare.s Hash(*ctx.RMD160Context)
 
  Declare.s AsciiHash(AsciiString.s)   
  Declare.s MemoryHash(*Buffer, Size)   
  Declare.s FileHash(Filename$, Offset.q=0, Length.q=0)
 
EndDeclareModule

Module RMD160
 
  DisableDebugger ; required !!!
  EnableExplicit
  EnableASM
 
  ;-Private macros
 
  CompilerIf #PB_Compiler_Processor = #PB_Processor_x86
    Macro rax : eax : EndMacro
    Macro rdx : edx : EndMacro
    Macro rbx : ebx : EndMacro
    Macro rsp : esp : EndMacro
    Macro rbp : ebp : EndMacro
    Macro rsi : esi : EndMacro   
    Macro rdi : edi : EndMacro
  CompilerEndIf

  Macro M_fn (fn, r0, r1, r2, r3, r4, idx, rot)
    CompilerSelect fn & $f
      CompilerCase 1 ; r1 ! r2 ! r3
        !mov eax, r1
        !xor eax, r2
        !xor eax, r3
      CompilerCase 2 ; ((r2 ! r3) & r1) ! r3
        !mov eax, r2
        !xor eax, r3
        !and eax, r1
        !xor eax, r3
        CompilerIf fn & $10
          !add r0, 0x7a6d76e9 
        CompilerElse
          !add r0, 0x5a827999
        CompilerEndIf
      CompilerCase 3 ; (~r2 | r1) ! r3
        !mov eax, r2
        !not eax
        !or eax, r1
        !xor eax, r3
        CompilerIf fn & $10
          !add r0, 0x6d703ef3
        CompilerElse
          !add r0, 0x6ed9eba1
        CompilerEndIf     
      CompilerCase 4 ; ((r1 ! r2) & r3) ! r2
        !mov eax, r1
        !xor eax, r2
        !and eax, r3
        !xor eax, r2
        CompilerIf fn & $10
          !add r0, 0x5c4dd124
        CompilerElse
          !add r0, 0x8f1bbcdc
        CompilerEndIf     
      CompilerCase 5 ; (~r3 | r2) ! r1
        !mov eax, r3
        !not eax
        !or eax, r2
        !xor eax, r1
        CompilerIf fn & $10
          !add r0, 0x50a28be6
        CompilerElse
          !add r0, 0xa953fd4e
        CompilerEndIf     
    CompilerEndSelect
    add r0, [rbp + idx*4]
    !add r0, eax
    !rol r0, rot
    !add r0, r4
    !rol r2, 10
  EndMacro
 
  ;-Private procedures
 
  Procedure ProcessBlock(*state, *block)
   
    ; process 1 block of 64 bytes
   
    ; get procedure arguments
    mov rax, [p.p_state]
    mov rdx, [p.p_block]
   
    ; backup
    sub rsp, 64           ; reserve 64 bytes of stack space
    mov [rsp + 24], rax   ; backup of *state argument
    mov [rsp + 32], rbx   ; backup of rbx register
    mov [rsp + 40], rbp   ; backup of rbp register
    mov [rsp + 48], rsi   ; backup of rsi register
    mov [rsp + 56], rdi   ; backup of rdi register
    mov rbp, rdx          ; rbp -> *msg
   
    ; load state
    mov ebx, [rax     ]
    mov ecx, [rax +  4]
    mov edx, [rax +  8]
    mov esi, [rax + 12]
    mov edi, [rax + 16]
   
    ; left half rounds
    M_fn ($01, ebx, ecx, edx, esi, edi,  0, 11)
    M_fn ($01, edi, ebx, ecx, edx, esi,  1, 14)
    M_fn ($01, esi, edi, ebx, ecx, edx,  2, 15)
    M_fn ($01, edx, esi, edi, ebx, ecx,  3, 12)
    M_fn ($01, ecx, edx, esi, edi, ebx,  4,  5)
    M_fn ($01, ebx, ecx, edx, esi, edi,  5,  8)
    M_fn ($01, edi, ebx, ecx, edx, esi,  6,  7)
    M_fn ($01, esi, edi, ebx, ecx, edx,  7,  9)
    M_fn ($01, edx, esi, edi, ebx, ecx,  8, 11)
    M_fn ($01, ecx, edx, esi, edi, ebx,  9, 13)
    M_fn ($01, ebx, ecx, edx, esi, edi, 10, 14)
    M_fn ($01, edi, ebx, ecx, edx, esi, 11, 15)
    M_fn ($01, esi, edi, ebx, ecx, edx, 12,  6)
    M_fn ($01, edx, esi, edi, ebx, ecx, 13,  7)
    M_fn ($01, ecx, edx, esi, edi, ebx, 14,  9)
    M_fn ($01, ebx, ecx, edx, esi, edi, 15,  8)
    M_fn ($02, edi, ebx, ecx, edx, esi,  7,  7)
    M_fn ($02, esi, edi, ebx, ecx, edx,  4,  6)
    M_fn ($02, edx, esi, edi, ebx, ecx, 13,  8)
    M_fn ($02, ecx, edx, esi, edi, ebx,  1, 13)
    M_fn ($02, ebx, ecx, edx, esi, edi, 10, 11)
    M_fn ($02, edi, ebx, ecx, edx, esi,  6,  9)
    M_fn ($02, esi, edi, ebx, ecx, edx, 15,  7)
    M_fn ($02, edx, esi, edi, ebx, ecx,  3, 15)
    M_fn ($02, ecx, edx, esi, edi, ebx, 12,  7)
    M_fn ($02, ebx, ecx, edx, esi, edi,  0, 12)
    M_fn ($02, edi, ebx, ecx, edx, esi,  9, 15)
    M_fn ($02, esi, edi, ebx, ecx, edx,  5,  9)
    M_fn ($02, edx, esi, edi, ebx, ecx,  2, 11)
    M_fn ($02, ecx, edx, esi, edi, ebx, 14,  7)
    M_fn ($02, ebx, ecx, edx, esi, edi, 11, 13)
    M_fn ($02, edi, ebx, ecx, edx, esi,  8, 12)
    M_fn ($03, esi, edi, ebx, ecx, edx,  3, 11)
    M_fn ($03, edx, esi, edi, ebx, ecx, 10, 13)
    M_fn ($03, ecx, edx, esi, edi, ebx, 14,  6)
    M_fn ($03, ebx, ecx, edx, esi, edi,  4,  7)
    M_fn ($03, edi, ebx, ecx, edx, esi,  9, 14)
    M_fn ($03, esi, edi, ebx, ecx, edx, 15,  9)
    M_fn ($03, edx, esi, edi, ebx, ecx,  8, 13)
    M_fn ($03, ecx, edx, esi, edi, ebx,  1, 15)
    M_fn ($03, ebx, ecx, edx, esi, edi,  2, 14)
    M_fn ($03, edi, ebx, ecx, edx, esi,  7,  8)
    M_fn ($03, esi, edi, ebx, ecx, edx,  0, 13)
    M_fn ($03, edx, esi, edi, ebx, ecx,  6,  6)
    M_fn ($03, ecx, edx, esi, edi, ebx, 13,  5)
    M_fn ($03, ebx, ecx, edx, esi, edi, 11, 12)
    M_fn ($03, edi, ebx, ecx, edx, esi,  5,  7)
    M_fn ($03, esi, edi, ebx, ecx, edx, 12,  5)
    M_fn ($04, edx, esi, edi, ebx, ecx,  1, 11)
    M_fn ($04, ecx, edx, esi, edi, ebx,  9, 12)
    M_fn ($04, ebx, ecx, edx, esi, edi, 11, 14)
    M_fn ($04, edi, ebx, ecx, edx, esi, 10, 15)
    M_fn ($04, esi, edi, ebx, ecx, edx,  0, 14)
    M_fn ($04, edx, esi, edi, ebx, ecx,  8, 15)
    M_fn ($04, ecx, edx, esi, edi, ebx, 12,  9)
    M_fn ($04, ebx, ecx, edx, esi, edi,  4,  8)
    M_fn ($04, edi, ebx, ecx, edx, esi, 13,  9)
    M_fn ($04, esi, edi, ebx, ecx, edx,  3, 14)
    M_fn ($04, edx, esi, edi, ebx, ecx,  7,  5)
    M_fn ($04, ecx, edx, esi, edi, ebx, 15,  6)
    M_fn ($04, ebx, ecx, edx, esi, edi, 14,  8)
    M_fn ($04, edi, ebx, ecx, edx, esi,  5,  6)
    M_fn ($04, esi, edi, ebx, ecx, edx,  6,  5)
    M_fn ($04, edx, esi, edi, ebx, ecx,  2, 12)
    M_fn ($05, ecx, edx, esi, edi, ebx,  4,  9)
    M_fn ($05, ebx, ecx, edx, esi, edi,  0, 15)
    M_fn ($05, edi, ebx, ecx, edx, esi,  5,  5)
    M_fn ($05, esi, edi, ebx, ecx, edx,  9, 11)
    M_fn ($05, edx, esi, edi, ebx, ecx,  7,  6)
    M_fn ($05, ecx, edx, esi, edi, ebx, 12,  8)
    M_fn ($05, ebx, ecx, edx, esi, edi,  2, 13)
    M_fn ($05, edi, ebx, ecx, edx, esi, 10, 12)
    M_fn ($05, esi, edi, ebx, ecx, edx, 14,  5)
    M_fn ($05, edx, esi, edi, ebx, ecx,  1, 12)
    M_fn ($05, ecx, edx, esi, edi, ebx,  3, 13)
    M_fn ($05, ebx, ecx, edx, esi, edi,  8, 14)
    M_fn ($05, edi, ebx, ecx, edx, esi, 11, 11)
    M_fn ($05, esi, edi, ebx, ecx, edx,  6,  8)
    M_fn ($05, edx, esi, edi, ebx, ecx, 15,  5)
    M_fn ($05, ecx, edx, esi, edi, ebx, 13,  6)
   
    ; save left half results
    mov [rsp     ], ebx
    mov [rsp +  4], ecx
    mov [rsp +  8], edx
    mov [rsp + 12], esi
    mov [rsp + 16], edi
   
    ; reload state
    mov rax, [rsp + 24]
    mov ebx, [rax     ]
    mov ecx, [rax +  4]
    mov edx, [rax +  8]
    mov esi, [rax + 12]
    mov edi, [rax + 16]
   
    ; right half rounds
    M_fn ($15, ebx, ecx, edx, esi, edi,  5,  8)
    M_fn ($15, edi, ebx, ecx, edx, esi, 14,  9)
    M_fn ($15, esi, edi, ebx, ecx, edx,  7,  9)
    M_fn ($15, edx, esi, edi, ebx, ecx,  0, 11)
    M_fn ($15, ecx, edx, esi, edi, ebx,  9, 13)
    M_fn ($15, ebx, ecx, edx, esi, edi,  2, 15)
    M_fn ($15, edi, ebx, ecx, edx, esi, 11, 15)
    M_fn ($15, esi, edi, ebx, ecx, edx,  4,  5)
    M_fn ($15, edx, esi, edi, ebx, ecx, 13,  7)
    M_fn ($15, ecx, edx, esi, edi, ebx,  6,  7)
    M_fn ($15, ebx, ecx, edx, esi, edi, 15,  8)
    M_fn ($15, edi, ebx, ecx, edx, esi,  8, 11)
    M_fn ($15, esi, edi, ebx, ecx, edx,  1, 14)
    M_fn ($15, edx, esi, edi, ebx, ecx, 10, 14)
    M_fn ($15, ecx, edx, esi, edi, ebx,  3, 12)
    M_fn ($15, ebx, ecx, edx, esi, edi, 12,  6)
    M_fn ($14, edi, ebx, ecx, edx, esi,  6,  9)
    M_fn ($14, esi, edi, ebx, ecx, edx, 11, 13)
    M_fn ($14, edx, esi, edi, ebx, ecx,  3, 15)
    M_fn ($14, ecx, edx, esi, edi, ebx,  7,  7)
    M_fn ($14, ebx, ecx, edx, esi, edi,  0, 12)
    M_fn ($14, edi, ebx, ecx, edx, esi, 13,  8)
    M_fn ($14, esi, edi, ebx, ecx, edx,  5,  9)
    M_fn ($14, edx, esi, edi, ebx, ecx, 10, 11)
    M_fn ($14, ecx, edx, esi, edi, ebx, 14,  7)
    M_fn ($14, ebx, ecx, edx, esi, edi, 15,  7)
    M_fn ($14, edi, ebx, ecx, edx, esi,  8, 12)
    M_fn ($14, esi, edi, ebx, ecx, edx, 12,  7)
    M_fn ($14, edx, esi, edi, ebx, ecx,  4,  6)
    M_fn ($14, ecx, edx, esi, edi, ebx,  9, 15)
    M_fn ($14, ebx, ecx, edx, esi, edi,  1, 13)
    M_fn ($14, edi, ebx, ecx, edx, esi,  2, 11)
    M_fn ($13, esi, edi, ebx, ecx, edx, 15,  9)
    M_fn ($13, edx, esi, edi, ebx, ecx,  5,  7)
    M_fn ($13, ecx, edx, esi, edi, ebx,  1, 15)
    M_fn ($13, ebx, ecx, edx, esi, edi,  3, 11)
    M_fn ($13, edi, ebx, ecx, edx, esi,  7,  8)
    M_fn ($13, esi, edi, ebx, ecx, edx, 14,  6)
    M_fn ($13, edx, esi, edi, ebx, ecx,  6,  6)
    M_fn ($13, ecx, edx, esi, edi, ebx,  9, 14)
    M_fn ($13, ebx, ecx, edx, esi, edi, 11, 12)
    M_fn ($13, edi, ebx, ecx, edx, esi,  8, 13)
    M_fn ($13, esi, edi, ebx, ecx, edx, 12,  5)
    M_fn ($13, edx, esi, edi, ebx, ecx,  2, 14)
    M_fn ($13, ecx, edx, esi, edi, ebx, 10, 13)
    M_fn ($13, ebx, ecx, edx, esi, edi,  0, 13)
    M_fn ($13, edi, ebx, ecx, edx, esi,  4,  7)
    M_fn ($13, esi, edi, ebx, ecx, edx, 13,  5)
    M_fn ($12, edx, esi, edi, ebx, ecx,  8, 15)
    M_fn ($12, ecx, edx, esi, edi, ebx,  6,  5)
    M_fn ($12, ebx, ecx, edx, esi, edi,  4,  8)
    M_fn ($12, edi, ebx, ecx, edx, esi,  1, 11)
    M_fn ($12, esi, edi, ebx, ecx, edx,  3, 14)
    M_fn ($12, edx, esi, edi, ebx, ecx, 11, 14)
    M_fn ($12, ecx, edx, esi, edi, ebx, 15,  6)
    M_fn ($12, ebx, ecx, edx, esi, edi,  0, 14)
    M_fn ($12, edi, ebx, ecx, edx, esi,  5,  6)
    M_fn ($12, esi, edi, ebx, ecx, edx, 12,  9)
    M_fn ($12, edx, esi, edi, ebx, ecx,  2, 12)
    M_fn ($12, ecx, edx, esi, edi, ebx, 13,  9)
    M_fn ($12, ebx, ecx, edx, esi, edi,  9, 12)
    M_fn ($12, edi, ebx, ecx, edx, esi,  7,  5)
    M_fn ($12, esi, edi, ebx, ecx, edx, 10, 15)
    M_fn ($12, edx, esi, edi, ebx, ecx, 14,  8)
    M_fn ($11, ecx, edx, esi, edi, ebx, 12,  8)
    M_fn ($11, ebx, ecx, edx, esi, edi, 15,  5)
    M_fn ($11, edi, ebx, ecx, edx, esi, 10, 12)
    M_fn ($11, esi, edi, ebx, ecx, edx,  4,  9)
    M_fn ($11, edx, esi, edi, ebx, ecx,  1, 12)
    M_fn ($11, ecx, edx, esi, edi, ebx,  5,  5)
    M_fn ($11, ebx, ecx, edx, esi, edi,  8, 14)
    M_fn ($11, edi, ebx, ecx, edx, esi,  7,  6)
    M_fn ($11, esi, edi, ebx, ecx, edx,  6,  8)
    M_fn ($11, edx, esi, edi, ebx, ecx,  2, 13)
    M_fn ($11, ecx, edx, esi, edi, ebx, 13,  6)
    M_fn ($11, ebx, ecx, edx, esi, edi, 14,  5)
    M_fn ($11, edi, ebx, ecx, edx, esi,  0, 15)
    M_fn ($11, esi, edi, ebx, ecx, edx,  3, 13)
    M_fn ($11, edx, esi, edi, ebx, ecx,  9, 11)
    M_fn ($11, ecx, edx, esi, edi, ebx, 11, 11)
   
    ; combine left half and right half results
    add ecx, [rsp     ]
    add edx, [rsp +  4]
    add esi, [rsp +  8]
    add edi, [rsp + 12]
    add ebx, [rsp + 16]
   
    ; update state
    mov rax, [rsp + 24]
    add edx, [rax     ]
    add esi, [rax +  4]
    add edi, [rax +  8]
    add ebx, [rax + 12]
    add ecx, [rax + 16]
    mov [rax     ], esi
    mov [rax +  4], edi
    mov [rax +  8], ebx
    mov [rax + 12], ecx
    mov [rax + 16], edx
   
    ; restore
    mov rbx, [rsp + 32]   ; restore rbx register
    mov rbp, [rsp + 40]   ; restore rbp register
    mov rsi, [rsp + 48]   ; restore rsi register
    mov rdi, [rsp + 56]   ; restore rdi register   
    add rsp, 64           ; release stack space
   
  EndProcedure
 
  ;- Public procedures
 
  Procedure.s Hash(*ctx.RMD160Context)
    Protected Dim buffer.a(40)
    ; backup (without altering stack pointer)
    mov [rsp -  8], rbx
    mov [rsp - 16], rdi
    ; convert state to hex
    mov rax, [p.p_ctx]
    mov rdx, [p.a_buffer]
    lea rdi, [.lut]
    !mov ecx, 19
    !.loop:
    movzx ebx, byte [rax + rcx]
    movzx ebx, word [rdi + rbx*2]
    mov [rdx + rcx*2], bx
    !sub ecx, 1
    !jnc .loop
    !jmp .restore
    ; lookup table
    !.lut:
    !db '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
    !db '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
    !db '404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f'
    !db '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
    !db '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
    !db 'a0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebf'
    !db 'c0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedf'
    !db 'e0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff'
    ; restore
    !.restore:
    mov rbx, [rsp -  8]
    mov rdi, [rsp - 16]
    ProcedureReturn PeekS(@buffer(), -1, #PB_Ascii)
  EndProcedure
 
  Procedure Init(*ctx.RMD160Context)
    *ctx\state[0] = $67452301
    *ctx\state[1] = $efcdab89
    *ctx\state[2] = $98badcfe
    *ctx\state[3] = $10325476
    *ctx\state[4] = $c3d2e1f0
    *ctx\msglen = 0
  EndProcedure
 
  Procedure Update(*ctx.RMD160Context, *bytes, size)
    Protected i, left
    i = *ctx\msglen & 63
    *ctx\msglen + size
    ; handle partially filled buffer
    If i
      left = 64 - i
      If size < left
        CopyMemory(*bytes, @*ctx\buffer[i], size)
        ProcedureReturn
      Else
        CopyMemory(*bytes, @*ctx\buffer[i], left)
        ProcessBlock(*ctx, @*ctx\buffer[0])
        *bytes + left
        size - left
      EndIf
    EndIf
    ; main loop
    While size >= 64
      ProcessBlock(*ctx, *bytes)
      *bytes + 64
      size - 64
    Wend
    ; put remaining bytes in buffer
    If size
      CopyMemory(*bytes, @*ctx\buffer[0], size)
    EndIf
  EndProcedure
 
  Procedure Final(*ctx.RMD160Context)
    Protected i, *msgbitlen.Quad
    ; append $80
    i = *ctx\msglen & 63
    *ctx\buffer[i] = $80
    i + 1
    ; process if not enough room for msgbitlen
    If i > 56
      FillMemory(@*ctx\buffer[i], 64 - i)
      ProcessBlock(*ctx, @*ctx\buffer[0])
      i = 0
    EndIf
    ; process final block
    FillMemory(@*ctx\buffer[i], 56 - i)
    *msgbitlen = @*ctx\buffer[56]
    *msgbitlen\q = *ctx\msglen << 3
    ProcessBlock(*ctx, @*ctx\buffer[0])
  EndProcedure
 
  Procedure.s AsciiHash(AsciiString.s)
    Protected size = Len(AsciiString)
    Protected Dim buffer.a(size)
    PokeS(@buffer(), AsciiString, size, #PB_Ascii)
    ProcedureReturn MemoryHash(@buffer(), size)
  EndProcedure
 
  Procedure.s MemoryHash(*Buffer, Size)   
    Protected ctx.RMD160Context
    Init(@ctx)
    Update(@ctx, *Buffer, Size)
    Final(@ctx)
    ProcedureReturn Hash(@ctx)
  EndProcedure
   
  Procedure.s FileHash(Filename.s, Offset.q=0, Length.q=0)
    Protected file, error, *buffer, ctx.RMD160Context
    Protected.q bytes_read, bytes_to_process, total_bytes_read
   
    file = ReadFile(#PB_Any, Filename)
    If file
      *buffer = AllocateMemory($8000, #PB_Memory_NoClear)
      If *buffer
        Init(@ctx)
       
        bytes_to_process = Lof(file)
        If Offset < bytes_to_process
          FileSeek(file, Offset)
          If Length = 0
            Length = bytes_to_process
          EndIf
          If Offset + Length > bytes_to_process
            Length = bytes_to_process - Offset
          EndIf
          bytes_to_process = Length
           
          While Length > 0
            bytes_read = ReadData(file, *buffer, $8000)
            If bytes_read
              If bytes_read > Length
                bytes_read = Length
              EndIf
              Update(@ctx, *buffer, bytes_read)
              Length - bytes_read
              total_bytes_read + bytes_read
            Else
              error = #True
              Break
            EndIf
          Wend
         
        EndIf
        FreeMemory(*buffer)
      EndIf
      CloseFile(file)
    EndIf
   
    If error
      ProcedureReturn ""
    Else
      Final(@ctx)
      ProcedureReturn Hash(@ctx)
    EndIf
 
  EndProcedure
 
EndModule
; IDE Options = PureBasic 5.31 (Windows - x64)
; CursorPosition = 505
; FirstLine = 459
; Folding = ----
; EnableXP