DeclareModule SHA256

Structure SHA256DATA
  Size.q
  ChunkSize.i
  *Hash.LongArray
  *Chunk.LongArray
EndStructure

Declare.i ExamineSHA256()
Declare   NextSHA256(*Id.SHA256DATA, *Memory, Size.q)
Declare.i FinishSHA256(*Id.SHA256DATA)

Declare.i SHA256(*Memory, Size.q = #PB_Any)
Declare.i SHA256String(String.s, Format.i = #PB_Ascii)
Declare.i SHA256File(FileName.s)

Declare.s ConvertToHexString(*Hash)

Declare.s FinishSHA256Fingerprint(*Id.SHA256DATA)

Declare.s SHA256Fingerprint(*Memory, Size.q = #PB_Any)
Declare.s SHA256StringFingerprint(String.s, Format.i = #PB_Ascii)
Declare.s SHA256FileFingerprint(FileName.s)

EndDeclareModule

; ==================================================================================================

Module SHA256

EnableExplicit

DisableDebugger

Structure LongArray
  l.l[0]
EndStructure

Macro RightShift(Number, Count)
  ((Number >> Count) & (~($FFFFFFFF << (32 - Count))))
EndMacro

Macro RightRotate(Number, Count)
  (RightShift(Number, Count) | (Number << (32 - Count)))
EndMacro

Procedure.l EndianSwap32(x.l)
  ProcedureReturn x<<24 | ((x<<8) & $00FF0000) | ((x>>8) & $0000FF00) | ((x>>24) & $000000FF)
EndProcedure

Procedure.q EndianSwap64(x.q)
  Protected z.q = x << 56
  z = z | ((x << 40) & $00FF000000000000)
  z = z | ((x << 24) & $0000FF0000000000)
  z = z | ((x <<  8) & $000000FF00000000)
  z = z | ((x >>  8) & $00000000FF000000)
  z = z | ((x >> 24) & $0000000000FF0000)
  z = z | ((x >> 40) & $000000000000FF00)
  z = z | ((x >> 56) & $00000000000000FF)
  ProcedureReturn z
EndProcedure

DataSection
  k:
  Data.l $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5
  Data.l $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174
  Data.l $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da
  Data.l $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967
  Data.l $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85
  Data.l $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070
  Data.l $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3
  Data.l $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2
EndDataSection

Procedure.i ExamineSHA256()
  Protected *Id.SHA256DATA = AllocateMemory(SizeOf(SHA256DATA))
  *Id\Hash = AllocateMemory(32, #PB_Memory_NoClear)
  *Id\Chunk = AllocateMemory(256, #PB_Memory_NoClear)
  *Id\Hash\l[0] = $6A09E667 : *Id\Hash\l[1] = $BB67AE85 : *Id\Hash\l[2] = $3C6EF372 : *Id\Hash\l[3] = $A54FF53A
  *Id\Hash\l[4] = $510E527F : *Id\Hash\l[5] = $9B05688C : *Id\Hash\l[6] = $1F83D9AB : *Id\Hash\l[7] = $5BE0CD19
  ProcedureReturn *Id
EndProcedure

Procedure ProcessChunk(*Id.SHA256DATA)
  Protected *k.Long = ?k
  Protected *Chunk.Long = *Id\Chunk
  Protected.i a, b, c, d, e, f, g, h, t1, t2, s0, s1, maj, ch, c15, c2, i
  For i = 0 To 15 : *Id\Chunk\l[i] = EndianSwap32(*Id\Chunk\l[i]) : Next
  For i = 16 To 63
    c15 = *Id\Chunk\l[i-15]
    c2 = *Id\Chunk\l[i-2]
    s0 = RightRotate(c15, 7) ! RightRotate(c15, 18) ! RightShift(c15, 3)
    s1 = RightRotate(c2, 17) ! RightRotate(c2, 19) ! RightShift(c2, 10)
    *Id\Chunk\l[i] = *Id\Chunk\l[i-16] + s0 + *Id\Chunk\l[i-7] + s1
  Next
  a = *Id\Hash\l[0] : b = *Id\Hash\l[1] : c = *Id\Hash\l[2] : d = *Id\Hash\l[3]
  e = *Id\Hash\l[4] : f = *Id\Hash\l[5] : g = *Id\Hash\l[6] : h = *Id\Hash\l[7] 
  For i = 0 To 63
    s0 = RightRotate(a, 2) ! RightRotate(a, 13) ! RightRotate(a, 22)
    maj = (a & b) ! (a & c) ! (b & c)
    t2 = S0 + maj
    s1 = RightRotate(e, 6) ! RightRotate(e, 11) ! RightRotate(e, 25)
    ch = (e & f) ! ((~ e) & g)
    t1 = h + S1 + ch + *k\l + *Chunk\l
    h = g : g = f : f = e : e = d + t1
    d = c : c = b : b = a : a = t1 + t2
    *k+4
    *Chunk+4
  Next
  *Id\Hash\l[0] + a : *Id\Hash\l[1] + b : *Id\Hash\l[2] + c : *Id\Hash\l[3] + d
  *Id\Hash\l[4] + e : *Id\Hash\l[5] + f : *Id\Hash\l[6] + g : *Id\Hash\l[7] + h
EndProcedure

Procedure NextSHA256(*Id.SHA256DATA, *Memory, Size.q)
  Protected.i d, NumberOfChunks, i
  *Id\Size + Size
  If *Id\ChunkSize > 0
    d = (64 - *Id\ChunkSize)
    If d > Size : d = Size : EndIf
    CopyMemory(*Memory, *Id\Chunk + *Id\ChunkSize, d)
    *Memory + d : Size - d : *Id\ChunkSize + d
    If *Id\ChunkSize = 64
      ProcessChunk(*Id.SHA256DATA)
      *Id\ChunkSize = 0
    EndIf
  EndIf
  NumberOfChunks = Size / 64
  For i = 0 To NumberOfChunks - 1
    CopyMemory(*Memory+i*64, *Id\Chunk, 64)
    ProcessChunk(*Id)
  Next
  If Size % 64 <> 0
    CopyMemory(*Memory + NumberOfChunks * 64, *Id\Chunk, Size % 64)
    *Id\ChunkSize = Size % 64
  EndIf
EndProcedure

Procedure.i FinishSHA256(*Id.SHA256DATA)
  Protected i.i
  Protected *Hash = *Id\Hash
  Protected BitLength.q = *Id\Size * 8 + 1 + 64
  If BitLength % 512 <> 0
    BitLength = BitLength + (512 - (BitLength % 512))
  EndIf
  FillMemory(*Id\Chunk+*Id\ChunkSize, 64-*Id\ChunkSize, 0)
  PokeA(*Id\Chunk+*Id\ChunkSize, $80)
  If (BitLength/8 - *Id\Size) > 64
    ProcessChunk(*Id)
    FillMemory(*Id\Chunk, 64, 0)
  EndIf
  PokeQ(*Id\Chunk + 64 - 8, EndianSwap64(*Id\Size*8))
  ProcessChunk(*Id)
  For i = 0 To 7 : *Id\Hash\l[i] = EndianSwap32(*Id\Hash\l[i]) : Next
  FreeMemory(*Id\Chunk)
  FreeMemory(*Id)
  ProcedureReturn *Id\Hash
EndProcedure

Procedure.i SHA256(*Memory, Size.q = #PB_Any)
  Protected *Id = ExamineSHA256()
  If Size = #PB_Any : Size = MemorySize(*Memory) : EndIf
  NextSHA256(*Id, *Memory, Size)
  ProcedureReturn FinishSHA256(*Id)
EndProcedure

Procedure.i SHA256String(String.s, Format.i = #PB_Ascii)
  Protected Size.i, *Memory, *Hash
  Size = StringByteLength(String, Format)
  *Memory = AllocateMemory(Size + 1)
  PokeS(*Memory, String, -1, Format)
  *Hash = SHA256(*Memory, Size)
  FreeMemory(*Memory)
  ProcedureReturn *Hash
EndProcedure

Procedure.i SHA256File(FileName.s)
  Protected Size.i, File.i = ReadFile(#PB_Any, FileName)
  If Not File : ProcedureReturn 0 : EndIf
  Protected *Id = ExamineSHA256()
  Protected *Memory = AllocateMemory(640000, #PB_Memory_NoClear)
  While Not Eof(File)
    Size = ReadData(File, *Memory, 640000)
    NextSHA256(*Id, *Memory, Size)
  Wend
  FreeMemory(*Memory)
  CloseFile(File)
  ProcedureReturn FinishSHA256(*Id)
EndProcedure

Macro HexByte(Hash)
  RSet(Hex(PeekA(Hash), #PB_Ascii), 2, "0")
EndMacro

Macro HexLong(Hash)
  HexByte(Hash) + HexByte(Hash+1) + HexByte(Hash+2) + HexByte(Hash+3)
EndMacro

Macro HexSHA256(Hash)
  HexLong(Hash) + HexLong(Hash+4) + HexLong(Hash+8) + HexLong(Hash+12) + HexLong(Hash+16) + HexLong(Hash+20) + HexLong(Hash+24) + HexLong(Hash+28)
EndMacro

Procedure.s ConvertToHexString(*Hash)
  ProcedureReturn HexSHA256(*Hash)
EndProcedure

Procedure.s FinishSHA256Fingerprint(*Id.SHA256DATA)
  Protected *Hash, Hash.s
  *Hash = FinishSHA256(*Id.SHA256DATA)
  Hash = HexSHA256(*Hash)
  FreeMemory(*Hash)
  ProcedureReturn Hash
EndProcedure

Procedure.s SHA256Fingerprint(*Memory, Size.q = #PB_Any)
  Protected *Hash, Hash.s
  If Size = #PB_Any : Size = MemorySize(*Memory) : EndIf
  *Hash = SHA256(*Memory, Size)
  Hash = HexSHA256(*Hash)
  FreeMemory(*Hash)
  ProcedureReturn Hash
EndProcedure

Procedure.s SHA256StringFingerprint(String.s, Format.i = #PB_Ascii)
  Protected *Hash, Hash.s
  *Hash = SHA256String(String, Format)
  Hash = HexSHA256(*Hash)
  FreeMemory(*Hash)
  ProcedureReturn Hash
EndProcedure

Procedure.s SHA256FileFingerprint(FileName.s)
  Protected *Hash, Hash.s
  *Hash = SHA256File(FileName)
  Hash = HexSHA256(*Hash)
  FreeMemory(*Hash)
  ProcedureReturn Hash
EndProcedure

EndModule
; IDE Options = PureBasic 5.31 (Windows - x64)
; CursorPosition = 239
; Folding = ----
; EnableUnicode
; EnableXP