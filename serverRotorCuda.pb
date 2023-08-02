EnableExplicit
IncludeFile "/libs/Curve64.pb"
IncludeFile "/libs/RMD160.pbi"
IncludeFile "/libs/sha256_mod.pbi"

Structure RANGE_MAPPER
  count.i
  Array *ptr(0);<- our 'stack'
EndStructure

Structure ClientInfoStructure  
  authorization.i  
  time.i
  ClientId.i
  ClientIP.i
  name.s  
  quit.i   
  nopow.b
EndStructure

Structure ServerStructure
  Quit.i
  Map Client.ClientInfoStructure()
EndStructure

Structure SocketStructure
  id.i
  ClientId.i
  req.s  
EndStructure

Structure RangeStructure
  id.i
  rb$
  re$  
EndStructure

Structure settingsStructure
  port.i    
  rangeB$
  rangeE$
  address$
  jobtimeout.i
  divpow.i
  deviderint.i
  maxbyte.i
  outFilename$
  mapFilename$
  Databasedirectory$
EndStructure

Structure jobStructure  
  rangeid.i 
  jobstarttime.i
  name.s
  powaddress$
EndStructure

Enumeration
  #File
  #Server
  #LOGFILE
  #dbfile
EndEnumeration

#colorBlue=1
#colorGreen=2
#colorCyan=3
#colorRed=4
#colorMagenta=5
#colorBrown=6
#colorDefault=7
#colorDarkgrey=8
#colorYellow=14
#colorWhite=15
#colorbrightmagenta=13
#colorBrightGreen = 10

#ADDRESSSIZE=34
#HEADERSIZE=64+#ADDRESSSIZE+1
#DATASIZE=65
#APPVERSION="2.0"

#DatabaseFile="Personstate.db" 

Declare m_sethex32(*bin, *hash, szbytes)
Declare.s m_gethex32(*bin, szbytes)  

Define ctx.RMD160::RMD160Context
Define Server.ServerStructure
Define MutexConsole = CreateMutex()
Define MutexClientMap = CreateMutex()
Define socketreqMutex = CreateMutex()
Define LogMutex = CreateMutex()
Define RndMutex = CreateMutex()
Define SQLMutex = CreateMutex()
Define NewList *range_task.RangeStructure();<- holds pointers to all randomly selected map elements
Define range_mapper.RANGE_MAPPER  ;<- holds pointers to all free map elements

Define NewList range.RangeStructure()
Define *rangeB,*rangeE, currange, isFind=#False, subrangewidth.i
Define NewList winkeylist.s()
Define NewMap settings.settingsStructure()
Define NewMap socketreq.SocketStructure()
Define NewMap job.jobStructure()
Define CurentElementIndex
Define stratumwork_g
Define totalscanned, lastserialsearch

Define *CurveP, *CurveGX, *CurveGY, *Curveqn
*CurveP = Curve::m_getCurveValues()
*CurveGX = *CurveP+32
*CurveGY = *CurveP+64
*Curveqn = *CurveP+96

Procedure SPrint(text$, cl)
 Shared MutexConsole
  LockMutex(MutexConsole)
  ConsoleColor(cl,0)
  Debug FormatDate("%hh:%ii:%ss ", Date())+" "+text$
  PrintN(FormatDate("%hh:%ii:%ss ", Date())+" "+text$)  
  ConsoleColor(#colorDefault,0)
  UnlockMutex(MutexConsole)
EndProcedure

Procedure.i FillMapper();<- fill the 'stack' (doesnt matter how big the map is)
  Protected index.i
  Shared range_mapper, range()
  
  With range_mapper
    index = ListSize(range())
    If index
      \count = index - 1
      Dim \ptr(\count)
      If ArraySize(\ptr(),1) = \count
        index = 0
        ForEach range()
          \ptr(index) = range()
          index + 1
        Next
        ProcedureReturn index
      EndIf
    EndIf
    ProcedureReturn #False
  EndWith
EndProcedure

Procedure.i GetRandomMapElement();<- get a random map element of all available map elements and put the pointer into the task list
  Protected index.i
  Protected bytes.i
  Protected *ptr
  Shared range_mapper, range(), *range_task()
  
  With range_mapper
    If Not \count = -1
      index = Random(\count)
      Debug ">"+index
      If AddElement(*range_task())
        *ptr = \ptr(index)
        If Not \count = index
          bytes = (ArraySize(\ptr()) - index + 1) * SizeOf(Integer)
          CopyMemory(@\ptr(index + 1),@\ptr(index),bytes)
        EndIf
        \count - 1
        *range_task() = *ptr
      EndIf
    EndIf
    ProcedureReturn *ptr
  EndWith
EndProcedure

Procedure.i GetWalkMapElement();<- get a random map element of all available map elements and put the pointer into the task list
  Protected index.i
  Protected bytes.i
  Protected *ptr
  Protected jmp
  Shared range_mapper, range(), *range_task(), CurentElementIndex
  
  With range_mapper
    If Not \count = -1
      If \count = 0
        CurentElementIndex = 0
      Else
        jmp = Random(\count/2)        
        ;PrintN("Index:"+Str(CurentElementIndex)+" jmp:"+Str(jmp)+"=>+"+Str(CurentElementIndex + jmp))
        CurentElementIndex = (CurentElementIndex + jmp)%\count
      EndIf
      index = CurentElementIndex
      Debug ">"+index
      If AddElement(*range_task())
        *ptr = \ptr(index)
        If Not \count = index
          bytes = (ArraySize(\ptr()) - index + 1) * SizeOf(Integer)
          CopyMemory(@\ptr(index + 1),@\ptr(index),bytes)
        EndIf
        \count - 1
        *range_task() = *ptr
      EndIf
    EndIf
    ProcedureReturn *ptr
  EndWith
EndProcedure

Procedure.i ReturnElement(id)
  Shared range_mapper, *range_task()
  With range_mapper
    ForEach *range_task()
      If *range_task()\id = id
        Debug "retrun "+*range_task()
        \ptr(\count+1) = *range_task()
        DeleteElement(*range_task(),1)        
        \count + 1
        
      EndIf
    Next
  EndWith
EndProcedure

Procedure.i DelElementFromTask(id)  
 Shared *range_task()
  ForEach *range_task()
    If *range_task()\id = id       
        DeleteElement(*range_task(),1)  
    EndIf
  Next
EndProcedure

Procedure tobin(*mem,a$) 
  Protected pos
  pos=1  
  While pos<Len(a$)
    PokeA(*mem,Val("$"+Mid(a$,pos,2)))
    *mem+1
  pos+2
  Wend  
EndProcedure

Procedure.s uncomressed2commpressedPub(ha$)
  Protected Str1.s, Str2.s, x$,y$,ru$,rc$
  If Left(ha$,2)="04" And Len(ha$)=130
    ha$=Right(ha$,Len(ha$)-2)
  EndIf
  Str1=Left(ha$,64)
  Str2=Right(ha$,64)
  Debug Str1
  Debug Str2
  
  x$=PeekS(@Str1,-1,#PB_Ascii)
  x$=RSet(x$,64,"0")
  y$=PeekS(@Str2,-1,#PB_Ascii)
  y$=RSet(y$,64,"0")
  ru$="04"+x$+y$
  If FindString("13579bdf",Right(y$,1))>0
    rc$="03"+x$
  Else
    rc$="02"+x$
  EndIf
  
  ProcedureReturn rc$

EndProcedure

Procedure.s MyencodeBase58(hexa$)
  Protected *digits, ret$, length, *pbegin, digitslen, i,j, carry, pszBase58$="123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"    
  length =Len(hexa$)/2
  *pbegin=AllocateMemory(length)
  *digits=AllocateMemory(256)
 
    tobin(*pbegin,hexa$) 
    digitslen =1
    ; Skip leading zeroes
    For i = 0 To length-1
    If PeekC(*pbegin+i)=0     
      ret$+"1"
    Else
      Break
    EndIf  
    Next i
    
    For i = 0 To length-1
      carry = PeekC(*pbegin+i)    
      For j = 0 To digitslen-1
        carry+PeekC(*digits+j)<<8       
        PokeC(*digits+j,carry%58 )
        carry/58
      Next j
      While carry>0        
        PokeC(*digits+digitslen,carry%58 )
        digitslen+1
        carry/58      
      Wend
     
    Next i  
    For i = 0 To digitslen-1      
      ret$+Mid(pszBase58$,PeekC(*digits+digitslen - 1 - i)+1,1)
    Next i
  FreeMemory(*pbegin)
  FreeMemory(*digits)

  
  ProcedureReturn ret$
EndProcedure

Procedure.s getBTCAddresFromPub(pub$)  
  Protected *a=AllocateMemory(Len(pub$)/2) , resser.s=Space(256), checksum$, extendedRIPEMD$
  ;1 - Take the corresponding public key generated with it (33 bytes, 1 byte 0x02 (y-coord is even), and 32 bytes corresponding to X coordinate)
  tobin(*a,pub$)
  ;2 - Perform SHA-256 hashing on the public key
  resser=SHA256::SHA256Fingerprint(*a,Len(pub$)/2)
  ;PrintN("2>"+ resser)
  tobin(*a,resser)
  ;3 - Perform RIPEMD-160 hashing on the result of SHA-256
  resser=RMD160::MemoryHash(*a, Len(resser)/2) 
  ;PrintN("3>"+ resser)
  ;4 - Add version byte in front of RIPEMD-160 hash (0x00 for Main Network)
  resser = "00"+resser
  extendedRIPEMD$ = resser
  ;PrintN("4>"+ resser)
  tobin(*a,resser)
  ;5 - Perform SHA-256 hash on the extended RIPEMD-160 result
  resser=SHA256::SHA256Fingerprint(*a,Len(resser)/2)
  ;PrintN("5>"+ resser)
  tobin(*a,resser)
  ;6 - Perform SHA-256 hash on the result of the previous SHA-256 hash
  resser=SHA256::SHA256Fingerprint(*a,Len(resser)/2)
  ;PrintN("6>"+ resser)
  ;7 - Take the first 4 bytes of the second SHA-256 hash. This is the address checksum
  checksum$ = Left(resser,8)
  ;PrintN("7>"+ checksum$)
  ;8 - Add the 4 checksum bytes from stage 7 at the end of extended RIPEMD-160 hash from stage 4. This is the 25-byte binary Bitcoin Address.
  resser = extendedRIPEMD$+checksum$
  ;PrintN("8>"+ resser)
  ;9 - Convert the result from a byte string into a base58 string using Base58Check encoding. This is the most commonly used Bitcoin Address format
  ;resser = Base58encode(resser)
  resser = MyencodeBase58(resser)
  
  ;PrintN("9>"+ resser)
  FreeMemory(*a)
  ProcedureReturn resser
EndProcedure

Procedure.s genPOWaddress(rb$,re$)
  Protected *rb_l, *re_l, res$, a$, randomkey, pu$, pc$, testb, teste
  Protected *cx, *cy, *rndkey, procname$="[GENPOW] "
  
  Shared subrangewidth,*CurveGX, *CurveGY, *CurveP
  
  *cx = AllocateMemory(160)
  *cy = *cx+32
  *rb_l = *cx+64
  *re_l = *cx+96
  *rndkey = *cx+128
  
  m_sethex32(*rb_l, @rb$,32)
  m_sethex32(*re_l, @re$,32)
  
  Repeat
    randomkey=Random(subrangewidth-1,1)
    a$ = Hex(randomkey)
    m_sethex32(*rndkey, @a$,32)
    Curve::m_addModX64(*rndkey,*rb_l,*rndkey,*CurveP)  
    ;
    testb=Curve::m_check_less_more_equilX64(*rndkey,*rb_l)
    teste=Curve::m_check_less_more_equilX64(*rndkey,*re_l)
    Sprint(procname$+"Random key      :"+Hex(randomkey),#colorDarkgrey) 
  Until (testb=2 And teste=1)
  Curve::m_PTMULX64(*cx, *cy, *CurveGX, *CurveGY, *rndkey,*CurveP)
  pu$ = m_gethex32(*cx, 32)+m_gethex32(*cy, 32) 
  pc$ = uncomressed2commpressedPub(pu$)  
  Sprint(procname$+"Subrange width  :"+Hex(subrangewidth),#colorDarkgrey) 
  Sprint(procname$+"RangeB          :"+m_gethex32(*rb_l, 32)  ,#colorDarkgrey)
  Sprint(procname$+"Random key range:"+m_gethex32(*rndkey, 32)  ,#colorDarkgrey)
  Sprint(procname$+"RangeE          :"+m_gethex32(*re_l, 32)  ,#colorDarkgrey)
  Sprint(procname$+"Random pubX     :"+m_gethex32(*cx, 32),#colorDarkgrey)
  Sprint(procname$+"Random pubY     :"+m_gethex32(*cy, 32),#colorDarkgrey)
  Sprint(procname$+"Random pub  c.  :"+pc$,#colorDarkgrey)  
  res$ = getBTCAddresFromPub(pc$) 
  Debug "pow address c.  :"+res$
  
  FreeMemory(*cx)
 
  ProcedureReturn res$
EndProcedure


Procedure CheckDatabaseUpdate(Database, Query$)
  Protected Result ,procname$ = "[MYSQL] "
   Result = DatabaseUpdate(Database, Query$)
   If Result = 0
      Sprint(procname$+" Error ["+DatabaseError()+"]", #colorRed)
   EndIf
   
   ProcedureReturn Result
EndProcedure

Procedure SQLTxstate_Init()
 Protected Directory$, result=1, procname$ = "[MYSQL] "
 Shared settings()
 
 Directory$ = settings("1")\Databasedirectory$
 If FileSize(Directory$+"/"+#DatabaseFile)>-1 
   Sprint(procname$+" DB exist", #colorDefault) 
 Else
   If CreateFile(#dbfile, Directory$+"/"+#DatabaseFile)
     Sprint(procname$+" DB created", #colorDefault)
     CloseFile(#dbfile)
   Else
     Sprint(procname$+" Error [Can`t creat DB]", #colorRed)
     result=0
   EndIf
 EndIf
 ; block_table
 If OpenDatabase(#dbfile, Directory$+"/"+#DatabaseFile, "", "")
   Debug "SQL_Init>"+#DatabaseFile+" open"
   If DatabaseQuery(#dbfile, "SELECT 1 FROM person_table ")
     Debug "person_table exist"
     
   Else
     DatabaseError()
     If CheckDatabaseUpdate(#dbfile, "CREATE TABLE person_table (id INTEGER PRIMARY KEY AUTOINCREMENT, name CHAR(64), jobscan INT, jobreject INT)")
       Debug "person_table create"
     Else
       result=0
     EndIf
   EndIf
   CloseDatabase(#dbfile)
 Else  
   Sprint(procname$+" Error ["+DatabaseError()+"]", #colorRed)
   result=0
 EndIf 
ProcedureReturn result
EndProcedure

Procedure SQLTxstate_Set(SQL.s)
 Protected result=0, Directory$, procname$ = "[MYSQL] "
 Shared settings(), SQLMutex
 Directory$ = settings("1")\Databasedirectory$
 LockMutex(SQLMutex)
 If OpenDatabase(#dbfile, Directory$+"/"+#DatabaseFile, "", "")
   Debug "SQL_Set>"+#DatabaseFile+" open"
   result = CheckDatabaseUpdate(#dbfile, SQL)
   CloseDatabase(#dbfile)
 Else
   Sprint(procname$+" Error [Can`t open DB]", #colorRed)
 EndIf
 UnlockMutex(SQLMutex)
ProcedureReturn result
EndProcedure

Procedure.s SQLTxstate_Get(SQL.s)
  
  ; <,> columns seperator
  ; <;> rows separator
  Protected result$="", Directory$, dbcol, i,procname$ = "[MYSQL] "
  Shared settings(), SQLMutex
 Directory$ = settings("1")\Databasedirectory$
 LockMutex(SQLMutex)
 If OpenDatabase(#dbfile, Directory$+"/"+#DatabaseFile, "", "")
   Debug "SQL_Getcolumn>"+#DatabaseFile+" open "+SQL
   If DatabaseQuery(#dbfile, SQL.s)
     dbcol = DatabaseColumns(#dbfile)
     Debug "Answer ColumnN:"+Str(dbcol)
           
     While NextDatabaseRow(#dbfile)
          If dbcol
           For i = 0 To dbcol-1
             result$ + GetDatabaseString(#dbfile, i)
             If i<dbcol-1
               result$+","
             EndIf             
           Next i
         EndIf
         result$+";"
     Wend
     result$=RTrim(result$ ,";")    

     FinishDatabaseQuery(#dbfile)
   Else
      Sprint(procname$+" Error ["+DatabaseError()+"]", #colorRed)
   EndIf
   CloseDatabase(#dbfile)
 Else
   Sprint(procname$+" Error [Can`t open DB]", #colorRed)
 EndIf
 UnlockMutex(SQLMutex)
ProcedureReturn result$
EndProcedure

Procedure.s m_cutHex(a$)
  a$=Trim(UCase(a$)) 
  If Left(a$,2)="0X" 
    a$=Mid(a$,3,Len(a$)-2)
  EndIf 
  If Len(a$)=1
    a$="0"+a$
  EndIf
ProcedureReturn LCase(a$)
EndProcedure

Procedure.s getElem(js.i,pname.s="",pelem.l=0,aelem.l=0)
  Protected result$,jsFloat_g
  
  result$=""
  If IsJSON(js) And GetJSONMember(JSONValue(js), pname)
  Select JSONType(GetJSONMember(JSONValue(js), pname))      
      Case #PB_JSON_String
          result$= GetJSONString(GetJSONMember(JSONValue(js), pname))          
      Case #PB_JSON_Array         
       If JSONArraySize(GetJSONMember(JSONValue(js), pname))>pelem
         Select JSONType(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
           Case #PB_JSON_String
             result$= GetJSONString(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
           Case #PB_JSON_Number            
             result$= Str(GetJSONInteger(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem)))    
             jsFloat_g=GetJSONDouble(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))
             
           Case #PB_JSON_Array
             If JSONArraySize(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem))>aelem             
                result$+ GetJSONString(GetJSONElement(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem),aelem))
             EndIf
          Case #PB_JSON_Boolean
             result$=Str(GetJSONBoolean(GetJSONElement(GetJSONMember(JSONValue(js), pname), pelem)))
             
         EndSelect
          
        EndIf        
      Case #PB_JSON_Boolean
        result$=Str(GetJSONBoolean(GetJSONMember(JSONValue(js), pname)))        
      Case #PB_JSON_Number        
        result$= Str(GetJSONInteger(GetJSONMember(JSONValue(js), pname)))        
    EndSelect  
  EndIf
  ProcedureReturn result$
EndProcedure

Procedure.s getprogparam()
  Protected parametrscount, datares$, i, params$
  Shared  settings()
  parametrscount=CountProgramParameters()
  
  i=0
  While i<parametrscount  
    Select LCase(ProgramParameter(i))
          
      Case "-port"
        Debug "found -port"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\port = Val(datares$)
          Sprint( "-port "+Str(settings("1")\port),#colordefault)
        EndIf
      Case "-range"
        Debug "found -range"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\rangeB$ = StringField(datares$,1,":")
          settings("1")\rangeE$ = StringField(datares$,2,":")
          Sprint( "-range "+settings("1")\rangeB$+":"+settings("1")\rangeE$,#colordefault)
        EndIf        
      Case "-dp"
        Debug "found -dp"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\divpow = Val(datares$)
          Sprint( "-divpow "+Str(settings("1")\divpow),#colordefault)
        EndIf
      Case "-out"
        Debug "found -out"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\outFilename$ = datares$
          Sprint( "-out "+settings("1")\outFilename$,#colordefault)
        EndIf
      Case "-map"
        Debug "found -map"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\mapFilename$ = datares$
          Sprint( "-map "+settings("1")\mapFilename$,#colordefault)
        EndIf 
      Case "-address"
        Debug "found -address"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\address$ = datares$
          Sprint( "-address "+settings("1")\address$,#colordefault)
        EndIf           
    EndSelect
    i+1 
  Wend
  
  Debug "all params["+params$+"]"
ProcedureReturn params$
EndProcedure

Procedure.s m_gethex32(*bin, szbytes)  
  Protected *sertemp=AllocateMemory(szbytes*2, #PB_Memory_NoClear)
  Protected res$  
  ;************************************************************************
  ;Convert bytes to HEX string 
  ;************************************************************************ 
  Curve::m_serializeX64(*bin,0,*sertemp,szbytes/4)  
  res$=PeekS(*sertemp,szbytes*2, #PB_Ascii)
  FreeMemory(*sertemp)
ProcedureReturn res$
EndProcedure

Procedure m_sethex32(*bin, *hash, szbytes)
  Protected a$=PeekS(*hash), i
  ;************************************************************************
  ;Convert HEX string to bytes 
  ;************************************************************************
  a$ = m_cutHex(a$)
  a$=RSet(a$,szbytes*2,"0")  
  Curve::m_deserializeX64(*bin,0,@a$,szbytes/4)  
EndProcedure


Procedure initrange()
  Protected *bufferResult, i,*buffer, *one,*bufferEnd
  Shared *rangeB,*rangeE, range()
  Shared settings()
  
  *bufferResult=AllocateMemory(32)
  *buffer=AllocateMemory(32)
  *one=AllocateMemory(32)
  *bufferEnd=AllocateMemory(32)
  m_sethex32(*one, @"1", 32)

  Curve::m_subX64(*bufferResult, *rangeE, *rangeB)
  For i = 0 To settings("1")\divpow-1  
    Curve::m_shrX64(*bufferResult)
  Next i
  ;Sprint(m_gethex32(*bufferResult, 32),#colorDarkgrey)
  CopyMemory(*bufferResult,*buffer,32)
  CopyMemory(*rangeB,*bufferResult,32)
  For i = 0 To settings("1")\deviderint-1
    Curve::m_addX64(*bufferEnd,*buffer,*bufferResult)
    AddElement(range())
    range()\id = i
    range()\rb$=m_gethex32(*bufferResult, settings("1")\maxbyte)
    range()\re$=m_gethex32(*bufferEnd, settings("1")\maxbyte)
    Debug "rb:"+range()\rb$
    Debug "re:"+range()\re$
    Curve::m_addX64(*bufferResult,*bufferEnd,*one)
    If Not i%10000
      Print(Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Str(i))
    EndIf
  Next i
  PrintN("")
FreeMemory(*bufferResult)
FreeMemory(*buffer)
FreeMemory(*one)
FreeMemory(*bufferEnd)

EndProcedure
  
Procedure savenewmap() 
  Protected err=0, i,Text$, *MemoryBuffer=AllocateMemory(32)
  Shared range(), *rangeB,*rangeE, isFind, settings()
  
  If Not CreateFile(#File, settings("1")\mapFilename$ )   
      Sprint( "Can`t creat "+settings("1")\mapFilename$+" map file", #colorRed)
      err=1
  EndIf    
  If Not err
    WriteData(#File, *rangeB, 32)
    WriteData(#File, *rangeE, 32)
    WriteData(#File, @settings("1")\address$, #ADDRESSSIZE)
    PokeB(*MemoryBuffer,isFind) ;is not found
    WriteData(#File, *MemoryBuffer, 1)
    
    ResetList(range())
    While NextElement(range())
      m_sethex32(*MemoryBuffer, @range()\rb$, 32)
      WriteData(#File, *MemoryBuffer, 32)
      
      m_sethex32(*MemoryBuffer, @range()\re$, 32)
      WriteData(#File, *MemoryBuffer, 32)
      
      PokeB(*MemoryBuffer,0)
      WriteData(#File, *MemoryBuffer, 1)
      If Not i%10000
        Print(Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Str(i))
      EndIf
      i+1
    Wend
    PrintN("")
    CloseFile(#File)
  EndIf
  FreeMemory(*MemoryBuffer)
ProcedureReturn err
EndProcedure

Procedure cmpmapfile()
  Protected err=0, i,Text$, buf$, bufb$,bufend$, bufaddr$, *MemoryBuffer=AllocateMemory(#HEADERSIZE), procname$ = "[CMP] "
  Shared range(),  isFind, settings()
  
  If FileSize(settings("1")\mapFilename$)=-1 
      Sprint(procname$+ "Can`t found "+settings("1")\mapFilename$+" map file", #colorRed)
      err=1
  EndIf
  If Not err
    If ReadFile(#File, settings("1")\mapFilename$,#PB_File_SharedRead)
      ;read header
      ReadData(#File, *MemoryBuffer, #HEADERSIZE)      
      bufb$ = m_gethex32(*MemoryBuffer, settings("1")\maxbyte)  
      bufend$ = m_gethex32(*MemoryBuffer+32, settings("1")\maxbyte)  
      bufaddr$ = PeekS(*MemoryBuffer+64,#ADDRESSSIZE)
      isFind = PeekB(*MemoryBuffer+64+#ADDRESSSIZE)
      Sprint(procname$+"Header:", #colorBrown)
      Sprint(procname$+bufb$+" "+bufend$+" "+bufaddr$+" ["+Str(isFind)+"]", #colorBrown)
      If CompareMemoryString(@bufb$, @settings("1")\rangeB$ ,#PB_String_NoCase)<>#PB_String_Equal
        err=2
        Sprint(procname$+ "Invalid begin range in header", #colorRed)
      EndIf
      If CompareMemoryString(@bufend$, @settings("1")\rangeE$ ,#PB_String_NoCase)<>#PB_String_Equal
        err=2
        Sprint(procname$+ "Invalid end range in header", #colorRed)
      EndIf
      If CompareMemoryString(@bufaddr$, @settings("1")\address$ ,#PB_String_CaseSensitive)<>#PB_String_Equal
        err=2
        Sprint(procname$+ "Invalid address in header", #colorRed)
      EndIf
      
      CloseFile(#File)
    Else
      Sprint(procname$+ "Can`t read "+settings("1")\mapFilename$+" map file", #colorRed)
      err=1
    EndIf
  EndIf
  FreeMemory(*MemoryBuffer)
ProcedureReturn err  
EndProcedure

Procedure loadmap() 
  Protected err=0, i, buf$, bufb$,bufend$, bufaddr$, *MemoryBuffer=AllocateMemory(#HEADERSIZE), procname$ = "[LDMAP] ", isScan
  Shared range(),  isFind, settings(), totalscanned
  
  If Not ReadFile(#File, settings("1")\mapFilename$ )   
      Sprint( "Can`t read "+settings("1")\mapFilename$+" map file", #colorRed)
      err=1
  EndIf
  If Not err
      ReadData(#File, *MemoryBuffer, #HEADERSIZE)      
      bufb$ = m_gethex32(*MemoryBuffer, settings("1")\maxbyte)  
      bufend$ = m_gethex32(*MemoryBuffer+32, settings("1")\maxbyte)  
      bufaddr$ = PeekS(*MemoryBuffer+64,#ADDRESSSIZE)
      isFind = PeekB(*MemoryBuffer+64+#ADDRESSSIZE)
      Sprint(procname$+"Header:", #colorBrown)
      Sprint(procname$+bufb$+" "+bufend$+" "+bufaddr$+" ["+Str(isFind)+"]", #colorBrown)
      If CompareMemoryString(@bufb$, @settings("1")\rangeB$ ,#PB_String_NoCase)<>#PB_String_Equal
        err=2
        Sprint( procname$+"Invalid begin range in header", #colorRed)
      EndIf
      If CompareMemoryString(@bufend$, @settings("1")\rangeE$ ,#PB_String_NoCase)<>#PB_String_Equal
        err=2
        Sprint(procname$+ "Invalid end range in header", #colorRed)
      EndIf
      If CompareMemoryString(@bufaddr$, @settings("1")\address$ ,#PB_String_CaseSensitive)<>#PB_String_Equal
        err=2
        Sprint(procname$+ "Invalid address in header", #colorRed)
      EndIf
      If Not err
        i=0
        While Not Eof(#File)
          ReadData(#File, *MemoryBuffer, #DATASIZE)
          isScan = PeekB(*MemoryBuffer+64)
          If isScan=1
            ;ew don`t need this data anymore
            totalscanned+1
          Else
            AddElement(range())
            range()\id = i
            range()\rb$ = m_gethex32(*MemoryBuffer, settings("1")\maxbyte) 
            range()\re$ = m_gethex32(*MemoryBuffer+32, settings("1")\maxbyte)
            Debug "range("+Str(i)+") not scanned"
            
          EndIf
          
          i+1
          If Not i%10000
            Print(Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Chr(8)+Str(i))
          EndIf
        Wend
        PrintN("")
        If i<>settings("1")\deviderint
          Sprint(procname$+ "Need ["+Str(settings("1")\deviderint)+" items, got ["+Str(i)+"]", #colorRed)
          err=3
        EndIf
      Else
        Sprint( procname$+"Invalid header", #colorRed)
      EndIf
    CloseFile(#File)
  EndIf
  FreeMemory(*MemoryBuffer)
ProcedureReturn err
EndProcedure

Procedure updatemap(nline) 
  Protected err=0, i, buf$, bufb$,bufend$, bufaddr$, *MemoryBuffer=AllocateMemory(#HEADERSIZE), pos, procname$ = "[UPDATEMAP] "
  Shared range(), settings()
  
  If Not OpenFile(#File, settings("1")\mapFilename$ ,#PB_File_SharedRead)   
      Sprint( "Can`t open "+settings("1")\mapFilename$+" map file", #colorRed)
      err=1
  EndIf
  If Not err
      ReadData(#File, *MemoryBuffer, #HEADERSIZE)      
      bufb$ = m_gethex32(*MemoryBuffer, settings("1")\maxbyte)  
      bufend$ = m_gethex32(*MemoryBuffer+32, settings("1")\maxbyte)  
      bufaddr$ = PeekS(*MemoryBuffer+64,#ADDRESSSIZE)
      ;Sprint("Header:", #colorBrown)
      ;Sprint(bufb$+" "+bufend$+" "+bufaddr$, #colorBrown)
      If CompareMemoryString(@bufb$, @settings("1")\rangeB$ ,#PB_String_NoCase)<>#PB_String_Equal
        err=2
        Sprint( procname$+"Invalid begin range in header", #colorRed)
      EndIf
      If CompareMemoryString(@bufend$, @settings("1")\rangeE$ ,#PB_String_NoCase)<>#PB_String_Equal
        err=2
        Sprint(procname$+ "Invalid end range in header", #colorRed)
      EndIf
      If CompareMemoryString(@bufaddr$, @settings("1")\address$ ,#PB_String_CaseSensitive)<>#PB_String_Equal
        err=2
        Sprint(procname$+ "Invalid address in header", #colorRed)
      EndIf
      If Not err
        pos=#HEADERSIZE+#DATASIZE*nline+#DATASIZE-1
        FileSeek(#File, pos ,#PB_Absolute)
        
        
        PokeB(*MemoryBuffer,1)
        WriteData(#File, *MemoryBuffer, 1)
      Else
        Sprint(procname$+ "Invalid header", #colorRed)
      EndIf
    CloseFile(#File)
  EndIf
  FreeMemory(*MemoryBuffer)
ProcedureReturn err
EndProcedure

Procedure updatfind() 
  Protected err=0, i, buf$, bufb$,bufend$, bufaddr$, *MemoryBuffer=AllocateMemory(#HEADERSIZE), pos, procname$ = "[UPDATEHEADER] "
  Shared range(), settings(), isFind
  
  If Not OpenFile(#File, settings("1")\mapFilename$ ,#PB_File_SharedRead)   
      Sprint( "Can`t open "+settings("1")\mapFilename$+" map file", #colorRed)
      err=1
  EndIf
  If Not err
      ReadData(#File, *MemoryBuffer, #HEADERSIZE)      
      bufb$ = m_gethex32(*MemoryBuffer, settings("1")\maxbyte)  
      bufend$ = m_gethex32(*MemoryBuffer+32, settings("1")\maxbyte)  
      bufaddr$ = PeekS(*MemoryBuffer+64,#ADDRESSSIZE)
      Sprint(procname$+"Header:", #colorBrown)
      Sprint(procname$+bufb$+" "+bufend$+" "+bufaddr$+" [["+Str(isFind)+"]", #colorBrown)
      If CompareMemoryString(@bufb$, @settings("1")\rangeB$ ,#PB_String_NoCase)<>#PB_String_Equal
        err=2
        Sprint(procname$+ "Invalid begin range in header", #colorRed)
      EndIf
      If CompareMemoryString(@bufend$, @settings("1")\rangeE$ ,#PB_String_NoCase)<>#PB_String_Equal
        err=2
        Sprint( procname$+"Invalid end range in header", #colorRed)
      EndIf
      If CompareMemoryString(@bufaddr$, @settings("1")\address$ ,#PB_String_CaseSensitive)<>#PB_String_Equal
        err=2
        Sprint( procname$+"Invalid address in header", #colorRed)
      EndIf
      If Not err
        pos=#HEADERSIZE-1
        FileSeek(#File, pos ,#PB_Absolute)
                
        PokeB(*MemoryBuffer,isFind)
        WriteData(#File, *MemoryBuffer, 1)
      Else
        Sprint(procname$+"Invalid header", #colorRed)
      EndIf
    CloseFile(#File)
  EndIf
  FreeMemory(*MemoryBuffer)
ProcedureReturn err
EndProcedure

Procedure.i SendQuestionstratum(con_id,string$)
 Protected err
 err=0
 
  If con_id
    SendNetworkString(con_id,string$+#LF$,#PB_Ascii)
    Debug "send to miner :"+Str(con_id)+">"+string$
  EndIf
 ProcedureReturn err  
EndProcedure

Procedure Socket_ClientDisconnect(ClientID.i, *Server.ServerStructure) 
  If FindMapElement(*Server\Client(), Str(ClientID))    
    DeleteMapElement(*Server\Client(), Str(ClientID))   
    ;SPrint("TOTAL CLIENTS: " + Str(MapSize(*Server\Client())),#colorMagenta)   
  EndIf
EndProcedure

Procedure checkkey(key$)
  Protected *cx,*cy, *key, pk$, pb$, pa$, pbtemp$, err=0
  Shared *CurveP, *CurveGX, *CurveGY, *Curveqn, settings()
  *cx=AllocateMemory(96)
  *cy=*cx+32
  *key=*cx+64
  pa$ = StringField(key$,1," ")
  pk$ = StringField(key$,2," ")
  pb$ = StringField(key$,3," ")
  If pk$ And pb$
    pk$=RSet(pk$,64,"0")
    If Len(pb$)=66
      pb$ = Right(pb$,64)
    EndIf    
    m_sethex32(*key, @pk$, 32)  
    Curve::m_PTMULX64(*cx, *cy, *CurveGX, *CurveGY, *key,*CurveP)
    pbtemp$ = m_gethex32(*cx, 32)
    If CompareMemoryString(@pb$, @pbtemp$, #PB_String_NoCase)<>#PB_String_Equal
      Sprint("[CHECKER] INVALID COLLISION ["+pbtemp$+"]["+pb$+"]",#colorMagenta)
      err=1
    EndIf
    If Not err
      If CompareMemoryString(@pa$, @settings("1")\address$, #PB_String_NoCase)<>#PB_String_Equal
        Sprint("[CHECKER] INVALID ADDRESS ["+pa$+"]["+settings("1")\address$+"]",#colorMagenta)
        err=1
      EndIf
    EndIf
  Else
    Sprint("PB or PK is zero",#colorMagenta)
    err=1
  EndIf
  FreeMemory(*cx)
ProcedureReturn err
EndProcedure

Procedure.s keychecker(key$)
  Protected *cx,*cy, *key, pk$, pc$, pbtemp$, res$
  Shared *CurveP, *CurveGX, *CurveGY, *Curveqn, settings()
  *cx=AllocateMemory(96)
  *cy=*cx+32
  *key=*cx+64
  
    pk$=RSet(key$,64,"0")       
    m_sethex32(*key, @pk$, 32)  
    Curve::m_PTMULX64(*cx, *cy, *CurveGX, *CurveGY, *key,*CurveP)
    pbtemp$ = m_gethex32(*cx, 32)+m_gethex32(*cy, 32)
    pc$ = uncomressed2commpressedPub(pbtemp$)  
    res$ = getBTCAddresFromPub(pc$)
  FreeMemory(*cx)
ProcedureReturn res$
EndProcedure

Procedure validatekeys(key1$,key2$, powaddress$, address$)
  Protected addr$, res=0
  If key1$<>""
    addr$ = keychecker(key1$)
    If CompareMemoryString(@addr$, @powaddress$, #PB_String_NoCase)<>#PB_String_Equal
      Sprint("[CHECKER] INVALID POW KEY ["+key1$+"]",#colorMagenta)
      Sprint("[CHECKER] ADDR ["+addr$+"] NOT EQUIL TO ["+powaddress$+"]",#colorMagenta)
    Else
      ;valid powkey
      res = res|1
      Sprint("[CHECKER] POW KEY VALID",#colorCyan)
    EndIf
  EndIf
     
  If key2$<>""
    addr$ = keychecker(key2$)
    If CompareMemoryString(@addr$, @address$, #PB_String_NoCase)<>#PB_String_Equal
      Sprint("[CHECKER] INVALID SOLUTION KEY ["+key2$+"]",#colorMagenta)
      Sprint("[CHECKER] ADDR ["+addr$+"] NOT EQUIL TO ["+address$+"]",#colorMagenta)
    Else
      Sprint("[CHECKER] SOLUTION KEY VALID",#colorGreen)
      res = res|2
    EndIf
  EndIf
  
  ProcedureReturn res
EndProcedure

Procedure AnaliseRequest(reqid)
  Protected answer_f$, metod_json_answer$, id_json_answer, pars_res, name$, pass$, tempjson, get_work, get_resfalse.s, hash$, Values, currange, totalN, randomcurrange, powaddress$, key1$, key2$, nopow
  Protected ClientID, ClientIP, key$, *Buffer, starttime, res
  Protected procname$ = "[SERVER] ", Error$, *q.RangeStructure
  
  Shared MutexClientMap,  settings(),  socketreq(), socketreqMutex, Server.ServerStructure, job(), isFind, LogMutex,RndMutex, totalscanned, range(),  lastserialsearch, range_mapper, *range_task()
  
  Debug( "Thread AnaliseRequest #"+Str(reqid)+" started") 
  
  ClientID = socketreq(Str(reqid))\ClientId
  ClientIP = Server\Client(Str(ClientID))\ClientIP
  answer_f$ = socketreq(Str(reqid))\req
   If Server\Client(Str(ClientID))\authorization
     procname$ + Str(ClientID)+"\"+IPString(ClientIP)+" ["+Server\Client(Str(ClientID))\name+"] "
  EndIf
   
  pars_res=ParseJSON(#PB_Any, answer_f$) 
  If pars_res 
    metod_json_answer$ = LCase(getElem(pars_res,"method",0))
    id_json_answer = Val(LCase(getElem(pars_res,"id",0)))
                  
    Select metod_json_answer$
      Case "login"         
        If Not Server\Client(Str(ClientID))\authorization
            If isFind=#False
              name$=LCase(getElem(pars_res,"params",0))
              pass$ = getElem(pars_res,"params",1)
              nopow = Val(getElem(pars_res,"params",2))
              If Len(name$)>45
                name$=Left(name$,45)
              EndIf
              If name$         
                LockMutex(MutexClientMap)                  
                  Server\Client(Str(ClientID))\name = name$           
                  Server\Client(Str(ClientID))\authorization = #True 
                  Server\Client(Str(ClientID))\nopow = nopow
                UnlockMutex(MutexClientMap) 
                
                ;Sprint(procname$+Str(ClientID)+"\"+IPString(ClientIP)+" AUTORIZED AS ["+name$ + "] NOPOW ["+Str(nopow)+"]",#colorMagenta)
                tempjson = CreateJSON(#PB_Any)
                If tempjson 
                  get_work = SetJSONObject(JSONValue(tempjson))
                  SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
                  SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
                  SetJSONBoolean(AddJSONMember(get_work, "result"), #True)                   
                  SetJSONNull(AddJSONMember(get_work, "error"))
                  get_resfalse.s=ComposeJSON(tempjson)
                  FreeJSON(tempjson)
                  SendQuestionstratum(ClientID,get_resfalse)
                EndIf
              
              Else 
                Error$ = "invalid_login"
              EndIf
          
              
            Else
              Error$ = "keyfounded"  
            EndIf 
        Else
           Error$ = "Already_authorized" 
        EndIf
        
        If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
        
      Case "checkjob"
        If Server\Client(Str(ClientID))\authorization
          hash$=LCase(getElem(pars_res,"params",0))        
          If FindMapElement(job(),hash$)
            tempjson = CreateJSON(#PB_Any)
            If tempjson 
              get_work = SetJSONObject(JSONValue(tempjson))
              SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
              SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
              SetJSONBoolean(AddJSONMember(get_work, "result"), #True)                   
              SetJSONNull(AddJSONMember(get_work, "error"))
              get_resfalse.s=ComposeJSON(tempjson)
              FreeJSON(tempjson)
              SendQuestionstratum(ClientID,get_resfalse)
            EndIf
          Else
            Error$ = "job_no_longer_exist"            
          EndIf
        Else
          Error$ = "Unauthorized"                              
        EndIf                           
        
        If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
        
      Case "getwork"
        If Server\Client(Str(ClientID))\authorization
          LockMutex(RndMutex)
          If Not range_mapper\count=-1 Or ListSize(*range_task())>0            
            starttime = ElapsedMilliseconds()
            If Not range_mapper\count=-1
              ;*q = GetRandomMapElement()
              *q = GetWalkMapElement()
              
                  
                  hash$=*q\rb$+RSet(Hex(ElapsedMilliseconds()),8,"0")+*q\re$+Server\Client(Str(ClientID))\name
                  *Buffer=AllocateMemory(Len(hash$))
                  PokeS(*Buffer,hash$, Len(hash$), #PB_Ascii)
                  hash$ = LCase(SHA1Fingerprint(*Buffer, Len(hash$)))
                  FreeMemory(*Buffer)                
                  job(hash$)\jobstarttime = Date()
                  job(hash$)\rangeid = *q\id
                  job(hash$)\name = Server\Client(Str(ClientID))\name
                  
                  If Server\Client(Str(ClientID))\nopow=0
                    powaddress$ = genPOWaddress(*q\rb$,*q\re$) ;--genPowaddress
                    job(hash$)\powaddress$ = powaddress$
                  Else
                    powaddress$=""
                  EndIf
                  
                  tempjson = CreateJSON(#PB_Any)
                  If tempjson   
                      get_work = SetJSONObject(JSONValue(tempjson))   
                      SetJSONInteger(AddJSONMember(get_work, "id"), 0)       
                      Values =SetJSONArray(AddJSONMember(get_work, "result"))
                      SetJSONString(AddJSONElement(Values), hash$)
                      SetJSONString(AddJSONElement(Values), *q\rb$)
                      SetJSONString(AddJSONElement(Values), *q\re$) 
                      SetJSONString(AddJSONElement(Values), settings("1")\address$)
                      SetJSONString(AddJSONElement(Values), powaddress$)  
                      SetJSONNull(AddJSONMember(get_work, "error"))
                      get_resfalse=ComposeJSON(tempjson)
                      FreeJSON(tempjson)
                      SendQuestionstratum(ClientID,get_resfalse)
                  EndIf
                  Sprint(procname$+" ASK JOB, RANGE["+job(hash$)\rangeid+"] HASH ["+hash$+"] POW ["+powaddress$+"] in "+Str(ElapsedMilliseconds()-starttime)+"ms",#colorDarkgrey)  
              Else
                  Sprint(procname$+"Here some buzzy subranges but other subranges scanned!",#colorRed)
                  Error$ = "range_scanned" 
              EndIf
            
          Else
            Sprint(procname$+"All range scanned!",#colorRed)
            Error$ = "range_scanned" 
          EndIf
          UnlockMutex(RndMutex)
        Else
          Error$ = "Unauthorized"                              
        EndIf                           
        
        If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
        
      Case "submitwork"
        If Server\Client(Str(ClientID))\authorization
          hash$=LCase(getElem(pars_res,"params",0)) 
          key1$=LCase(getElem(pars_res,"params",1))
          key2$=LCase(getElem(pars_res,"params",2))
          
          If FindMapElement(job(),hash$)
            LockMutex(RndMutex)
              powaddress$ = job(hash$)\powaddress$
            UnlockMutex(RndMutex)
            res = validatekeys(key1$,key2$,powaddress$,settings("1")\address$)
            If res>1              
                ;key solved
                SPrint (procname$+"*** "+key2$+" ***"+Chr(13),#colorGreen)
                LockMutex(LogMutex)
                If  CreateFile(#LOGFILE, FormatDate("%dd_%mm-%hh_%ii_%ss ", Date())+"_"+settings("1")\outFilename$,#PB_File_SharedRead )
                   WriteStringN(#LOGFILE,FormatDate("%dd/%mm/%hh:%ii:%ss:", Date())+key2$+"  "+settings("1")\address$,#PB_UTF8)
                   FlushFileBuffers(#LOGFILE)
                   CloseFile(#LOGFILE)
                EndIf
                UnlockMutex(LogMutex)
                
                isFind=#True
                If Not updatfind()
                  Sprint(procname$+"header updated",#colorDarkgrey)
                EndIf
                Sprint(procname$+" SUBMIT KEY!",#colorGreen)
            EndIf
            
            If res Or Server\Client(Str(ClientID))\nopow=1
              If res&1 Or Server\Client(Str(ClientID))\nopow=1
                LockMutex(RndMutex)

                If updatemap(job(hash$)\rangeid)
                  Sprint("Can`t update map!",#colorRed)
                Else
                  Sprint("map ["+Str(job(hash$)\rangeid)+"] updated",#colorDarkgrey)
                  
                EndIf
                Sprint(procname$+" SUBMIT JOB, RANGE["+job(hash$)\rangeid+"] HASH ["+hash$+"]",#colorDarkgrey)
                totalscanned+1
                DelElementFromTask(job(hash$)\rangeid)
                DeleteMapElement(job(),hash$)
                UnlockMutex(RndMutex)
              EndIf
              
                tempjson = CreateJSON(#PB_Any)
                If tempjson 
                  get_work = SetJSONObject(JSONValue(tempjson))
                  SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
                  SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
                  SetJSONBoolean(AddJSONMember(get_work, "result"), #True) 
                  SetJSONString(AddJSONMember(get_work, "error"),Error$)
                  get_resfalse.s=ComposeJSON(tempjson)
                  FreeJSON(tempjson)
                  SendQuestionstratum(ClientID,get_resfalse)
                EndIf
              
              If Not Val(SQLTxstate_Get("SELECT id FROM person_table WHERE name='"+Server\Client(Str(ClientID))\name+"'" ))
                SQLTxstate_Set("INSERT INTO person_table (name, jobscan, jobreject) VALUES ('"+Server\Client(Str(ClientID))\name+"', 1, 0)")
              Else
                SQLTxstate_Set("UPDATE person_table SET jobscan=jobscan+1 WHERE name ='"+Server\Client(Str(ClientID))\name+"'")
              EndIf
            Else              
              Error$ = "invalid_pow_result" 
              If Not Val(SQLTxstate_Get("SELECT id FROM person_table WHERE name='"+Server\Client(Str(ClientID))\name+"'" ))
                SQLTxstate_Set("INSERT INTO person_table (name, jobscan, jobreject) VALUES ('"+Server\Client(Str(ClientID))\name+"', 0, 1)")
              Else
                SQLTxstate_Set("UPDATE person_table SET jobreject=jobreject+1 WHERE name ='"+Server\Client(Str(ClientID))\name+"'")
              EndIf
            EndIf            
                                 
          Else
            Error$ = "invalid_job" 
            If Not Val(SQLTxstate_Get("SELECT id FROM person_table WHERE name='"+Server\Client(Str(ClientID))\name+"'" ))
              SQLTxstate_Set("INSERT INTO person_table (name, jobscan, jobreject) VALUES ('"+Server\Client(Str(ClientID))\name+"', 0, 1)")
            Else
              SQLTxstate_Set("UPDATE person_table SET jobreject=jobreject+1 WHERE name ='"+Server\Client(Str(ClientID))\name+"'")
            EndIf
          EndIf
        Else
          Error$ = "Unauthorized"                              
        EndIf                           
        
        If Error$
          tempjson = CreateJSON(#PB_Any)
          If tempjson 
            get_work = SetJSONObject(JSONValue(tempjson))
            SetJSONString(AddJSONMember(get_work, "jsonrpc"), "2.0")
            SetJSONInteger(AddJSONMember(get_work, "id"), id_json_answer) 
            SetJSONBoolean(AddJSONMember(get_work, "result"), #False) 
            SetJSONString(AddJSONMember(get_work, "error"),Error$)
            get_resfalse.s=ComposeJSON(tempjson)
            FreeJSON(tempjson)
            SendQuestionstratum(ClientID,get_resfalse)
          EndIf 
          Sprint(procname$+"REQUEST REJECTED => "+LCase(Error$), #colorRed)
        EndIf
    EndSelect
    
      
        
         
    
    If IsJSON(pars_res)
      FreeJSON(pars_res) 
    EndIf
  EndIf
  
  LockMutex(socketreqMutex)
    DeleteMapElement(socketreq(), Str(reqid))
  UnlockMutex(socketreqMutex)
  Debug( "Thread AnaliseRequest #"+Str(reqid)+" ended") 
    
EndProcedure

Procedure Socket_Server(*Server.ServerStructure)
  
  Protected.i SEvent, ClientID, i, ClientIP,  ReceivedBytes
  Protected answer_t$, answer_f$, pos, pos2, pars_res, metod_json_answer$, id_json_answer, get_work, get_restrue.s, resultArr, ArrayValue, resultArr_1, mining_set_difficulty.s, filename$, computeCRC32file
  Protected full_size
  Protected  get_true.s, get_resfalse.s, poolid, socket, answer_ff$, ShareID, curmill
  Protected name$, worker$, posname,  a$, pass$
  Protected NewMap templist(), socketreqN, zz
  Protected *Buffer, counter, tempjson, extr$, mining_subscribe_answer.s, flagidpool

  Shared MutexClientMap, settings(), socketreq(), socketreqMutex, stratumwork_g
  
  SPrint("[SERVER] started and listen port:"+Str(settings("1")\port),#colorGreen)
  
  *Buffer = AllocateMemory(65536)
  If *Buffer
    Repeat
      stratumwork_g = 0
      ;-delete unatorized users
      counter+1
      If counter&$1ff=0 
      
        LockMutex(MutexClientMap)        
          ForEach *Server\Client()        
            If *Server\Client()\time And Not*Server\Client()\authorization 
              ; if connected but not autorized more than 2s
              If Date() - *Server\Client()\time > 2              
                  templist(Str(*Server\Client()\ClientID))=*Server\Client()\quit        
              EndIf 
            EndIf
          Next
        UnlockMutex(MutexClientMap)       
       
        ForEach templist()
          If templist()=#False
            If Val(MapKey(templist()))>0
              CloseNetworkConnection(Val(MapKey(templist())))
            EndIf
            Debug( "CLIENT: "+MapKey(templist()) + " TIME OUT. KICK...")
          Else
            Debug( "CLIENT: "+MapKey(templist()) + " DISCONNECTED...")
          EndIf
          LockMutex(MutexClientMap)
            Socket_ClientDisconnect(Val(MapKey(templist())), *Server)
          UnlockMutex(MutexClientMap)
        Next
        ClearMap(templist())
      EndIf
      
      SEvent = NetworkServerEvent(#Server)
      If SEvent 
        ClientID = EventClient()
        ClientIP = GetClientIP(ClientID)
        Select SEvent
          Case #PB_NetworkEvent_Connect
            
              LockMutex(MutexClientMap)
                *Server\Client(Str(ClientID))\ClientId = ClientID
                *Server\Client(Str(ClientID))\ClientIP = ClientIP                                     
                *Server\Client(Str(ClientID))\time = Date()
                *Server\Client(Str(ClientID))\authorization = #False
                *Server\Client(Str(ClientID))\quit = #False
                
              UnlockMutex(MutexClientMap)
                
              ;SPrint( "CLIENT: "+Str(ClientID)+" IP: "+IPString(*Server\Client(Str(ClientID))\ClientIP)+" HAS CONNECTED !",#colorMagenta)
              ;SPrint("TOTAL CLIENTS: " + Str(MapSize(*Server\Client())),#colorMagenta)
              
            Case #PB_NetworkEvent_Data
              *Server\Client(Str(ClientID))\time = Date()
              ;FillMemory(*Buffer, 10000)
              ReceivedBytes = ReceiveNetworkData(ClientID, *Buffer, 65536)            
              If ReceivedBytes>=0
                Debug "recieved "+Str(ReceivedBytes)+" bytes"                
                  answer_t$=PeekS(*Buffer, ReceivedBytes,#PB_Ascii)  
                  Debug answer_t$
                  Repeat              
                    pos=1
                    pos=FindString(answer_t$, "{",pos)
                    pos2=FindString(answer_t$, "}",pos)
                    If pos And pos2
                      zz= CountString(Mid(answer_t$,pos,pos2-pos),"{")  
                      While zz    
                        pos2=FindString(answer_t$, "}",pos2)
                        If pos2
                          zz-1
                          pos2+1
                        Else
                          zz=0
                        EndIf    
                      Wend
                      answer_f$ = Mid(answer_t$,pos,pos2-pos)  
                      
                      LockMutex(socketreqMutex)
                      
                      If MapSize(socketreq())=0
                        socketreqN = 0
                      EndIf
                      
                      socketreq(Str(socketreqN))\id = socketreqN
                      socketreq(Str(socketreqN))\ClientId = ClientID
                      socketreq(Str(socketreqN))\req = answer_f$    
                      UnlockMutex(socketreqMutex)
                      ;CreateThread(@AnaliseRequest(),socketreqN)
                      AnaliseRequest(socketreqN)
                      
                     
                      socketreqN+1
                        
                      
                   
                      answer_t$ = Mid(answer_t$,pos2) 
                      pos2+1
                     EndIf     
                  Until  pos=0 Or pos2=0  
                                
             EndIf
          Case #PB_NetworkEvent_Disconnect
          ;SPrint( "CLIENT: "+Str(ClientID) + " IP:"+IPString(*Server\Client(Str(ClientID))\ClientIP)+" DISCONNECT (CLIENT SIDE)",#colorMagenta)
          LockMutex(MutexClientMap)
              *Server\Client(Str(ClientID))\quit = #True
              Socket_ClientDisconnect(ClientID, *Server)
          UnlockMutex(MutexClientMap)
              
      EndSelect
    EndIf
    Delay(1)    
    Until *Server\Quit   
    FreeMemory(*Buffer)
    FreeMap(templist())
  Else
    SPrint("Can't allocate buffer",#colorred)
  EndIf  
EndProcedure

Procedure timer60s(i)
  Protected procname$ ="[JOBTIMER] "
  Shared RndMutex, job(), settings(),isFind, range()
  SPrint(procname$+"Started",#colorCyan)
  While isFind=#False
    LockMutex(RndMutex)
    ForEach job()
      If Date()-job()\jobstarttime>settings("1")\jobtimeout
        SPrint(procname$+"JOB["+MapKey(job())+"] TIMEOUT ["+job()\name+"]",#colorCyan)        
        ReturnElement(job()\rangeid)        
        DeleteMapElement(job(),MapKey(job()))
      EndIf
    Next
    UnlockMutex(RndMutex)
    Delay(60000)
  Wend
EndProcedure

Procedure ErrorHandler()
  Protected ErrorMessage$

  ErrorMessage$ = "A program error was detected:" + Chr(13) 
  ErrorMessage$ + Chr(13)
  ErrorMessage$ + "Error Message:   " + ErrorMessage()      + Chr(13)
  ErrorMessage$ + "Error Code:      " + Str(ErrorCode())    + Chr(13)  
  ErrorMessage$ + "Code Address:    " + Str(ErrorAddress()) + Chr(13)
 
  If ErrorCode() = #PB_OnError_InvalidMemory   
    ErrorMessage$ + "Target Address:  " + Str(ErrorTargetAddress()) + Chr(13)
  EndIf
 
  If ErrorLine() = -1
    ErrorMessage$ + "Sourcecode line: Enable OnError lines support to get code line information." + Chr(13)
  Else
    ErrorMessage$ + "Sourcecode line: " + Str(ErrorLine()) + Chr(13)
    ErrorMessage$ + "Sourcecode file: " + ErrorFile() + Chr(13)
  EndIf
 
  ErrorMessage$ + Chr(13)
  ErrorMessage$ + "Register content:" + Chr(13)
 
  CompilerSelect #PB_Compiler_Processor 
    CompilerCase #PB_Processor_x86
      ErrorMessage$ + "EAX = " + Str(ErrorRegister(#PB_OnError_EAX)) + Chr(13)
      ErrorMessage$ + "EBX = " + Str(ErrorRegister(#PB_OnError_EBX)) + Chr(13)
      ErrorMessage$ + "ECX = " + Str(ErrorRegister(#PB_OnError_ECX)) + Chr(13)
      ErrorMessage$ + "EDX = " + Str(ErrorRegister(#PB_OnError_EDX)) + Chr(13)
      ErrorMessage$ + "EBP = " + Str(ErrorRegister(#PB_OnError_EBP)) + Chr(13)
      ErrorMessage$ + "ESI = " + Str(ErrorRegister(#PB_OnError_ESI)) + Chr(13)
      ErrorMessage$ + "EDI = " + Str(ErrorRegister(#PB_OnError_EDI)) + Chr(13)
      ErrorMessage$ + "ESP = " + Str(ErrorRegister(#PB_OnError_ESP)) + Chr(13)
 
    CompilerCase #PB_Processor_x64
      ErrorMessage$ + "RAX = " + Str(ErrorRegister(#PB_OnError_RAX)) + Chr(13)
      ErrorMessage$ + "RBX = " + Str(ErrorRegister(#PB_OnError_RBX)) + Chr(13)
      ErrorMessage$ + "RCX = " + Str(ErrorRegister(#PB_OnError_RCX)) + Chr(13)
      ErrorMessage$ + "RDX = " + Str(ErrorRegister(#PB_OnError_RDX)) + Chr(13)
      ErrorMessage$ + "RBP = " + Str(ErrorRegister(#PB_OnError_RBP)) + Chr(13)
      ErrorMessage$ + "RSI = " + Str(ErrorRegister(#PB_OnError_RSI)) + Chr(13)
      ErrorMessage$ + "RDI = " + Str(ErrorRegister(#PB_OnError_RDI)) + Chr(13)
      ErrorMessage$ + "RSP = " + Str(ErrorRegister(#PB_OnError_RSP)) + Chr(13)
      ErrorMessage$ + "Display of registers R8-R15 skipped."         + Chr(13)
 
    CompilerCase #PB_Processor_PowerPC
      ErrorMessage$ + "r0 = " + Str(ErrorRegister(#PB_OnError_r0)) + Chr(13)
      ErrorMessage$ + "r1 = " + Str(ErrorRegister(#PB_OnError_r1)) + Chr(13)
      ErrorMessage$ + "r2 = " + Str(ErrorRegister(#PB_OnError_r2)) + Chr(13)
      ErrorMessage$ + "r3 = " + Str(ErrorRegister(#PB_OnError_r3)) + Chr(13)
      ErrorMessage$ + "r4 = " + Str(ErrorRegister(#PB_OnError_r4)) + Chr(13)
      ErrorMessage$ + "r5 = " + Str(ErrorRegister(#PB_OnError_r5)) + Chr(13)
      ErrorMessage$ + "r6 = " + Str(ErrorRegister(#PB_OnError_r6)) + Chr(13)
      ErrorMessage$ + "r7 = " + Str(ErrorRegister(#PB_OnError_r7)) + Chr(13)
      ErrorMessage$ + "Display of registers r8-R31 skipped."       + Chr(13)
 
  CompilerEndSelect
  
   If  CreateFile(#LOGFILE, FormatDate("%dd_%mm-%hh_%ii_%ss ", Date())+"_error_log.txt",#PB_File_SharedRead )
     WriteStringN(#LOGFILE,FormatDate("%dd/%mm/%hh:%ii:%ss:", Date())+ErrorMessage$,#PB_UTF8)
     FlushFileBuffers(#LOGFILE)
     CloseFile(#LOGFILE)
   EndIf
   
End
EndProcedure
OnErrorCall(@ErrorHandler())

Define *maxwidthsubrange
UseSQLiteDatabase() 
OpenConsole()
InitNetwork()
settings("1")\port=8000
settings("1")\jobtimeout=86400
settings("1")\divpow=20
settings("1")\address$="16jY7qLJnxb7CHZyqBP8qca9d51gAjyXQN"
settings("1")\rangeB$="8000000000000000"
settings("1")\rangeE$="ffffffffffffffff"
settings("1")\outFilename$="winkey.txt"
settings("1")\mapFilename$="map.bin"
settings("1")\Databasedirectory$=GetCurrentDirectory() 

getprogparam()


settings("1")\deviderint=Int(Pow(2,settings("1")\divpow))
Define result, quit = #False,  i, StratServ, Thread, lastlogtime, procname$ = "[MAIN] ", *genhelper,*Rangetotal, eachbitinrange, wholebitinrange, *temp,*temp2, tempv,a$

*rangeB=AllocateMemory(32)
*rangeE=AllocateMemory(32)
*Rangetotal = AllocateMemory(32)
*genhelper= AllocateMemory(32)
*maxwidthsubrange = AllocateMemory(32)
*temp = AllocateMemory(32)
*temp2 = AllocateMemory(32)

m_sethex32(*maxwidthsubrange, @"CCCCCCCCCCCCCCC", 32);--define max width of subrange

settings("1")\maxbyte=Len(settings("1")\rangeE$)/2
If Len(settings("1")\rangeE$)>settings("1")\maxbyte*2
  settings("1")\maxbyte+1
EndIf
If settings("1")\maxbyte<4
  settings("1")\maxbyte=4
EndIf
If settings("1")\maxbyte%4
  settings("1")\maxbyte+4-settings("1")\maxbyte%4
EndIf
settings("1")\rangeB$=RSet(settings("1")\rangeB$,settings("1")\maxbyte*2,"0")
settings("1")\rangeE$=RSet(settings("1")\rangeE$,settings("1")\maxbyte*2,"0")
m_sethex32(*rangeB, @settings("1")\rangeB$, 32)
m_sethex32(*rangeE, @settings("1")\rangeE$, 32)

;calculate subrange length
Curve::m_subModX64(*Rangetotal,*rangeE,*rangeB,*CurveP)
   
CopyMemory(*Rangetotal,*genhelper,32)
    
wholebitinrange=0
While Curve::m_check_nonzeroX64(*genhelper)
  Curve::m_shrX64(*genhelper)
  wholebitinrange+1
Wend

CopyMemory(*Rangetotal,*genhelper,32)
eachbitinrange=settings("1")\divpow
While eachbitinrange
  Curve::m_shrX64(*genhelper)
  eachbitinrange-1
Wend

eachbitinrange = wholebitinrange - settings("1")\divpow

sprint("app version: "+#APPVERSION,#colorDarkgrey)
sprint("Devider        : 2^"+Str(settings("1")\divpow)+"="+Str(settings("1")\deviderint), #colorDarkgrey)
sprint("Range          : "+m_gethex32(*rangeB, settings("1")\maxbyte)+":"+m_gethex32(*rangeE, settings("1")\maxbyte),#colorDarkgrey)
sprint("Whole Range    : "+m_gethex32(*Rangetotal, settings("1")\maxbyte)+" = 2^"+Str(wholebitinrange),#colorDarkgrey)
sprint("Total Subranges: "+Str(settings("1")\deviderint),#colorDarkgrey)
sprint("Each   Subrange: "+m_gethex32(*genhelper, settings("1")\maxbyte)+" = 2^"+Str(eachbitinrange),#colorDarkgrey)
sprint("Search Address : "+settings("1")\address$,#colorDarkgrey)
sprint("JobTimeout     : "+settings("1")\jobtimeout+"s",#colorDarkgrey)
sprint("OutFile        : "+settings("1")\outFilename$,#colorDarkgrey)
sprint("MapFile        : "+settings("1")\mapFilename$,#colorDarkgrey)

If Curve::m_check_less_more_equilX64(*genhelper,*maxwidthsubrange)=2
  sprint("Subrange more then 2^63!!!", #colorRed)
  Input()
  End
EndIf
subrangewidth = PeekI(*genhelper)
sprint("Subrange width  :"+Str(subrangewidth),#colorDarkgrey)

;Check if subrange is power of two
a$=Hex(subrangewidth+1)
m_sethex32(*temp, @a$, 32)
tempv = settings("1")\divpow
While tempv  
  Curve::m_shlX64(*temp)
  tempv-1 
Wend

m_sethex32(*temp2, @"1", 32)
Curve::m_SubModX64(*temp,*temp,*temp2,*Curveqn) 

If Curve::m_check_less_more_equilX64(*temp,*Rangetotal)<>0
  sprint("Range is not power of two!!!", #colorRed)
  Input()
  End
EndIf

If cmpmapfile()
  sprint("Create new map ranges", #colorBrown)
  initrange()
  If savenewmap() 
    Input()
    End
  EndIf
  If FileSize(#DatabaseFile)>0
    sprint("Clear "+#DatabaseFile+" file, due to new map file", #colorBrown)
    DeleteFile(#DatabaseFile, #PB_FileSystem_Force)
  EndIf
Else
  sprint(procname$+"Loading map ranges", #colorBrown)
  If loadmap()
    Input()
    End
  EndIf
EndIf


If isFind=#True
  Sprint(procname$+"Key already solved!!!",#colorGreen)
  Input()
  End
EndIf
FillMapper()
Sprint(procname$+"Total ranges: "+Str(settings("1")\deviderint)+" scanned: "+Str(totalscanned)+" left: "+Str(range_mapper\count+1), #colorDefault)

CurentElementIndex = Random(range_mapper\count)
Sprint(procname$+"Initial index:"+Str(CurentElementIndex),#colorWhite) 
Sprint(procname$+"Press any key to continue",#colorWhite)  
Input()

SQLTxstate_Init()

If settings("1")\port
  StratServ = CreateNetworkServer(#Server,settings("1")\port,#PB_Network_TCP)
  If StratServ=0 
    Sprint(procname$+"Can't create the SSserver ("+Str(settings("1")\port)+" in use ?).", #colorred) 
    Delay(2000)        
    End  
  EndIf
  Thread = CreateThread(@Socket_Server(), @Server)
  CreateThread(@timer60s(), 0)
Else
  End
EndIf

Define local_totalbazzy, local_totalscanned
local_totalbazzy = ListSize(*range_task())
local_totalscanned = totalscanned
lastlogtime= Date()
While isFind=#False
  Delay(100)
  If Date()-lastlogtime>5
    lastlogtime= Date()
    If local_totalbazzy<>ListSize(*range_task()) Or local_totalscanned<>totalscanned
      local_totalbazzy = ListSize(*range_task())
      local_totalscanned = totalscanned
      Sprint(procname$+"Buzzy["+Str(local_totalbazzy)+"] Scanned["+Str(local_totalscanned)+"] Total["+Str(settings("1")\deviderint)+"] Left["+Str(settings("1")\deviderint-local_totalscanned)+"]",#colorDefault)      
    EndIf
  EndIf
Wend

Input()
End
; IDE Options = PureBasic 5.31 (Windows - x64)
; ExecutableFormat = Console
; CursorPosition = 639
; FirstLine = 624
; Folding = ------
; EnableXP
; EnableOnError
; Executable = serverRotorCudaX64.exe
; CommandLine = -range 8000000000000000:ffffffffffffffff -dp 22  -map mmm.bin -address 122AJhKLEfkFBaGAd84pLp1kfE7xK3GdT8