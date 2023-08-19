EnableExplicit
IncludeFile "libs/Curve64.pb"


Structure settingsStructure
  port.i  
  host.s
  hash$
  rangeB$
  rangeE$
  address$  
  powaddress$
  outFilename$ 
  Progname.s
  name.s
  pass.s
  device$  
  blocks$  
  rekey$
  nopow.b
EndStructure

Structure CrackStructure
  isok.b
  Compiler.i
  isRunning.i
  killapp.b
EndStructure

Structure checkjobStructure
  err.i
  isRunning.i 
  hash$
  timestamp.i
EndStructure

Enumeration
#File
EndEnumeration

Enumeration
#sendgetjob_id
#login_id
#sendkey_id
#checkjob_id
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

#CHECKJOBTIME=60
#APPVERSION="1.0"

Global Dim DecodeArray(24)

Define MutexConsole = CreateMutex()
Define NewMap settings.settingsStructure()
Define *rangeB,*rangeE, isFind=#False


Define *CurveP, *CurveGX, *CurveGY, *Curveqn
*CurveP = Curve::m_getCurveValues()
*CurveGX = *CurveP+32
*CurveGY = *CurveP+64
*Curveqn = *CurveP+96

Procedure SPrint(text$, cl)
  Shared MutexConsole
  LockMutex(MutexConsole)
  ;ConsoleColor(cl,0)
  Debug FormatDate("%hh:%ii:%ss ", Date())+" "+text$
  PrintN(FormatDate("%hh:%ii:%ss ", Date())+" "+text$)  
  ;ConsoleColor(#colorDefault,0)
  UnlockMutex(MutexConsole)
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
      Case "-nopow"
        Debug "found -nopow"
        i+1             
          settings("1")\nopow=1             
          Sprint( "-nopow flag set",#colordefault)
         
      Case "-prog"
        Debug "found -prog"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\Progname = StringField(datares$,1,".")         
          Sprint( "-prog "+settings("1")\Progname,#colordefault)
        EndIf 
      Case "-name"
        Debug "found -name"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\name = datares$         
          Sprint( "-name "+settings("1")\name,#colordefault)
        EndIf
      Case "-pass"
        Debug "found -pass"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\pass = datares$         
          Sprint( "-pass "+settings("1")\pass,#colordefault)
        EndIf
       Case "-pool"
        Debug "found -pool"
        
        i+1  
        datares$ = ProgramParameter(i)         
        If datares$<>"" And Left(datares$,1)<>"-"
          If GetURLPart(datares$, #PB_URL_Protocol)=""
             datares$="http://"+datares$
          EndIf          
          settings("1")\host =GetURLPart(datares$, #PB_URL_Site)
          settings("1")\port = Val(GetURLPart(datares$, #PB_URL_Port))
          Sprint( "-pool "+settings("1")\host+":"+settings("1")\port,#colordefault)
        EndIf
       Case "-d"
        Debug "found -d"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\device$ = datares$
          Sprint( "-d "+settings("1")\device$,#colordefault)
        EndIf 
       
       Case "-b"
        Debug "found -b"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\blocks$ = datares$         
          Sprint( "-b "+settings("1")\blocks$,#colordefault)
        EndIf 
        
       Case "-r"
        Debug "found -r"
        i+1             
        datares$ = ProgramParameter(i)
        If datares$<>"" And Left(datares$,1)<>"-"
          settings("1")\rekey$ = datares$         
          Sprint( "-r "+settings("1")\rekey$,#colordefault)
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

Procedure.i SendQuestion(con_id,string$)
  Protected err
  If con_id
    SendNetworkString(con_id,string$+#LF$,#PB_Ascii)
  EndIf
ProcedureReturn err
EndProcedure

Procedure GetJobHost()
  Protected totalloadbytes, maxloadbytes, loadedbytes, *pp, i, err, batchCRC32, get_work_getjob_string.s, quit=#False,  timeout
  Protected Connect, dis=1, pars_res, *Buffer, ReceivedBytes, answer_t$, pos, pos2, answer_f$, tempjson, get_work, Values, get_work_authorize_string.s, id_json_answer,msginit$, isAuthorized
  Shared settings() 
  
  *Buffer = AllocateMemory(65536)
  msginit$ ="[GETWORK_GJ] "
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #login_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "Login")
    Values =SetJSONArray(AddJSONMember(get_work, "params"))      
    SetJSONString(AddJSONElement(Values), settings("1")\name)     
    SetJSONString(AddJSONElement(Values), settings("1")\pass)
    SetJSONString(AddJSONElement(Values), Str(settings("1")\nopow))
    get_work_authorize_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
  EndIf
  
  tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #sendgetjob_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "getwork")
      get_work_getjob_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    
  Repeat
    If dis=1
      isAuthorized =#False      
      Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
      If Not Connect
        
        While Not Connect And timeout<5
          timeout+1
          Debug "try conect to getwork"
          Delay(1000)
          Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
        Wend
        If Not Connect
          ;cant` connect
           Connect = 0
            err=2
            quit = #True
            Break
        EndIf
      EndIf   
      dis=0
      SendQuestion(Connect,get_work_authorize_string) 
      
    EndIf
  
  If Connect
    Select NetworkClientEvent(Connect) 
        Case #PB_NetworkEvent_Data     
        ReceivedBytes = ReceiveNetworkData(Connect, *Buffer, 65536) 
        If ReceivedBytes>0
          answer_t$=PeekS(*Buffer, ReceivedBytes,#PB_Ascii)  
          Debug answer_t$
          pos=FindString(answer_t$, "{")
          While pos                
            pos2=FindString(answer_t$, "}",pos+1)
            If pos2            
              answer_f$=Mid(answer_t$,pos,pos2-pos+1)            
              answer_f$ = RTrim(answer_f$,"}")
              answer_f$ = LTrim(answer_f$,"{")                   
              answer_f$ = "{"+answer_f$+"}"
              Debug">>"+answer_f$        
              pars_res=ParseJSON(#PB_Any, answer_f$)                    
              If pars_res
                id_json_answer=Val(LCase(m_cutHex(getElem(pars_res,"id",0))))
                If id_json_answer
                  Select id_json_answer 
                    Case #login_id
                      If Not Val(getElem(pars_res,"result",0))
                        If LCase(getElem(pars_res,"error",0))="invalid_login"
                          Sprint(msginit$+">>Invalid Login<<",#colorRed)
                          Delay(1000)
                          dis=1
                          isAuthorized =#False
                        ElseIf LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        EndIf
                      Else
                        Sprint(msginit$+"Authorized", #colorBrown)  
                        isAuthorized =#True
                        SendQuestion(Connect,get_work_getjob_string)
                        
                        
                      EndIf
                  EndSelect
                Else                 
                      Debug"*****"
                      If getElem(pars_res,"error",0)=""
                        Sprint(msginit$+">>Got Job from host<<",#colorBrown)
                        settings("1")\hash$ = getElem(pars_res,"result",0)
                        settings("1")\rangeB$ = getElem(pars_res,"result",1)
                        settings("1")\rangeE$ = getElem(pars_res,"result",2)
                        settings("1")\address$ = getElem(pars_res,"result",3)
                        settings("1")\powaddress$ = getElem(pars_res,"result",4)
                        Sprint(msginit$+"HASH    ["+settings("1")\hash$+"]",#colorBrown)
                        Sprint(msginit$+"RANGEB  ["+settings("1")\rangeB$+"]",#colorBrown)
                        Sprint(msginit$+"RANGEE  ["+settings("1")\rangeE$+"]",#colorBrown)
                        Sprint(msginit$+"ADDRESS ["+settings("1")\address$+"]",#colorBrown)     
                        Sprint(msginit$+"POW ADDR["+settings("1")\powaddress$+"]",#colorBrown)
                        quit = #True
                        Break
                      Else
                        Sprint(msginit$+getElem(pars_res,"error",0), #colorRed)
                        If LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        ElseIf LCase(getElem(pars_res,"error",0))="range_scanned"
                          err=4
                          quit = #True
                          Break
                        Else
                          err=5
                          quit = #True
                          Break
                        EndIf
                        
                      EndIf
                EndIf
                If IsJSON(pars_res)
                  FreeJSON(pars_res)
                EndIf 
              Else
                    Sprint(msginit$+" unknown json",#colorred)
              EndIf
              answer_t$= Right(answer_t$, Len(answer_t$)-pos2)
              pos=FindString(answer_t$, "{")
            Else
              pos=0
            EndIf
          Wend
        EndIf
        
      Case #PB_NetworkEvent_Disconnect
        Debug "getwork disconnected"
        Connect = 0
        err=1
        quit = #True
    EndSelect
    
    
  EndIf
  Delay (1)
Until quit
If Connect
  CloseNetworkConnection(Connect)
EndIf
FreeMemory(*Buffer)
ProcedureReturn err  
EndProcedure

Procedure CheckJobHost(*checkjob.checkjobStructure)
  Protected totalloadbytes, maxloadbytes, loadedbytes, *pp, i, err, batchCRC32, get_work_checkwork_string.s, quit=#False,  timeout
  Protected Connect, dis=1, pars_res, *Buffer, ReceivedBytes, answer_t$, pos, pos2, answer_f$, tempjson, get_work, Values, get_work_authorize_string.s, id_json_answer,msginit$, isAuthorized
  Protected sendtime.i
  Shared settings()
  *Buffer = AllocateMemory(65536)
  msginit$ ="[GETWORK_CJ] "
  
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #login_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "Login")
    Values =SetJSONArray(AddJSONMember(get_work, "params"))      
    SetJSONString(AddJSONElement(Values), settings("1")\name)     
    SetJSONString(AddJSONElement(Values), settings("1")\pass)
    SetJSONString(AddJSONElement(Values), Str(settings("1")\nopow))
    get_work_authorize_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
  EndIf
  
  tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #checkjob_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "checkjob")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))         
      SetJSONString(AddJSONElement(Values), *checkjob\hash$)
      get_work_checkwork_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
  
  Repeat
    If dis=1
      isAuthorized =#False    
      Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
      If Not Connect
        
        While Not Connect And timeout<5
          timeout+1
          Debug "try conect to getwork"
          Delay(1000)
          Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
        Wend
        If Not Connect
          ;cant` connect
           Connect = 0
            err=2
            quit = #True
            Break
        EndIf
      EndIf   
      dis=0
      SendQuestion(Connect,get_work_authorize_string) 
      sendtime = Date()
    EndIf
  
  If Connect
    Select NetworkClientEvent(Connect) 
        Case #PB_NetworkEvent_Data     
        ReceivedBytes = ReceiveNetworkData(Connect, *Buffer, 65536) 
        If ReceivedBytes>0
          answer_t$=PeekS(*Buffer, ReceivedBytes,#PB_Ascii)  
          Debug answer_t$
          pos=FindString(answer_t$, "{")
          While pos                
            pos2=FindString(answer_t$, "}",pos+1)
            If pos2            
              answer_f$=Mid(answer_t$,pos,pos2-pos+1)            
              answer_f$ = RTrim(answer_f$,"}")
              answer_f$ = LTrim(answer_f$,"{")                   
              answer_f$ = "{"+answer_f$+"}"
              Debug">>"+answer_f$        
              pars_res=ParseJSON(#PB_Any, answer_f$)                    
              If pars_res
                id_json_answer=Val(LCase(m_cutHex(getElem(pars_res,"id",0))))
                If id_json_answer
                  Select id_json_answer                            
                    Case #login_id
                      If Not Val(getElem(pars_res,"result",0))
                        If LCase(getElem(pars_res,"error",0))="invalid_login"
                          Sprint(msginit$+">>Invalid Login<<",#colorRed)
                          Delay(1000)
                          dis=1
                          isAuthorized =#False
                        ElseIf LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        EndIf
                      Else
                        ;Sprint(msginit$+"Authorized", #colorBrown)  
                        isAuthorized =#True
                        SendQuestion(Connect,get_work_checkwork_string)   
                      EndIf
                      
                    Case #checkjob_id
                      If Not Val(getElem(pars_res,"result",0))
                        ;Sprint(msginit$+">>"+getElem(pars_res,"error",0)+"<<",#colorRed)
                        If LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        ElseIf LCase(getElem(pars_res,"error",0))="job_no_longer_exist"
                          err=4
                          quit = #True
                          Break
                        Else
                          Delay(1000)
                          dis=1                          
                        EndIf                        
                      Else                                     
                        quit = #True
                        Break
                      EndIf
                  EndSelect
                EndIf
                If IsJSON(pars_res)
                  FreeJSON(pars_res)
                EndIf 
              Else
                    Sprint(msginit$+"Unknown json",#colorred)
              EndIf
              answer_t$= Right(answer_t$, Len(answer_t$)-pos2)
              pos=FindString(answer_t$, "{")
            Else
              pos=0
            EndIf
          Wend
        EndIf
        
      Case #PB_NetworkEvent_Disconnect
        Debug "getwork disconnected"
        Connect = 0
        err=1
        quit = #True
    EndSelect
    
    
  EndIf
  Delay (1)
  If quit = #False And sendtime And Date()-sendtime>5 And err=0
    ;timeout
    err=5
    quit = #True
  EndIf
Until quit
If Connect
  CloseNetworkConnection(Connect)
EndIf
FreeMemory(*Buffer)
*checkjob\err = err
*checkjob\isRunning=2

 
EndProcedure

Procedure sendSubmitWork(key1$,key2$)
  Protected totalloadbytes, maxloadbytes, loadedbytes, *pp, i, err, batchCRC32, get_work_sendbatch_string.s, quit=#False,  timeout
  Protected Connect, dis=1, pars_res, *Buffer, ReceivedBytes, answer_t$, pos, pos2, answer_f$, tempjson, get_work, Values, get_work_authorize_string.s, id_json_answer,msginit$, isAuthorized
  Shared settings()
  *Buffer = AllocateMemory(65536)
  msginit$ ="[GETWORK_SW] "
  tempjson = CreateJSON(#PB_Any)
  If tempjson   
    get_work = SetJSONObject(JSONValue(tempjson))   
    SetJSONInteger(AddJSONMember(get_work, "id"), #login_id) 
    SetJSONString(AddJSONMember(get_work, "method"), "Login")
    Values =SetJSONArray(AddJSONMember(get_work, "params"))      
    SetJSONString(AddJSONElement(Values), settings("1")\name)     
    SetJSONString(AddJSONElement(Values), settings("1")\pass)
    SetJSONString(AddJSONElement(Values), Str(settings("1")\nopow))
    get_work_authorize_string=ComposeJSON(tempjson)
    FreeJSON(tempjson)
  EndIf
  
  tempjson = CreateJSON(#PB_Any)
    If tempjson   
      get_work = SetJSONObject(JSONValue(tempjson))   
      SetJSONInteger(AddJSONMember(get_work, "id"), #sendkey_id) 
      SetJSONString(AddJSONMember(get_work, "method"), "submitwork")
      Values =SetJSONArray(AddJSONMember(get_work, "params"))         
      SetJSONString(AddJSONElement(Values), settings("1")\hash$)
      SetJSONString(AddJSONElement(Values), key1$)
      SetJSONString(AddJSONElement(Values), key2$)
      get_work_sendbatch_string=ComposeJSON(tempjson)
      FreeJSON(tempjson)
    EndIf
    
  Repeat
    If dis=1
      isAuthorized =#False      
      Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
      If Not Connect
        
        While Not Connect And timeout<5
          timeout+1
          Debug "try conect to getwork"
          Delay(1000)
          Connect = OpenNetworkConnection(settings("1")\host ,settings("1")\port,#PB_Network_TCP,10000)
        Wend
        If Not Connect
          ;cant` connect
           Connect = 0
            err=2
            quit = #True
            Break
        EndIf
      EndIf   
      dis=0
      SendQuestion(Connect,get_work_authorize_string) 
      
    EndIf
  
  If Connect
    Select NetworkClientEvent(Connect) 
        Case #PB_NetworkEvent_Data     
        ReceivedBytes = ReceiveNetworkData(Connect, *Buffer, 65536) 
        If ReceivedBytes>0
          answer_t$=PeekS(*Buffer, ReceivedBytes,#PB_Ascii)  
          Debug answer_t$
          pos=FindString(answer_t$, "{")
          While pos                
            pos2=FindString(answer_t$, "}",pos+1)
            If pos2            
              answer_f$=Mid(answer_t$,pos,pos2-pos+1)            
              answer_f$ = RTrim(answer_f$,"}")
              answer_f$ = LTrim(answer_f$,"{")                   
              answer_f$ = "{"+answer_f$+"}"
              Debug">>"+answer_f$        
              pars_res=ParseJSON(#PB_Any, answer_f$)                    
              If pars_res
                id_json_answer=Val(LCase(m_cutHex(getElem(pars_res,"id",0))))
                If id_json_answer
                  Select id_json_answer                            
                    Case #login_id
                       If Not Val(getElem(pars_res,"result",0))
                        If LCase(getElem(pars_res,"error",0))="invalid_login"
                          Sprint(msginit$+">>Invalid Login<<",#colorRed)
                          Delay(1000)
                          dis=1
                          isAuthorized =#False
                        ElseIf LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        EndIf
                      Else
                        Sprint(msginit$+"Authorized", #colorBrown)  
                        isAuthorized =#True
                        SendQuestion(Connect,get_work_sendbatch_string)
                        
                        
                      EndIf
                      
                    Case #sendkey_id
                      If Not Val(getElem(pars_res,"result",0))
                        Sprint(msginit$+">>"+getElem(pars_res,"error",0)+"<<",#colorRed)
                        If LCase(getElem(pars_res,"error",0))="keyfounded"
                          err=3
                          quit = #True
                          Break
                        ElseIf LCase(getElem(pars_res,"error",0))="invalid_job"
                          err=4
                          quit = #True
                          Break
                        Else
                          Delay(1000)
                          dis=1
                          isAuthorized =#False
                        EndIf                        
                      Else
                        Sprint(msginit$+"Job was send to host", #colorBrown)                         
                        quit = #True
                        Break
                      EndIf
                  EndSelect
                EndIf
                If IsJSON(pars_res)
                  FreeJSON(pars_res)
                EndIf 
              Else
                    Sprint(msginit$+"Unknown json",#colorred)
              EndIf
              answer_t$= Right(answer_t$, Len(answer_t$)-pos2)
              pos=FindString(answer_t$, "{")
            Else
              pos=0
            EndIf
          Wend
        EndIf
        
      Case #PB_NetworkEvent_Disconnect
        Debug "getwork disconnected"
        Connect = 0
        err=1
        quit = #True
    EndSelect
    
    
  EndIf
  Delay (1)
Until quit
If Connect
  CloseNetworkConnection(Connect)
EndIf
FreeMemory(*Buffer)

ProcedureReturn err  
EndProcedure


Procedure runcuBitCrack(*vanity.CrackStructure)
  Protected  procname$="[SOLVER] ", Output$, err$, Outputcuted$, err, dead$, totaldelemiter,lasttimedead, params$, n, symbolN
  Protected string_win$=LCase("Private key:")  
  Protected *Buffer=AllocateMemory(65536)
  Protected cls$=LSet("", 120, Chr(8))
  Shared settings()
  *vanity\isok=1
  params$="--coin BTC "
  If settings("1")\nopow=0
    params$+" -m ADDRESSES -g "
  Else
    params$+" -m ADDRESS -g "
  EndIf
  If settings("1")\device$
    params$+"--gpui "+settings("1")\device$+" "
  EndIf  
  If settings("1")\blocks$
    params$+"--gpux "+settings("1")\blocks$+" "
  EndIf 
  
   
  params$+"--range "+settings("1")\rangeB$+":"+settings("1")\rangeE$+" -o "+settings("1")\outFilename$
  If settings("1")\nopow=0
    params$ +" -i "+"curjob.bin"
  Else
    params$ + " "+ settings("1")\address$
  EndIf
  *vanity\Compiler = RunProgram("./Rotor", params$,"",#PB_Program_Open)  
  If *vanity\Compiler
    SPrint(procname$+"["+settings("1")\Progname+"] programm running..",#colorYellow)
    SPrint(procname$+"params ["+params$+"]",#colorYellow)
    While ProgramRunning(*vanity\Compiler)
      
      Delay(20)
      If *vanity\killapp
        ;we need kill app
        If ProgramRunning(*vanity\Compiler)
          KillProgram(*vanity\Compiler)
        EndIf
      EndIf
    Wend
    PrintN("")
    If *vanity\killapp=0
      SPrint(procname$+"["+settings("1")\Progname+"] programm finished code["+Str(ProgramExitCode(*vanity\Compiler))+"]",#colorYellow)
      *vanity\isok =  0
      If ProgramExitCode(*vanity\Compiler)<>1 And ProgramExitCode(*vanity\Compiler)<>0
        SPrint(procname$+"["+settings("1")\Progname+"] has error",#colorRed)        
        End
      EndIf
    Else
      SPrint(procname$+"["+settings("1")\Progname+"] was killed, due to job is no longer exist",#colorRed)
      *vanity\isok = 0
    EndIf
      *vanity\Compiler=0  
  Else
    SPrint(procname$+"Can't found ["+settings("1")\Progname+"] programm",#colorred)     
    End
  EndIf
  FreeMemory(*Buffer)
  *vanity\isRunning=0
  *vanity\killapp = 0
EndProcedure
  
Procedure.i DecodeBase58(Address$) 
  Protected i, j, p
  Protected charSet$ = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  Protected c$  
  For j = 0 To 24      
      DecodeArray(j) = 0
  Next j 
  For i = 1 To Len(Address$)
    c$ = Mid(Address$, i, 1)
    p = FindString(charSet$, c$) - 1
    If p = -1 : ProcedureReturn #False : EndIf; Address contains invalid Base58 character
    For j = 24 To 1 Step -1
      p + 58 * DecodeArray(j)
      DecodeArray(j) = p % 256
      p  / 256
    Next j    
    If p <> 0 : ProcedureReturn #False : EndIf ; Address is too long
  Next i
  ProcedureReturn #True
EndProcedure

settings("1")\host="127.0.0.1"
settings("1")\port=8000
settings("1")\name="Friend"
settings("1")\pass="x"
settings("1")\Progname="Rotor-Cuda"
settings("1")\outFilename$="xxx.txt"
settings("1")\device$ ="0"
settings("1")\rekey$ =""
settings("1")\nopow=0
InitNetwork()
OpenConsole()
getprogparam()

Define i, sortstatus
Define *MemoryID0 = AllocateMemory(20)
If Not *MemoryID0
  sprint("Can`t allocate memory", #colorRed)
  End
EndIf
Define *MemoryID1 = AllocateMemory(20)
If Not *MemoryID1
  sprint("Can`t allocate memory", #colorRed)
  End
EndIf

If settings("1")\device$  
  If CountString(settings("1")\outFilename$,".")
    settings("1")\outFilename$=StringField(settings("1")\outFilename$,1,".")+RemoveString(settings("1")\device$,",")+"."+StringField(settings("1")\outFilename$,2,".")
  Else
    settings("1")\outFilename$=settings("1")\outFilename$+RemoveString(settings("1")\device$,",")
  EndIf
EndIf
sprint("app version: "+#APPVERSION,#colorDarkgrey)
sprint("Host: "+settings("1")\host,#colorDarkgrey)
sprint("Port: "+settings("1")\port,#colorDarkgrey)
sprint("Name: "+settings("1")\name,#colorDarkgrey)
sprint("Pass: "+settings("1")\pass,#colorDarkgrey)
sprint("Prog: "+settings("1")\Progname,#colorDarkgrey)
If settings("1")\nopow=0
  sprint("Pow: Yes",#colorDarkgrey)
Else
  sprint("Pow: No",#colorDarkgrey)
EndIf
If FileSize(settings("1")\outFilename$)>0
  DeleteFile(settings("1")\outFilename$ )
EndIf

Define err, msginit$="[MAIN] ", winkey$, quit=#False, bitcrackres.CrackStructure, key1$, key2$, line$, pa$, pk$, checkjob.checkjobStructure, Thread 
While isFind=#False And quit=#False
  Repeat
    err = GetJobHost()
      Select err
        Case 1
          Sprint(msginit$+"Server disconnected",#colorRed)
        Case 2
          Sprint(msginit$+"Server not connected",#colorRed)
        Case 3
          Sprint(msginit$+"Key already founded by host",#colorRed)
          isFind=#True         
        Case 4
          Sprint(msginit$+"All range_scanned",#colorRed)
          isFind=#True
        Case 5
          Sprint(msginit$+"Unknown server error",#colorRed)          
      EndSelect
      If err
        Delay(5000)
      EndIf
  Until err=0 Or err=3 Or err=4
  If Not err
    key1$=""
    key2$=""
    bitcrackres\isRunning = 1
    checkjob\timestamp = Date()    
    
    ;settings("1")\address$="1EeAxcprB2PpCnr34VfZdFrkUWuxyiNEFv"
    ;settings("1")\powaddress$="1BRDVGjmjgdjED1RubYFoLJyqShqnzTWBi"
    ;create job file for KeyHunt(rmd 160 sorted)   
    If settings("1")\nopow=0 
      DecodeBase58(settings("1")\address$)
      For i=0 To 19
          PokeC(*MemoryID0+i,DecodeArray(i+1))
      Next i
      PrintN(m_gethex32(*MemoryID0, 20))   
      DecodeBase58(settings("1")\powaddress$)
      For i=0 To 19
          PokeC(*MemoryID1+i,DecodeArray(i+1))
        Next i
      PrintN(m_gethex32(*MemoryID1, 20))     
      sortstatus=0  
      For i=0 To 19     
        PrintN(Str(PeekC(*MemoryID0+i))+" "+Str(PeekC(*MemoryID1+i)))
        If PeekC(*MemoryID0+i)>PeekC(*MemoryID1+i)
          sortstatus=1
          Break      
        EndIf
        If PeekC(*MemoryID0+i)<PeekC(*MemoryID1+i)        
          Break      
        EndIf
      Next i
      PrintN("status:"+Str(sortstatus))
      
      If CreateFile(#File, "curjob.bin")
        If sortstatus
          WriteData(#File, *MemoryID1, 20)
          WriteData(#File, *MemoryID0, 20)
        Else
          WriteData(#File, *MemoryID0, 20)
          WriteData(#File, *MemoryID1, 20)
        EndIf      
        CloseFile(#File)
      EndIf
    EndIf
    
    Thread = CreateThread(@runcuBitCrack(),@bitcrackres.CrackStructure)
    If Thread
      While bitcrackres\isRunning
        Delay(100)
        If bitcrackres\isRunning And Date()-checkjob\timestamp>#CHECKJOBTIME And checkjob\isRunning = 0
          checkjob\err = 0
          checkjob\isRunning = 1
          checkjob\hash$ = settings("1")\hash$
          CreateThread(@CheckJobHost(), @checkjob.checkjobStructure)
        EndIf
        If checkjob\isRunning = 2
          ;got result about current job
          Select checkjob\err
            Case 1
              ;Server disconnected, maybe enternet issue
            Case 2
              ;Server not connected, maybe enternet issue
            Case 3
              ;Key already founded by host
              ;stop bitcrack and quit
              Sprint(msginit$+"Key already founded by host",#colorRed)
              isFind=#True
              bitcrackres\killapp = 1              
            Case 4
              ;invalid job              
              ;Sprint(msginit$+"Current job is no longer exist",#colorRed)
              bitcrackres\killapp = 1 
            Case 5
              ;timeout, maybe enternet issue
          EndSelect
          checkjob\isRunning=0
          checkjob\timestamp=Date()
          
        EndIf
        If FileSize(settings("1")\outFilename$)>0
          If RenameFile(settings("1")\outFilename$, "temp.txt")
            If Not ReadFile(#File, "temp.txt")   
                Sprint( "Something bad happened when read output", #colorRed)
                quit=#True
              Else
                pk$="":pa$=""
                While Not Eof(#File)
                  line$ = ReadString(#File,#PB_Ascii)
                  If Len(line$)>2 
                    If FindString(line$, "Priv (HEX):", #PB_String_NoCase)
                      pk$ =  LTrim(StringField(line$,2,":") ," ")
                    EndIf
                    If FindString(line$, "PubAddress:", #PB_String_NoCase)
                      pa$ =  LTrim(StringField(line$,2,":") ," ")
                    EndIf                                   
                    If CompareMemoryString(@pa$, @settings("1")\address$, #PB_String_NoCase)=#PB_String_Equal
                      ;mamma mia it is solution!
                      key2$=pk$                
                    Else
                      If CompareMemoryString(@pa$, @settings("1")\powaddress$, #PB_String_NoCase)=#PB_String_Equal
                        key1$=pk$
                      Else
                        Sprint( "Invalid collision found"+pa$, #colorRed)
                        quit=#True
                      EndIf
                    EndIf
                  EndIf
                Wend
                CloseFile(#File)                
                DeleteFile("temp.txt" )
                If key1$="" And key2$<>""
                  ;send key immediately! do not wait while pow address will be solved..
                  Repeat
                    err = sendSubmitWork(key1$,key2$)
                    Select err
                      Case 1
                        Sprint(msginit$+"Server disconnected",#colorRed)
                      Case 2
                        Sprint(msginit$+"Server not connected",#colorRed)
                      Case 3
                        Sprint(msginit$+"Key already founded by host",#colorRed)
                        isFind=#True
                      Case 4
                        Sprint(msginit$+"invalid job",#colorRed)
                    EndSelect
                  Until err=0 Or err=3 Or err=4
                EndIf
            EndIf
          Else
            Sprint( "Something bad happened when rename file", #colorRed)
            quit=#True
          EndIf
          
          
        EndIf
      Wend
      err =  bitcrackres\isok
      err=0
      If checkjob\isRunning= 1
        While checkjob\isRunning = 1
          ;wait while jobchecker finished
          Delay(100)
        Wend
        checkjob\isRunning=0
        checkjob\timestamp=Date()
      EndIf
    Else
      Sprint(msginit$+"Can`t create thread to run "+settings("1")\Progname,#colorRed)
      quit=#True
    EndIf    
    If err
      Sprint(msginit$+"Something went wrong during launching "+settings("1")\Progname,#colorRed)
      quit=#True
    EndIf
    If Not err And checkjob\err <>3 And checkjob\err <>4
      If key1$ Or key2$ Or settings("1")\nopow=1     
        If key2$=""
          Sprint("Didn`t find address in this range -(",#colorDarkgrey)
        EndIf
          Repeat
            err = sendSubmitWork(key1$,key2$)
            Select err
              Case 1
                Sprint(msginit$+"Server disconnected",#colorRed)
              Case 2
                Sprint(msginit$+"Server not connected",#colorRed)
              Case 3
                Sprint(msginit$+"Key already founded by host",#colorRed)
                isFind=#True
              Case 4
                Sprint(msginit$+"invalid job",#colorRed)
             EndSelect
          Until err=0 Or err=3 Or err=4
      Else        
          Sprint(msginit$+"Hmm, here problem with solver app!",#colorRed)
          quit=#True       
      EndIf 
    EndIf
  EndIf
Wend  

End
; IDE Options = PureBasic 5.31 (Linux - x64)
; ExecutableFormat = Console
; CursorPosition = 749
; FirstLine = 731
; Folding = ---
; EnableThread
; EnableXP
; Executable = clientRotorCudaX64
; DisableDebugger