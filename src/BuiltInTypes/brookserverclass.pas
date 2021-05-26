unit BrookServerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, StackClass,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer;

type
  TBrookServerInstance = class (TInstanceOf)
    public
      procedure RunServer;
      constructor Create(APort: integer; ADebug: boolean);
  end;

  THTTPServer = class(TBrookHTTPServer)
  protected
    FUltraInstance: TBrookServerInstance;

    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;

  public
    property UltraInstance: TBrookServerInstance read FUltraInstance write FUltraInstance ;

  end;

implementation

uses
  StringInstanceClass, UltraGenInterfaceClass, Dos, StrUtils, ARClass, ListInstanceClass, UltraWebHandlersClass,
  DateTimeInstanceClass, httpprotocol, BrookHTTPUploads, CoreFunctionsClass, BrookStringMap, BrookHTTPCookies, ByteStreamClass;

function StrToBytes(Astr: string): TBytes;
var
  i, len: integer;
  Bytes: TBytes;
begin                            
  len := Length(ASTr);
  SetLength(Bytes, Len);
  for i:=1 to len do
    Bytes[i-1] := ord(AStr[i]);
  Result := Bytes;
end;

procedure RaiseInternalException(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse; ErrMsg: string);
var
  Status: integer = 500;
  len: integer;
  htmlErr: string;
begin
  htmlErr := '<h1>UltraGen ERROR!</h1><pre style="white-space: pre-wrap; font-size: 12pt"><h3>Error while fetching content at "' + ARequest.Path + '"</h3><br>'+ReplaceStr(ErrMsg, '<', '&lt') +'</pre>';
  Len := Length(htmlerr);
  WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
      ARequest.Method + ': '+
      ARequest.Path+' -- '+ IntToStr(Status) +
      //' ' + AResponse.s +
      ', ' + IntToStr(Len) + ' B, Content-Type: text/html; charset=utf-8', #13);
  WriteLn(ErrMsg);
  AResponse.Send(htmlErr,
    'text/html; charset=utf-8', Status);
end;

procedure SetCookie(AKey, AValue:string; AResponse: TBrookHTTPResponse);
var
  ACookie: TBrookHTTPCookie;
begin
  ACookie := AResponse.Cookies.Add;
  ACookie.Name := AKey;
  ACookie.Value := httpencode(AValue);
  ACookie.Path := '/';
end;

procedure SetCookie(Akey: string; ACookieObj: TInstanceOf; AResponse: TBrookHTTPResponse);
var
  ACookie: TBrookHTTPCookie;
  CookieStr: string;
  CookieOpts: TActivationRecord;
  j: integer;
  AObj: TObject;
begin
  //CookieStr := '';
  //CookieStr := CookieStr + TInstanceOf(ACookieObj.PMembers.Find('value')).AsString;
  //CookieOpts := TDictionaryInstance(ACookieObj.PMembers.Find('params')).PValue;
  //for j:=0 to CookieOpts.PMembers.Count - 1 do
  //begin
  //  if CookieOpts.PMembers[j].ClassName = 'TBooleanInstance' then
  //  begin
  //    if TBooleanInstance(CookieOpts.PMembers[j]).PValue then
  //      CookieStr := CookieStr + ';' + CookieOpts.PMembers.NameOfIndex(j)
  //  end
  //  else
  //    CookieStr := CookieStr + ';' + CookieOpts.PMembers.NameOfIndex(j) + '=' + TInstanceOf(CookieOpts.PMembers[j]).AsString;
  //
  //end;
  ACookie := AResponse.Cookies.Add;
  ACookie.Name := AKey;
  ACookie.Value := TInstanceOf(ACookieObj.PMembers.Find('value')).AsString;
  AObj := ACookieObj.PMembers.Find('sameSite');
  if AObj.ClassNameIs('TStringInstance') then
  begin
    CookieStr := lowercase(TStringInstance(AObj).PValue);
    if CookieStr = 'lax' then
      ACookie.SameSite := ssLax
    else if CookieStr = 'strict' then
      ACookie.SameSite := ssStrict
    else
      ACookie.SameSite := ssNone;
  end;
  AObj := ACookieObj.PMembers.Find('httpOnly');
  if AObj.ClassNameIs('TBooleanInstance') then
    ACookie.HttpOnly := TBooleanInstance(AObj).PValue;

  AObj := ACookieObj.PMembers.Find('expires');
  if AObj.ClassNameIs('TDateTimeInstance') then
    ACookie.Expires := TDateTimeInstance(AObj).PValue;

  AObj := ACookieObj.PMembers.Find('maxAge');
  if AObj.ClassNameIs('TIntegerInstance') then
    if TIntegerInstance(AObj).PValue > -1 then
      ACookie.MaxAge := TIntegerInstance(AObj).PValue;

  AObj := ACookieObj.PMembers.Find('domain');
  if AObj.ClassNameIs('TStringInstance') then
    ACookie.Domain := TStringInstance(AObj).PValue;

  AObj := ACookieObj.PMembers.Find('path');
  if AObj.ClassNameIs('TStringInstance') then
    ACookie.Path := TStringInstance(AObj).PValue;

  AObj := ACookieObj.PMembers.Find('secure');
  if AObj.ClassNameIs('TBooleanInstance') then
    ACookie.Secure := TBooleanInstance(AObj).PValue;
end;

procedure SetCookiesFromUltra(AResponse: TBrookHTTPResponse; ADict: TActivationRecord);
var
  AListInst: TListInstance;
  Gene: TInstanceOf;
  CookieOpts: TActivationRecord;
  CookieStr, Akey: string;
  i, j, len: integer;
  ACookie: TClassInstance;

begin
  for i:=0 to ADict.PMembers.Count-1 do
  begin
    AKey := ADict.PMembers.NameOfIndex(i);
    Gene := TInstanceOf(ADict.PMembers[i]);
    if (Gene.ClassNameIs('TClassInstance')) then
    begin
      if (TClassInstance(Gene).PValue = 'Cookie') then
      begin
        SetCookie(Akey, Gene, AResponse);
      end
      else
        SetCookie(Akey, Gene.AsString, AResponse);
    end
    else
      SetCookie(Akey, Gene.AsString, AResponse);
  end;
end;

procedure LogRequest(ARequest: TBrookHTTPRequest; Status, Len: integer; ContentType:string);
begin
   WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
        ARequest.Method + ': '+
        ARequest.Path+' -- '+ IntToStr(Status) + ' ' +
        ', ' + IntToStr(Len) + ' B, Content-Type: ' + ContentType, #13);
end;

procedure SendFavIcon(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  AStream: TFileStream;
begin

  AStream := TFileStream.Create(GetEnv('ULTRAGEN_HOME') + DirectorySeparator + 'assets'  + DirectorySeparator + 'favicon.ico', fmOpenRead);
  LogRequest(ARequest, 200, AStream.Size, 'image/x-icon');
  AResponse.SendStream(AStream, True, 200);
end;

procedure SetHeadersFromUltra(AResponse: TBrookHTTPResponse; ADict: TActivationRecord);
var
  i: integer;
begin
  for i:=0 to ADict.PMembers.Count-1 do
  begin
    AResponse.Headers.Add(ADict.PMembers.NameOfIndex(i), TInstanceOf(ADict.PMembers[i]).AsString);
  end;
end;

function ProcessAppResponse(AResponse: TBrookHTTPResponse; AppResponse: TDataType): integer;
var
  StatusInst: TInstanceOf;
  ADict: TActivationRecord;
  Status: integer = 200;
begin
  if AppResponse <> nil then
  begin
    StatusInst := TInstanceOf(AppResponse.PMembers.Find('status'));
    if StatusInst <> nil then
      if StatusInst.ClassNameIs('TIntegerInstance') then
        Status := TIntegerInstance(StatusInst).PValue;
    ADict := TDictionaryInstance(AppResponse.PMembers.Find('$headers')).PValue;
    if ADict.PMembers.Count > 0 then
    begin
      SetHeadersFromUltra(AResponse, ADict);
    end;
    ADict := TDictionaryInstance(AppResponse.PMembers.Find('$cookies')).PValue;
    if ADict.PMembers.Count > 0 then
    begin
      SetCookiesFromUltra(AResponse, ADict);
    end;
    // TODO : process response
  end;
  Result := Status
end;

function SetRequestCookiesToUltra(ARequest: TBrookHTTPRequest): TDictionaryInstance;
var
  WebVars: TActivationRecord;
  K, V: string;
  First: boolean;
  Pair: TBrookStringPair;
  ADict: TDictionaryInstance;
begin
  WebVars := TActivationRecord.Create('requestCookies', AR_DICT, -1);
  First := ARequest.Cookies.First(Pair);
  if First then
  begin
    WebVars.AddMember(Pair.Name, TStringInstance.Create(Pair.Value));
    while (ARequest.Cookies.Next(Pair)) do
    begin
      WebVars.AddMember(Pair.Name, TStringInstance.Create(Pair.Value));
    end;
  end;
  ADict := TDictionaryInstance.Create(WebVars, TStringInstance.Create(''));
  Result := ADict;
end;

function SetArgsFromUltra(ARequest: TBrookHTTPRequest): TDictionaryInstance;
var
  Args: TActivationRecord;
  k: string;
  AInst: TListInstance;
  Pair: TBrookStringPair;
begin
  Args := TActivationRecord.Create('args', AR_DICT, -1);
  if ARequest.Params.First(Pair) then
  begin
    repeat
      k := Pair.Name;
      if AnsiEndsStr('[]', k) then
      begin
        k := Copy(k, 1, RPos('[', k) - 1);
        Ainst := TListInstance(Args.PMembers.Find(k));
        if Ainst = nil then
        begin
          AInst := TListInstance.Create();
          Args.PMembers.Add(k, AInst);
        end;
        TListInstance(AInst).Add(TStringInstance.Create(Pair.Value));
      end
      else
        Args.PMembers.Add(k, TStringInstance.Create(Pair.Value));
    until not ARequest.Params.Next(Pair)
  end;

  Result := TDictionaryInstance.Create(Args);
end;

function SetFormPostBodyFromUltra(ARequest: TBrookHTTPRequest): TDictionaryInstance;
var
  Args: TActivationRecord;
  k: string;
  AInst: TListInstance;
  Pair: TBrookStringPair;
begin
  Args := TActivationRecord.Create('post', AR_DICT, -1);
  if ARequest.Fields.First(Pair) then
  begin
    repeat
      k := Pair.Name;
      if AnsiEndsStr('[]', k) then
      begin
        k := Copy(k, 1, RPos('[', k) - 1);
        Ainst := TListInstance(Args.PMembers.Find(k));
        if Ainst = nil then
        begin
          AInst := TListInstance.Create();
          Args.PMembers.Add(k, AInst);
        end;
        TListInstance(AInst).Add(TStringInstance.Create(Pair.Value));
      end
      else
        Args.PMembers.Add(k, TStringInstance.Create(Pair.Value));
    until not ARequest.Fields.Next(Pair)
  end;

  Result := TDictionaryInstance.Create(Args);
end;

function SetRequestHeadersToUltra(ARequest: TBrookHTTPRequest): TDictionaryInstance;
var
  WebVars: TActivationRecord;
  K, V: string;
  First: boolean;
  Pair: TBrookStringPair;
  ADict: TDictionaryInstance;
begin
  WebVars := TActivationRecord.Create('requestHeaders', AR_DICT, -1);
  First := ARequest.Headers.First(Pair);
  if First then
  begin
    WebVars.AddMember(Pair.Name, TStringInstance.Create(Pair.Value));
    while (ARequest.Headers.Next(Pair)) do
    begin
      WebVars.AddMember(Pair.Name, TStringInstance.Create(Pair.Value));
    end;
  end;
  ADict := TDictionaryInstance.Create(WebVars, TStringInstance.Create(''));
  Result := ADict;
end;

function SetFilesPostBodyFromUltra(ARequest: TBrookHTTPRequest): TDictionaryInstance;
var
  FileData, Args: TActivationRecord;
  i: integer;
  k: string;
  AInst: TInstanceOf;
  AFile: TBrookHTTPUpload;
begin
  Args := TActivationRecord.Create('files', AR_DICT, -1);
  //AFile := ARequest.Files.First;
  //if AFile <> nil then
  //begin
  //  repeat
  //    AFile := Arequest.Files[i];
    for AFile in ARequest.Files do
    begin
      k := AFile.Field;
      FileData := TActivationrecord.Create(k, AR_DICT, -1);
      FileData.AddMember('name', TStringInstance.Create(AFile.Name));
      FileData.AddMember('tempName', TStringInstance.Create(AFile.Directory));
      FileData.AddMember('size', TIntegerInstance.Create(AFile.Size));
      FileData.AddMember('contentType', TStringInstance.Create(AFile.Mime));
      FileData.AddMember('handle',TUploadedInstance.Create(AFile));
      if AnsiEndsStr('[]', k) then
      begin
        k := Copy(k, 1, RPos('[', k) - 1);
        Ainst := TListInstance(Args.PMembers.Find(k));
        if Ainst = nil then
        begin
          AInst := TListInstance.Create();
          Args.PMembers.Add(k, AInst);
        end;
        TListInstance(AInst).Add(TDictionaryInstance.Create(FileData));
      end
      else
        Args.PMembers.Add(k, TDictionaryInstance.Create(FileData));
    end;
    //  AFile := ARequest.Files.Next;
    //until AFile = nil
  //end;
  Result := TDictionaryInstance.Create(Args);
end;

function SetQueryStringToUltra(ARequest: TBrookHTTPRequest): string;
var
  Ret: string;
begin
  Ret := ReplaceStr(ARequest.Params.ToString, sLineBreak, '&');
  Ret := Copy(Ret, 1, Length(Ret) - 1);
  Result := ret;
end;

function SetJsonPostBodyFromUltra(ARequest: TBrookHTTPRequest): TInstanceOf;
var
  AParser: TCoreFunction;
  Ret: TInstanceOf;
begin
  AParser := TCoreFunction.Create;
  Ret := AParser.ParseJson(ARequest.Payload.ToString);
  AParser.Free;
  Result := Ret;
end;

function SetRequestDict(ARequest: TBrookHTTPRequest): TUltraAdapter;
var
  Adapter: TUltraAdapter;
begin
  Adapter := TUltraAdapter.Create('$request');
  Adapter.AddMember('route', ARequest.Path);
  Adapter.AddMember('method', ARequest.Method);
  Adapter.AddMember('server', 'Brook');
  Adapter.ActRec.AddMember('headers', SetRequestHeadersToUltra(ARequest));
  Adapter.ActRec.AddMember('cookies', SetRequestCookiesToUltra(ARequest));
  Adapter.AddMember('query', SetQueryStringToUltra(ARequest));
  Adapter.ActRec.AddMember('args', SetArgsFromUltra(ARequest));
  Adapter.AddMember('body', ARequest.Payload.ToString);
  Adapter.ActRec.AddMember('rawBody', TByteStreamInstance.Create(ARequest.Payload.ToString));
  if ARequest.ContentType = 'application/x-www-form-urlencoded' then
    Adapter.ActRec.AddMember('form', SetFormPostBodyFromUltra(ARequest));
  if ARequest.ContentType = 'application/json' then
    Adapter.ActRec.AddMember('json', SetJsonPostBodyFromUltra(ARequest));
  if AnsiStartsStr('multipart/form-data;', ARequest.ContentType) then
  begin
    Adapter.ActRec.AddMember('form', SetFormPostBodyFromUltra(ARequest));
    Adapter.ActRec.AddMember('files', SetFilesPostBodyFromUltra(ARequest));
  end;
  Result := Adapter;
end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Content: TBytes;
  Len, Status: integer;
  UltraResult: TUltraResult;
  Adapter: TUltraAdapter;
  Prelude: TSTringList;
  ContentType: string = '';
  AppResponse: TDataType;
  IndexHandler, ExceptionHandler,UltraHome: string;
  ADict: TActivationRecord;
  AInst: TInstanceOf;
  StatusInst: TInstanceOf;
  i: integer;
  Redirected: boolean = False;
  WebHandlers: TUltraBrookHandlers;
begin
  WebHandlers := TUltraBrookHandlers.Create(ARequest, Aresponse);
  Status := 0;
  try
  begin
    AResponse.Headers.Add('X-Powered-By', 'UltraGen/Brook server');
    if ARequest.Path = '/favicon.ico' then
    begin
      SendFavIcon(ARequest, AResponse);
      Exit;
    end;

    Adapter := SetRequestDict(ARequest);

    UltraHome := ReplaceStr(GetEnv('ULTRAGEN_HOME'), '\', '\\');
    Prelude := TStringList.Create;
    Prelude.Add('addModulePath(["'+ UltraHome + '", "modules"].path())');
    Prelude.Add('include @Core');
    IndexHandler := TStringInstance(FUltraInstance.PMembers.Find('indexHandler')).PValue;
    ExceptionHandler := TStringInstance(FUltraInstance.PMembers.Find('exceptionHandler')).PValue;
    UltraResult := TUltraInterface.InterpretScriptWithResult(IndexHandler, Prelude, Adapter, UltraHome, WebHandlers);
    AppResponse := TDataType(UltraResult.ActRec.GetMember('AppResponse'));
    Status := ProcessAppResponse(AResponse, AppResponse);

    Len := Length(UltraResult.LiveOutput);
    ContentType := AResponse.Headers.Get('Content-Type');
    if ContentType = '' then
      ContentType := 'text/html; charset=utf-8';

    LogRequest(ARequest, Status, Len, ContentType);
    if not (Redirected or UltraResult.Redirected) then
      AResponse.Send(UltraResult.LiveOutput, ContentType, Status);
  end;
  except on E: Exception do
    if TBooleanInstance(FUltraInstance.PMembers.Find('debug')).PValue then
      RaiseInternalException(ARequest, AResponse, E.Message)
    else
    begin
      Adapter.AddMember('$stacktrace', E.Message);
      try
        WriteLn(E.Message);
        ContentType := 'text/html; charset=utf-8';
        UltraResult := TUltraInterface.InterpretScriptWithResult(ExceptionHandler, Prelude, Adapter, UltraHome, WebHandlers);
        AppResponse := TDataType(UltraResult.ActRec.GetMember('AppResponse'));
        ProcessAppResponse(AResponse, AppResponse);
        Len := Length(UltraResult.LiveOutput);
        Status := 500;
        LogRequest(ARequest, Status, Len, ContentType);
        AResponse.Send(UltraResult.LiveOutput, ContentType, Status);

      except on F: Exception do
        if True then
        begin
          WriteLn('While running application exception handler, another exception occurred:', #13, #10, #13, #10, F.Message);
          AResponse.Send('500 Internal Server Error', 'text/html; charset=utf-8', 500);
        end;
      end;
    end;
  end;
end;

constructor TBrookServerInstance.Create(APort: integer; ADebug: boolean);
var
  len, i: integer;
  Inst: TFunctionInstance;
begin
  FError := TRue;
  inherited Create;
  try
    FMembers.Add('port', TIntegerInstance.Create(Aport));
    FMembers.Add('title', TStringInstance.Create('Untitled'));
    FMembers.Add('indexHandler', TStringInstance.Create('index.ultra'));
    FMembers.Add('exceptionHandler', TStringInstance.Create('exception.ultra'));
    FMembers.Add('debug', TBooleanInstance.Create(ADebug));
    FMembers.Add('uploadsDir', TStringInstance.Create(ReplaceStr(GetEnv('ULTRAGEN_HOME'), '\', '\\') + 'brook_uploads'));
    {len := ThisType.PMembers.Count;
    for i := 0 to len-1 do
    begin
      Inst := TFunctionInstance(ThisType.PMembers[i]);
      FMembers.Add(Inst.PName, Inst);
    end;}
    Ferror := False;
  except on E: Exception do
    FErrorMsg := E.Message;

  end;
end;

procedure TBrookServerInstance.RunServer();
var
  MPort: integer;
  MTitle: string;
begin
  MTitle := TInstanceOf(FMembers.Find('title')).AsString;
  MPort := TInstanceOf(FMembers.Find('port')).PIntValue;
  FError := True;
  with THTTPServer.Create(nil) do
  try
    try
      UploadsDir := TStringInstance(FMembers.Find('uploadsDir')).PValue;
      UltraInstance := Self;
      Port := MPort;
      Open;
      if not Active then
        Exit;
      ForceDirectories(UploadsDir);
      FError := False;
      WriteLn('Running '+ MTitle +' in '+'Brook High Performance Server at port ' + IntTostr(MPort), #13);
      Writeln('Press enter to stop server');
      ReadLn;

    except on E: Exception do
      FErrorMsg := E.Message;
    end;
  finally
    Free;
  end;

end;


end.

