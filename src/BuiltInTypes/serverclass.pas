unit ServerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, StackClass,
  httpdefs, httproute, fphttpapp, fphttpserver, fpwebfile, ARClass;

type
  TProcessedAppResponse = record
    Status: integer;
    ContentType: string;
  end;

  TServerInstance = class (TInstanceOf)
    public
      procedure ExecuteAction(ARequest: TRequest; AResponse: TResponse);
      procedure RunServer;
      procedure StopServer(ARequest: TRequest; AResponse: TResponse);
      procedure SetStaticPath(AALias, APath: string);
      procedure SetStaticPaths(AStatic: TActivationRecord);
      constructor Create(APort: integer; ADebug: boolean);
  end;

implementation

uses
  StringInstanceClass, UltraGenInterfaceClass, Dos, StrUtils, ListInstanceClass, UltraWebHandlersClass,
  DateTimeInstanceClass, httpprotocol, ByteStreamClass, CoreFunctionsClass;

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

procedure RaiseInternalException(ARequest: TRequest; AResponse: TResponse; ErrMsg: string);
var
  Status: integer = 500;
  len: integer;
  htmlErr: string;
begin
  htmlErr := '<h1>UltraGen ERROR!</h1><pre style="white-space: pre-wrap; font-size: 12pt"><h3>Error while fetching content at "' + ARequest.URI + '"</h3><br>'+ReplaceStr(ErrMsg, '<', '&lt') +'</pre>';
  Len := Length(htmlerr);
  WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
      ARequest.Method + ': '+
      ARequest.URI+' -- '+ IntToStr(Status) +
      //' ' + AResponse.s +
      ', ' + IntToStr(Len) + ' B, Content-Type: text/html; charset=utf-8', #13);
  WriteLn(ErrMsg);
  //AResponse.Send(htmlErr, 'text/html', Status);
  AResponse.ContentType := 'text/html; charset=utf-8';
  AResponse.Content := htmlErr;
  AResponse.SendResponse;
end;

procedure SetCookie(AKey, AValue:string; AResponse: TResponse);
var
  ACookie: TCookie;
begin
  ACookie := AResponse.Cookies.Add;
  ACookie.Name := AKey;
  ACookie.Value := AValue;
  ACookie.Path := '/';
end;

procedure SetCookie(Akey: string; ACookieObj: TInstanceOf; AResponse: TResponse);
var
  ACookie: TCookie;
  CookieStr: string;
  CookieOpts: TActivationRecord;
  j: integer;
  AObj: TObject;
begin
  ACookie := AResponse.Cookies.Add;
  {AObj := ACookieObj.PMembers.Find('sameSite');
  if AObj.ClassNameIs('TStringInstance') then
  begin
    CookieStr := lowercase(TStringInstance(AObj).PValue);
    if CookieStr = 'lax' then
      ACookie.SameSite := ssLax
    else if CookieStr = 'strict' then
      ACookie.SameSite := ssStrict
    else
      ACookie.SameSite := ssNone;
  end;}
  ACookie.Name := AKey;
  ACookie.Value := TInstanceOf(ACookieObj.PMembers.Find('value')).AsString;
  AObj := ACookieObj.PMembers.Find('httpOnly');
  if AObj.ClassNameIs('TBooleanInstance') then
    ACookie.HttpOnly := TBooleanInstance(AObj).PValue;

  AObj := ACookieObj.PMembers.Find('expires');
  if AObj.ClassNameIs('TDateTimeInstance') then
    ACookie.Expires := TDateTimeInstance(AObj).PValue;

  {AObj := ACookieObj.PMembers.Find('maxAge');
  if AObj.ClassNameIs('TIntegerInstance') then
    ACookie.MaxAge := TIntegerInstance(AObj).PValue;}

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

procedure SetCookiesFromUltra(AResponse: TResponse; ADict: TActivationRecord);
var
  AListInst: TListInstance;
  Gene: TInstanceOf;

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
        SetCookie(AKey, Gene, AResponse);
      end
      else
        SetCookie(AKey, httpencode(Gene.AsString), AResponse);
    end
    else
      SetCookie(AKey, httpencode(Gene.AsString), AResponse);
  end;
end;

procedure LogRequest(ARequest: TRequest; Status, Len: integer; ContentType:string);
begin
   WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
        ARequest.Method + ': '+
        ARequest.URI+' -- '+ IntToStr(Status) + ' ' +
        ', ' + IntToStr(Len) + ' B, Content-Type: ' + ContentType, #13);
end;

procedure SendFavIcon(ARequest: TRequest; AResponse: TResponse);
var
  AStream: TFileStream;
begin

  AStream := TFileStream.Create(GetEnv('ULTRAGEN_HOME') + DirectorySeparator + 'assets'  + DirectorySeparator + 'favicon.ico', fmOpenRead);
  LogRequest(ARequest, 200, AStream.Size, 'image/x-icon');
  AResponse.Code := 200;
  AResponse.ContentType := 'image/x-icon';
  AResponse.ContentStream := AStream;
  AResponse.SendResponse;
  AResponse.ContentStream.Free;
end;

procedure SetHeadersFromUltra(AResponse: TResponse; ADict: TActivationRecord);
var
  i: integer;
begin
  for i:=0 to ADict.PMembers.Count-1 do
  begin
    AResponse.SetCustomHeader(ADict.PMembers.NameOfIndex(i), TInstanceOf(ADict.PMembers[i]).AsString);
  end;
end;

procedure TServerInstance.SetStaticPath(AALias, APath: string);
begin
  RegisterFileLocation(AAlias, APath);
end;

procedure TServerInstance.SetStaticPaths(AStatic: TActivationRecord);
var
  i: integer;
begin
  if AStatic.PMembers.Count > 0 then
  begin
    for i:=0 to Astatic.PMembers.Count-1 do
    begin
      RegisterFileLocation(AStatic.PMembers.NameOfIndex(i), TStringInstance(AStatic.PMembers[i]).PValue);
    end;
  end;
end;

function ProcessAppResponse(AResponse: TResponse; AppResponse: TDataType): integer;
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

procedure SendResponse(AResponse: TResponse; Status: integer; ContentType: string; Content: string);
begin
  AResponse.Code := Status;
  AResponse.ContentType := ContentType;
  AResponse.Content := Content;
  AResponse.SendResponse;
end;

function SetRequestHeadersToUltra(ARequest: TRequest): TDictionaryInstance;
var
  WebVars: TActivationRecord;
  K, V: string;
  i: integer;
begin
  WebVars := TActivationRecord.Create('requestHeaders', AR_DICT, -1);

  if ARequest.Accept <> '' then
    WebVars.AddMember('Accept', TStringInstance.Create(ARequest.Accept));
  if ARequest.AcceptCharset <> '' then
    WebVars.AddMember('Accept-Charset', TStringInstance.Create(ARequest.AcceptCharset));
  if ARequest.AcceptEncoding <> '' then
    WebVars.AddMember('Accept-Encoding', TStringInstance.Create(ARequest.AcceptEncoding));
  if ARequest.AcceptLanguage <> '' then
    WebVars.AddMember('Accept-Language', TStringInstance.Create(ARequest.AcceptLanguage));
  if ARequest.Authorization <> '' then
    WebVars.AddMember('Authorization', TStringInstance.Create(ARequest.Authorization));
  if ARequest.Connection <> '' then
    WebVars.AddMember('Connection', TStringInstance.Create(ARequest.Connection));
  if ARequest.ContentEncoding <> '' then
    WebVars.AddMember('Content-Encoding', TStringInstance.Create(ARequest.ContentEncoding));
  if ARequest.ContentLanguage <> '' then
    WebVars.AddMember('Content-Language', TStringInstance.Create(ARequest.ContentLanguage));
  if ARequest.ContentLength <> 0 then
    WebVars.AddMember('Content-Length', TStringInstance.Create(IntToStr(ARequest.ContentLength)));
  if ARequest.ContentType <> '' then
    WebVars.AddMember('Content-Type', TStringInstance.Create(ARequest.ContentType));
  if ARequest.Cookie <> '' then
    WebVars.AddMember('Cookie', TStringInstance.Create(ARequest.Cookie));
  if ARequest.Date <> '' then
    WebVars.AddMember('Date', TStringInstance.Create(ARequest.Date));
  if ARequest.Expires <> '' then
    WebVars.AddMember('Expires', TStringInstance.Create(ARequest.Expires));
  if ARequest.From <> '' then
    WebVars.AddMember('From', TStringInstance.Create(ARequest.From));
  if ARequest.IfMatch <> '' then
    WebVars.AddMember('If-Match', TStringInstance.Create(ARequest.IfMatch));
  if ARequest.IfNoneMatch <> '' then
    WebVars.AddMember('If-None-Match', TStringInstance.Create(ARequest.IfNoneMatch));
  if ARequest.IfRange <> '' then
    WebVars.AddMember('If-Range', TStringInstance.Create(ARequest.IfRange));
  if ARequest.IfUnModifiedSince <> '' then
    WebVars.AddMember('If-Unmodified-Since', TStringInstance.Create(ARequest.IfUnmodifiedSince));
  if ARequest.IfModifiedSince <> '' then
    WebVars.AddMember('If-Modified-Since', TStringInstance.Create(ARequest.IfModifiedSince));
  if ARequest.TE <> '' then
    WebVars.AddMember('Te', TStringInstance.Create(ARequest.TE));
  if ARequest.Upgrade <> '' then
    WebVars.AddMember('Upgrade', TStringInstance.Create(ARequest.Upgrade));
  if ARequest.LastModified <> '' then
    WebVars.AddMember('Last-Modified', TStringInstance.Create(ARequest.LastModified));
  if ARequest.Location <> '' then
    WebVars.AddMember('Location', TStringInstance.Create(ARequest.Location));
  if ARequest.Pragma <> '' then
    WebVars.AddMember('Pragma', TStringInstance.Create(ARequest.Pragma));
  if ARequest.Referer <> '' then
    WebVars.AddMember('Referer', TStringInstance.Create(ARequest.Referer));
  if ARequest.RetryAfter <> '' then
    WebVars.AddMember('Retry-After', TStringInstance.Create(ARequest.RetryAfter));
  if ARequest.Server <> '' then
    WebVars.AddMember('Server', TStringInstance.Create(ARequest.Server));
  if ARequest.SetCookie <> '' then
    WebVars.AddMember('Set-Cookie', TStringInstance.Create(ARequest.SetCookie));
  if ARequest.UserAgent <> '' then
    WebVars.AddMember('User-Agent', TStringInstance.Create(ARequest.UserAgent));
  if ARequest.WWWAuthenticate <> '' then
    WebVars.AddMember('WWW-Authenticate', TStringInstance.Create(ARequest.WWWAuthenticate));
  if ARequest.HTTPXRequestedWith <> '' then
    WebVars.AddMember('X-Requested-With', TStringInstance.Create(ARequest.HTTPXRequestedWith));

  for V in ARequest.CustomHeaders do
  begin
    i := Pos('=', V);
    K := Copy(V, 1, i-1);
    WebVars.AddMember(K, TStringInstance.Create(ARequest.CustomHeaders.Values[K]));
  end;
  WebVars.AddMember('Host', TStringInstance.Create(ARequest.Host));
  Result := TDictionaryInstance.Create(WebVars, TStringInstance.Create(''));
end;

function SetArgsFromUltra(ARequest: TRequest): TDictionaryInstance;
var
  Args: TActivationRecord;
  i: integer;
  k, v: string;
  AInst: TListInstance;
begin
  Args := TActivationRecord.Create('args', AR_DICT, -1);
  if ARequest.QueryFields.Count > 0 then
  begin
    for i := 0 to ARequest.QueryFields.Count-1 do
    begin
      k := ARequest.QueryFields.Names[i];
      v := ARequest.QueryFields.ValueFromIndex[i];
      if AnsiEndsStr('[]', k) then
      begin
        k := Copy(k, 1, RPos('[', k) - 1);
        Ainst := TListInstance(Args.PMembers.Find(k));
        if Ainst = nil then
        begin
          AInst := TListInstance.Create();
          Args.PMembers.Add(k, AInst);
        end;
        TListInstance(AInst).Add(TStringInstance.Create(v));
      end
      else
        Args.PMembers.Add(k, TStringInstance.Create(v));
    end;
  end;
  Result := TDictionaryInstance.Create(Args);
end;

function SetFormPostBodyFromUltra(ARequest: TRequest): TDictionaryInstance;
var
  Args: TActivationRecord;
  i: integer;
  k, v: string;
  AInst: TListInstance;
begin
  Args := TActivationRecord.Create('formBody', AR_DICT, -1);
  if ARequest.ContentFields.Count > 0 then
  begin
    for i := 0 to ARequest.ContentFields.Count-1 do
    begin
      k := ARequest.ContentFields.Names[i];
      v := ARequest.ContentFields.ValueFromIndex[i];
      if AnsiEndsStr('[]', k) then
      begin
        k := Copy(k, 1, RPos('[', k) - 1);
        Ainst := TListInstance(Args.PMembers.Find(k));
        if Ainst = nil then
        begin
          AInst := TListInstance.Create();
          Args.PMembers.Add(k, AInst);
        end;
        TListInstance(AInst).Add(TStringInstance.Create(v));
      end
      else
        Args.PMembers.Add(k, TStringInstance.Create(v));
    end;
  end;
  Result := TDictionaryInstance.Create(Args);
end;

function SetJsonPostBodyFromUltra(ARequest: TRequest): TInstanceOf;
var
  AParser: TCoreFunction;
  Ret: TInstanceOf;
begin
  AParser := TCoreFunction.Create;
  Ret := AParser.ParseJson(ARequest.Content);
  AParser.Free;
  Result := Ret;
end;

function SetFilesPostBodyFromUltra(ARequest: TRequest): TDictionaryInstance;
var
  FileData, Args: TActivationRecord;
  AFile: TUploadedFile;
  i: integer;
  k: string;
  AInst: TInstanceOf;
begin
  Args := TActivationRecord.Create('files', AR_DICT, -1);
  if ARequest.Files.Count > 0 then
  begin
    for i := 0 to ARequest.Files.Count-1 do
    begin
      AFile := Arequest.Files[i];
      k := AFile.FieldName;
      FileData := TActivationrecord.Create(k, AR_DICT, -1);
      FileData.AddMember('name', TStringInstance.Create(AFile.FileName));
      FileData.AddMember('tempName', TStringInstance.Create(AFile.LocalFileName));
      FileData.AddMember('disposition', TStringInstance.Create(AFile.Disposition));
      FileData.AddMember('size', TIntegerInstance.Create(AFile.Size));
      FileData.AddMember('contentType', TStringInstance.Create(AFile.ContentType));
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
  end;
  Result := TDictionaryInstance.Create(Args);
end;

function SetRequestCookiesToUltra(ARequest: TRequest): TDictionaryInstance;
var
  WebVars: TActivationRecord;
  K, V: string;
  i: integer;
begin
  WebVars := TActivationRecord.Create('cookies', AR_DICT, -1);
  for V in ARequest.CookieFields do
  begin
    i := Pos('=', V);
    K := Copy(V, 1, i-1);
    WebVars.AddMember(K, TStringInstance.Create(ARequest.CookieFields.Values[K]));
  end;
  Result := TDictionaryInstance.Create(WebVars, TStringInstance.Create(''));
end;

function SetRequestDict(ARequest: TRequest): TUltraAdapter;
var
  Adapter: TUltraAdapter;
begin
  Adapter := TUltraAdapter.Create('$request');
  Adapter.AddMember('server', 'FPWeb');
  Adapter.AddMember('route', ARequest.URI);
  Adapter.AddMember('method', ARequest.Method);
  Adapter.ActRec.AddMember('headers', SetRequestHeadersToUltra(ARequest));
  Adapter.ActRec.AddMember('cookies', SetRequestCookiesToUltra(ARequest));
  Adapter.AddMember('query', ARequest.QueryString);
  Adapter.ActRec.AddMember('args', SetArgsFromUltra(ARequest));
  Adapter.AddMember('body', ARequest.Content);
  Adapter.ActRec.AddMember('rawBody', TByteStreamInstance.Create(ARequest.Content));
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

procedure TServerInstance.ExecuteAction(ARequest: TRequest; AResponse: TResponse);
var
  Content: TBytes;
  Len, Status: integer;
  UltraResult: TUltraResult;
  Adapter: TUltraAdapter;
  Prelude: TSTringList;
  ContentType: string = 'text/html; charset=utf-8';
  AppResponse: TDataType;
  IndexHandler, ExceptionHandler, UltraHome: string;
  ADict: TActivationRecord;
  AInst: TInstanceOf;
  StatusInst: TInstanceOf;
  i: integer;
  Redirected: boolean = False;
  WebHandlers: TUltraFPWebHandlers;
begin
  Status := 0;
  WebHandlers := TUltraFPWebHandlers.Create(ARequest, AResponse);
  try
  begin

    if ARequest.URI = '/favicon.ico' then
    begin
      SendFavIcon(ARequest, AResponse);
      Exit;
    end;

    Adapter := SetRequestDict(ARequest);
    AResponse.SetCustomHeader('X-Powered-By', 'UltraGen/FPWeb server');
    UltraHome := ReplaceStr(GetEnv('ULTRAGEN_HOME'), '\', '\\');
    Prelude := TStringList.Create;
    Prelude.Add('addModulePath(["'+ UltraHome + '", "modules"].path())');
    Prelude.Add('include @Core');
    IndexHandler := TStringInstance(FMembers.Find('indexHandler')).PValue;
    ExceptionHandler := TStringInstance(FMembers.Find('exceptionHandler')).PValue;
    UltraResult := TUltraInterface.InterpretScriptWithResult(IndexHandler, Prelude, Adapter, UltraHome, WebHandlers);
    AppResponse := TDataType(UltraResult.ActRec.GetMember('AppResponse'));
    Status := ProcessAppResponse(AResponse, AppResponse);
    Len := Length(UltraResult.LiveOutput);
    ContentType := AResponse.GetCustomHeader('Content-Type');
    if ContentType = '' then
      ContentType := 'text/html; charset=utf-8';
    LogRequest(ARequest, Status, Len, ContentType);
    if not (Redirected or UltraResult.Redirected) then
    begin
      SendResponse(AResponse, Status, ContentType, UltraResult.LiveOutput);
    end;
  end;
  except on E: Exception do
    if TBooleanInstance(FMembers.Find('debug')).PValue then
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
        SendResponse(AResponse, Status, ContentType, UltraResult.LiveOutput);
      except on F: Exception do
        if True then
        begin
          WriteLn('While running application exception handler, another exception occurred:', #13, #10, #13, #10, F.Message);

          SendResponse(AResponse, 500, 'text/html; charset=utf-8', '500 Internal Server Error');
        end;
      end;
    end;
  end;
end;

constructor TServerInstance.Create(APort: integer; ADebug: boolean);
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
    FMembers.Add('stopRoute', TStringInstance.Create('/off'));
    Ferror := False;
  except on E: Exception do
    FErrorMsg := E.Message;

  end;
end;

procedure TServerInstance.StopServer(ARequest: TRequest ;AResponse: TResponse);
begin
  AResponse.Content := 'Server stopped';
  WriteLn('Stopping server', #13);
  Application.Terminate;

  WriteLn('Server stopped', #13);
end;

procedure TServerInstance.RunServer();
var
  MPort: integer;
  MTitle, MStopRoute: string;
  AInst: TInstanceOf;
begin
  MStopRoute := '';
  MTitle := TInstanceOf(FMembers.Find('title')).AsString;
  MPort := TInstanceOf(FMembers.Find('port')).PIntValue;
  AInst := TInstanceOf(FMembers.Find('stopRoute'));
  if not AInst.ClassNameIs('TNullInstance') then
    MStopRoute := TInstanceOf(FMembers.Find('stopRoute')).AsString;
  FError := False;
  if MStopRoute <> '' then
    HTTPRouter.RegisterRoute(MStopRoute, @StopServer);

  HTTPRouter.RegisterRoute('*', @ExecuteAction);
  WriteLn('Running '+TInstanceOf(FMembers.Find('title')).AsString+' in '+'UltraGen Builtin Development Server at port '+IntToStr(MPort), #13);
  if MStopRoute <> '' then
    WriteLn('Run a request to "' +MStopRoute+ '" to stop server')
  else
    WriteLn('No stop route defined');

  Application.Title := 'UltraGen Builtin Development Server';
  Application.Port := MPort;
  Application.Threaded := True;
  Application.Initialize;
  Application.Run;

end;


end.

