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
  DateTimeInstanceClass, httpprotocol;

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
      ', ' + IntToStr(Len) + ' B, Content-Type: text/html', #13);
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

procedure TServerInstance.ExecuteAction(ARequest: TRequest; AResponse: TResponse);
var
  Content: TBytes;
  Len, Status: integer;
  UltraResult: TUltraResult;
  Adapter: TUltraAdapter;
  Prelude: TSTringList;
  ContentType: string = 'text/html';
  AppResponse: TDataType;
  IndexHandler, ExceptionHandler: string;
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
    AResponse.SetCustomHeader('X-Powered-By', 'UltraGen/FPWeb server');
    if ARequest.URI = '/favicon.ico' then
    begin
      SendFavIcon(ARequest, AResponse);
      Exit;
    end;

    Adapter := TUltraAdapter.Create('$request');
    Adapter.AddMember('route', ARequest.URI);
    Adapter.AddMember('method', ARequest.Method);

    Prelude := TStringList.Create;
    Prelude.Add('addModulePath(["'+ ReplaceStr(GetEnv('ULTRAGEN_HOME'), '\', '\\') + '", "modules"].path())');
    Prelude.Add('include @Core');
    IndexHandler := TStringInstance(FMembers.Find('indexHandler')).PValue;
    ExceptionHandler := TStringInstance(FMembers.Find('exceptionHandler')).PValue;
    UltraResult := TUltraInterface.InterpretScriptWithResult(IndexHandler, Prelude, Adapter, WebHandlers);
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
        ContentType := 'text/html';
        UltraResult := TUltraInterface.InterpretScriptWithResult(ExceptionHandler, Prelude, Adapter, WebHandlers);
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

          SendResponse(AResponse, 500, 'text/html', '500 Internal Server Error');
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

