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
  StringInstanceClass, UltraGenInterfaceClass, Dos, StrUtils, ARClass, ListInstanceClass;

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
      ', ' + IntToStr(Len) + ' B, Content-Type: text/html', #13);
  WriteLn(ErrMsg);
  AResponse.Send(htmlErr,
    'text/html', Status);
end;

procedure SetCookiesFromUltra(AResponse: TBrookHTTPResponse; ADict: TActivationRecord);
var
  AListInst: TListInstance;
  Gene: TInstanceOf;
  CookieOpts: TActivationRecord;
  CookieStr: string;
  i: integer;
  ACookie: TClassInstance;

begin
  for i:=0 to ADict.PMembers.Count-1 do
  begin
    Gene := TInstanceOf(ADict.PMembers[i]);
    if (Gene.ClassNameIs('TListInstance')) then
    begin
      AListInst := TListInstance(Gene);
      // $cookie['key'] = ['value', {'path': '/', 'max-age': 310000, 'Secure': true}]
      AResponse.Headers.Add('Set-Cookie', ADict.PMembers.NameOfIndex(i) + '=' + TInstanceOf(ADict.PMembers[i]).AsString);
    end
    else if (Gene.ClassNameIs('TClassInstance')) then
    begin
      if (TClassInstance(Gene).PValue = 'Cookie') then
      begin
        ACookie := TClassInstance(Gene);
        AResponse.Headers.Add('Set-Cookie', ADict.PMembers.NameOfIndex(i) + '=' + TInstanceOf(ACookie.PMembers.Find('value')).AsString);
      end;
    end;


    // AResponse.SetCookie(ADict.PMembers.NameOfIndex(i), TInstanceOf(ADict.PMembers[i]).AsString);
  end;
end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
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
  i: integer;
begin
  try
  begin
    AResponse.Headers.Add('X-Powered-By', 'UltraGen/Brook server');
    Adapter := TUltraAdapter.Create('$request');
    //Adapter.ActRec.AddMember('AppResponse', );
    Adapter.AddMember('route', ARequest.Path);
    //Content := StrToBytes('{"jarbas": "Romero"}');
    Prelude := TStringList.Create;
    Prelude.Add('addModulePath(["'+ ReplaceStr(GetEnv('ULTRAGEN_HOME'), '\', '\\') + '", "modules"].path())');
    Prelude.Add('include @Core');
    IndexHandler := TStringInstance(FUltraInstance.PMembers.Find('indexHandler')).PValue;
    ExceptionHandler := TStringInstance(FUltraInstance.PMembers.Find('exceptionHandler')).PValue;
    UltraResult := TUltraInterface.InterpretScriptWithResult(IndexHandler, Prelude, Adapter);
    AppResponse := TDataType(UltraResult.ActRec.GetMember('AppResponse'));
    if AppResponse <> nil then
    begin
      ADict := TDictionaryInstance(AppResponse.PMembers.Find('$headers')).PValue;
      if ADict.PMembers.Count > 0 then
      begin
        for i:=0 to ADict.PMembers.Count-1 do
          AResponse.Headers.Add( ADict.PMembers.NameOfIndex(i), TInstanceOf(ADict.PMembers[i]).AsString);
      end;
      ADict := TDictionaryInstance(AppResponse.PMembers.Find('$cookies')).PValue;
      if ADict.PMembers.Count > 0 then
      begin
        SetCookiesFromUltra(AResponse, ADict);
      end;// TODO : process response
    end;
    Status := 200;
    Content := StrToBytes(UltraResult.LiveOutput);
    Len := Length(Content);

    WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
        ARequest.Method + ': '+
        ARequest.Path+' -- '+ IntToStr(Status) +
        //' ' + AResponse.s +
        ', ' + IntToStr(Len) + ' B, Content-Type: ' + ContentType, #13);
    //AResponse.SendBytes(Content, Len, 'text/html', Status);
    //AResponse.SendBytes([], 1, 'text/html', Status);
    AResponse.Send(UltraResult.LiveOutput, 'text/html', Status);
  end;
  except on E: Exception do
    if TBooleanInstance(FUltraInstance.PMembers.Find('debug')).PValue then
      RaiseInternalException(ARequest, AResponse, E.Message)
    else
    begin
      Adapter.AddMember('$stacktrace', E.Message);
      try
        WriteLn(E.Message);
        UltraResult := TUltraInterface.InterpretScriptWithResult(ExceptionHandler, Prelude, Adapter);
        AResponse.Send(UltraResult.LiveOutput, 'text/html', 500);

      except on F: Exception do
        if True then
        begin
          WriteLn('While running application exception handler, another exception occurred:', #13, #10, #13, #10, F.Message);
          AResponse.Send('500 Internal Server Error', 'text/html', 500);
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
      UltraInstance := Self;
      Port := MPort;
      Open;
      if not Active then
        Exit;
      FError := False;
      WriteLn('Running '+ MTitle +' in '+'Brook High Performance Server at port ' + IntTostr(MPort), #13);
      ReadLn;

    except on E: Exception do
      FErrorMsg := E.Message;
    end;
  finally
    Free;
  end;

end;


end.

