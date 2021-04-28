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
  StringInstanceClass, UltraGenInterfaceClass, Dos, StrUtils, ARClass;

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

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Content: TBytes;
  Len, Status: integer;
  UltraResult: TUltraResult;
  Adapter: TUltraAdapter;
  Prelude: TSTringList;
  ContentType: string = 'text/html';
  AppResponse: TDataType;
begin
  Adapter := TUltraAdapter.Create('$request');
  //Adapter.ActRec.AddMember('AppResponse', );
  Adapter.AddMember('route', ARequest.Path);
  //Content := StrToBytes('{"jarbas": "Romero"}');
  Prelude := TStringList.Create;
  Prelude.Add('addModulePath(["'+ ReplaceStr(GetEnv('ULTRAGEN_HOME'), '\', '\\') + '", "modules"].path())');
  Prelude.Add('include @Core');
  UltraResult := TUltraInterface.InterpretScriptWithResult('index.ultra', Prelude, Adapter);
  writeln(UltraResult.ActRec.GetMember('a').AsString);
  AppResponse := TDataType(UltraResult.ActRec.GetMember('AppResponse'));
  if AppResponse <> nil then
    writeln(appresponse.AsString)
  else
    writeln('nao tem appresponse');
  Status := 200;
  Content := StrToBytes(UltraResult.LiveOutput);
  Len := Length(Content);

  WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
      ARequest.Method + ': '+
      ARequest.Path+' -- '+ IntToStr(Status) +
      //' ' + AResponse.s +
      ', ' + IntToStr(Len) + ' B, Content-Type: ' + ContentType, #13);
  AResponse.SendBytes(Content, Len, 'text/html', Status);
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

