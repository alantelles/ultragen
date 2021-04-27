unit BrookServerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer;

type
  TBrookServerInstance = class (TInstanceOf)
    public
      procedure RunServer;
      constructor Create(APort: integer; ADebug: boolean; ThisType: TDataType);
  end;

  THTTPServer = class(TBrookHTTPServer)
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  end;

implementation

uses
  StringInstanceClass;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('{"JARBAS": "ROMERO"}', 'application/json', 200);
end;

constructor TBrookServerInstance.Create(APort: integer; ADebug: boolean;  ThisType: TDataType);
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
    len := ThisType.PMembers.Count;
    for i := 0 to len-1 do
    begin
      Inst := TFunctionInstance(ThisType.PMembers[i]);
      FMembers.Add(Inst.PName, Inst);
    end;
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
      Port := MPort;
      Open;
      if not Active then
        Exit;
      FError := False;
      WriteLn('Running '+ MTitle +' in '+'UltraGen Builtin Development Server at port ' + IntTostr(MPort), #13);
      ReadLn;

    except on E: Exception do
      FErrorMsg := E.Message;
    end;
  finally
    Free;
  end;

end;


end.

