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
      constructor Create(APort: integer; ADebug: boolean);
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

constructor TBrookServerInstance.Create(APort: integer; ADebug: boolean);
begin
  inherited Create;
  FMembers.Add('port', TIntegerInstance.Create(3500));
  FMembers.Add('title', TStringInstance.Create('Untitled'));
  FMembers.Add('indexHandler', TStringInstance.Create('index.ultra'));
  FMembers.Add('exceptionHandler', TStringInstance.Create('exception.ultra'));
  FMembers.Add('debug', TBooleanInstance.Create(True));

end;

procedure TBrookServerInstance.RunServer();
var
  MPort: integer;
  MTitle: string;
begin
  MTitle := TInstanceOf(FMembers.Find('title')).AsString;
  MPort := TInstanceOf(FMembers.Find('port')).PIntValue;
  with THTTPServer.Create(nil) do
  try
    Port := MPort;
    Open;
    if not Active then
      Exit;
    WriteLn('Running '+ MTitle +' in '+'UltraGen Builtin Development Server at port ' + IntTostr(MPort), #13);
    ReadLn;
  finally
    Free;
  end;

end;


end.

