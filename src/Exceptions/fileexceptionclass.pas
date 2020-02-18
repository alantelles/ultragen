unit FileExceptionClass;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExceptionsFunctions;

const
  E_FILE_NOT_FOUND = 'The requested file "%%%" could not be found';

type EFileError = class
  private
    FType:string;
    FMessage:string;
    FCode:integer;
    FLineNumber:integer;
    FTempName:string;
    FFileName:string;
    FLine:string;
    FCanRaise:boolean;
    procedure ProcessMessage(ErrMsg:string);
  public
    property Message:string read FMessage write FMessage;
    constructor Create(EType:string; LineNum:integer; TempName:string; Line:string; AFileName:string);
    function TestFileExists:EFileError;
    procedure ERaise(CanRaise:boolean = True);

end;
implementation

uses ConstantsGlobals, StrUtils;


constructor EFileError.Create(EType:string; LineNum:integer; TempName:string; Line:string; AFileName:string);
begin
  FType := 'FileError';
  FLineNumber := LineNum;
  FTempName := TempName;
  FLine := Line;
  FFileName := AFileName;
  FCanRaise := False;
  ProcessMessage(EType);
end;

function EFileError.TestFileExists:EFileError;
var
  Test:boolean;
  i,len:integer;
begin
  FCanRaise := True;
  if FileExists(FFileName) then
    FCanRaise := False;
  Result := Self;
end;

procedure EFileError.ERaise(CanRaise:boolean = True);
begin
  if (CanRaise) or (FCanRaise) then
    RaiseException(FType,FMessage,FLineNumber,FTempName,FLine);
end;

procedure EFileError.ProcessMessage(ErrMsg:string);
var
  msg:string;
begin
  msg := ErrMsg;
  msg := ReplaceStr(msg,'%%%',FFileName);
  FMessage := msg;
end;

end.


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

end.

