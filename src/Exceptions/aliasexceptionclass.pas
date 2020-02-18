unit AliasExceptionClass;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExceptionsFunctions;

const
  E_FORBIDDEN_ALIAS_NAME = 'The identifier "%%%" is not a valid alias name';

type EAliasError = class
  private
    FType:string;
    FMessage:string;
    FCode:integer;
    FLineNumber:integer;
    FTempName:string;
    FAliasName:string;
    FLine:string;
    FCanRaise:boolean;
    procedure ProcessMessage(ErrMsg:string);
  public
    property Message:string read FMessage write FMessage;
    constructor Create(EType:string; LineNum:integer; TempName:string; Line:string; AnAliasName:string);
    function TestValidAliasName:EAliasError;
    procedure ERaise(CanRaise:boolean = True);

end;
implementation

uses ConstantsGlobals, StrUtils;


constructor EAliasError.Create(EType:string; LineNum:integer; TempName:string; Line:string; AnAliasName:string);
begin
  FType := 'AliasError';
  FLineNumber := LineNum;
  FTempName := TempName;
  FLine := Line;
  FAliasName := AnAliasName;
  FCanRaise := False;
  ProcessMessage(EType);
end;

function EAliasError.TestValidAliasName:EAliasError;
var
  Test:boolean;
  i,len:integer;
begin
  len := Length(FAliasName);
  if len = 0 then
    FCanRaise := True
  else
  begin
    for i:=1 to Len do
    begin
      if (i = 1) and (Pos(FAliasName[i], VARS_ALLOWED) = 0) then
      begin
        FCanRaise := True;
        break;
      end
      else if (Pos(FAliasName[i], VARS_ALLOWED+NUMBERS+':') = 0) then
      begin
        FCanRaise := True;
        break;
      end;
    end;
  end;
  Result := Self;
end;

procedure EAliasError.ERaise(CanRaise:boolean = True);
begin
  if (CanRaise) or (FCanRaise) then
    RaiseException(FType,FMessage,FLineNumber,FTempName,FLine);
end;

procedure EAliasError.ProcessMessage(ErrMsg:string);
var
  msg:string;
begin
  msg := ErrMsg;
  msg := ReplaceStr(msg,'%%%',FAliasName);
  FMessage := msg;
end;

end.


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

end.

