unit VariableExceptionsClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExceptionsFunctions;

const
  E_VAR_NOT_EXIST = 'The referenced variable "%%%" does not exist';
  E_FORBBIDEN_VAR_NAME = 'The identifier "%%%" is not a valid variable identifier';

type EVariableError = class
  private
    FType:string;
    FMessage:string;
    FCode:integer;
    FLineNumber:integer;
    FTempName:string;
    FVarName:string;
    FLine:string;
    FCanRaise:boolean;
    procedure ProcessMessage(ErrMsg:string; AVarName:string='');
  public
    property Message:string read FMessage write FMessage;
    constructor Create(EType:string; LineNum:integer; TempName:string; Line:string; AVarName:string='');
    function TestValidName:EVariableError;
    procedure ERaise(CanRaise:boolean = True);

end;
implementation

uses ConstantsGlobals, StrUtils;


constructor EVariableError.Create(EType:string; LineNum:integer; TempName:string; Line:string; AVarName:string='');
begin
  FType := 'VariableError';
  FLineNumber := LineNum;
  FTempName := TempName;
  FLine := Line;
  FVarName := AVarName;
  FCanRaise := False;
  ProcessMessage(EType,AVarName);
end;

function EVariableError.TestValidName:EVariableError;
var
  Test:boolean;
  i,len:integer;
begin
  len := Length(FVarName);
  if len = 0 then
    FCanRaise := True
  else
  begin
    for i:=1 to Len do
    begin
      if (i = 1) and (Pos(FVarName[i], VARS_ALLOWED) = 0) then
      begin
        FCanRaise := True;
        break;
      end
      else if (Pos(FVarName[i], VARS_ALLOWED+NUMBERS) = 0) then
      begin
        FCanRaise := True;
        break;
      end;
    end;
  end;
  Result := Self;
end;

procedure EVariableError.ERaise(CanRaise:boolean = True);
begin
  if (CanRaise) or (FCanRaise) then
    RaiseException(FType,FMessage,FLineNumber,FTempName,FLine);
end;

procedure EVariableError.ProcessMessage(ErrMsg:string;AVarName:string='');
begin
  FMessage := ReplaceStr(ErrMsg,'%%%',AVarName);
end;

end.

