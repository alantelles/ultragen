unit GenExceptionsClass;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExceptionsFunctions,
  GenFileSetClass;

const
  E_GEN_NOT_EXIST = 'The referenced gen alias "%%%" does not exist';
  E_FORBIDDEN_KEY_NAME = 'The identifier "%%%" is not a valid key identifier for implicit association.'+sLineBreak+'    Use setValue for this purpose';
  E_FORBIDDEN_ALIAS_NAME = 'The identifier $$$ is not a valid alias name';
  E_GEN_ALREADY_EXISTS = 'The alias identifier "$$$" is already being used.'+sLineBreak+'    Use unloadGen:''$$$'' to reuse this alias';

type EGenError = class
  private
    FType:string;
    FMessage:string;
    FCode:integer;
    FLineNumber:integer;
    FTempName:string;
    FKeyName:string;
    FAliasName:string;
    FIndex:integer;
    FLine:string;
    FCanRaise:boolean;
    procedure ProcessMessage(ErrMsg:string);
  public
    property Message:string read FMessage write FMessage;
    constructor Create(EType:string; LineNum:integer; TempName:string; Line:string; AKeyName:string; AnAliasName:string; AnIndex:integer);
    function TestValidKeyName:EGenError;
    function TestAliasExists(AGenSet:TGenFileSet):EGenError;
    procedure ERaise(CanRaise:boolean = True);

end;
implementation

uses ConstantsGlobals, StrUtils;


constructor EGenError.Create(EType:string; LineNum:integer; TempName:string; Line:string; AKeyName:string; AnAliasName:string; AnIndex:integer);
begin
  FType := 'GenError';
  FLineNumber := LineNum;
  FTempName := TempName;
  FLine := Line;
  FKeyName := AKeyName;
  FAliasName := AnAliasName;
  FIndex := AnIndex;
  FCanRaise := False;
  ProcessMessage(EType);
end;

function EGenError.TestAliasExists(AGenSet:TGenFileSet):EGenError;
var
  i,len:integer;
begin
  len := Length(AGenSet.GenFiles);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      if AGenSet.GenFiles[i].GenAlias = FAliasName then
      begin
        FCanRaise := True;
        break;
      end;
    end;
  end
  else
      FCanRaise := False;
  Result := Self;
end;

function EGenError.TestValidKeyName:EGenError;
var
  Test:boolean;
  i,len:integer;
begin
  len := Length(FKeyName);
  if len = 0 then
    FCanRaise := True
  else
  begin
    for i:=1 to Len do
    begin
      if (i = 1) and (Pos(FKeyName[i], VARS_ALLOWED) = 0) then
      begin
        FCanRaise := True;
        break;
      end
      else if (Pos(FKeyName[i], VARS_ALLOWED+NUMBERS+':') = 0) then
      begin
        FCanRaise := True;
        break;
      end;
    end;
  end;
  Result := Self;
end;

procedure EGenError.ERaise(CanRaise:boolean = True);
begin
  if (CanRaise) or (FCanRaise) then
    RaiseException(FType,FMessage,FLineNumber,FTempName,FLine);
end;

procedure EGenError.ProcessMessage(ErrMsg:string);
var
  msg:string;
begin
  msg := ErrMsg;
  msg := ReplaceStr(msg,'%%%',FKeyName);
  msg := ReplaceStr(msg,'$$$',FAliasName);
  msg := ReplaceStr(msg,'+++',IntToStr(FIndex));
  FMessage := msg;
end;

end.

