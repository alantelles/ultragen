unit ExtensionClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TemplateClass, StrUtils;

type
  TExtensionCaller = class
  private
    FPureParams, FParams:TStringList;
    FExtensionName, FProcName:string;
    FTemplate:TTemplate;
  public
    constructor Create(AMod, AName:string; var APureParams:TStringList; var AParams:TStringList; var ATemplate:TTemplate);
    procedure ExecProc;
    function ExecFunc:string;
    { Extensions procs }
    procedure PocCallProc;
    procedure GenDBCallProc;

    { Extensions funcs }
    function PocCallFunc:string;
    function GenDBCallFunc:string;

  end;


implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals,

  { available Extensions }
  POCExtension, GenDBExtension;

constructor TExtensionCaller.Create(AMod, AName:string; var APureParams:TStringList; var AParams:TStringList; var ATemplate:TTemplate);
begin
  FPureParams := APureParams;
  FParams := AParams;
  FExtensionname := AMod;
  FprocName := AName;
  FTemplate := ATemplate;
end;

procedure TExtensionCaller.ExecProc;
begin
  if FExtensionName = 'POC' then
    PocCallProc;
end;

function TExtensionCaller.ExecFunc:string;
begin
  if FExtensionName = 'POC' then
    Result := PocCallFunc;
end;

{ modules callers }
procedure TExtensionCaller.PocCallProc;
var
  APoc:TPocExtension;
begin
  APoc := TPocExtension.Create(FProcName, FPureParams, FParams, FTemplate);
  APoc.CallProc;
  APoc.Free;
end;

function TExtensionCaller.PocCallFunc:string;
var
  APoc:TPocExtension;
  Return:string;
begin
  APoc := TPocExtension.Create(FProcName, FPureParams, FParams, FTemplate);
  Return := APoc.CallFunc;
  APoc.Free;
  Result := Return;
end;

procedure TExtensionCaller.GenDBCallProc;
var
  AGenDB:TGenDBExtension;
begin
  AGenDB := TGenDBExtension.Create(FProcName, FPureParams, FParams, FTemplate);
  AGenDB.CallProc;
  AGenDB.Free;
end;

function TExtensionCaller.GenDBCallFunc:string;
var
  AGenDB:TGenDBExtension;
  Return:string;
begin
  AGenDB := TGenDBExtension.Create(FProcName, FPureParams, FParams, FTemplate);
  Return := AGenDB.CallFunc;
  AGenDB.Free;
  Result := Return;
end;

end.

