unit parentmoduleclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ModuleClass, StrUtils;

type
  TModule = class
    private
      { modules }
      procedure PocProc;
    protected
      FUltra:TModuleCaller;
      FParams, FPureParams:TStringList;
      FName, FMod :string;
    public
      property Params:TStringList read FParams write FParams;
      property PureParams:TStringList read FPureParams write FPureParams;
      constructor Create(AMod, AProc:string; var AParams:TStringList; var APureParams:TStringList; var AModule:TModuleCaller);
      procedure ExecProcedure;



  end;

implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals

  ,{ Registered modules }
  POCModule;

constructor TModule.Create(Amod, AProc:string; var AParams:TStringList; var APureParams:TStringList; var AModule:TModuleCaller);
begin
  FUltra := AModule;
  FParams := Params;
  FPureParams := PureParams;
  FMod := AMod;
  FName := AProc;
end;

{ registering modules }

procedure TModule.ExecProcedure;
begin
  if FMod = 'POC' then
    PocProc;
end;

{ end registering modules }

{ modules }
procedure TModule.PocProc;
var
  APoc:TPocModule;
begin
  APoc := TPocModule.Create(FName, FParams, FPureParams, FUltra);
  APoc.CallProc;
  APoc.Free;
end;


end.
