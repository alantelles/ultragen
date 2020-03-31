unit ModuleClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TemplateClass, StrUtils;

type
  TModuleCaller = class
  private
    FTemplate:TTemplate;
  public
    property Template:TTemplate read FTemplate write FTemplate;
    constructor Create(var ATemplate:TTemplate);
    procedure ExecProcedure(AMod, AName:string; var Params:TStringList; var PureParams:TStringList);
  end;


implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals, ParentModuleClass;

constructor TModuleCaller.Create(var ATemplate:TTemplate);
begin
  FTemplate := ATemplate;
end;

procedure TModuleCaller.ExecProcedure(AMod, AName:string; var Params:TStringList; var PureParams:TStringList);
var
  ACall:TModule;
begin
  ACall := TModule.Create(AMod, AName, Params, PureParams,Self);
  ACall.ExecProcedure;
  ACall.Free;
end;

end.

