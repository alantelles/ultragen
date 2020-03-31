unit POCExtension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtensionClass, StrUtils, TemplateClass;

type
  TPocExtension = class
    private
      FPureParams, FParams:TStringList;
      FName :string;
      FTemplate:TTemplate;
    public
      constructor Create(AProc:string; var APureParams:TStringList; var AParams:TStringList; var ATemplate:TTemplate);
      procedure CallProc;
      function CallFunc:string;

      { procedures }
      procedure UpperAll;

      { functions }
  end;

implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals;

constructor TPocExtension.Create(AProc:string; var APureParams:TStringList; var AParams:TStringList; var ATemplate:TTemplate);
begin
  FPureParams := APureParams;
  FParams := AParams;
  FTemplate := ATemplate;
  FName := AProc;
end;

procedure TPocExtension.CallProc;
begin
  if FName = 'print' then
    Writeln ('vc esta printando de dentro do modulo')
  else if FName = 'upperall' then
    UpperAll;
end;

function TPocExtension.CallFunc:string;
begin
  if FName = 'go' then
  begin
    FParams.SkipLastLineBreak := True;
    FParams.LineBreak := ', ';
    Result := FParams.Text;
  end;
end;


procedure TPocExtension.UpperAll;
var
  S:string;
begin
  WriteLn(Uppercase(FParams.Text));
end;


end.

