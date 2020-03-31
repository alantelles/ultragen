unit GenDBExtension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtensionClass, StrUtils, TemplateClass;

type
  TGenDBExtension = class
    private
      FPureParams, FParams:TStringList;
      FName :string;
      FTemplate: TTemplate;
    public
      constructor Create(AProc:string; var APureParams:TStringList; var AParams:TStringList; var ATemplate:TTemplate);
      procedure CallProc;
      function CallFunc:string;

      { procedures }
      procedure FindWhere;



      { functions }
  end;

implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals;

constructor TGenDBExtension.Create(AProc:string; var APureParams:TStringList; var AParams:TStringList; var ATemplate:TTemplate);
begin
  FPureParams := APureParams;
  FParams := AParams;
  FName := AProc;
  FTemplate := ATemplate;
end;

procedure TGenDBExtension.CallProc;
begin
  if FName = 'findWhere' then
    FindWhere;
end;

function TGenDBExtension.CallFunc:string;
begin
  if FName = 'go' then
  begin
    FParams.SkipLastLineBreak := True;
    FParams.LineBreak := ', ';
    Result := FParams.Text;
  end;
end;

{ procedures }

procedure TGenDBExtension.FindWhere;
var
  APath, AKey, AValue, APrefix: string;
  P:TStringList;
begin
  FTemplate.LoadGenFolder(Params);
end;



end.

