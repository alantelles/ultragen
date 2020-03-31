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
  ConstantsGlobals, VariablesGlobals, TypesGlobals, GenFileClass, FileUtil;

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
  // APath, AKey, AValue, APrefix: string;
  i: integer=0;
  AGenFile:TGenFile;
  Files:TStringList;
  F:string;
begin
  AGenFile := TGenFile.Create;
  Files := TStringList.Create;
  FindAllFiles(Files, FParams[0], '*.gen;*.GEN', True);
  for F in Files do
  begin
    AGenFile.Load(F);
    if AGenFile.GetValue(FParams[2]).Value = FParams[3] then
    begin
      FTemplate.GenFileSet.Add(True, F , FParams[1]+GEN_SUB_LEVEL+IntToStr(i));
      i := i + 1;
    end;
  end;
  Files.Free;
  AGenFile.Free;
end;



end.

