unit TypesBootStrapClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, InstanceOfClass, CoreFunctionsClass;

type
  // TMethod is a string hash table pairing name and signature

  TBootstrap = class
  private
    FTypes: TFPObjectHashTable;
    FmethodsOfTypes: TFPObjectHashTable;
  public
    property PTypes: TFPObjectHashTable read FTypes write FTypes;
    constructor Create;
    procedure RegisterMethods(ATypeName: string; AMethodList: TFPStringHashTable);
    procedure RegisterType(ATypeName: string);
    procedure RegisterMethod(ATypeName, AMethodName, ASignature: string);
    function FunctionExists(AName: string; AType: string = 'core'): boolean;
    function Execute(AName: string; var AArgList: TInstanceList): TInstanceOf;
    function Execute(AName: string; AInstance:TInstanceOf; var AArgList: TInstanceList): TInstanceOf;
  end;

var

  RegisteredMethods: TBootStrap;
  TypeName: string;

implementation
uses
  StringInstanceClass;

constructor TBootStrap.Create;
begin
  FTypes := TFPObjectHashTable.Create();
  FMethodsOfTypes := TFPObjectHashTable.Create;
end;

procedure TBootStrap.RegisterMethods(ATypeName: string; AMethodList: TFPStringHashTable);
begin
  FTypes.Add(ATypeName, AMethodList);
end;

function TBootStrap.FunctionExists(AName: string; AType: string = 'core'): boolean;
var
  Ret: boolean = False;
  Ex: TFPStringHashTable;
begin
  Ex := TFPStringHashTable(FTypes[AType]);
  if Ex <> nil then
  begin
    if Ex[AName] <> '' then
      Ret := True;
  end;
  Result := Ret;
end;

procedure TBootStrap.RegisterType(ATypeName: string);
begin
  FTypes.Add(ATypeName, TFPStringHashTable.Create);
end;

procedure TBootStrap.RegisterMethod(ATypeName, AMethodName, ASignature: string);
begin
  try
    TFPStringHashTable(FTypes[ATypeName]).Add(AMethodName, ASignature);

  except
    raise Exception.Create('Type "' + ATypeName + '" does not exist');
  end;
end;


function TBootStrap.Execute(AName: string; AInstance:TInstanceOf; var AArgList: TInstanceList): TInstanceOf;
var
  conv: TStringInstance;
begin
  Conv := TStringInstance(AInstance);
  Conv.PArgs := AArgList;
  Conv.PMetname := AName;
  Result := Conv.Execute;
end;
function TBootStrap.Execute(AName: string; var AArgList: TInstanceList): TInstanceOf;
// is core
var
  ACore: TCoreFunction;
  Ret: TInstanceOf;
begin
  ACore := TCoreFunction.Create;
  Ret := ACore.Execute(AName, AArgList);
  ACore.Free;
  Result := Ret;
end;

begin
  RegisteredMethods := TBootStrap.Create;
  {$INCLUDE 'corefunctions.pp' }
  {$INCLUDE 'stringmethods.pp' }

end.
