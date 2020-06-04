unit CoreFunctionsClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, Strutils, InstanceOfClass;

type
  TParamList = array of string;

  TCoreFunction = class
    private
      FFuncList : TStringList;
      FParams: TinstanceList;
    public
      constructor Create;
      function FunctionExists(FName:string):boolean;
      function Execute(Fname:string; var AArgList:TInstanceList ):TInstanceOf;


      //core
      function Print:TInstanceOf;
      function InlinePrint:TInstanceOf;
      function CastToStr:TStringInstance;
	end;

implementation

uses
  CoreUtils;

constructor TCoreFunction.Create;
begin
  FFuncList := TStringList.Create;
  FFuncList.Add('print');
  FFuncList.Add('inline');
  FFuncList.Add('str');
end;

function TCoreFunction.FunctionExists(FName:string):boolean;
begin
  Result := FFuncList.IndexOf(Fname) > -1;
end;

function TCoreFunction.Execute(Fname:string; var AArgList:TInstanceList ):TInstanceOf;
begin
  FParams := AArgList;
  if FName = 'print' then
    Result := Print
  else if FName = 'inline' then
    Result := InlinePrint
  else if FName = 'str' then
    Result := CastToStr;
end;

function TCoreFunction.CastToStr:TStringInstance;
begin
  if FParams[0].ClassNameIs('TIntegerInstance') then
    Result := TStringInstance.Create(IntToStr(TIntegerInstance(FParams[0]).PValue))
  else if FParams[0].ClassNameIs('TStringInstance') then
    Result := TStringInstance.Create(TStringInstance(FParams[0]).PValue)
  else if FParams[0].ClassNameIs('TFloatInstance') then
    Result := TStringInstance.Create(FloatToStr(TFloatInstance(FParams[0]).PValue))
  else if FParams[0].ClassNameIs('TNullInstance') then
    Result := TStringInstance.Create(TNullInstance(FParams[0]).PValue)
  else if FParams[0].ClassNameIs('TBooleanInstance') then
  begin
    if TBooleanInstance(FParams[0]).PValue then
      Result := TStringInstance.Create('true')
    else
      Result := TStringInstance.Create('false');
	end;
end;

function TCoreFunction.Print:TInstanceOf;
var
  AInst: TInstanceOf;
begin
  for AInst in FParams do
  begin
    if AInst.ClassNameIs('TIntegerInstance') then
      Write(TIntegerInstance(AInst).PValue)
    else if AInst.ClassNameIs('TBooleanInstance') then
      Write(BooleanToStr(TBooleanInstance(AInst).PValue))
    else if AInst.ClassNameIs('TFloatInstance') then
      Write(TFloatInstance(AInst).PValue)
    else if AInst.ClassNameIs('TStringInstance') then
      Write(TStringInstance(AInst).PValue)
    else if AInst.ClassNameIs('TNullInstance') then
      Write(TNullInstance(AInst).PValue);
	end;
  WriteLn;
  Result := TNullInstance.create;
end;

function TCoreFunction.InlinePrint:TInstanceOf;
var
  AInst: TInstanceOf;
begin
  for AInst in FParams do
  begin
    if AInst.ClassNameIs('TIntegerInstance') then
      Write(TIntegerInstance(AInst).PValue)
    else if AInst.ClassNameIs('TBooleanInstance') then
      Write(TBooleanInstance(AInst).PValue)
    else if AInst.ClassNameIs('TFloatInstance') then
      Write(TFloatInstance(AInst).PValue)
    else if AInst.ClassNameIs('TStringInstance') then
      Write(TStringInstance(AInst).PValue)
    else if AInst.ClassNameIs('TNullInstance') then
      Write(TNullInstance(AInst).PValue);
	end;
  Result := TNullInstance.create;
end;

end.

