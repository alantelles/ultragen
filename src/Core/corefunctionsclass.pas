unit CoreFunctionsClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, Strutils, InstanceOfClass, StringInstanceClass, ListInstanceClass;

type
  TParamList = array of string;

  TCoreFunction = class
    private
      FFuncList : TStringList;
      FParams: TinstanceList;
    public
      function Execute(Fname:string; var AArgList:TInstanceList ):TInstanceOf;


      //core
      // procs
      function Print:TInstanceOf;
      function InlinePrint:TInstanceOf;
      // functions
      function GetTypeOf:TStringInstance;
      function CastToStr:TStringInstance;
      function CastToInt:TIntegerInstance;

       //functions

      {$INCLUDE 'string/declarations.pp'}
      {$INCLUDE 'list/declarations.pp'}
	end;



implementation

uses
  CoreUtils, ExceptionsClasses, Math;

function TCoreFunction.Execute(Fname:string; var AArgList:TInstanceList ):TInstanceOf;
begin
  FParams := AArgList;

  // procs
  if FName = 'print' then
    Result := Print
  else if FName = 'inline' then
    Result := InlinePrint

  {$INCLUDE 'string/options.pp'}
  {$INCLUDE 'list/options.pp'}
  // functions
  else if FName = 'typeof' then
    Result := GetTypeOf
  else if FName = 'str' then
    Result := CastToStr
  else if FName = 'int' then
    Result := CastToInt;
end;

function TCoreFunction.Print:TInstanceOf;
var
  AInst: TInstanceOf;
  AFloat: Extended;
  AFStr:string;
begin
  for AInst in FParams do
  begin
    if AInst.ClassNameIs('TIntegerInstance') then
      Write(TIntegerInstance(AInst).PValue)
    else if AInst.ClassNameIs('TBooleanInstance') then
      Write(BooleanToStr(TBooleanInstance(AInst).PValue))
    else if AInst.ClassNameIs('TFloatInstance') then
    begin
      AFloat := TFloatInstance(AInst).PValue;
      AFStr := FloatToStrF(AFloat, ffFixed, 30, 30);
      while AFStr[Length(AFStr)] = '0' do
        AFStr := Copy(AFStr, 1, Length(AFStr) - 1);
      Write(AFStr);
    end
    else if AInst.ClassNameIs('TListInstance') then
    begin
      Write(TListInstance(AInst).AsString)
    end
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

function TCoreFunction.GetTypeOf:TStringInstance;
begin
  Result := TStringInstance.Create(FParams[0].ClassName);
end;

function TCoreFunction.CastToStr:TStringInstance;
begin
  if FParams[0].ClassNameIs('TIntegerInstance') then
    Result := TStringInstance.Create(IntToStr(TIntegerInstance(FParams[0]).PValue))
  else if FParams[0].ClassNameIs('TStringInstance') then
    Result := TStringInstance(FParams[0])
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

function TCoreFunction.CastToInt:TIntegerInstance;
var
  ResInt: integer;
begin
  if FParams[0].ClassNameIs('TIntegerInstance') then
    Result := TIntegerInstance(FParams[0])
  else if FParams[0].ClassNameIs('TStringInstance') then
  begin
    try
      ResInt := StrToInt(TStringInstance(FParams[0]).PValue);
      Result := TIntegerInstance.Create(ResInt);
    except
      raise ETypeError.Create('Can''t convert value "'+TStringInstance(FParams[0]).PValue+'" to integer');
    end;
  end
  else if FParams[0].ClassNameIs('TFloatInstance') then
  begin
    if Trunc(TFloatInstance(FParams[0]).PValue) = TFloatInstance(FParams[0]).PValue then
      Result := TIntegerInstance.Create(Trunc(TFloatInstance(FParams[0]).PValue))
    else
      raise ETypeError.Create('Can''t convert value "'+FloatToStr(TFloatInstance(FParams[0]).PValue)+'" to integer');
  end;
end;

{$INCLUDE 'string/functions.pp'}
{$INCLUDE 'list/functions.pp'}



end.

