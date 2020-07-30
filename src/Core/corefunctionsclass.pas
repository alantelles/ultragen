unit CoreFunctionsClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, Strutils, InstanceOfClass, InterpreterClass, StringInstanceClass,
      ListInstanceClass, ServerClass, ARClass;

type
  TParamList = array of string;

  TCoreFunction = class
    private
      FFuncList : TStringList;
      FParams: TinstanceList;
      FObj: TInstanceOf;
      FInter: TInterpreter;
    public
      function Execute(AInter: TInterpreter; Fname:string; var AArgList:TInstanceList;var  AObj: TInstanceOf):TInstanceOf;


      //core
      // procs
      function Print:TInstanceOf;
      function InlinePrint:TInstanceOf;
      // functions

      function GetTypeOf:TStringInstance;
      function CastToStr:TStringInstance;
      function CastToInt:TIntegerInstance;
      function Range: TListInstance;
      procedure DumpLive;

       //functions

      {$INCLUDE 'string/declarations.pp'}
      {$INCLUDE 'list/declarations.pp'}
      {$INCLUDE 'integer/declarations.pp'}
      {$INCLUDE 'os/declarations.pp'}
      {$INCLUDE 'filesystem/declarations.pp'}
      {$INCLUDE 'dict/declarations.pp'}
      {$INCLUDE 'server/declarations.pp'}
	end;



implementation

uses
  CoreUtils, ExceptionsClasses, Math, ASTClass, crt, LazUTF8, FileUtil, Dos;

function TCoreFunction.Execute(AInter: TInterpreter; Fname:string; var AArgList:TInstanceList; var AObj: TInstanceOf):TInstanceOf;
var
  AType:string = '';
  AuxStr:string;
  DotPos, len, i: integer;
  Ret: TInstanceOf;

begin
  FInter := AInter;
  Ret := TNullInstance.create;
  if AObj <> nil then
  begin
    if AObj.ClassNameIs('TBuiltInType') then
      AType := TBuiltInType(AObj).PValue
    else
    begin
      AType := AObj.ClassName;
      FObj := AObj;
    end;
  end;
  FParams := AArgList;

  if AType = '' then
  begin
    if FName = 'print' then
	    Ret := Print
	  else if FName = 'inline' then
	    Ret := InlinePrint
    else if FName = 'saveLive' then
      DumpLive

    //system
    else if FName = 'clear' then
      clrscr
    else if FName = 'input' then
    begin
      ReadLn(AuxStr);
      Ret := TStringInstance.Create(AuxStr)
		end
		else if FName = 'pause' then
    begin
      if FParams[0].ClassNameIs('TIntegerInstance') then
        Sleep(TIntegerInstance(FParams[0]).PValue)
      else
        //raise EArgumentsError.Create('Argument type for this function must be integer', '', 1, 1);
        AInter.RaiseException('Argument type for this function must be integer', 'Arguments');
		end
		// end system
		else if FName = 'range' then
      Ret := Range
    else if FName = 'typeof' then
      Ret := GetTypeOf
    else if FName = 'str' then
      Ret := CastToStr
    else if FName = 'int' then
      Ret := CastToInt
    else
      raise ERunTimeError.Create('Referenced function "' + FName + '" does not exist.', '', 1, 1);
  // procs
	end
  {$INCLUDE 'string/options.pp'}
  {$INCLUDE 'list/options.pp'}
  {$INCLUDE 'integer/options.pp'}
  {$INCLUDE 'dict/options.pp'}
  {$INCLUDE 'os/options.pp'}
  {$INCLUDE 'filesystem/options.pp'}
  {$INCLUDE 'server/options.pp'}
  else
    raise ERunTimeError.Create('Referenced function "' + FName + '" does not exist.', '', 1, 1);
  // functions
  Result := Ret;
end;

procedure TCoreFunction.DumpLive;
var
  AFile: TStringList;
begin
  if Length(FParams) <> 1 then
    raise ERunTimeError.Create(E_INVALID_ARGS, '', 1, 1);
  AFile := TStringList.Create;
  AFile.SkipLastLineBreak := True;
  AFile.Text := Finter.GetLive;
  try
    try
      AFile.SaveToFile(TStringInstance(FParams[0]).PValue);

    except
      on E: Exception do
        raise ERunTimeError.Create(E.Message, '', 1, 1);
    end;

  finally
    AFile.Free;
  end;
end;

function TCoreFunction.Range: TListInstance;
var
  start, asize, len, i, counter, step:integer;
  Alist: TInstanceList;
begin
  step := 1;
  asize := 0;
  if Length(FParams) = 3 then
    step := TIntegerInstance(FParams[2]).PValue;
  counter := 0;
  if Length(FParams) = 1 then
  begin
    len := TIntegerInstance(FParams[0]).PValue;
    SetLength(AList, len);
    if len > 0 then
    begin
      for i:=0 to len - 1 do
      begin
        AList[counter] := TIntegerInstance.Create(i);
        counter := counter + 1;
			end;
		end;
	end
  else if (Length(FParams) = 2) or (Length(FParams) = 3) then
  begin
    start := TIntegerInstance(FParams[0]).PValue;
    len := TIntegerInstance(FParams[1]).PValue;
    SetLength(AList, 0);
    if len > start then //  3, 5
    begin
      for i:=start to len - 1 do
      begin
        if (counter mod step <> 0) then
        begin
          counter := counter + 1;
          continue;
				end;
        ASize := ASize + 1;
        SetLength(AList, ASize);
			  AList[Asize - 1] := TIntegerInstance.Create(i);
        counter := counter + 1;
			end;
		end
    else // 5, 3
    begin
      SetLength(AList, start - len);
      for i := start downto len + 1 do
      begin
        AList[counter] := TIntegerInstance.Create(i);
        counter := counter + 1;
			end;
    end;
	end
  else
    raise EArgumentsError.Create(E_INVALID_ARGS, '', 1, 1);
  Result := TListInstance.Create(AList);
end;



function TCoreFunction.Print:TInstanceOf;
var
  AInst: TInstanceOf;
  AFloat: Extended;
  AFStr:string;
begin
  {for AInst in FParams do
  begin
    Write(AInst.AsString);
	end;}
  InlinePrint;
  WriteLn;
  Result := TNullInstance.create;
end;

function TCoreFunction.InlinePrint:TInstanceOf;
var
  AInst: TInstanceOf;
  AFloat: Extended;
  AFStr:string;
begin
  for AInst in FParams do
  begin
    Write(AInst.AsString);
	end;
  Result := TNullInstance.create;
end;

function TCoreFunction.GetTypeOf:TStringInstance;
var
  Astr: string;
begin
  AStr := FParams[0].ClassName;
  if FParams[0].ClassNameIs('TBuiltInType') then
    AStr := 'BuiltInType: '+TBuiltInType(FParams[0]).AsString;
  Result := TStringInstance.Create(AStr);
end;

function TCoreFunction.CastToStr:TStringInstance;
begin
  Result := TStringInstance.Create(FParams[0].AsString);
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
      raise ETypeError.Create('Can''t convert value "'+TStringInstance(FParams[0]).PValue+'" to integer', '', 1, 1);
    end;
  end
  else if FParams[0].ClassNameIs('TFloatInstance') then
  begin
    if Trunc(TFloatInstance(FParams[0]).PValue) = TFloatInstance(FParams[0]).PValue then
      Result := TIntegerInstance.Create(Trunc(TFloatInstance(FParams[0]).PValue))
    else
      raise ETypeError.Create('Can''t convert value "'+FloatToStr(TFloatInstance(FParams[0]).PValue)+'" to integer', '', 1, 1);
  end
  else
    raise ETypeError.Create('Can''t convert value "'+FParams[0].ClassName+'" to integer', '', 1, 1);
end;

{$INCLUDE 'string/functions.pp'}
{$INCLUDE 'list/functions.pp'}
{$INCLUDE 'os/functions.pp'}

{$INCLUDE 'filesystem/functions.pp'}
{$INCLUDE 'dict/functions.pp'}
{$INCLUDE 'server/functions.pp'}

{$INCLUDE 'integer/functions.pp'}



end.

