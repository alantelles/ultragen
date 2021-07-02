unit CoreFunctionsClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, Strutils,
      InterpreterClass, InstanceOfClass, StringInstanceClass, DateTimeInstanceClass, ByteStreamClass, DBInstanceClass,
      ListInstanceClass, ServerClass, ARClass, HttpClientInstanceClass, JsonTools, httpdefs, BrookServerClass;

type
  TParamList = array of string;

  TCoreFunction = class
    private
      FFuncList : TStringList;
      FParams: TinstanceList;
      FObj: TInstanceOf;
      FInter: TInterpreter;
      procedure TraverseJsonObj(ANode: TJsonNode; var AHostDict: TDictionaryInstance);
      procedure TraverseJsonList(ANode: TJsonNode; var AHostList: TListInstance);
      procedure JsonValAssign(ANode: TJsonNode; var AInst: TDictionaryInstance);
      procedure JsonValAssign(ANode: TJsonNode; var AInst: TListInstance);

    public
      function Execute(AInter: TInterpreter; Fname:string; var AArgList:TInstanceList;var  AObj: TInstanceOf):TInstanceOf;


      //core
      // procs
      function Print:TInstanceOf;
      function InlinePrint:TInstanceOf;
      function ConcatValues: TStringInstance;

      // functions

      function GetTypeOf:TDataType;
      function CastToStr:TStringInstance;
      function CastToByte:TByteInstance;
      function CastToInt:TIntegerInstance;
      function CastToFloat:TFloatInstance;
      function Range: TListInstance;
      procedure DumpLive;
      function ParseJson: TInstanceOf;
      function ParseJson(AInput: string): TInstanceOf;
      function ParseJsonFile: TInstanceOf;


       //functions

      {$INCLUDE 'bytestream/declarations.pp'}
      {$INCLUDE 'string/declarations.pp'}
      {$INCLUDE 'list/declarations.pp'}
      {$INCLUDE 'integer/declarations.pp'}
      {$INCLUDE 'os/declarations.pp'}
      {$INCLUDE 'filesystem/declarations.pp'}
      {$INCLUDE 'dict/declarations.pp'}
      {$INCLUDE 'server/declarations.pp'}
      {$INCLUDE 'appresponse/declarations.pp'}
      {$INCLUDE 'brookappresponse/declarations.pp'}
      {$INCLUDE 'httpclient/declarations.pp'}
      {$INCLUDE 'brookserver/declarations.pp'}
      {$INCLUDE 'helpers/declarations.pp' }
	end;

var
  ACookie: TCookie;

implementation

uses
  CoreUtils, ExceptionsClasses, Math, ASTClass, crt, LazUTF8, FileUtil, Dos, Tokens, MarkdownProcessor, DateUtils,
  BrookHTTPResponse, UltraWebHandlersClass, HttpProtocol;

function TCoreFunction.Execute(AInter: TInterpreter; Fname:string; var AArgList:TInstanceList; var AObj: TInstanceOf):TInstanceOf;
var
  AType:string = '';
  AuxStr:string;
  AuxDateTime: TDateTime;
  AuxBool: boolean;
  DotPos, len, i, j, AuxInt: integer;
  Ret, Aux: TInstanceOf;
  MDProc: TMarkdownProcessor;
  AuxStrList: TStringList;
begin
  FInter := AInter;
  Ret := TNullInstance.create;
  if AObj <> nil then
  begin
    if AObj.ClassNameIs('TDataType') then
    begin
      AType := TDataType(AObj).PValue;
      FObj := AObj;
    end
    else
    begin
      AType := AObj.ClassName;
      FObj := AObj;
    end;
  end
  else
    AType := 'TCoreInstance';
  FParams := AArgList;

  if AType = 'TCoreInstance' then
  begin
    if FName = 'print' then
	    Ret := Print
    else if FName = 'inline' then
	    Ret := InlinePrint
    else if FName = 'concat' then
	    Ret := ConcatValues
    else if FName = 'saveLive' then
      DumpLive
    else if FName = 'members' then
    begin
      if FParams[0].ClassNameIs('TDataType') then
      begin
        len := FParams[0].PMembers.Count;
        if len > 0 then
        begin
		      for i:=0 to len-1 do
		      begin
		        writeln(FParams[0].PMembers.NameOfIndex(i),': ', TInstanceOf(FParams[0].PMembers[i]).AsString);
				  end;
        end;
      end
      else
      begin
        Aux := Finter.PCallStack.Peek.GetTypeByInternalName(FParams[0].ClassName);
        len := FPArams[0].PMembers.Count;
        if len > 0 then
        begin
          for i:=0 to len-1 do
          begin
            writeln(FPArams[0].PMembers.NameOfIndex(i),': ', TinstanceOf(FPArams[0].PMembers[i]).AsString);
					    end;
				   end;
      end;
		end

    //system
    else if FName = 'clear' then
      clrscr
    else if FName = 'raise' then
    begin
      if Length(FParams) = 1 then
        FInter.RaiseException(FParams[0].AsString, 'Client')
      else if Length(FParams) = 2 then
        FInter.RaiseException(FParams[0].AsString, FParams[1].PStrValue)
      else
        FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
    end
    else if FName = 'input' then
    begin
      if Length(FParams) > 1 then
        AInter.RaiseException(E_INVALID_ARGS, 'Arguments');
      if Length(FParams) = 1 then
        Write(TStringInstance(FParams[0]).PValue);
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
    else if FName = 'byte' then
      Ret := CastToByte
    else if FName = 'float' then
      Ret := CastToFloat
    else if FName = 'int' then
      Ret := CastToInt
    else if FName = 'addModulePath' then
    begin
      len := Length(FParams);
      if len < 1 then
        Finter.RaiseException(E_INVALID_ARGS, 'Arguments')
      else
      begin
        for i:=0 to len - 1 do
        begin
          if (FParams[0].ClassNameIs('TStringInstance')) then
          begin
            if Finter.PModulesPath.IndexOf(FParams[0].AsString) < 0 then
              FInter.PModulesPath.Add(FParams[0].AsString)
					end
					else
            FInter.RaiseException(E_INVALID_ARGS_TYPE+ ', must be String', 'Arguments');
        end
      end
		end
    else if FName = 'urlEncode' then
        Ret := TStringInstance.Create(httpEncode(FParams[0].AsString))
    else if FName = 'urlDecode' then
        Ret := TStringInstance.Create(httpDecode(FParams[0].AsString))

    else if FName = 'locals' then
      Ret := TDictionaryInstance.Create(FInter.PCallStack.Peek)

    else if FName = 'dropModulePath' then
    begin
      len := Length(FParams);
      if len < 1 then
        Finter.RaiseException(E_INVALID_ARGS, 'Arguments')
      else
      begin
        for i:=0 to len - 1 do
        begin
          if (FParams[0].ClassNameIs('TStringInstance')) then
          begin
            j := Finter.PModulesPath.IndexOf(FParams[0].AsString);
            if j > -1 then
              FInter.PModulesPath.Delete(j);
					end
					else
            FInter.RaiseException(E_INVALID_ARGS_TYPE+ ', must be String', 'Arguments');
        end
      end
		end
		{else if FName = 'parseJson' then
      Ret := ParseJson}
    else
      raise ERunTimeError.Create('Referenced function "' + FName + '" does not exist.', '', 1, 1);
  // procs
	end

  else if Atype = 'TDBInstance' then
  begin
    if FName = 'connect' then
    begin
      TDBInstance(FObj).Connect;
    end
    else if FName = 'query' then
    begin
      TDBInstance(FObj).QueryDb;
    end
    else if FName = 'close' then
    begin
      TDBInstance(FObj).Disconnect;
    end
    else if FName = 'create' then
    begin
      Ret := TDBInstance.CreatePgConn;
    end;

  end

  else if AType = 'TMarkdownParserInstance' then
  begin
    if Fname = 'parse' then
    begin
      if Length(FParams) = 2 then
      begin
        MdProc := TMarkdownProcessor.CreateDialect(mdDaringFireball);
        MdProc.UnSafe := TBooleanInstance(FParams[1]).PValue;
      end
      else
        MdProc := TMarkdownProcessor.CreateDialect(mdCommonMark);
      Ret := TStringInstance.Create(MdProc.process(TStringInstance(FParams[0]).PValue));
      MdProc.Free;
    end
    else if FName = 'parseFile' then
    begin
      AuxStrList := TStringList.Create;
      AuxStrList.SkipLastLineBreak := True;
      AuxStrList.LoadFromFile(TStringInstance(FParams[0]).PValue);
      if Length(FParams) = 2 then
      begin
        MdProc := TMarkdownProcessor.CreateDialect(mdDaringFireball);
        MdProc.UnSafe := TBooleanInstance(FParams[1]).PValue;
      end
      else
        MdProc := TMarkdownProcessor.CreateDialect(mdCommonMark);
      Ret := TStringInstance.Create(MdProc.Process(AuxStrList.Text));
      Mdproc.Free;

    end;
  end

  else if AType = 'TJsonInstance' then
  begin
    if FName = 'parse' then
      Ret := ParseJson
    else if FName = 'parseFile' then
      Ret := ParseJsonFile
  end
  {$INCLUDE 'string/options.pp'}
  {$INCLUDE 'brookuploaded/options.pp'}
  {$INCLUDE 'list/options.pp'}
  {$INCLUDE 'integer/options.pp'}
  {$INCLUDE 'dict/options.pp'}
  {$INCLUDE 'os/options.pp'}
  {$INCLUDE 'helpers/options.pp'}
  {$INCLUDE 'filesystem/options.pp'}
  {$INCLUDE 'server/options.pp'}
  {$INCLUDE 'appresponse/options.pp'}
  {$INCLUDE 'httpclient/options.pp'}
  {$INCLUDE 'datetime/options.pp'}
  {$INCLUDE 'bytestream/options.pp'}
  {$INCLUDE 'brookserver/options.pp'}
  {$INCLUDE 'brookappresponse/options.pp'}

  else
    //raise ERunTimeError.Create('Referenced function "' + FName + '" does not exist.', '', 1, 1);
    FInter.RaiseException('Referenced function "' + FName + '" does not exist.', 'Name');
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

procedure TCorefunction.TraverseJsonList(ANode: TJsonNode; var AHostList: TListInstance);
var
  i: TJsonNode;
  ANewDict: TDictionaryInstance;
  Anewlist: TListInstance;
begin
  for i in ANode do
  begin
    if i.Kind = nkArray then
    begin
      Anewlist := TListInstance.Create;
      AHostList.Add(AnewList);
      TraverseJsonList(i, ANewList)
		end
		else if i.Kind = nkObject then
    begin
      ANewDict := TDictionaryInstance.Create(TActivationRecord.Create('json', AR_DICT, 1));
      AHostList.Add(AnewDict);
      TraverseJsonObj(i, ANewDict);
    end
    else
      JsonValAssign(i, AHostList);
	end;
end;

procedure TCorefunction.TraverseJsonObj(ANode: TJsonNode; var AHostDict: TDictionaryInstance);
var
  i: TJsonNode;
  ANewDict: TDictionaryInstance;
  ANewList: TListInstance;
begin
  for i in ANode do
  begin
    if i.Kind = nkArray then
    begin
      Anewlist := TListInstance.Create;
      AHostDict.PValue.AddMember(i.Name, ANewList);
      TraverseJsonList(i, ANewList)
		end
		else if i.Kind = nkObject then
    begin
      ANewDict := TDictionaryInstance.Create(TActivationRecord.Create('json', AR_DICT, 1));
      AHostDict.PValue.AddMember(i.Name, AnewDict);
      TraverseJsonObj(i, ANewDict);
    end
    else
      JsonValAssign(i, AHostDict);
	end;
end;

procedure TCoreFunction.JsonValAssign(ANode: TJsonNode; var AInst: TListInstance);
begin
  if ANode.Kind = nkString then
    AInst.add(TStringInstance.Create(ANode.AsString))
  else if ANode.Kind = nkBool then
    AInst.add( TBooleanInstance.Create(ANode.AsBoolean))
  else if ANode.Kind = nkNumber then
  begin
    if Pos('.', ANode.Value) > 0 then
      AInst.add(TFloatInstance.Create(ANode.AsNumber))
    else
      AInst.add(TIntegerInstance.Create(Floor(ANode.AsNumber)));
  end
  else if ANode.Kind = nkNull then
    AInst.add(TNullInstance.Create());
end;

procedure TCoreFunction.JsonValAssign(ANode: TJsonNode; var AInst: TDictionaryInstance);
begin
  if ANode.Kind = nkString then
  begin
    AInst.PValue.AddMember(ANode.Name, TStringInstance.Create(ANode.AsString))
	end
  else if ANode.Kind = nkBool then
    AInst.PValue.AddMember(ANode.Name, TBooleanInstance.Create(ANode.AsBoolean))
  else if ANode.Kind = nkNumber then
  begin
    if Pos('.', ANode.Value) > 0 then
      AInst.PValue.AddMember(ANode.Name, TFloatInstance.Create(ANode.AsNumber))
    else
      AInst.PValue.AddMember(ANode.Name, TIntegerInstance.Create(Floor(ANode.AsNumber)));
  end
  else if ANode.Kind = nkNull then
    AInst.PValue.AddMember(ANode.Name, TNullInstance.Create());
end;

function TCoreFunction.ParseJson(AInput: string): TInstanceOf;
var
  AJson: TJsonNode;
  ADict: TDictionaryInstance;
  AList: TListInstance;
begin


  AJson := TJsonNode.Create;
  try
    AJson.Parse(AInput);
  except on E: Exception do
    Raise Exception.Create(E.Message);
  end;
	//Traverse(AJson, ADict, AList, AJson.Kind);
  if AJson.Kind = nkArray then
  begin
    AList := TListInstance.Create();
    TraverseJsonList(AJson, AList);
    Result := AList;
	end
	else
  begin
    ADict := TDictionaryInstance.Create(TActivationRecord.Create('json', AR_DICT, 1));
    TraverseJsonObj(AJson, ADict);
    Result := ADict;
  end;
end;

function TCoreFunction.ParseJson: TInstanceOf;
var
  AJson: TJsonNode;
  ADict: TDictionaryInstance;
  AList: TListInstance;
begin


  AJson := TJsonNode.Create;
  try
    AJson.Parse(TStringInstance(FParams[0]).PValue);

	except on E: Exception do
    FInter.RaiseException(E.Message, Copy(e.ClassName, 2, Length(E.ClassName)));
	end;
	//Traverse(AJson, ADict, AList, AJson.Kind);
  if AJson.Kind = nkArray then
  begin
    AList := TListInstance.Create();
    TraverseJsonList(AJson, AList);
    Result := AList;
	end
	else
  begin
    ADict := TDictionaryInstance.Create(TActivationRecord.Create('json', AR_DICT, 1));
    TraverseJsonObj(AJson, ADict);
    Result := ADict;
	end;
end;

function TCoreFunction.ParseJsonFile: TInstanceOf;
var
  AJson: TJsonNode;
  ADict: TDictionaryInstance;
  AList: TListInstance;
  AStrList: TStringList;
  AStr: string;
begin


  AJson := TJsonNode.Create;
  try
    AStrList := TStringList.Create;
    AStrList.LoadFromFile(TStringInstance(FParams[0]).PValue);
    AStr := AStrList.Text;
    AStrList.Free;
    AJson.Parse(AStr);

	except on E: Exception do
    FInter.RaiseException(E.Message, Copy(e.ClassName, 2, Length(E.ClassName)));
	end;
	//Traverse(AJson, ADict, AList, AJson.Kind);
  if AJson.Kind = nkArray then
  begin
    AList := TListInstance.Create();
    TraverseJsonList(AJson, AList);
    Result := AList;
	end
	else
  begin
    ADict := TDictionaryInstance.Create(TActivationRecord.Create('json', AR_DICT, 1));
    TraverseJsonObj(AJson, ADict);
    Result := ADict;
	end;
end;

function TCoreFunction.Print:TInstanceOf;
var
  AInst: TInstanceOf;
  AFloat: Extended;
begin
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

function TCoreFunction.ConcatValues:TStringInstance;
var
  AInst: TInstanceOf;
  AFloat: Extended;
  AFStr:string = '';
begin
  for AInst in FParams do
    AFStr := AFstr + AInst.AsString;
  Result := TStringInstance.Create(AFStr);
end;

function TCoreFunction.GetTypeOf:TDataType;
var
  Astr: string;
  AR:TActivationRecord;
  i, nowLevel: integer;
  GotType: TInstanceOf = nil;
begin

  AStr := FParams[0].ClassName;
  if FParams[0].ClassNameIs('TDataType') then
  begin
    AStr := 'DataType';
    AR := FInter.PCallStack.Peek();
    for i := AR.PNestingLevel downto 1 do
    begin
      GotType := AR.GetMember(AStr);
      if GotType <> nil then
        break
      else
        if i > 1 then
          AR := FInter.PCallStack.GetByLevel(i - 1);
    end;
    if GotType <> nil then
      AStr := TDataType(GotType).PFrontName;
  end

  else if FParams[0].ClassNameIs('TClassInstance') then
  begin
    AStr := TClassInstance(FParams[0]).PValue;
    AR := FInter.PCallStack.Peek();
    for i := AR.PNestingLevel downto 1 do
    begin
      GotType := AR.GetMember(AStr);
      if GotType <> nil then
        break
      else
        if i > 1 then
          AR := FInter.PCallStack.GetByLevel(i - 1);
    end;
    if GotType <> nil then
      AStr := TDataType(GotType).PFrontName;
  end
  else
  begin
    AR := FInter.PCallStack.Peek();
    for i := AR.PNestingLevel downto 1 do
    begin
      GotType := AR.GetTypeByInternalName(AStr);
      if GotType <> nil then
        break
      else
        if i > 1 then
          AR := FInter.PCallStack.GetByLevel(i - 1);
    end;
    if GotType <> nil then
      AStr := TDataType(GotType).PFrontName;
  end;
  Result := TDataType(GotType);
end;

function TCoreFunction.CastToStr:TStringInstance;
begin
  Result := TStringInstance.Create(FParams[0].AsString);
end;

function TCoreFunction.CastToByte:TByteInstance;
begin
  if FParams[0].ClassNameIs('TIntegerInstance') then
  begin
    if (TIntegerInstance(FParams[0]).PValue > -1) and (TIntegerInstance(FParams[0]).PValue < 256) then
      Result := TByteInstance.Create(TIntegerInstance(FParams[0]).PValue)
    else
      FInter.RaiseException('Value out of range for byte casting', 'RunTime');
  end
  else
    FInter.RaiseException('Invalid value for byte casting', 'RunTime');
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
      FInter.RaiseException('Can''t convert value "'+TStringInstance(FParams[0]).PValue+'" to integer', 'Type');
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

function TCoreFunction.CastToFloat:TFloatInstance;
var
  ResInt: extended;
begin
  if FParams[0].ClassNameIs('TFloatInstance') then
    Result := TFloatInstance(FParams[0])
  else if FParams[0].ClassNameIs('TStringInstance') then
  begin
    try
      ResInt := StrToFloat(TStringInstance(FParams[0]).PValue);
      Result := TFloatInstance.Create(ResInt);
    except
      FInter.RaiseException('Can''t convert value "'+TStringInstance(FParams[0]).PValue+'" to float', 'Type');
    end;
  end
  else if FParams[0].ClassNameIs('TIntegerInstance') then
  begin
    Result := TFloatInstance.Create(FParams[0].PIntValue * 1.0);

  end
  else
    raise ETypeError.Create('Can''t convert value "'+FParams[0].ClassName+'" to float', '', 1, 1);
end;



{$INCLUDE 'string/functions.pp'}
{$INCLUDE 'list/functions.pp'}
{$INCLUDE 'bytestream/functions.pp'}

{$INCLUDE 'os/functions.pp'}
{$INCLUDE 'helpers/functions.pp'}

{$INCLUDE 'filesystem/functions.pp'}
{$INCLUDE 'dict/functions.pp'}
{$INCLUDE 'server/functions.pp'}
{$INCLUDE 'appresponse/functions.pp'}
{$INCLUDE 'brookappresponse/functions.pp'}

{$INCLUDE 'brookserver/functions.pp'}

{$INCLUDE 'integer/functions.pp'}
{$INCLUDE 'httpclient/functions.pp'}



end.
