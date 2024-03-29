unit CoreFunctionsClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, Strutils,
      InterpreterClass, InstanceOfClass, StringInstanceClass, RegexInstanceClass, DateTimeInstanceClass, ByteStreamClass, DBInstanceClass,
      ListInstanceClass, ServerClass, ARClass, HttpClientInstanceClass, JsonTools, httpdefs, BrookServerClass;

type
  TParamList = array of string;
  TParamCountList = array of integer;

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
      function checkArgTypes(TypesAllowed: TParamList): string;
      procedure checkArgCount(CountsAllowed: TParamCountList);
      procedure checkMinArgCount(MinArgCount: integer);

    public
      function Execute(AInter: TInterpreter; Fname:string; var AArgList:TInstanceList;var  AObj: TInstanceOf):TInstanceOf;


      //core
      // procs
      function Print:TInstanceOf;
      function InlinePrint:TInstanceOf;
      function ConcatValues: TStringInstance;
      procedure LocalizeAttrs;
      procedure SetObjAttr;

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

procedure TCoreFunction.checkArgCount(CountsAllowed: TParamCountList);
var
  i, lenArgs: integer;
begin
  lenArgs := Length(FParams);
  for i in CountsAllowed do
  begin
    if lenArgs = i then
      Exit;
  end;
  FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
end;

procedure TCoreFunction.checkMinArgCount(MinArgCount: integer);
begin
  if Length(FParams) < MinArgCount then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
end;

function TCoreFunction.checkArgTypes(TypesAllowed: TParamList): string;
var
  i, lenArgs, lenTypes, len: integer;
  Ret: string = '';
begin
  lenTypes := Length(TypesAllowed);
  lenArgs := Length(FParams);
  len := min(lenTypes, lenArgs);
  if len > 0 then
  begin
    for i:=0 to len - 1 do
    begin
      if (TypesAllowed[i] <> '*') and (FParams[i].PTypeFrontName <> TypesAllowed[i]) then
      begin
         Ret := 'Wrong type for argument ' + IntToStr(i) + '. Expected "' + TypesAllowed[i] + '", got "' + FParams[i].PtypeFrontName + '"';
         FInter.RaiseException(Ret, 'Arguments');
         Break;
      end;
    end;
  end;
  Result := Ret;
end;

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
    else if FName = 'localizeAttrs' then
      LocalizeAttrs
    else if Fname = 'setAttr' then
      SetObjAttr
    else if FName = 'members' then
    begin
      checkArgCount([1]);
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
    begin
      checkArgCount([0]);
      clrscr
    end
    else if FName = 'raise' then
    begin
      checkArgCount([1, 2]);
      checkArgTypes(['TStringInstance', 'TStringInstance']);
      if Length(FParams) = 1 then
        FInter.RaiseException(FParams[0].AsString, 'Client')
      else if Length(FParams) = 2 then
        FInter.RaiseException(FParams[0].AsString, FParams[1].PStrValue)
      else
        FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
    end
    else if FName = 'input' then
    begin
      checkArgCount([0, 1]);
      checkArgTypes(['TStringInstance']);
      if Length(FParams) = 1 then
        Write(TStringInstance(FParams[0]).PValue);
      ReadLn(AuxStr);
      Ret := TStringInstance.Create(AuxStr)
    end
    else if FName = 'pause' then
    begin
      checkArgCount([1]);
      checkArgTypes(['TIntegerInstance']);
      Sleep(TIntegerInstance(FParams[0]).PValue);
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
      checkArgCount([1]);
      checkArgTypes(['TStringInstance']);
      if Finter.PModulesPath.IndexOf(FParams[0].PStrValue) < 0 then
        FInter.PModulesPath.Add(FParams[0].PStrValue);
    end
    else if FName = 'urlEncode' then
    begin
      checkArgCount([1]);
      checkArgTypes(['TStringInstance']);
        Ret := TStringInstance.Create(httpEncode(FParams[0].PStrValue))
    end
    else if FName = 'urlDecode' then
    begin
      checkArgCount([1]);
      checkArgTypes(['TStringInstance']);
      Ret := TStringInstance.Create(httpDecode(FParams[0].PStrValue))
    end

    else if FName = 'locals' then
    begin
      checkArgCount([0]);
      Ret := TDictionaryInstance.Create(FInter.PCallStack.Peek)
    end
    else if FName = 'dropModulePath' then
    begin
      checkArgCount([1]);
      checkArgTypes(['TStringInstance']);
      j := Finter.PModulesPath.IndexOf(FParams[0].PStrValue);
      if j > -1 then
        FInter.PModulesPath.Delete(j);
    end
		{else if FName = 'parseJson' then
      Ret := ParseJson}
    else
      FInter.RaiseException('Referenced function "' + FName + '" does not exist.', 'RumTime');
  // procs
  end

  else if Atype = 'TDBInstance' then
  begin
    if FName = 'connect' then
    begin
      checkArgCount([0]);
      TDBInstance(FObj).StartConnection;
    end
    else if FName = 'query' then
    begin
      checkArgCount([1, 2]);
      checkArgTypes(['TStringInstance']);
      if Length(FParams) = 1 then
      begin
        try
          Ret := TDBInstance(FObj).QueryDb(FParams[0].PStrValue);
        except on E: Exception do
          FInter.RaiseException('Error while executing database operation: ' + E.Message, 'UltraDb')
        end;
      end
      else if Length(FParams) = 2 then
      begin
        Ret := TDBInstance(FObj).QueryDb(FPArams[0].PStrValue, FParams[1]);
      end
      else
        FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
    end
    else if FName = 'close' then
    begin
      TDBInstance(FObj).Disconnect;
    end
    else if FName = 'create' then
    begin
      checkArgCount([1]);
      checkArgTypes(['TIntegerInstance']);
      Ret := TDBInstance.Create(FParams[0].PIntValue);
    end;

  end

  else if AType = 'TMarkdownParserInstance' then
  begin
    if Fname = 'parse' then
    begin
      checkArgCount([1, 2]);
      checkArgTypes(['TStringInstance', 'TBooleanInstance']);
      if Length(FParams) = 2 then
      begin
        MdProc := TMarkdownProcessor.CreateDialect(mdDaringFireball);
        MdProc.UnSafe := TBooleanInstance(FParams[1]).PValue;
      end
      else
        MdProc := TMarkdownProcessor.CreateDialect(mdCommonMark);
      Ret := TStringInstance.Create(MdProc.process(FParams[0].PStrValue));
      MdProc.Free;
    end
    else if FName = 'parseFile' then
    begin
      checkArgCount([1, 2]);
      checkArgTypes(['TStringInstance', 'TBooleanInstance']);
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
  {$INCLUDE 'regex/options.pp'}
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

procedure TCoreFunction.SetObjAttr;
var
  AName: string;
begin
  checkArgCount([3]);
  checkArgTypes(['*', 'TStringInstance', '*']);
  AName := FParams[1].PStrValue;
  FParams[0].PMembers.Add(AName, FParams[2]);
end;

procedure TCoreFunction.DumpLive;
var
  AFile: TStringList;
begin
  checkArgCount([1]);
  checkArgTypes(['TStringInstance']);
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
        FInter.RaiseException(E.Message, 'Internal');
    end;

  finally
    AFile.Free;
  end;
end;

procedure TCoreFunction.LocalizeAttrs;
var
  AInst, AMember: TInstanceOf;
  AActRec: TActivationRecord;
  i: integer;
begin
  checkArgCount([1]);
  AActRec := FInter.PCallStack.Peek;
  AInst := FParams[0];
  if AInst.PMembers.Count > 0 then
  begin
    for i:=0 to AInst.PMembers.Count - 1 do
    begin
      AMember := TInstanceOf(AInst.PMembers[i]);
      AActRec.AddMember(AInst.PMembers.NameOfIndex(i), AMember);
    end;
  end;
end;

function TCoreFunction.Range: TListInstance;
var
  start, asize, lenArgs, len, i, counter, step:integer;
  Alist: TInstanceList;
begin
  checkArgCount([1,2,3]);
  checkArgTypes(['TIntegerInstance', 'TIntegerInstance', 'TIntegerInstance']);
  lenArgs := Length(FParams);
  step := 1;
  asize := 0;
  if lenArgs = 3 then
    step := TIntegerInstance(FParams[2]).PValue;
  counter := 0;
  if lenArgs = 1 then
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
  else
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
  end;
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
    FInter.RaiseException(E.Message, 'JSON');
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
  checkArgCount([1]);
  checkArgTypes(['TStringInstance']);
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
  checkArgCount([1]);
  checkArgTypes(['TStringInstance']);
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
  checkArgCount([1]);
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
  if GotType = nil then
    FInter.RaiseException('Internally named type ' + AStr + ' is not loaded on application', 'Type');
  Result := TDataType(GotType);
end;

function TCoreFunction.CastToStr:TStringInstance;
begin
  checkArgCount([1]);

  Result := TStringInstance.Create(FParams[0].AsString);
end;

function TCoreFunction.CastToByte:TByteInstance;
begin
  checkArgCount([1]);
  checkArgTypes(['TIntegerInstance']);
  if (TIntegerInstance(FParams[0]).PValue > -1) and (TIntegerInstance(FParams[0]).PValue < 256) then
    Result := TByteInstance.Create(TIntegerInstance(FParams[0]).PValue)
  else
    FInter.RaiseException('Value out of range for byte casting', 'RunTime');
end;

function TCoreFunction.CastToInt:TIntegerInstance;
var
  ResInt: integer;
begin
  checkArgCount([1]);

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
      FInter.RaiseException('Can''t convert value "'+FloatToStr(TFloatInstance(FParams[0]).PValue)+'" to integer', 'RunTime');
  end
  else
    FInter.RaiseException('Can''t convert value "'+FParams[0].ClassName+'" to integer', 'RunTime');
end;

function TCoreFunction.CastToFloat:TFloatInstance;
var
  ResInt: extended;
begin
  checkArgCount([1]);
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
    FInter.RaiseException('Can''t convert value "'+FParams[0].ClassName+'" to float', 'Type');

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
