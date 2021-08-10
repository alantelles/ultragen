unit InterpreterClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ASTClass,
  BinOpClass, NumClass, UnaryOpClass, BinLogicOpClass, UnaryLogicopClass,
  ImpParserClass, StrUtils, LoggingClass, FlowControlASTClass,
  StackClass,
  ARClass, InstanceofClass,
  StringInstanceClass, ExceptionsClasses,
  ListInstanceClass, UltraWebHandlersClass;

type
  TInterpreter = class
  private
    FTree: TAST;
    FCallStack: TStack;
    FBreakSignal: boolean;
    FReturnSignal: boolean;
    FReturnValue: TInstanceOf;
    FContinueSignal: boolean;
    FLiveOutput: string;
    FDontPush: boolean;
    FNameSpace, FInsertActRec: TActivationRecord;
    FInsertName: string;
    FParentStack: TStack;
    FTrace: TStringList;
    FNowNode: TAST;
    FUltraHome: string;
    FModulesPath: TStringList;
    FDontDestroyLast: boolean;
    FExceptionThrown: boolean;

    FWebHandlers: TUltraWebHandlers;
    FRedirected: boolean;
    procedure BootStrapRegister;
    function checkArgType(Arg: TInstanceOf; Expected: TVariableReference): boolean;


  public
    property PRedirected: boolean read FRedirected write FRedirected;
    property PCallStack: TStack read FCallStack write FCallStack;
    property PTree: TAST read FTree;
    property PExceptionThrown: boolean read FExceptionThrown write FExceptionThrown;
    property PWebHandlers: TUltraWebHandlers read FWebHandlers write FWebHandlers;
    property PLive: string read FLiveOutput;
    property PInsertActRec: TActivationRecord read FInsertActRec write FInsertActRec;
    property PModulesPath: TStringList read FModulesPath write FModulesPath;
    property PUltraHome: string read FUltraHome write FUltraHome;
    procedure RaiseException(AMsg: string; AType: string);
    constructor Create(var ATree: TAST);
    destructor Destroy; override;
    function GetLive: string;
    procedure PassCallStack(var ACallStack: TStack; ToParent: boolean);

    function Interpret(DontPush: boolean = False; ANameActRec: TActivationRecord = nil): string;
    function Interpret(InsertActRec: TActivationRecord; Insertname: string; DontDestroyLast: boolean = False): string;
    function ExecuteFunctionByInstance(AFunction: TFunctionInstance; Args: TASTList; ANode: TAST; ASrcinstance: TInstanceOf): TInstanceOf;
    function ProcessFuncArgs(var AFunction: TFunctionInstance; var AActRec: TActivationRecord; AEvalParams: TASTList): TInstanceList;


    function Visit(ANode: TAST; ASrcInstance: TInstanceOf = nil): TInstanceOf;
      {$INCLUDE 'interpreter/visitations_declarations.pp'}

  end;



implementation

uses
  Math, TokenClass, Tokens, CoreFunctionsClass, LexerClass, DateTimeInstanceClass, RegexInstanceClass,
  ServerClass, Dos, TypeLoaderClass, ByteStreamClass, HttpClientInstanceClass, BrookServerClass;

constructor TInterpreter.Create(var ATree: TAST);
begin
  FTree := ATree;
  FCallStack := TStack.Create;
  FBreakSignal := False;
  FTrace := TStringList.Create;
  FTrace.SkipLastLineBreak := True;
  FTrace.LineBreak := sLineBreak + '+ ';
  FExceptionThrown := False;
  //FUltraHome := GetEnv('ULTRAGEN_HOME');
  //FModulesPath := FUltraHome + DirectorySeparator + 'modules'
  FModulesPath := TStringList.Create;
  FRedirected := False;
end;

destructor TInterpreter.Destroy;
begin
  FModulesPath.Free;
end;

procedure TInterpreter.RaiseException(AMsg: string; AType: string);
begin
  FExceptionThrown := True;
  EClientException.Create(AMsg, FTrace, FNowNode.PToken, AType);
end;

procedure TInterpreter.BootstrapRegister;
const
  ST_ACCESS = ':';
var
  AActRec: TActivationRecord;
  FileExplorer: TActivationRecord;
  ACoreType, AStrType, AIntType, AFloatType, AListType, ABoolType,
  AFuncType, AOSType, ADictType, AServerType, AJsonType, ARegexType: TDataType;

  AStrFunc,
  AIntFunc, AIntStFunc, AListFunc, AServerFunc, AOSFunc, ADictFunc, AHttpClientFunc,
  ABoolFunc, AFloatFunc, ACoreFunc, AJsonFunc, ADataTypeFunc, ARegexFunc: TFunctionInstance;
  ANameSpace: TDictionaryInstance;
begin

  {AStrType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TStringInstance', True);
  AIntType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TIntegerInstance', True);}
  AFloatFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFloatInstance', True, False, False, False);

  {AListType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TListInstance', True);
  AFuncType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFunctionInstance', True);}
  ABoolFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TBooleanInstance', True, False, False, False);
  AJsonFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TJsonInstance', True, False, False, False);
  {ADictType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TDictionaryInstance', True);
  AOSType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TOSInstance', True);
  AFSType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFileSystemInstance', True);
  AServerType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TServerInstance', True);}

  AActRec := FCallStack.GetFirst();

  TTypeLoader.LoadType('ByteStream', self, AActrec);



  ABoolType := TDataType.Create('TBooleanInstance', 'Boolean');
  AFloatType := TDataType.Create('TFloatInstance', 'Float');
  AFuncType := TDataType.Create('TFunctionInstance', 'Function');

  // ACoreType := TDataType.Create('TCoreInstance', 'Core');
  ACoreFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TCoreInstance', True, False, False, False);

  // AActRec.AddMember('parseJson', ACoreFunc);

  //TTypeLoader.LoadType('Request', self, AActRec);

  {$INCLUDE 'builtin_functions/register_builtins.pp' }
  // response handler
//AServerType.PMembers.Add('redirect', AServerFunc);
//AServerType.PMembers.Add('setStatusCode', AServerFunc);
//AServerType.PMembers.Add('setStatusText', AServerFunc);
//AActRec.AddMember('Server', AServerType);

  // AActrec.AddMember('Core', ACoreType);
  AActRec.AddMember('DataType', TDataType.Create('DataType', 'DataType'));
  AActRec.AddMember('NullType', TDataType.Create('TNullInstance', 'NullType'));
  AActrec.AddMember('Boolean', ABoolType);
  AActrec.AddMember('Function', AFuncType);
  AActrec.AddMember('Float', AFloatType);

  AJsonType := TDataType.Create('TJsonInstance', 'JSON');
  AJsonType.PMembers.Add('parse', AJsonFunc);
  AJsonType.PMembers.Add('parseFile', AJsonFunc);
  AActRec.AddMember('JSON', AJsonType);
end;


procedure TInterpreter.PassCallStack(var ACallStack: TStack; ToParent: boolean);
begin
  if ToParent then
    FParentStack := ACallStack
  else
    FCallStack := ACallStack;
end;

function TInterpreter.Interpret(DontPush: boolean = False;
  ANameActRec: TActivationRecord = nil): string;
var
  Ret: TInstanceOf;
begin
  FDontPush := DontPush;
  FNameSpace := ANameActRec;
  Ret := Visit(FTree);
  {try
    try
      Ret := Visit(FTree);
    except on E: Exception do
      //if not FExceptionThrown then
        RaiseException(E.Message, 'Internal');
    end;
  finally
  end;}

  Result := '';
end;

function TInterpreter.Interpret(InsertActRec: TActivationRecord; InsertName: string; DontDestroyLast: boolean = False): string;
var
  Ret: TInstanceOf;
begin
  FDontDestroyLast := DontDestroyLast;
  FInsertActRec := InsertActRec;
  FInsertName := InsertName;
  try
    try
      Ret := Visit(FTree);
    except on E: Exception do
      //if not FExceptionThrown then
        RaiseException(E.Message, 'Internal');
    end;
  finally
  end;

  Result := '';
end;

function TInterpreter.GetLive: string;
var
  AActRec: TActivationRecord;
  AVal: TStringInstance;
  i: TInstanceOf;
begin
  AActrec := FCallStack.Peek();
  i := AActRec.GetMember('__LIVE__');
  Aval := TStringInstance(i);
  Result := AVal.PValue;
end;

function TInterpreter.VisitAssignedTest(ANode: TAssignedTest): TBooleanInstance;
var
  Ret: TBooleanInstance;
begin
  try
    Visit(ANode.PValue);
    Ret := TBooleanInstance.Create(True)
  except
    Ret := TBooleanInstance.Create(False);
  end;
  Result := Ret;
end;

function TInterpreter.VisitExpandArgs(ANode: TExpandArgs): TListInstance;
var
  GenInst: TInstanceOf;
  ListInst: TListInstance;
begin
  GenInst := Visit(ANode.PSrcList);
  if GenInst.ClassNameIs('TListInstance') then
    ListInst := TListInstance(GenInst);
  ListInst.PExpand := True;
  Result := ListInst;
end;

procedure TInterpreter.VisitPlainTextEmbed(ANode: TPlainTextEmbed);
var
  //AState: TAST;
  i, len: integer;
begin
  len := Length(Anode.PNodes);
  if len > 0 then
    for i := 0 to len - 1 do
      Visit(ANode.PNodes[i]);
end;

function TInterpreter.VisitDict(ANode: TDictNode): TDictionaryInstance;
var
  AActRec: TActivationRecord;
  //AKey: TAST;
  ATypedKey: TDictKeyNode;
  ARepo, O2, ADef: TInstanceOf;
  ACast: TListInstance;
  len, i, j: integer;
begin
  AActRec := TActivationRecord.Create('Any', AR_DICT, 1);
  ADef := nil;
  len := Length(ANode.PKeys);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      ATypedKey := TDictkeyNode(ANode.PKeys[i]);
      ARepo := Visit(ATypedkey.PKey);
      if ARepo.ClassNameIs('TStringInstance') or
        ARepo.ClassNameIs('TIntegerInstance') then
        AActRec.AddMember(ARepo.AsString, Visit(ATypedKey.PValue))
      else if ARepo.ClassNameIs('TNullInstance') then
      begin
        O2 := Visit(ATypedKey.PValue);
        O2.CopyInstance(ADef);
      end
      else if ARepo.ClassNameIs('TListInstance') then
      begin
        ACast := TListInstance(ARepo);
        if ACast.Count > 0 then
        begin
          for j := 0 to ACast.Count - 1 do
          begin
            O2 := ACast.PValue[j];
            if O2.ClassNameIs('TStringInstance') or
              O2.ClassNameIs('TIntegerInstance') then
              AActRec.AddMember(ACast.PValue[j].AsString, Visit(ATypedKey.PValue))
            else
              ERunTimeError.Create('This type can''t be used as a Dict key');
          end;
        end;
      end
      else
        ERunTimeError.Create('This type can''t be used as a Dict key');
    end;
  end;
  Result := TDictionaryInstance.Create(AActRec, ADef);
end;

function TInterpreter.checkArgType(Arg: TInstanceOf; Expected: TVariableReference): boolean;
var
  TypeVisited: TInstanceOf;
  ArgTypeStr, TypeVisitedStr: string;
  isClass, isDataType: boolean;
begin
  isClass := Arg.ClassNameIs('TClassInstance');
  isDataType := Arg.ClassNameIs('TDataType');
  if isClass {or isDataType} then
    ArgTypeStr := TInstanceOf(Arg.PMembers.Find('$internal')).AsString
  else if isDataType then
    ArgTypeStr := 'DataType'
  else
    ArgTypeStr := Arg.ClassName;

  TypeVisited := Visit(Expected);
  isDataType := TypeVisited.ClassNameIs('TDataType');
  TypeVisitedStr := TInstanceOf(TypeVisited.PMembers.Find('$internal')).AsString;

  Result := TypeVisitedStr = ArgTypeStr;
end;

function TInterpreter.ProcessFuncArgs(var AFunction: TFunctionInstance; var AActRec: TActivationRecord; AEvalParams: TASTList): TInstanceList;
var
  len, detour, ups, i, j, lenArgs, lenParams: integer;
  AVal: TInstanceOf;
  ArgsList, VarArgsList: TInstanceList;
  ArgsListInstance, VarArgsListInstance: TListInstance;
begin
  SetLength(ArgsList, 0);

  detour := 0;
  ups := 0;
  len := Length(AEvalParams);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      if AEvalParams[i].ClassNameIs('TExpandArgs') then
      begin
        AVal := VisitExpandArgs(TExpandArgs(AEvalParams[i]));
        if AVal.ClassNameIs('TListInstance') then
        begin
          for j := 0 to TListInstance(AVal).Count-1 do
          begin
            ups := ups + 1;
            SetLength(ArgsList, ups);
            ArgsList[i + detour] := TListInstance(AVal).PValue[j];
            detour := detour + 1;
          end;
          detour := detour - 1;
        end
        else
          RaiseException('Only lists can be expanded', 'Arguments');
      end
      else
      begin
        ups := ups + 1;
        SetLength(ArgsList, ups);
        ArgsList[i + detour] := Visit(AEvalParams[i]);
      end;
    end;
  end;
  if not AFunction.PIsBuiltin then
  begin
    LenArgs := length(ArgsList);
    LenParams := length(Afunction.PParams);
    if not AFunction.PAccVarargs and (LenArgs > LenParams) then
      RaiseException(E_INVALID_ARGS, 'Arguments');
    for i:=0 to LenParams - 1 do
    begin

      if i < LenArgs then
      begin
        if (TParam(AFunction.PParams[i]).PArgType <> nil) then
        begin
          if not (checkArgType(ArgsList[i], TVariableReference(TParam(AFunction.PParams[i]).PArgType))) then
            RaiseException(E_INVALID_ARGS_TYPE, 'Arguments');
        end;
        AActRec.AddMember(TParam(AFunction.PParams[i]).PNode.PValue, ArgsList[i])
      end
      else
      begin
        if TParam(AFunction.PParams[i]).PDefValue <> nil then
        begin
          AVal := Visit(TParam(AFunction.PParams[i]).PDefValue);
          if (TParam(AFunction.PParams[i]).PArgType <> nil) then
          begin
            if not (checkArgType(AVal, TVariableReference(TParam(AFunction.PParams[i]).PArgType))) then
              RaiseException(E_INVALID_ARGS_TYPE, 'Arguments');
          end;
          ups := Length(ArgsList);
          SetLength(ArgsList, ups + 1);
          ArgsList[ups] := AVal;
          AActRec.AddMember(TParam(AFunction.PParams[i]).PNode.PValue, AVal);
        end;
      end;
    end;
    if AFunction.PAccVarargs then
    begin
      LenArgs := length(ArgsList);
      Setlength(VarArgsList, LenArgs - lenParams);
      for i:=LenParams to LenArgs-1 do
      begin
        VarArgsList[i - LenParams] := ArgsList[i];
      end;
      VarArgslistInstance := TListInstance.Create(VarArgsList);
      VarArgsListInstance.LockAll;
      AActRec.AddMember('$varArgs', VarArgsListInstance);
    end;
    ArgslistInstance := TListInstance.Create(ArgsList);
    ArgsListInstance.LockAll;
    AActRec.AddMember('$funcArgs', ArgsListInstance);
  end;
  Result := ArgsList;
end;

procedure TInterpreter.VisitLiveOutput(ANode: TLiveOutput);
var
  AVal: TInstanceOf;
  AActRec: TActivationRecord;
  aLive, ASet: string;
  ALiveVal: TStringInstance;
begin
  AVal := Visit(ANode.PValue);
  AActRec := FCallStack.Peek();
  ALiveVal := TStringInstance(AActrec.GetMember('__LIVE__'));
  ASet := ALiveVal.PValue + AVal.AsString;
  AActRec.AddMember('__LIVE__', TStringInstance.Create(ASet));
end;

function TInterpreter.VisitLivePrint(ANode: TLivePrint): TStringInstance;
var
  AActRec: TActivationRecord;
begin
  AActRec := FCallStack.Peek();
  Result := TStringInstance(AActrec.GetMember('__LIVE__'));
end;

procedure TInterpreter.VisitBreak(Anode: TBreakLoop);
begin
  FBreakSignal := True;
end;

procedure TInterpreter.VisitContinue(Anode: TContinueLoop);
begin
  FContinueSignal := True;
end;

function TInterpreter.VisitReturn(ANode: TReturnFunction): TInstanceOf;
var
  AVal: TInstanceOf;
  AActRec: TActivationRecord;
begin
  AActRec := FCallStack.Peek();
  FReturnSignal := True;
  //AVal := Visit(ANode.PValue);
  //AVal.CopyInstance(AActRec.PReturnValue);
  AActRec.PReturnValue := Visit(ANode.PValue);
  Result := AActRec.PReturnValue;
end;

procedure TInterpreter.VisitPlainText(ANode: TPlainText);
var
  AActRec: TActivationRecord;
  ANewLive: TStringInstance;
  aLive: string;
begin
  AActRec := FCallStack.Peek();
  ALive := TStringInstance(AActrec.GetMember('__LIVE__')).PValue;
  Alive := ALive + ANode.PValue;
  AActRec.AddMember('__LIVE__', TStringInstance.Create(ALive));
end;

procedure TInterpreter.VisitInterpolation(ANode: TInterpolation);
var
  AActRec: TActivationRecord;
  ALive, Aval: string;
begin
  AActRec := FCallStack.Peek();
  ALive := TStringInstance(AActrec.GetMember('__LIVE__')).PValue;
  AVal := Visit(ANode.POper).AsString;
  Alive := ALive + AVal;
  AActRec.AddMember('__LIVE__', TStringInstance.Create(ALive));
end;

function TInterpreter.VisitFunctionDefinition(ANode: TFunctionDefinition): TFunctionInstance;
var
  i, len: integer;
  AValue: TFunctionInstance;
  AActRec: TActivationRecord;
  ABlock: TAST;
  RealType: TDataType;
  t, t2: string;
begin
  logtext('INTER', 'Interpreter', 'Visiting function definition');
  AActRec := FCallStack.Peek;
  if ANode.PType <> '' then
  begin
    RealType := TDataType(AActRec.GetMember(ANode.PType));
    if RealType <> nil then
    begin
      AValue := TFunctionInstance.Create(ANode.PName, ANode.PParamList,
        ANode.PBlock, RealType.PValue, False, ANode.PIsDecorator, ANode.PAcceptVarargs, ANode.PIsInstanceFunction);
      AValue.PMembers.Add('$paramCount', TIntegerInstance.Create(Length(ANode.PParamList)));
      RealType.PMembers.Add(ANode.PName, AValue);
    end
    else
      ERunTimeError.Create('Referenced type ' + ANode.PType +
      ' does not exist',
      FTrace, ANode.PToken);
  end
  else
  begin
    AValue := TFunctionInstance.Create(ANode.PName, ANode.PParamList,
        ANode.PBlock, 'TCoreFunction', False, ANode.PIsDecorator, ANode.PAcceptVarargs, False);
    AValue.PMembers.Add('$paramCount', TIntegerInstance.Create(Length(ANode.PParamList)));
    AActRec.AddMember(ANode.PName, AValue);
    Result := AValue;
  end;


       {
	else
  begin
    t := ANode.PName;
    t2 := ANode.PType
	end;
	AValue := TFunctionInstance.Create(ANode.PName, ANode.PParamList,
    ANode.PBlock, RealType.PValue, False);
  AActRec.AddMember(t, AValue);
	Result := TInstanceOf.Create;}
end;

function TInterpreter.VisitDecoratorDefinition(ANode: TDecoratorDefinition): TDecoratorInstance;
var
  i, len: integer;
  AValue: TDecoratorInstance;
  AActRec: TActivationRecord;
  ABlock: TAST;
  RealType: TDataType;
  t, t2: string;
begin
  logtext('INTER', 'Interpreter', 'Visiting decorator definition');
  AActRec := FCallStack.Peek;
  if ANode.PType <> '' then
  begin
    RealType := TDataType(AActRec.GetMember(ANode.PType));
    if RealType <> nil then
    begin
      RealType.PMembers.Add(ANode.PName, TFunctionInstance.Create(ANode.PName, ANode.PParamList,
        ANode.PBlock, RealType.PValue, False, ANode.PIsDecorator, ANode.PAcceptVarargs, False));
    end
    else
      ERunTimeError.Create('Referenced type ' + ANode.PType +
      ' does not exist',
      FTrace, ANode.PToken);
  end
  else
  begin
    AValue := TDecoratorInstance.Create(ANode.PName, ANode.PParamList,
        ANode.PBlock, 'TCoreFunction', False, ANode.PIsDecorator, ANode.PAcceptVarargs, False);
    AActRec.AddMember(ANode.PName, AValue);
    Result := AValue;
  end;
end;

function TInterpreter.VisitNewObject(ANode: TNewObject): TInstanceOf;
var
  Ret, ToCast, ArgInst: TInstanceOf;
  NowAct, NewAct: TActivationRecord;
  ActInst: TDictionaryInstance;
  Gene: TInstanceOf;
  ABuilt: TDataType;
  ArgsList: TInstanceList;
  len, i, len2, i2: integer;
  SearchStart: integer;
  ConstRet: TFunctionInstance;
  // byte stream section
  c: char;
  bytearray: array of byte;
  nextEntry: byte;
  lenByte: integer = 0;
begin
  NowAct := FCallStack.Peek();
  // surgery
 // SearchStart := FCallStack.Peek().PNestingLevel;
 // for i:=SearchStart downto 1 do
 // begin
 //   NowAct := FCallStack.GetByLevel(i);
 //   Gene := NowAct.GetMember(ANode.PName);
 //   if Gene <> nil then
 //     break;
	//end;
  // end surgery
  Gene := Visit(ANode.PRef);
  if Gene <> nil then
  begin
    if Gene.ClassNameIs('TDictionaryInstance') then
    begin
      ActInst := TDictionaryInstance(Gene);
      ActInst.PValue.CopyActRec(NewAct);
      Result := TDictionaryInstance.Create(NewAct);
    end
    else if Gene.ClassNameIs('TDataType') then
    begin
      ABuilt := TDataType(Gene);
      len := Length(ANode.PArgs);
      SetLength(ArgsList, len);
      if len > 0 then
      begin
        for i := 0 to len - 1 do
          ArgsList[i] := Visit(ANode.PArgs[i]);
      end;
      if ABuilt.PValue = 'TServerInstance' then
      begin
        Ret := TServerInstance.Create(TIntegerInstance(ArgsList[0]).PValue, TBooleanInstance(ArgsList[1]).PValue);
      end
      //else if ABuilt.PValue = 'TBrookResponseHandlerInstance' then
      //begin
      //  Ret := TClassInstance.Create('AppResponse');
      //
      //end
      else if ABuilt.PValue = 'TBrookServerInstance' then
      begin
        Ret := TBrookserverInstance.Create(TIntegerInstance(ArgsList[0]).PValue, TBooleanInstance(ArgsList[1]).PValue);
      end
      else if ABuilt.PValue = 'THttpClientInstance' then
      begin
        if Len = 1 then
        begin
          Ret := THttpClientInstance.Create(TStringInstance(ArgsList[0]).PValue)
        end
        else
          RaiseException(E_INVALID_ARGS, 'Arguments');
      end
      else if ABuilt.PValue = 'TDateTimeInstance' then
      begin
        Ret := TDateTimeInstance.ConstructDateTime(ArgsList);
      end
      else if ABuilt.PValue = 'TRegexInstance' then
        Ret := TRegexInstance.Create
      else if ABuilt.PValue = 'TByteStreamInstance' then
      begin
        if Len = 0 then
          SetLength(ByteArray, 0)
        else
        begin
          if not ArgsList[0].ClassNameIs('TListInstance') then
            RaiseException(E_INVALID_ARGS_TYPE, 'Type');
          for ArgInst in TListInstance(ArgsList[0]).PValue do
          begin
            lenByte := lenByte + 1;
            if ArgInst.ClassNameIs('TIntegerInstance') then
            begin
              if (ArgInst.PIntValue > 255) or (Arginst.PIntValue < 0) then
                RaiseException('An integer item from a stream input list must by between 0 and 255', 'Byte');
              Setlength(Bytearray, lenByte);
              ByteArray[lenByte - 1] := ArgInst.PIntValue;
            end
            else if ArgInst.ClassNameIs('TBooleanInstance') then
            begin
              setLength(ByteArray, lenbyte);
              if Arginst.PBoolValue then
                ByteArray[lenbyte - 1] := 1
              else
                Bytearray[lenByte - 1] := 0;
            end
            else if Arginst.ClassNameIs('TStringInstance') then
            begin
              lenbyte := lenbyte - 1;
              for c in Arginst.PStrValue do
              begin
                lenbyte := lenbyte + 1;
                setlength(bytearray, lenbyte);
                bytearray[lenbyte - 1] := ord(c);
              end;
            end
            else if ArgInst.ClassNameIs('TByteInstance') then
            begin

              SetLength(bytearray, lenbyte);
              ByteArray[lenByte - 1] := ArgInst.PIntValue;
            end
            else
              RaiseException('Type ' + ArgInst.ClassName + ' is not accepted as ByteStream input list', 'Type');
            {if not Arginst.ClassNameIs('TByteInstance') then
              RaiseException('All items of a stream input list must be from type byte', 'Type');}
          end;
        end;
        Ret := TByteStreamInstance.Create(ByteArray);
      end
      else
      begin
        Ret := TClassInstance.Create(ABuilt.PValue);
        if ABuilt.PMembers.Count > 0 then
        begin
          for i2:=0 to ABuilt.PMembers.Count-1 do
            Ret.PMembers.Add(Abuilt.PMembers.NameOfIndex(i2), TObject(ABuilt.PMembers[i2]));
        end;
        ToCast := TInstanceOf(ABuilt.PMembers.Find('init'));
        if ToCast <> nil then
        begin
          if ToCast.ClassNameIs('TFunctionInstance') then
          begin
            ConstRet := TFunctionInstance(ToCast);
            if len > 0 then
            begin
              for i:=0 to len-1 do
              begin
                Ret.PMembers.Add(ConstRet.PParams[i].PToken.PValue, ArgsList[i]);
	      end;
	    end;
	    ExecuteFunctionByInstance(Constret, ANode.PArgs, Anode, ret);
          end;
        end;
      end;
      Result := Ret;
    end;
  end
  else
  begin
    ERunTimeError.Create('Referenced type or namespace ' + ANode.PName +
      ' does not exist',
      FTrace, ANode.PToken);
  end;

end;

procedure TInterpreter.VisitConditional(ANode: TConditional);
var
  //ACondition: TAST;
  len, i: integer;
  Return: TBooleanInstance;
begin
  len := Length(ANode.PConditions);
  if len > 0 then
  begin
    //for ACondition in ANode.PConditions do
    for i := 0 to len - 1 do
    begin
      Return := VisitIfCondition(TIfConditionBlock(ANode.PConditions[i]));
      if Return.PValue then
        break;
    end;
  end;
end;

function TInterpreter.VisitIfCondition(ANode: TIfConditionBlock): TBooleanInstance;
var
  AEval: TBooleanInstance = nil;
  //AState: TAST;
  len, i: integer;
begin
  if ANode.PCondition <> nil then
  begin
    AEval := TBooleanInstance(Visit(ANode.PCondition));
    if AEval.PValue or (ANode.PCondition = nil) then
    begin
      len := Length(ANode.PBlock);
      if len > 0 then
      begin
        for i := 0 to len - 1 do
        begin
          if FBreakSignal or FContinueSignal then
            break
          else if FReturnSignal then
            break
          else
            Visit(ANode.PBlock[i]);
        end;

      end;
    end;
  end
  else
  begin
    AEval := TBooleanInstance.Create(False);
    len := Length(ANode.PBlock);
    if len > 0 then
    begin
      for i := 0 to len - 1 do
      begin
        if FBreakSignal or FContinueSignal then
        begin
          break;
        end
        else if FReturnSignal then
          break
        else
          Visit(ANode.PBlock[i]);
      end;
    end;
  end;
  Result := AEval;
end;

procedure TInterpreter.VisitWhileLoop(ANode: TWhileLoop);
var
  //AState: TAST;
  len, i: integer;
begin
  while TBooleanInstance(Visit(ANode.PCondition)).PValue do
  begin
    len := Length(ANode.PBlock);
    if len > 0 then
    begin
      for i := 0 to len - 1 do
      begin
        if FBreakSignal or FContinueSignal then
        begin
          FContinueSignal := False;
          break;
        end
        else
          Visit(ANode.PBLock[i]);
      end;
      if FBreakSignal then
        break;
    end;
  end;
  FBreakSignal := False;
end;

procedure TInterpreter.VisitLoadType(ANode: TLoadType);
var
  len, i: integer;
  AActRec: TActivationRecord;
begin
  AActRec := FCallStack.Peek;
  len := Length(ANode.PTypeNames);
  for i := 0 to len-1 do
    TTypeLoader.LoadType(ANode.PTypeNames[i].PToken.PValue, Self, AActRec);
end;

procedure TInterpreter.VisitForLoop(ANode: TForLoop);
var
  AInst, AListRes: TInstanceOf;
  AInt, AIndex: TIntegerInstance;
  AActRec: TActivationRecord;
  len, len2, j, i: integer;
  AByteStream: TByteStreamInput;
  ACandidate: TListInstance;
  AStr: string;
begin
  AActRec := FCallStack.Peek;
  AListRes := Visit(ANode.PList);
  if AListRes.ClassNameIs('TListInstance') then
  begin
    //ACandidate := TListInstance.Create;
    //TListInstance(AListRes).CopyList(ACandidate);
    ACandidate := TListInstance(AlistRes);
    if ACandidate.Count > 0 then
    begin
      for i := 0 to ACandidate.Count - 1 do
      begin
        AActRec.AddMember(Anode.PVar, ACandidate.PValue[i]);
        AActRec.AddMember('_' + Anode.PVar, TIntegerInstance.Create(i));
        len := Length(ANode.PBlock);
        if len > 0 then
        begin
          for j := 0 to len - 1 do
          begin
            if FBreakSignal or FContinueSignal or FReturnSignal then
            begin
              FContinueSignal := False;
              break;
            end
            else
              Visit(ANode.PBlock[j]);
          end;
        end;
        if FBreakSignal then
          break;
      end;
    end;
    //ACandidate.Free;
  end
  else if AListRes.ClassNameIs('TByteStreamInstance') then
  begin
    AByteStream := TByteStreamInstance(AListRes).PValue;
    len := Length(AByteStream);
    if len > 0 then
    begin
      for i := 0 to len - 1 do
      begin
        AActRec.AddMember(Anode.PVar, TIntegerInstance.Create(AByteStream[i]));
        AActRec.AddMember('_' + Anode.PVar, TIntegerInstance.Create(i));
        len2 := Length(ANode.PBlock);
        if len2 > 0 then
        begin
          for j := 0 to len2 - 1 do
          begin
            if FBreakSignal or FContinueSignal or FReturnSignal then
            begin
              FContinueSignal := False;
              break;
            end
            else
              Visit(ANode.PBlock[j]);
          end;
        end;
        if FBreakSignal then
          break;
      end;
    end;
    //ACandidate.Free;
  end

  else if AListRes.ClassNameIs('TStringInstance') then
  begin
    len := Length(AListRes.PStrValue);
    if len > 0 then
    begin
      for i := 0 to len - 1 do
      begin
        AActRec.AddMember(Anode.PVar, TStringInstance.Create(AListRes.PStrValue[i+1]));
        AActRec.AddMember('_' + Anode.PVar, TIntegerInstance.Create(i));
        len2 := Length(ANode.PBlock);
        if len2 > 0 then
        begin
          for j := 0 to len2 - 1 do
          begin
            if FBreakSignal or FContinueSignal or FReturnSignal then
            begin
              FContinueSignal := False;
              break;
            end
            else
              Visit(ANode.PBlock[j]);
          end;
        end;
      end;
    end;
  end
  else if AListRes.ClassNameIs('TIntegerInstance') then
  begin
    len := AListRes.PIntValue;
    if len > 0 then
    begin
      for i := 0 to len - 1 do
      begin
        AActRec.AddMember(Anode.PVar, TIntegerInstance.Create(i));
        AActRec.AddMember('_' + Anode.PVar, TIntegerInstance.Create(i));
        len2 := Length(ANode.PBlock);
        if len2 > 0 then
        begin
          for j := 0 to len2 - 1 do
          begin
            if FBreakSignal or FContinueSignal or FReturnSignal then
            begin
              FContinueSignal := False;
              break;
            end
            else
              Visit(ANode.PBlock[j]);
          end;
        end;
      end;
    end;
  end;
  FBreakSignal := False;
end;



procedure TInterpreter.VisitVarAssign(ANode: TVarAssign; ASrc: TInstanceOf);
var
  AValue: TInstanceOf;
  AName: string;
  AActRec: TActivationRecord;
  TryAdd: boolean;
begin
  LogText(INTER, 'Interpreter', 'VarAssign visitation');
  AName := ANode.PVarName.PValue;
  AValue := Visit(ANode.PValue);
  try
    if AValue.PError then
      RaiseException(AValue.PErrorMsg, 'Internal');
    if AValue.ClassNameIs('TFunctionInstance') then
      AValue := TFunctionInstance(AValue);

    if AValue.ClassNameIs('TFunctionInstance') then
    begin
      if TFunctionInstance(AValue).PIsBuiltin then
      begin
        ERunTimeError.Create('Can''t assign builtin function "' +
          ANode.PValue.PToken.PValue + '" to variable "' + AName + '"',
          FTrace, ANode.PToken);
      end;
    end;

  except on E: Exception do
    RaiseException(E.Message, 'Internal');
  end;
  AActrec := FCallStack.Peek;
  if ASrc = nil then
  begin
    if not AActRec.AddMember(AName, AValue) then
      ERunTimeError.Create('Can''t redefine constant value "'+Aname+'"', FTrace, ANode.PVarName);
  end
  else
  begin
    if AName[1] = '$' then
      ERunTimeError.Create('Can''t redefine constant attribute "'+Aname+'" from "' + ASrc.ClassName + '"', FTrace, ANode.PVarName);
    if AValue.ClassNameIs('TFunctionInstance') and ASrc.ClassNameIs('TClassInstance') then
      TFunctionInstance(AValue).PIsInstanceFunction := True;
    ASrc.PMembers.Add(AName, AValue);
  end;
end;

procedure TInterpreter.VisitListAssign(ANode: TListAssign);
var
  ASrc, AValue, AIndex, AddedValue: TInstanceOf;
  ASrcAct: TDictionaryInstance;
  ASrcList: TListInstance;
begin
  LogText(INTER, 'Interpreter', 'VarList visitation');
  ASrc := Visit(ANode.PSrc);
  Aindex := Visit(ANode.PEntry);
  AValue := Visit(ANode.PValue);
  AValue.CopyInstance(AddedValue);
  if ASrc.ClassNameIs('TDictionaryInstance') then
  begin
    ASrcAct := TDictionaryInstance(ASrc);
    if ASrcAct.PChangeLocked then
      EValueError.Create('Can''t change values of change locked Dict', FTrace, ANode.PToken);
    if AIndex.ClassNameIs('TStringInstance') then
      ASrcAct.PValue.AddMember(TStringInstance(AIndex).PValue, AddedValue)
		else
    begin
      ERunTimeError.Create('Invalid type for Dict index',
        FTrace, ANode.PToken);
    end;
  end
  else if ASrc.ClassNameIs('TListInstance') then
  begin

    ASrcList := TListInstance(ASrc);
    if ASrcList.PChangeLocked then
      EValueError.Create('Can''t change values of change locked List', FTrace, ANode.PToken);
    if AIndex.ClassNameIs('TIntegerInstance') then
      ASrcList.SetItem(TIntegerInstance(AIndex).PValue, AddedValue)
    else
    begin
      ERunTimeError.Create('Invalid type for List index',
        FTrace, ANode.PToken);
    end;
  end;

end;

function TInterpreter.VisitVariableReference(ANode: TVariableReference; ASrc: TInstanceOf{ = nil}): TInstanceOf;
var
  AName: string;
  AActRec: TActivationRecord;
  Start, i: integer;
  Ret, BRet: TInstanceOf;
begin
  AName := Anode.PToken.PValue;
  if ASrc = nil then
  begin
        Start := FCallStack.Peek().PNestingLevel;
        for i:=Start downto 1 do
        begin
          AActRec := FCallStack.getBylevel(i);
          Ret := AActRec.GetMember(AName);
          if ret <> nil then
            break
				end;
		    if Ret = nil then
		    begin
		      ERunTimeError.Create('Referenced variable "' + Aname + '" does not exist',
		        FTrace, ANode.PToken);
		    end;
        // BRet.CopyInstance(Ret);
        // AActRec := FCallStack.Peek;
        // AActRec.AddMember(AName, Ret);
	end
  else
  begin
    //try
      Ret := TInstanceOf(ASrc.PMembers.Find(Aname));
      if (Ret <> nil) then
      begin
        if Ret.ClassNameIs('TIntegerInstance') then
          Ret := TIntegerInstance.Create(Ret.PIntValue)
        else if Ret.ClassNameIs('TBooleanInstance') then
          Ret := TBooleanInstance.Create(Ret.PBoolValue)
        else if Ret.ClassNameIs('TStringInstance') then
          Ret := TStringInstance.Create(Ret.PStrValue)
        else if Ret.ClassNameIs('TFloatInstance') then
          Ret := TFloatInstance.Create(Ret.PFloatValue)
        else if Ret.ClassNameIs('TNullInstance') then
          Ret := TNullInstance.Create;
      end;
      if Ret = nil then
        ERunTimeError.Create('Referenced attribute "'+AName+'" does not exist', FTrace, ANode.PToken);

		//except
      //ERunTimeError.Create('This type ('+ASrc.ClassName+') doesn''t have attributes', Ftrace, ANode.PToken);
		//end;

	end;
	LogText(INTER, 'Interpreter', 'Getting value of "' + ANode.PToken.PValue +
    '" from type ' + Ret.ClassName);
  Result := Ret;
end;

function TInterpreter.VisitMethodCall(ANode: TMethodCall): TInstanceOf;
begin
  LogText(INTER, 'Interpreter', 'Visiting method ' + ANode.PToken.PValue);
  Result := Visit(ANode.Poper, Visit(ANode.PSrc));
end;

function TInterpreter.VisitFunctionCallByInstance(ANode: TFunctionCallByInstance; ASrcInstance: TInstanceOf = nil):TInstanceOf;
var
  AFuncInst: TFunctionInstance;
  ArgsList: TInstanceList;
  len, i: integer;
  Arec: TInstanceOf;
begin
  AFuncInst := TFunctionInstance(Visit(ANode.PFuncInst, ASrcInstance));
  {len := Length(Anode.PEvalParams);
  if len > 0 then
  begin
    SetLength(ArgsList, len);
    for i:=0 to len-1 do
    begin
      ARec := Visit(ANode.PEvalParams[i]);
      Arec.CopyInstance(ArgsList[i]);
		end;
	end;}
  Result := ExecuteFunctionByinstance(AFuncInst, ANode.PEvalParams, ANode, ASrcInstance);
end;

function TInterpreter.ExecuteFunctionByInstance(AFunction: TFunctionInstance; Args: TASTList; ANode: TAST; AsrcInstance: TInstanceOf): TInstanceOf;
var
  AActRec: TActivationRecord;
  lenArgs, LenParams, len, i: integer;
  Adef, AIter, ACopy, AReturn: TInstanceOf;
  Aux: TFunctionInstance;
  AParamName: string;
  ArgsList, VarArgsList: TInstanceList;
  ArgsListInstanced, VarArgsListInstanced: TListInstance;
begin
  AActRec := TActivationRecord.Create(AFunction.PName, AR_FUNCTION,
          FCallStack.PLevel + 1);
  AActRec.AddMember('__LIVE__', TStringInstance.Create(''));

  if AFunction.PIsInstanceFunction then
  begin
    if ASrcInstance.ClassNameIs('TDataType') then
      RaiseException('Method "' + AFunction.PName + '" only available for  "' + ASrcInstance.AsString + '"  object instances', 'RunTime');
    AActRec.AddMember('self', ASrcInstance);
  end;
  ProcessFuncArgs(AFunction, AActrec, Args);
  FCallStack.Push(AActRec);
  len := Length(AFunction.PBlock);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      if FReturnSignal then
      begin
	FReturnSignal := False;
	AActRec.PReturnValue.CopyInstance(AReturn);
	FCallStack.Pop();
	Result := AReturn;
	exit;
      end
      else
	Visit(AFunction.PBlock[i]);
    end;
  end;
  AReturn := AActRec.GetMember('__LIVE__');
  FCallStack.Pop();
  Result := AReturn;
  exit;
end;

function TInterpreter.VisitDecoratorCall(AFunctionInstance: TFunctionInstance; ADecorated: TASTList): TInstanceOf;
var
  NewParams: TASTList;
  Instanced: TInstanceOf;
  Decorated: TFunctionInstance;
  last, len, len2, i: integer;
  DecCopy: TDecoratorInstance;
  AToken: TToken;
begin
  // Instanced := TFunctionInstance.Create(ADecorated.PName, ADecorated.PParamList, ADecorated.PBlock, '', False);

  last := Length(ADecorated) - 1;
  Instanced := Visit(ADecorated[last]);
  if not Instanced.ClassNameIs('TFunctionInstance') then
    ERunTimeError.Create('Last argument of a decorator call must be a function', FTrace, nil);
  len := Length(AFunctionInstance.PParams);
  SetLength(NewParams, len);
  for i:=0 to len-1 do
  begin
    AToken := TToken.Create;
    AToken.PValue := AFunctionInstance.PParams[i].PToken.PValue;
    AToken.PType := AFunctionInstance.PParams[i].PToken.PType;
    NewParams[i] := TParam.Create(Atoken, AFunctionInstance.PParams[i]);
    TParam(NewParams[i]).PDefValue := ADecorated[i];
  end;
  Decorated := TFunctionInstance.Create(FormatDateTime('yyyymmddhhnnsszzz', Now), TFunctionInstance(Instanced).PParams, AFunctionInstance.PBlock, 'TCoreFunction', False, AFunctionInstance.PIsDecorator, TFunctionInstance(Instanced).PAccVarargs, False);
  Decorated.PDecorParams := NewParams;
  Result := Decorated;
end;



function TInterpreter.VisitFunctionCall(ANode: TFunctionCall;
  ASrcInstance: TInstanceOf = nil): TInstanceOf;
const
  ST_ACCESS = ':';
var
  x, y: ^word;
  ACoreExec: TCoreFunction;
  AFuncName, ASrcName, compl: string;
  AActRec: TActivationRecord;
  AParamName, DisplayType: string;
  len, len2, i, j, ri: integer;
  AParamState: TParam;
  FuncDef, Aux: TFunctioninstance;
  ADef: TInstanceOf = nil;
  ACopy: TInstanceOf;
  AReturn, AIter, Zika: TInstanceOf;
  LenArgs, LenParams: integer;
  ArgsList, VarList: TInstanceList;
  ArgsListInstanced, VarListInstanced: TListInstance;
  searchStart: integer;
  ExpandedVisited: TInstanceList;
  NewEvalParams, Expanded: TASTList;
begin

  AFuncName := ANode.PFuncName;
  LogText(INTER, 'Interpreter', 'Visiting function ' + ANode.PFuncName);
  if (ASrcInstance <> nil) and ASrcInstance.ClassNameIs('TClassInstance') then
  begin
    ACopy := TInstanceOf(ASrcInstance.PMembers.Find(ANode.PFuncName));
    if ACopy.ClassNameIs('TFunctionInstance') then
      FuncDef := TFunctionInstance(ACopy)
    else if ACopy.ClassNameIs('TDecoratorInstance') then
    begin
      Result := VisitDecoratorCall(FuncDef, ANode.PEvalParams);
      exit;
    end;
  end
  else
  begin
    SearchStart := FCallStack.Peek().PNestingLevel;
    for i:=SearchStart downto 1 do
    begin
      AActRec := FCallStack.GetByLevel(i);
      FuncDef := AActRec.GetFunction(AFuncName, ASrcInstance);
      if FuncDef <> nil then
        break;
      end;
  end;
  if FuncDef <> nil then
  begin
    NewEvalParams := ANode.PEvalParams;
    if ASrcInstance <> nil then
    begin
      if ASrcInstance.ClassNameIs('TClassInstance') then
        DisplayType := ASrcInstance.AsString
      else
        DisplayType := ASrcInstance.ClassName;
      if FuncDef.PIsInstanceFunction then
      begin
        if ASrcInstance.ClassNameIs('TDataType') then
          RaiseException('Method "' + AFuncName + '" only available for "' + ASrcInstance.AsString + '" object instances', 'RunTime');
      end
      else
      begin
        if not ASrcInstance.ClassNameIs('TDataType') then
          RaiseException('Method "' + AFuncName + '" is not available for "' + DisplayType + '" object instances', 'RunTime');
      end;
    end;
    if FuncDef.ClassNameIs('TDecoratorInstance') then
    begin
      // Decorate execute
      Result := VisitDecoratorCall(FuncDef, NewEvalParams);
    end
    else
    begin
      if not FuncDef.PIsBuiltin then
      begin
        AActRec := TActivationRecord.Create(FuncDef.PName, AR_FUNCTION,
          FCallStack.PLevel + 1);
        AActRec.AddMember('__LIVE__', TStringInstance.Create(''));
        if ASrcInstance <> nil then
          if FuncDef.PIsInstanceFunction then
            AActRec.AddMember('self', ASrcInstance);

        if FuncDef.PDecorParams <> nil then
        begin
          LenArgs := Length(FuncDef.PDecorParams);
          for i:=0 to LenArgs-1 do
          begin
            AIter := Visit(TParam(Funcdef.PDecorParams[i]).PDefValue);
            AParamName := TParam(Funcdef.PDecorParams[i]).PNode.PValue;
            AActRec.AddMember(AParamName, AIter);
          end;
        end;
        ProcessFuncArgs(FuncDef, AActRec, ANode.PEvalParams);

        FCallStack.Push(AActRec);
        len := Length(Funcdef.PBlock);
        if len > 0 then
        begin
          for i := 0 to len - 1 do
          begin
            if FReturnSignal then
            begin
              FReturnSignal := False;
              AActRec.PReturnValue.CopyInstance(AReturn);
              FCallStack.Pop();
              Result := AReturn;
              exit;
            end
            else
              Visit(FuncDef.PBlock[i]);
          end;
        end;
        AReturn := AActRec.GetMember('__LIVE__');
        FCallStack.Pop();
        Result := AReturn;
        exit;
      end
      else
      begin
        ArgsList := ProcessFuncArgs(FuncDef, AActRec, ANode.PEvalParams);
        ACoreExec := TCoreFunction.Create;
        AReturn := ACoreExec.Execute(Self, AFuncName, ArgsList, ASrcInstance);
        ACoreExec.Free;
        Result := AReturn;
        exit;
      end;
    end;
  end
  else
  begin
    if ASrcInstance = nil then
      ERunTimeError.Create('Referenced function "' + AFuncName + '" does not exist.',
		        FTrace, ANode.PToken)
    else if ASrcInstance.ClassNameIs('TDataType') then
      ERunTimeError.Create('Referenced function "' + AFuncName + '" does not exist for type "'+TDataType(ASrcInstance).PFrontName+'".',
		          FTrace, ANode.PToken)
    else
      ERunTimeError.Create('Referenced function "' + AFuncName + '" does not exist for type "'+TDataType(AActRec.GetTypeByInternalName(ASrcInstance.ClassName)).PFrontName+'".',
		          FTrace, ANode.PToken);
  end;
  // end of new
end;


procedure TInterpreter.VisitProgram(ANode: TProgram);
var
  //AChild: TAST;
  len, i, NowLevel: integer;
  AActRec, AParRec: TActivationRecord;
  InsertedDicted: TDictionaryInstance;
  IncObject: TDataType;
  refreshType: boolean;
begin
  LogText(INTER, 'Interpreter', 'Visiting a program');
  if FNameSpace = nil then
    AActRec := TActivationRecord.Create('PROGRAM', AR_PROGRAM, 1)
  else
    AActRec := TActivationRecord.Create(FNameSpace.PName, AR_NAMESPACE, 1);
  AActRec.AddMember('__LIVE__', TStringInstance.Create(''));
  if not FDontPush then
    // root execution
  begin
    if FinsertActRec <> nil then
    begin
      InsertedDicted := TDictionaryInstance.Create(FInsertActRec);
      AActRec.AddMember(FInsertName, InsertedDicted);
    end;
    FCallStack.Push(AActRec);
    BootStrapRegister;
  end
  else
  begin
    if FNameSpace <> nil then
    begin
      FCallStack.Push(AActRec);
    end;
  end;
  len := Length(ANode.PPreludes);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      Visit(ANode.PPreludes[i]);
    end;
  end;

  len := Length(ANode.PChildren);
  if len > 0 then
  begin

    for i := 0 to len - 1 do
    begin
      //LogText(INTER, 'Interpreter', 'Visiting a child ' + AChild.ToString);
      FTrace.Clear;
      Visit(ANode.PChildren[i]);
    end;
  end;
  if FNameSpace <> nil then
  begin
    AActRec := FCallStack.Peek();
    len := AActRec.PMembers.Count;
    if len > 0 then
    begin
      AParRec := FParentStack.Peek();
      RefreshType := False;
      for i := AParRec.PNestingLevel downto 1 do
      begin
        IncObject := TDataType(AParRec.GetMember(FNameSpace.PName));
        if (IncObject <> nil) and (IncObject.ClassNameIs('TDataType')) then
          break;
        if i > 1 then
          AParRec := FParentStack.GetByLevel(i);
      end;

      if IncObject = nil then
      begin
        RefreshType := True;
        IncObject := TDataType.Create(FNameSpace.PName, FNameSpace.PName, True)
      end;
      for i:=0 to len - 1 do
      begin
        IncObject.PMembers.Add(
          AActRec.PMembers.NameOfIndex(i),
          TInstanceOf(AActRec.PMembers[i])
        );
      end;
    end;
    if RefreshType then
      AParRec.AddMember(FNameSpace.PName, IncObject);
    FCallStack := FParentStack;
  end;

  FTrace.Free;
  FLiveOutput := GetLive();
  if not FDontPush and not FDontDestroyLast then
    FCallStack.Pop();
end;

procedure TInterpreter.VisitNoOp(ANode: TNoOp);
begin

end;

procedure TInterpreter.VisitIncludeScript(ANode: TIncludeScript);
var
  AFileName: string;
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
  AInter: TInterpreter;
  ANameSp: TActivationRecord;
  AName, AModPath: string;
begin
  if ANode.PIsModule then
  begin
    for AmodPath in FModulesPath do
    begin
      AFileName := AmodPath +
                DirectorySeparator +
                ReplaceStr(ANode.PModulePath, '.', DirectorySeparator);
      if FileExists (AFileName + '.ultra') then
        AFileName := AFileName + '.ultra'
      else if DirectoryExists(AFileName) then
      begin
        if FileExists (AFileName + DirectorySeparator + '_init.ultra') then
          AFileName := AFileName + DirectorySeparator + '_init.ultra'
        else
          ERunTimeError.Create('Specified module "'+ANode.PModulePath+'" does not exist', FTrace, ANode.PToken);
      end
    end
	end
	else
  begin
    AFileName := TStringInstance(Visit(ANode.PFilename)).PValue;
    if DirectoryExists(AFileName) then
      AFileName := AFileName + DirectorySeparator + '_init.ultra';

  end;
  ALexer := TLexer.Create(AFileName);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode;
  AParser.Free;
  AInter := TInterpreter.Create(ATree);
  Ainter.PModulesPath := FModulesPath;

  if (ANode.PNamespace <> '') then
  begin
    Aname := ANode.PNamespace;
    if ANode.PNamespace = '0' then
    begin
      AName := Copy(AFileName, 1, Rpos('.ultra', AFileName) - 1);
      Aname := Copy(AName, RPos(DirectorySeparator, Aname) + 1, Length(AName));
    end;
    AInter.PassCallStack(FCallStack, True);
    ANameSp := TActivationRecord.Create(AName, AR_NAMESPACE, 1);
    AInter.Interpret(True, ANameSp);
  end
  else
  begin
    AInter.PassCallStack(FCallStack, False);
    AInter.Interpret(True);
  end;
  //ATree.Free;
  //AInter.Free;
end;

function TInterpreter.VisitNameSpaceGet(Anode: TNamespaceGet): TInstanceOf;
var
  AActRec, ARef: TActivationRecord;
  ANameRec: TDictionaryInstance;
  Ret, ADictCand, AKeyCand: TInstanceOf;
  AKey: TStringInstance;
begin
  AActrec := FCallStack.Peek();
  ADictCand := AActRec.GetMember(ANode.PName);
  if ADictCand.ClassNameIs('TDictionaryInstance') then
  begin
    Aref := TDictionaryInstance(ADictCand).PValue;
    FCallStack.Push(ARef);
  end
  else
    ERunTimeError.Create('Referenced name is not a Dict type',
      FTrace, ANode.PToken);
  Ret := Visit(ANode.Poper);
  FCallStack.Pop();
  Result := ret;

end;

function TInterpreter.VisitNameSpaceState(Anode: TNamespaceState): TInstanceOf;
var
  AActRec, ARef: TActivationRecord;
  ANameRec: TDictionaryInstance;
  Ret, ArefVal: TInstanceOf;
begin
  AActrec := FCallStack.Peek();
  ARefVal := AActRec.GetMember(ANode.PName);
  if ARefVal = nil then
  begin
    ARef := TActivationRecord.Create(ANode.PName, AR_NAMESPACE, 1);
    ANameRec := TDictionaryInstance.Create(Aref);
    AActRec.AddMember(ANode.PName, ANameRec);
  end
  else
  begin
    Aref := TDictionaryInstance(ARefVal).PValue;
    ANameRec := TDictionaryInstance.Create(Aref);
  end;
  FCallStack.Push(AnameRec.PValue);
  Ret := Visit(ANode.POper);
  FcallStack.Pop();
  Result := ret;
end;

function TInterpreter.Visit(ANode: TAST; ASrcInstance: TInstanceOf = nil): TInstanceOf;
var
  AuxBool: boolean;
  Acd, Adir, Afn: string;
  len: integer;
  Ret: TInstanceOf;
begin
  FNowNode := ANode;
  if ANode.ClassName <> 'TProgram' then
  begin
    if FTrace.Count = 0 then
    begin
      Acd := GetCurrentDir;
      if Length(Acd) > 0 then
      begin
        Acd := Acd + DirectorySeparator;
        try
          Adir := ExtractFilePath(ANode.PToken.PScriptName);
          if Length(Adir) > 0 then
            Adir := Adir + DirectorySeparator;
          AFn := ExtractFileName(ANode.PToken.PScriptName);
          FTrace.Add(ANode.PToken.PScriptName + ' at < ' +
            IntToStr(ANode.PToken.PLineNo) + ':' + IntToStr(ANode.PToken.PCharNo) + ' >');
        except
          FTrace.Add('(token unavailable)');
        end;
      end;

    end;
    FTrace.Add(ANode.ToString);
  end;
  {$INCLUDE 'interpreter/node_switcher.pp'}
  Result := Ret;
end;

procedure TInterpreter.VisitClassDefinition(ANode: TClassDefinition);
var
  AActRec: TActivationRecord;
begin
  AActRec := FCallStack.Peek;
  if (AActRec.GetMember(Anode.PToken.PValue) = nil) then
    AActRec.AddMember(ANode.PToken.PValue, TDataType.Create(ANode.PToken.PValue, ANode.PToken.PValue, True));
end;

function TInterpreter.VisitUnaryOpFloat(ANode: TUnaryOp): TFloatInstance;
var
  AOper: string;
  ATerm: TInstanceOf;
begin
  ATerm := Visit(ANode.PExpr);
  if ATerm.ClassNameIs('TFloatInstance') then
  begin
    AOper := ANode.POper.PType;
    if AOper = T_PLUS then
    begin
      ATerm.PFloatValue := ATerm.PFloatValue;
      TFloatInstance(ATerm).PValue := TFloatInstance(ATerm).PValue
    end
    else if AOper = T_MINUS then
    begin
      TFloatInstance(Aterm).PValue := TFloatInstance(ATerm).PValue * (-1);
      Aterm.PFloatValue := ATerm.PFloatValue * (-1);
    end;
    Result := TFloatInstance(ATerm);
  end
  else
    ERunTimeError.Create('Invalid operation for class ' + ATerm.ClassName,
      FTrace, ANode.PToken);
end;

function TInterpreter.VisitUnaryOp(ANode: TUnaryOp): TIntegerInstance;
var
  AOper: string;
  Ret: TIntegerInstance;
  ATerm: TInstanceOf;
begin
  ATerm := Visit(ANode.PExpr);
  if ATerm.ClassNameIs('TIntegerInstance') then
  begin
    AOper := ANode.POper.PType;
    if AOper = T_PLUS then
    begin
      ATerm.PIntValue := ATerm.PIntValue;
      TIntegerInstance(Aterm).PValue := TIntegerInstance(ATerm).PValue
    end
    else if AOper = T_MINUS then
    begin
      Aterm.PIntValue := ATerm.PIntValue * (-1);
      TIntegerInstance(Aterm).PValue := TIntegerInstance(ATerm).PValue * (-1);
    end;
    Result := TIntegerInstance(ATerm);
  end
  else
    ERunTimeError.Create('Invalid operation for class ' + ATerm.ClassName,
      FTrace, ANode.PToken);
end;

function TInterpreter.VisitUnaryLogicOp(ANode: TUnaryLogicOp): TBooleanInstance;
var
  AOper: string;
  AuxExt: boolean;
  ARes: TBooleanInstance;
begin
  try
    ARes := TBooleanInstance(Visit(ANode.PExpr));
    ARes.PValue := not ARes.PValue;
    Result := ARes;

  except
    ERunTimeError.Create('Invalid operation for class ' + ARes.ClassName,
      FTrace, ANode.PToken);
  end;
end;

function TInterpreter.VisitBinLogicOp(ANode: TBinLogicOp): TBooleanInstance;
var
  LeftExt, RightExt: extended;
  LeftInt, RightInt: integer;
  LeftBool, RightBool: boolean;
  LeftAddr, RightAddr: ^word;
  LeftStr, RightStr: string;
  LeftClass, RightClass: string;
  AResl, AResR: TInstanceOf;
  Cmp: boolean;

begin
  //try
    AResL := Visit(Anode.PLeft);
    AResR := Visit(Anode.PRight);
    LeftClass := AResL.ClassName;
    RightClass := AResR.ClassName;
    if LeftClass = RightClass then
    begin
      if LeftClass = 'TBooleanInstance' then
      begin
        LeftBool := TBooleanInstance(AResL).PValue;
        RightBool := TBooleanInstance(AResR).PValue;
        if (ANode.POper.PType = T_GT) then
          Cmp := LeftBool > RightBool
        else if (ANode.POper.PType = T_LT) then
          Cmp := LeftBool < RightBool
        else if (ANode.POper.PType = T_EQ) then
          Cmp := LeftBool = RightBool
        else if (ANode.POper.PType = T_LEQ) then
          Cmp := LeftBool <= RightBool
        else if (ANode.POper.PType = T_GEQ) then
          Cmp := LeftBool >= RightBool
        else if (ANode.POper.PType = T_NEQ) then
          Cmp := LeftBool <> RightBool
        else if (ANode.POper.PType = T_AND) then
          Cmp := LeftBool and RightBool
        else if (ANode.POper.PType = T_OR) then
          Cmp := LeftBool or RightBool;
        Result := TBooleanInstance.Create(Cmp);
        exit;
      end
      else if LeftClass = 'TStringInstance' then
      begin
        LeftStr := TStringInstance(AResL).PValue;
        RightStr := TStringInstance(AResR).PValue;
        if (ANode.POper.PType = T_GT) then
          Cmp := LeftStr > RightStr
        else if (ANode.POper.PType = T_LT) then
          Cmp := LeftStr < RightStr
        else if (ANode.POper.PType = T_EQ) then
          Cmp := LeftStr = RightStr
        else if (ANode.POper.PType = T_LEQ) then
          Cmp := LeftStr <= RightStr
        else if (ANode.POper.PType = T_GEQ) then
          Cmp := LeftStr >= RightStr
        else if (ANode.POper.PType = T_NEQ) then
          Cmp := LeftStr <> RightStr
        else
          ERunTimeError.Create('Logic operation ' + ANode.POper.PType +
            ' forbidden for type ' + LeftClass,
            FTrace, ANode.PToken);
        Result := TBooleanInstance.Create(Cmp);
        exit;
      end
      else if LeftClass = 'TIntegerInstance' then
      begin
        LeftInt := TIntegerInstance(AresL).PValue;
        RightInt := TIntegerInstance(AResR).PValue;
        if (ANode.POper.PType = T_GT) then
          Cmp := LeftInt > RightInt
        else if (ANode.POper.PType = T_LT) then
          Cmp := LeftInt < RightInt
        else if (ANode.POper.PType = T_EQ) then
          Cmp := LeftInt = RightInt
        else if (ANode.POper.PType = T_LEQ) then
          Cmp := LeftInt <= RightInt
        else if (ANode.POper.PType = T_GEQ) then
          Cmp := LeftInt >= RightInt
        else if (ANode.POper.PType = T_NEQ) then
          Cmp := LeftInt <> RightInt
        else if (ANode.POper.PType = T_AND) then
          Cmp := (LeftInt > 0) and (RightInt > 0)
        else if (ANode.POper.PType = T_OR) then
          Cmp := (LeftInt > 0) and (RightInt > 0);
        Result := TBooleanInstance.Create(Cmp);
        exit;
      end
      else if LeftClass = 'TFloatInstance' then
      begin
        LeftExt := TFloatInstance(AresL).PValue;
        RightExt := TFloatInstance(AResR).PValue;
        if (ANode.POper.PType = T_GT) then
          Cmp := LeftExt > RightExt
        else if (ANode.POper.PType = T_LT) then
          Cmp := LeftExt < RightExt
        else if (ANode.POper.PType = T_EQ) then
          Cmp := LeftExt = RightExt
        else if (ANode.POper.PType = T_LEQ) then
          Cmp := LeftExt <= RightExt
        else if (ANode.POper.PType = T_GEQ) then
          Cmp := LeftExt >= RightExt
        else if (ANode.POper.PType = T_NEQ) then
          Cmp := LeftExt <> RightExt
        else if (ANode.POper.PType = T_AND) then
          Cmp := (LeftExt > 0) and (RightExt > 0)
        else if (ANode.POper.PType = T_OR) then
          Cmp := (LeftExt > 0) and (RightExt > 0);
        Result := TBooleanInstance.Create(Cmp);
        exit;
      end
      else
      begin
        LeftAddr := addr(AResL);
        RightAddr := addr(AResR);
        LeftInt := LeftAddr^;
        RightInt := RightAddr^;
        if (ANode.POper.PType = T_GT) then
          Cmp := LeftInt > RightInt
        else if (ANode.POper.PType = T_LT) then
          Cmp := LeftInt < RightInt
        else if (ANode.POper.PType = T_EQ) then
          Cmp := LeftInt = RightInt
        else if (ANode.POper.PType = T_LEQ) then
          Cmp := LeftInt <= RightInt
        else if (ANode.POper.PType = T_GEQ) then
          Cmp := LeftInt >= RightInt
        else if (ANode.POper.PType = T_NEQ) then
          Cmp := LeftInt <> RightInt
        else if (ANode.POper.PType = T_AND) then
          Cmp := (LeftInt > 0) and (RightInt > 0)
        else if (ANode.POper.PType = T_OR) then
          Cmp := (LeftInt > 0) and (RightInt > 0);
        Result := TBooleanInstance.Create(Cmp);
        // RaiseException('Can''t compare instances of type ' + LeftClass, 'Type');
      end;
    end
    else
      ERunTimeError.Create('Can''t compare different types ' +
        LeftClass + ' and ' + RightClass + ' implicitly.',
        FTrace, ANode.PToken);
  //except
  //end;
end;

function TInterpreter.VisitBinOp(ANode: TBinOp): TInstanceOf;
var
  LeftExt, RightExt, ResExt: extended;
  LeftInt, RightInt: integer;
  LeftBool, RightBool, Leftnum, RightNum: boolean;
  LeftStr, RightStr: string;
  LeftClass, RightClass: string;
  AResl, AResR: TInstanceOf;
  StrRes: string;
  Numeric, StrOper, Mismatch: boolean;

begin
  LeftStr := ANode.PLeft.PToken.PType;
  AResL := Visit(Anode.PLeft);
  RightStr := ANode.PRight.PToken.PType;
  AResR := Visit(Anode.PRight);
  LeftClass := AResL.ClassName;
  RightClass := AresR.ClassName;
  LeftNum := (LeftClass = 'TIntegerInstance') or (LeftClass = 'TFloatInstance') or (LeftClass = 'TByteInstance');
  RightNum := (RightClass = 'TIntegerInstance') or (RightClass = 'TFloatInstance') or (RightClass = 'TByteInstance');
  Numeric := LeftNum and RightNum;
  StrOper := (LeftClass = 'TStringInstance') and (RightClass = 'TStringInstance');
  Mismatch := ((LeftClass = 'TStringInstance') and (RightClass <> 'TStringInstance')) and
    ((LeftClass <> 'TStringInstance') and (RightClass = 'TStringInstance'));
  if StrOper then
  begin
    if (ANode.POper.PType = T_PLUS) then
    begin
      StrRes := TStringInstance(AResL).PValue + TStringInstance(AResR).PValue;
      Result := TStringInstance.Create(Strres);
    end
    else
      ERunTimeError.Create('Invalid operation ' + Anode.POper.PType +
        ' between strings',
        FTrace, ANode.PToken);
  end
  else if Numeric then
  begin
    if AResl.ClassNameIs('TIntegerInstance') or AResL.ClassNameIs('TByteInstance') then
      LeftExt := TIntegerInstance(AresL).PValue
    else
      LeftExt := TFloatInstance(AResL).PValue;
    if AResR.ClassNameIs('TIntegerInstance') or AResR.ClassNameIs('TByteInstance') then
      RightExt := TIntegerInstance(AresR).PValue
    else
      RightExt := TFloatInstance(AResR).PValue;


    if (ANode.POper.PType = T_PLUS) then
      ResExt := LeftExt + RightExt
    else if (ANode.POper.PType = T_MINUS) then
      ResExt := LeftExt - RightExt
    else if (ANode.POper.PType = T_MULT) then
      ResExt := LeftExt * RightExt
    else if (ANode.POper.PType = T_INT_DIV) then
      ResExt := Floor(LeftExt / RightExt)
    else if (ANode.POper.PType = T_MODULUS) then
      ResExt := Floor(LeftExt) mod Floor(RightExt)
    else if (ANode.POper.PType = T_DIV) then
      ResExt := LeftExt / RightExt;
    if Floor(ResExt) = ResExt then
      Result := TIntegerInstance.Create(Floor(ResExt))
    else
      Result := TFloatInstance.Create(ResExt);
  end
  else if LeftClass = RightClass then
    ERunTimeError.Create('Invalid operation ' + ANode.POper.ptype +
      ' for type ' + LeftClass,
      FTrace, ANode.PToken)
  else
    ERunTimeError.Create('Can''t perform opertions between different types ' +
      LeftClass + ' and ' + RightClass + ' implicitly.',
      FTrace, ANode.PToken);

end;

function TInterpreter.VisitNumFloat(ANode: TNumFloat): TFloatInstance;
begin

  Result := TFloatInstance.Create(ANode.PValue.ToExtended);
end;

function TInterpreter.VisitByte(ANode: TByte): TByteInstance;
var
  up1, up0: integer;
  byteHex: string;
begin
  byteHex := uppercase(ANode.PValue);
  try
    up1 := StrToInt(byteHex[1]) * 16;
  except
    up1 := (ord(byteHex[1]) - 55) * 16;
  end;
  try
    up0 := StrToInt(byteHex[2]);
  except
    up0 := (ord(byteHex[2])) - 55;
  end;
  if (up0 + up1) > 255 then
    RaiseException('Byte value out of range', 'RunTime');
  Result := TByteInstance.Create(up1 + up0);
end;

function TInterpreter.VisitNull(ANode: TNull): TNullInstance;
begin

  Result := TNullInstance.Create;
end;

function TInterpreter.VisitNumInt(ANode: TNumInt): TIntegerInstance;
begin

  Result := TIntegerInstance.Create(ANode.PValue.Tointeger);
end;

function TInterpreter.VisitString(ANode: TString): TStringInstance;
begin

  Result := TStringInstance.Create(ANode.PValue);
end;

function TInterpreter.VisitList(ANode: TListAST): TListInstance;
var
  AnItem: TAST;
  AList: TInstanceList;
  len: integer = 0;
  len2, i: integer;
  AValue: TInstanceOf;
begin
  SetLength(AList, 0);
  len2 := Length(ANode.PArgs);
  if len2 > 0 then
  begin
    for i := 0 to len2 - 1 do
    begin
      len := len + 1;
      SetLength(AList, len);
      Anitem := Anode.PArgs[i];
      AValue := Visit(ANode.PArgs[i]);
      if AValue.ClassNameIs('TFunctionInstance') then
        AValue := TFunctionInstance(AValue);
      AList[len - 1] := AValue;
    end;
  end;
  Result := TListInstance.Create(AList);
end;

function TInterpreter.VisitListAccess(ANode: TListAccessAST): TInstanceOf;
var
  AList: TListInstance;
  AStr, AIndexStr: TStringInstance;
  AIndexInt: TIntegerInstance;
  AVarRef: TVariableReference;
  ARet, ASrc, AIndex: TInstanceOf;
begin
  AIndex := Visit(ANode.PIndex);
  if AIndex.ClassNameIs('TStringInstance') then
    AIndexStr := TStringInstance(AIndex)
  else if AIndex.ClassNameIs('TIntegerInstance') then
    AIndexInt := TIntegerInstance(AIndex)
  else if AIndex.ClassNameIs('TNullInstance') then
  begin

  end
  else
    ERunTimeError.Create('Foridden type for index using',
      FTrace, ANode.PToken);
  ASrc := Visit(Anode.PList);
  if ASrc.ClassNameIs('TListInstance') then
  begin
    if TListInstance(ASrc).Count > AIndexInt.PIntValue then
      ARet := TListInstance(ASrc).GetItem(AIndexInt)
    else
      RaiseException('Array index ' + IntToStr(AIndexInt.PIntValue) + ' queried out of bounds', 'Value');
  end
  else if ASrc.ClassNameIs('TStringInstance') then
  begin
    if AIndex.ClassNameIs('TIntegerInstance') then
      ARet := TStringInstance(ASrc).GetChar(AIndexInt)
    else
      ERunTimeError.Create('Foridden type for index using with List',
        FTrace, ANode.PToken);
  end
  else if ASrc.ClassNameIs('TDictionaryInstance') then
  begin
    if AIndex.ClassNameIs('TStringInstance') then
      ARet := TDictionaryInstance(ASrc).PValue.GetMember(AindexStr.PValue)
    else if AIndex.ClassNameIs('TIntegerInstance') then
      ARet := TDictionaryInstance(ASrc).PValue.GetMember(IntToStr(AIndexInt.PValue))
    else if Aindex.ClassNameIs('TNullInstance') then
      ARet := TDictionaryInstance(ASrc).PDefault
    else
      ERunTimeError.Create('Foridden type for index using with Dict',
        FTrace, ANode.PToken);
    if ARet = nil then
      ARet := TDictionaryInstance(ASrc).PDefault;
  end
  else
    ERunTimeError.Create('Forbidden type for indexing as list',
      FTrace, ANode.PToken);
  try
    Aret.AsString;
    Result := ARet;
  except
    ARet := TDictionaryInstance(ASrc).PDefault;
    try
      Aret.AsString;
      Result := ARet;
    except
      if AIndex.ClassNameIs('TNullInstance') then
        ERunTimeError.Create('This dictionary does not have a default value',
          FTrace, ANode.PToken)
      else
        ERunTimeError.Create('The referenced key "' + AIndex.AsString +
          '" does not exist at given Dict/List and this dictionary does not have a default fallback value.',
          FTrace, ANode.PToken);

    end;
  end;
end;

function Tinterpreter.VisitBoolean(ANode: TBoolean): TBooleanInstance;
begin
  if (ANode.PValue = T_LANG_TRUE) then
    Result := TBooleanInstance.Create(True)
  else if (ANode.PValue = T_LANG_FALSE) then
    Result := TBooleanInstance.Create(False);

end;

end.
