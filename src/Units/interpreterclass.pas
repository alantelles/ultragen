unit InterpreterClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ASTClass,
  BinOpClass, NumClass, UnaryOpClass, BinLogicOpClass, UnaryLogicopClass,
  ImpParserClass, StrUtils, LoggingClass, FlowControlASTClass,
  StackClass, ARClass, InstanceofClass,
  StringInstanceClass, ExceptionsClasses,
  ListInstanceClass;

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
    FNameSpace: TActivationRecord;
    FParentStack: TStack;
    FTrace: TStringList;
    FNowNode: TAST;
    FUltraHome: string;
    FModulesPath: string;
    procedure BootStrapRegister;


  public

    property PTree: TAST read FTree;
    property PLive: string read FLiveOutput;
    procedure RaiseException(AMsg: string; AType: string);
    constructor Create(var ATree: TAST);
    function GetLive: string;
    procedure PassCallStack(var ACallStack: TStack; ToParent: boolean);
    function Interpret(DontPush: boolean = False;
      ANameActRec: TActivationRecord = nil): string;



    function Visit(ANode: TAST; ASrcInstance: TInstanceOf = nil): TInstanceOf;
      {$INCLUDE 'interpreter/visitations_declarations.pp'}

  end;



implementation

uses
  Math, TokenClass, Tokens, CoreFunctionsClass, LexerClass,
  ServerClass, Dos;

constructor TInterpreter.Create(var ATree: TAST);
begin
  FTree := ATree;
  FCallStack := TStack.Create;
  FBreakSignal := False;
  FTrace := TStringList.Create;
  FTrace.SkipLastLineBreak := True;
  FTrace.LineBreak := sLineBreak + '+ ';
  FUltraHome := GetEnv('ULTRAGEN_HOME');
  FModulesPath := FUltraHome + DirectorySeparator + 'modules'
end;


procedure TInterpreter.RaiseException(AMsg: string; AType: string);
begin
  EClientException.Create(AMsg, FTrace, FNowNode.PToken, AType);
end;

procedure TInterpreter.BootstrapRegister;
const
  ST_ACCESS = ':';
var
  AActRec: TActivationRecord;
  FileExplorer: TActivationRecord;
  ACoreType, AStrType, AIntType, AFloatType, AListType, ABoolType,
  AFuncType, AOSType, AFSType, ADictType, AServerType: TFunctionInstance;
  ANameSpace: TDictionaryInstance;
begin
  ACoreType := TFunctionInstance.Create('BuiltIn', nil, nil, 'CoreFunction', True);
  AStrType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TStringInstance', True);
  AIntType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TIntegerInstance', True);
  AFloatType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFloatInstance', True);
  AListType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TListInstance', True);
  AFuncType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFunctionInstance', True);
  ABoolType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TBooleanInstance', True);
  AOSType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TOSInstance', True);
  AFSType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFileSystemInstance', True);
  ADictType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TDictionaryInstance', True);
  AServerType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TServerInstance', True);
  AActRec := FCallStack.GetFirst();

  // BuiltInTypesRegister
  AActRec.AddMember('OS', TBuiltInType.Create('TOSInstance'));
  AActRec.AddMember('FileSystem', TBuiltInType.Create('TFileSystemInstance'));
  AActrec.AddMember('Dict', TBuiltInType.Create('TDictionaryInstance'));
  AActRec.AddMember('Server', TBuiltInType.Create('TServerInstance'));
  AActRec.AddMember('String', TBuiltInType.Create('TStringInstance'));


  {$INCLUDE 'builtin_functions/register_builtins.pp' }
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

function TInterpreter.VisitFunctionDefinition(ANode: TFunctionDefinition): TInstanceOf;
var
  i, len: integer;
  AValue: TFunctionInstance;
  AActRec: TActivationRecord;
  ABlock: TAST;
begin
  logtext('INTER', 'Interpreter', 'Visiting function definition');
  AActRec := FCallStack.Peek;
  AValue := TFunctionInstance.Create(ANode.PName, ANode.PParamList,
    ANode.PBlock, ANode.ptype, False);
  AActrec.AddMember(ANode.PName, AValue);
  Result := TInstanceOf.Create;
end;

function TInterpreter.VisitNewObject(ANode: TNewObject): TInstanceOf;
var
  Ret: TInstanceOf;
  NowAct, NewAct: TActivationRecord;
  ActInst: TDictionaryInstance;
  Gene: TInstanceOf;
  ABuilt: TBuiltInType;
  ArgsList: TInstanceList;
  len, i: integer;
begin
  NowAct := FCallStack.Peek();
  Gene := NowAct.GetMember(ANode.PName);
  if Gene <> nil then
  begin
    if Gene.ClassNameIs('TDictionaryInstance') then
    begin
      ActInst := TDictionaryInstance(Gene);

      ActInst.PValue.CopyActRec(NewAct);

      Result := TDictionaryInstance.Create(NewAct);
    end
    else if Gene.ClassNameIs('TBuiltInType') then
    begin
      ABuilt := TBuiltInType(Gene);
      len := Length(ANode.PArgs);
      SetLength(ArgsList, len);
      if len > 0 then
      begin
        for i := 0 to len - 1 do
          ArgsList[i] := Visit(ANode.PArgs[i]);
      end;
      if ABuilt.PValue = 'TServerInstance' then
      begin
        Ret := TServerInstance.Create(TIntegerInstance(ArgsList[0]).PValue);
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

procedure TInterpreter.VisitForLoop(ANode: TForLoop);
var
  AInst, AListRes: TInstanceOf;
  AInt, AIndex: TIntegerInstance;
  AActRec: TActivationRecord;
  len, len2, j, i: integer;

  ACandidate: TListInstance;
  AStr: string;
begin
  AActRec := FCallStack.Peek;
  AListRes := Visit(ANode.PList);
  if AListRes.ClassNameIs('TListInstance') then
  begin
    ACandidate := TListInstance.Create;
    TListInstance(AListRes).CopyList(ACandidate);
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



procedure TInterpreter.VisitVarAssign(ANode: TVarAssign);
var
  AValue: TInstanceOf;
  AName: string;
  AActRec: TActivationRecord;
begin
  LogText(INTER, 'Interpreter', 'VarAssign visitation');
  AName := ANode.PVarName.PValue;
  AValue := Visit(ANode.PValue);
  if AValue.ClassNameIs('TFunctionInstance') then
  begin
    if TFunctionInstance(AValue).PIsBuiltin then
    begin
      ERunTimeError.Create('Can''t assign builtin function "' +
        ANode.PValue.PToken.PValue + '" to variable "' + AName + '"',
        FTrace, ANode.PToken);
    end;
  end;
  AActrec := FCallStack.Peek;
   if not AActRec.AddMember(AName, AValue) then
     ERunTimeError.Create('Can''t redefine constant value "'+Aname+'"', FTrace, ANode.PVarName);
end;

procedure TInterpreter.VisitListAssign(ANode: TListAssign);
var
  ASrc, AValue, AIndex: TInstanceOf;
  ASrcAct: TDictionaryInstance;
  ASrcList: TListInstance;
begin
  LogText(INTER, 'Interpreter', 'VarList visitation');
  ASrc := Visit(ANode.PSrc);
  Aindex := Visit(ANode.PEntry);
  AValue := Visit(ANode.PValue);
  if ASrc.ClassNameIs('TDictionaryInstance') then
  begin
    ASrcAct := TDictionaryInstance(ASrc);
    if ASrcAct.PChangeLocked then
      EValueError.Create('Can''t change values of change locked Dict', FTrace, ANode.PToken);
    if AIndex.ClassNameIs('TStringInstance') then
      ASrcAct.PValue.AddMember(TStringInstance(AIndex).PValue, AValue)
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
      ASrcList.SetItem(TIntegerInstance(AIndex).PValue, AValue)
    else
    begin
      ERunTimeError.Create('Invalid type for List index',
        FTrace, ANode.PToken);
    end;
  end;

end;

function TInterpreter.VisitVariableReference(ANode: TVariableReference): TInstanceOf;
var
  AName: string;
  AActRec, GlobalAR: TActivationRecord;
  Ret: TInstanceOf;
begin
  AName := Anode.PToken.PValue;
  AActRec := FCallStack.Peek();
  Ret := AActRec.GetMember(AName);
  if Ret = nil then
  begin
    GlobalAR := FCallStack.GetFirst;
    Ret := GlobalAR.GetMember(AName);
  end;
  if Ret = nil then
  begin
    ERunTimeError.Create('Referenced variable "' + Aname + '" does not exist',
      FTrace, ANode.PToken);
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

function TInterpreter.VisitFunctionCall(ANode: TFunctionCall;
  ASrcInstance: TInstanceOf = nil): TInstanceOf;
const
  ST_ACCESS = ':';
var
  x, y: ^word;
  ACoreExec: TCoreFunction;
  AFuncName, ASrcName, compl: string;
  AActRec: TActivationRecord;
  AParamName: string;
  len, len2, i: integer;
  AParamState: TParam;
  FuncDef, Aux: TFunctioninstance;
  ADef: TInstanceOf = nil;
  ACopy: TInstanceOf;
  AReturn, AIter, Zika: TInstanceOf;
  LenArgs, LenParams: integer;
  ArgsList: TInstanceList;
begin
  AFuncName := ANode.PFuncName;
  ASrcName := AFuncName;
  if ASrcInstance <> nil then
  begin
    if ASrcInstance.ClassNameIs('TBuiltInType') then
      ASrcName := TBuiltinType(ASrcInstance).PValue + ST_ACCESS + AFuncName
    else
      ASrcName := ASrcInstance.ClassName + ST_ACCESS + AFuncName;
  end;
  LogText(INTER, 'Interpreter', 'Visiting function ' + ANode.PFuncName);
  AActRec := FCallStack.Peek();
  FuncDef := AActRec.GetFunction(ASrcName, ASrcInstance);
  if FuncDef = nil then
  begin
    AActRec := FCallStack.GetFirst();
    FuncDef := AActRec.GetFunction(ASrcName, ASrcInstance);
  end;
  if FuncDef <> nil then
  begin
    if not FuncDef.PIsBuiltin then
    begin
      AActRec := TActivationRecord.Create(FuncDef.PName, AR_FUNCTION,
        FCallStack.PLevel + 1);
      AActRec.AddMember('__LIVE__', TStringInstance.Create(''));
      if ASrcInstance <> nil then
        AActRec.AddMember('self', ASrcInstance);
      //ASrcInstance.CopyInstance(Zika);
      //AActRec.AddMember('self', Zika);
      LenArgs := Length(Anode.PEvalParams);
      LenParams := Length(FuncDef.PParams);
      Len := Max(LenArgs, LenParams);
      if Len > 0 then
      begin
        for i := 0 to Len - 1 do
        begin
          if i > (LenArgs - 1) then
          begin
            if TParam(FuncDef.PParams[i]).PDefValue <> nil then
              ADef := Visit(TParam(FuncDef.PParams[i]).PDefValue)
            else
              EArgumentsError.Create(
                'Wrong number of arguments to call this function',
                FTrace, ANode.PToken);
          end
          else if i > (LenParams - 1) then
          begin
            EArgumentsError.Create(
              'Wrong number of arguments to call this function',
              FTrace, ANode.PToken);
          end
          else
          begin
            ADef := Visit(ANode.PEvalParams[i]);
          end;
          AParamName := TParam(FuncDef.PParams[i]).PNode.PValue;
          if ADef.ClassNameIs('TFunctionInstance') then
          begin
            Aux := TFunctionInstance(ADef);
            if Aux.PIsBuiltin then
              ERunTimeError.Create('Can''t assign builtin function "' +
                ANode.PEvalParams[i].PToken.PValue + '" as argument',
                FTrace, ANode.PToken);
            //AParamName := ANode.PEvalParams[i].PToken.PValue;
          end;
          ADef.CopyInstance(ACopy);

          AActRec.AddMember(AParamName, ACopy);
        end;
      end;
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
      ACoreExec := TCoreFunction.Create;
      len2 := Length(ANode.PEvalParams);
      SetLength(ArgsList, len2);
      if len2 > 0 then
      begin
        for i := 0 to len2 - 1 do
        begin
          ADef := Visit(ANode.PEvalParams[i], ASrcInstance);
          ADef.CopyInstance(ArgsList[i]);
        end;
      end;
      AReturn := ACoreExec.Execute(Self, AFuncName, ArgsList, ASrcInstance);
      ACoreExec.Free;
      Result := AReturn;
      exit;
    end;
  end
  else
  begin

    ERunTimeError.Create('Referenced function "' + AFuncName + '" does not exist.',
      FTrace, ANode.PToken);
  end;
  // end of new
end;


procedure TInterpreter.VisitProgram(ANode: TProgram);
var
  //AChild: TAST;
  len, i: integer;
  AActRec, AParRec: TActivationRecord;
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
    AParRec := FParentStack.Peek();
    AParRec.AddMember(FNameSpace.PName, TDictionaryInstance.Create(AActRec));
    FCallStack := FParentStack;
  end;
  FTrace.Free;
  FLiveOutput := GetLive();
  if not FDontPush then
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
  AName: string;
begin
  if ANode.PIsModule then
  begin
    AFileName := FModulesPath +
              DirectorySeparator +
              ReplaceStr(ANode.PModulePath, '.', DirectorySeparator);
    if FileExists (AFileName + '.ultra') then
      AFileName := AFileName + '.ultra'
    else if DirectoryExists(AFileName) then
    begin
      if FileExists (AFileName + DirectorySeparator + '_init.ultra') then
        AFileName := AFileName + DirectorySeparator + '_init.ultra'
      else
        ERunTimeError.Create('Specified module "'+ANode.PModulePath+'" does not exist');
    end
	end
	else
    AFileName := TStringInstance(Visit(ANode.PFilename)).PValue;
  ALexer := TLexer.Create(AFileName);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode;
  AParser.Free;
  AInter := TInterpreter.Create(ATree);

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
  AInter.Free;
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

function TInterpreter.VisitUnaryOpFloat(ANode: TUnaryOp): TFloatInstance;
var
  AOper: string;
  Ret: TFloatInstance;
begin
  try
    Ret := TFloatinstance(Visit(ANode.PExpr));
    AOper := ANode.POper.PType;
    if AOper = T_MINUS then
    begin
      Ret.PValue := Ret.PValue * (-1);
    end;
    Result := ret;
  except
    ERunTimeError.Create('Invalid operation for class ' + Ret.ClassName,
      FTrace, ANode.PToken);
  end;
end;

function TInterpreter.VisitUnaryOp(ANode: TUnaryOp): TIntegerInstance;
var
  AOper: string;
  Ret: TIntegerInstance;
begin
  try
    Ret := TIntegerinstance(Visit(ANode.PExpr));
    AOper := ANode.POper.PType;
    if AOper = T_PLUS then
      Ret.PValue := Ret.PValue
    else if AOper = T_MINUS then
    begin
      Ret.PValue := Ret.PValue * (-1);
    end;
    Result := ret;
  except
    ERunTimeError.Create('Invalid operation for class ' + Ret.ClassName,
      FTrace, ANode.PToken);
  end;
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
  LeftStr, RightStr: string;
  LeftClass, RightClass: string;
  AResl, AResR: TInstanceOf;
  Cmp: boolean;

begin
  try
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
        ERunTimeError.Create('Can''t compare instances of type ' + LeftClass,
          FTrace, ANode.PToken);
    end
    else
      ERunTimeError.Create('Can''t compare different types ' +
        LeftClass + ' and ' + RightClass + ' implicitly.',
        FTrace, ANode.PToken);
  except
  end;
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
  LeftNum := (LeftClass = 'TIntegerInstance') or (LeftClass = 'TFloatInstance');
  RightNum := (RightClass = 'TIntegerInstance') or (RightClass = 'TFloatInstance');
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
    if AResl.ClassNameIs('TIntegerInstance') then
      LeftExt := TIntegerInstance(AresL).PValue
    else
      LeftExt := TFloatInstance(AResL).PValue;
    if AResR.ClassNameIs('TIntegerInstance') then
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

function TInterpreter.VisitNull(ANode: TNull): TNullInstance;
begin

  Result := TNullInstance.Create;
end;

function TInterpreter.VisitNumInt(ANode: TNumInt): TIntegerInstance;
begin

  Result := TIntegerInstance.Create(ANode.PValue.ToInteger);
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
begin
  SetLength(AList, 0);
  len2 := Length(ANode.PArgs);
  if len2 > 0 then
  begin
    for i := 0 to len2 - 1 do
    begin
      len := len + 1;
      SetLength(AList, len);
      AList[len - 1] := Visit(ANode.PArgs[i]);
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
    ARet := TListInstance(ASrc).GetItem(AIndexInt)
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
