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
    PtrInst = ^TInstanceOf;

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
    FRunTimeInstances: TInstanceList;
    procedure BootStrapRegister;


  public

    property PTree: TAST read FTree;
    property PLive: string read FLiveOutput;
    procedure RaiseException(AMsg:string; AType: string);
    constructor Create(var ATree: TAST);
    function GetLive: string;
    procedure PassCallStack(var ACallStack: TStack; ToParent: boolean);
    procedure CleanStack;
    procedure FreeInstances;
    function Interpret(DontPush: boolean = False;
      ANameActRec: TActivationRecord = nil): string;



    function Visit(ANode: TAST; ASrcInstance: TInstanceOf = nil): TInstanceOf;
      {$INCLUDE 'interpreter/visitations_declarations.pp'}

  end;



implementation

uses
  Math,  TokenClass, Tokens, CoreFunctionsClass, LexerClass,

  ServerClass;

constructor TInterpreter.Create(var ATree: TAST);
begin
  FTree := ATree;
  FCallStack := TStack.Create;
  FBreakSignal := False;
  FTrace := TStringList.Create;
  FTrace.SkipLastLineBreak := True;
  FTrace.LineBreak := sLineBreak + '+ ';
  SetLength(FRunTimeInstances, 0);
end;

procedure TInterpreter.FreeInstances;
var
  len, i: integer;
  x : PtrInst;
  y: ^word;
begin
  len := Length(FRunTimeInstances);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      try
        //FRunTimeInstances[i].Free;
        //x := @FRunTimeInstances[i];
        //y := addr(x);
        FRunTimeInstances[i].Free;
        //writeln(i, ' -- freeing: ', FRunTimeInstances[i].ToString);

        //Writeln(@FRunTimeInstances);
      except
         //writeln(i, ' -- jump');
      end;

    end;
  end;
end;

procedure TInterpreter.RaiseException(AMsg:string; AType: string);
begin
  EClientException.Create(AMsg, FTrace, FNowNode.PToken, AType);
end;

procedure TInterpreter.BootstrapRegister;
const
  ST_ACCESS = ':';
var
  AActRec: TActivationRecord;
  FileExplorer: TActivationRecord;
  ACoreType, AStrType, AIntType, AFloatType, AListType,
  ABoolType, AFuncType, AOSType, AFSType, ADictType,
  AServerType: TFunctionInstance;
  ANameSpace: TActRecInstance;
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
  ADictType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TActRecInstance', True);
  AServerType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TServerInstance', True);
  AActRec := FCallStack.GetFirst();

  // BuiltInTypesRegister

  AActRec.AddMember('FileSystem', TBuiltInType.Create('TFileSystemInstance'));
  AActrec.AddMember('Dict', TBuiltInType.Create('TActRecInstance'));
  AActRec.AddMember('Server', TBuiltInType.Create('TServerInstance'));
  AActRec.AddMember('String', TBuiltInType.Create('TStringInstance'));


  AActRec.AddMember(AStrType.PType+ST_ACCESS+'init', AStrType);
  AActRec.AddMember(ADictType.PType+ST_ACCESS+'set', ADictType);
  AActRec.AddMember(ADictType.PType+ST_ACCESS+'get', ADictType);
  AActRec.AddMember(ADictType.PType+ST_ACCESS+'keys', ADictType);
  AActRec.AddMember(AServerType.PType+ST_ACCESS+'init', AServerType);
  AActRec.AddMember(AServerType.PType+ST_ACCESS+'setTitle', AServerType);
  AActRec.AddMember(AFSType.PType+ST_ACCESS+'mkdir', AFSType);

  AActRec.AddMember(AServerType.PType+ST_ACCESS+'setStopRoute', AServerType);
  AActRec.AddMember(AIntType.PType + ST_ACCESS + 'leftZeros', AIntType);


  {$INCLUDE 'builtin_functions/register_builtins.pp' }
end;

procedure TInterpreter.CleanStack;
begin
  FCallStack.Pop();
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
  //writeln(Length(FRunTimeInstances));
  Result := '';
end;

function TInterpreter.GetLive: string;
var
  AActRec: TActivationRecord;
  AVal: TStringInstance;
begin
  AActrec := FCallStack.Peek();
  Aval := TStringInstance(AActRec.GetMember('$_LIVE'));
  Result := AVal.PValue;
end;

procedure TInterpreter.VisitPlainTextEmbed(ANode: TPlainTextEmbed);
var
  //AState: TAST;
  i, len:integer;
begin
  len := Length(Anode.PNodes);
  if len > 0 then
    for i:=0 to len-1 do
      Visit(ANode.PNodes[i]);
  //for AState in Anode.PNodes do
    //Visit(AState);
end;

function TInterpreter.VisitDict(ANode: TDictNode): TActRecInstance;
var
  AActRec: TActivationRecord;
  //AKey: TAST;
  ATypedKey: TDictKeyNode;
  ARepo: TInstanceOf;
  len, i:integer;
begin
  AActRec := TActivationRecord.Create('Any', AR_DICT, 1);
  //for Akey in ANode.PKeys do
  len := Length(ANode.PKeys);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      ATypedKey := TDictkeyNode(ANode.PKeys[i]);
      AActRec.AddMember(Visit(ATypedkey.PKey).AsString,Visit(ATypedKey.PValue));
    end;
  end;
  Result := TActRecInstance.Create(AActRec);
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
  ALiveVal := TStringInstance(AActrec.GetMember('$_LIVE'));
  ALiveVal.PValue := ALiveVal.PValue + AVal.AsString;
  AActRec.AddMember('$_LIVE', ALiveVal);
end;

function TInterpreter.VisitLivePrint(ANode: TLivePrint): TStringInstance;
var
  AActRec: TActivationRecord;
begin
  AActRec := FCallStack.Peek();
  Result := TStringInstance(AActrec.GetMember('$_LIVE'));
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
begin
  FReturnSignal := True;
  AVal := Visit(ANode.PValue);
  FReturnValue := AVal;
  Result := AVal;
end;

procedure TInterpreter.VisitPlainText(ANode: TPlainText);
var
  AActRec: TActivationRecord;
  aLive: string;
begin
  AActRec := FCallStack.Peek();
  ALive := TStringInstance(AActrec.GetMember('$_LIVE')).PValue;
  TStringInstance(AActrec.GetMember('$_LIVE')).PValue := ALive + ANode.PValue;
end;

procedure TInterpreter.VisitInterpolation(ANode: TInterpolation);
var
  AActRec: TActivationRecord;
  ALive: string;
begin
  AActRec := FCallStack.Peek();
  ALive := AActrec.GetMember('$_LIVE').AsString;
  TStringInstance(AActrec.GetMember('$_LIVE')).PValue :=
    ALive + Visit(ANode.POper).AsString;
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
  ActInst: TActRecInstance;
  Gene: TInstanceOf;
  ABuilt: TBuiltInType;
begin
  NowAct := FCallStack.Peek();
  Gene := NowAct.GetMember(ANode.PName);
  if Gene <> nil then
  begin
    if Gene.ClassNameIs('TActRecInstance') then
    begin
      ActInst := TActRecInstance(Gene);
      ActInst.PValue.CopyActRec(NewAct);

      Result := TActrecInstance.Create(NewAct);
    end
    else if Gene.ClassNameIs('TBuiltInType') then
    begin
      ABuilt := TBuiltInType(Gene);


      if ABuilt.PValue = 'TServerInstance' then
        Ret := TServerInstance.Create;


      Result := Ret;
    end
  end
  else
  begin
     ERunTimeError.Create('Referenced type or namespace ' + ANode.PName + ' does not exist',
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
    for i:=0 to len-1 do
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
        //for AState in ANode.PBLock do
        for i:=0 to len-1 do
        begin
          if FBreakSignal or
             FContinueSignal then
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
      //for AState in ANode.PBLock do
    for i:=0 to len-1 do
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
  len, i:integer;
begin
  while TBooleanInstance(Visit(ANode.PCondition)).PValue do
  begin
    len := Length(ANode.PBlock);
    if len > 0 then
    begin
      //for AState in ANode.PBLock do
      for i:=0 to len-1 do
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
  AState: TAST;
  AList, AVar: TAST;
  AInst, AListRes: TInstanceOf;
  AInt, AIndex: TIntegerInstance;
  AActRec: TActivationRecord;
  i: integer = 0;
  len, len2, j: integer;
  ACandidate: TListInstance;
  AStr: string;
begin
  AActRec := FCallStack.Peek;
  AListRes := Visit(ANode.PList);
  if AListRes.ClassNameIs('TListInstance') then
    ACandidate := TListInstance(AListRes)
  else if AListRes.ClassNameIs('TStringInstance') then
  begin
    ACandidate := TListInstance.Create;
    for AStr in AListRes.AsString do
      ACandidate.Add(TStringInstance.Create(AStr));
  end
  else if AListRes.ClassNameIs('TIntegerInstance') and
    (StrToInt(AListRes.AsString) > -1) then
  begin
    len := StrToInt(AListRes.AsString);
    ACandidate := TListInstance.Create;
    if (len > 0) then
    begin
      for i := 0 to len - 1 do
        ACandidate.Add(TIntegerInstance.Create(i));
    end
    else
    begin
      //ACandidate.Free;
       ERunTimeError.Create(AListRes.ClassName + ' is not iterable',
          FTrace, ANode.PToken);
    end;
  end
  else
  begin
     ERunTimeError.Create(AListRes.ClassName + ' is not iterable',
          FTrace, ANode.PToken);
  end;
  len2 := Length(ACandidate.PValue);
  if len2 > 0 then
  begin
    //for AInst in ACandidate.PValue do
    for i:=0 to len2-1 do
    begin
      //AActRec.AddMember(Anode.PVar.PVarName.PValue, AInst);
      AActRec.AddMember(Anode.PVar.PVarName.PValue, ACandidate.PValue[i]);
      AIndex := TIntegerInstance.Create(i);
      AActRec.AddMember('_' + Anode.PVar.PVarName.PValue, AIndex);
      len := Length(ANode.PBlock);
      if len > 0 then
      begin
        //for AState in ANode.PBlock do
        for j:=0 to len-1 do
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
      //i := 1 + i;
      if FBreakSignal or FReturnSignal then
      begin
        break;
      end;
      AIndex.Free;
    end;
  end;
  {for i := 0 to ACandidate.Count - 1 do
  begin
    ACandidate.PValue[i].Free;
  end;
  ACandidate.Free;}
  FBreakSignal := False;
end;



procedure TInterpreter.VisitVarAssign(ANode: TVarAssign);
var
  AValue: TInstanceOf;
  AName: string;
  AActRec : TActivationRecord;
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
  AActRec.AddMember(AName, AValue);
end;

procedure TInterpreter.VisitListAssign(ANode: TListAssign);
var
  ASrc, AValue, AIndex: TInstanceOf;
  ASrcAct: TActRecInstance;
  ASrcList: TListInstance;
begin
  LogText(INTER, 'Interpreter', 'VarList visitation');
  ASrc := Visit(ANode.PSrc);
  Aindex := Visit(ANode.PEntry);
  AValue := Visit(ANode.PValue);
  if ASrc.ClassNameIs('TActRecInstance') then
  begin
    ASrcAct := TActrecInstance(ASrc);
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
  ResFloat: TFloatInstance;
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
  LogText(INTER, 'Interpreter', 'Getting value of "' +
    ANode.PToken.PValue + '" from type ' + Ret.ClassName);
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
  x : ^word;
  ACoreExec: TCoreFunction;
  AFuncName, ASrcName, compl: string;
  AActRec: TActivationRecord;
  AParamName: string;
  //AState: TAST;
  len, len2, i:integer;
  AParamState: TParam;
  FuncDef, Aux: TFunctioninstance;
  ADef: TInstanceOf = nil;
  AReturn, AIter: TInstanceOf;
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
      AActRec.AddMember('$_LIVE', TStringInstance.Create(''));
      AActRec.AddMember('self', ASrcInstance);
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
                'Wrong number of arguments to call this function');
          end
          else if i > (LenParams - 1) then
          begin
             EArgumentsError.Create(
              'Wrong number of arguments to call this function');
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
                ANode.PEvalParams[i].PToken.PValue + '" as argument');
            //AParamName := ANode.PEvalParams[i].PToken.PValue;
          end;
          AActRec.AddMember(AParamName, ADef);
        end;
      end;
      FCallStack.Push(AActRec);
      len := Length(Funcdef.PBlock);
      if len > 0 then
      begin
        //for AState in FuncDef.PBlock do
        for i:=0 to len-1 do
        begin
          if FReturnSignal then
          begin
            FReturnSignal := False;
            AReturn := FReturnValue;
            FCallStack.Pop();
            Result := FReturnValue;
            exit;
          end
          else
            //Visit(AState);
            Visit(FuncDef.PBlock[i]);

        end;
      end;
      AReturn := AActRec.GetMember('$_LIVE');
      //x := addr(AReturn);
      //writeln(x^);
      //writeln(sizeof(AReturn));
      FCallStack.Pop();
      Result := AReturn;
      exit;
    end
    else
    begin
      ACoreExec := TCoreFunction.Create;
      Len := 0;
      SetLength(ArgsList, 0);
      len2 := Length(ANode.PEvalParams);
      if len2 > 0 then
      begin
        //for AState in ANode.PEvalParams do
        for i:=0 to len2-1 do
        begin
          Len := Len + 1;
          SetLength(ArgsList, len);
          //ArgsList[len - 1] := Visit(AState, ASrcInstance);
          ArgsList[len - 1] := Visit(ANode.PEvalParams[i], ASrcInstance);
        end;
      //if FuncDef.ClassNameIs('TBuiltInType') then
        //ASrcInstance.
        AReturn := ACoreExec.Execute(Self, AFuncName, ArgsList, ASrcInstance);
      end;
      ACoreExec.Free;
      Result := AReturn;
      exit;
    end;
    FuncDef.Free;
    len := Length(ArgsList);
    if len > 0 then
    begin
      for i:=0 to len-1 do
        ArgsList[i].Free;
    end;
  end
  else
  begin

     ERunTimeError.Create('Referenced function "' + AFuncName +
      '" does not exist.');
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
  AActRec.AddMember('$_LIVE', TStringInstance.Create(''));
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
    //for AChild in Anode.PPreludes do
    for i:=0 to len - 1 do
      //Visit(AChild);
      Visit(ANode.PPreludes[i]);
  end;

  len := Length(ANode.PChildren);
  if len > 0 then
  begin
    //for AChild in ANode.PChildren do
    for i:=0 to len-1 do
    begin
      //LogText(INTER, 'Interpreter', 'Visiting a child ' + AChild.ToString);
      FTrace.Clear;
      //Visit(AChild);
      Visit(ANode.PChildren[i]);
    end;
  end;
  if FNameSpace <> nil then
  begin
    AActRec := FCallStack.Peek();
    AParRec := FParentStack.Peek();
    AParRec.AddMember(FNameSpace.PName, TActRecInstance.Create(AActRec));
    FCallStack := FParentStack;

  end;
  FTrace.Free;

  FLiveOutput := GetLive();
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
  AFileName := TStringInstance(Visit(ANode.PFilename)).PValue;
  ALexer := TLexer.Create(AFileName);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode;
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
  // AInter.Free;
end;

function TInterpreter.VisitNameSpaceGet(Anode: TNamespaceGet): TInstanceOf;
var
  AActRec, ARef: TActivationRecord;
  ANameRec: TActRecInstance;
  Ret, ADictCand, AKeyCand: TInstanceOf;
  AKey: TStringInstance;
begin
  AActrec := FCallStack.Peek();
  ADictCand := AActRec.GetMember(ANode.PName);
  if ADictCand.ClassNameIs('TActRecInstance') then
  begin
    Aref := TActrecInstance(ADictCand).PValue;
    FCallStack.Push(ARef);
  end
  else
     ERunTimeError.Create('Referenced name is not a Dict type');
  Ret := Visit(ANode.Poper);
  FCallStack.Pop();
  Result := ret;

end;

function TInterpreter.VisitNameSpaceState(Anode: TNamespaceState): TInstanceOf;
var
  AActRec, ARef: TActivationRecord;
  ANameRec: TActRecInstance;
  Ret, ArefVal: TInstanceOf;
begin
  AActrec := FCallStack.Peek();
  ARefVal := AActRec.GetMember(ANode.PName);
  if ARefVal = nil then
  begin
    ARef := TActivationRecord.Create(ANode.PName, AR_NAMESPACE, 1);
    ANameRec := TActRecInstance.Create(Aref);
    AActRec.AddMember(ANode.PName, ANameRec);
  end
  else
  begin
    Aref := TActrecInstance(ARefVal).PValue;
    ANameRec := TActRecInstance.Create(Aref);
  end;
  FCallStack.Push(AnameRec.PValue);
  Ret := Visit(ANode.POper);
  FcallStack.Pop();
  Result := ret;
end;

function TInterpreter.Visit(ANode: TAST; ASrcInstance: TInstanceOf = nil): TInstanceOf;
var
  AuxBool: boolean;
  Acd, Adir, Afn:string;
  len: integer;
  Ret: TInstanceOf;
begin
  len := Length(FRunTimeInstances);
  SetLength(FRunTimeInstances, len + 1);
  FNowNode := ANode;
  if ANode.ClassName <> 'TProgram' then
  begin
    if FTrace.Count = 0 then
    begin
      Acd := GetCurrentDir;
      if Length(Acd) > 0 then
        Acd := Acd + DirectorySeparator;
      Adir := ExtractFilePath(ANode.PToken.PScriptName);
      if Length(Adir) > 0 then
        Adir := Adir + DirectorySeparator;
      AFn := ExtractFileName(ANode.PToken.PScriptName);
      FTrace.Add(ANode.PToken.PScriptName +' at < ' +
        IntToStr(ANode.PToken.PLineNo)+':'+ IntToStr(ANode.PToken.PCharNo) + ' >');
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
    {if AOper = T_PLUS then
      Ret.PValueInt := Ret.PValueInt
    else }if AOper = T_MINUS then
    begin
      Ret.PValue := Ret.PValue * (-1);
    end;
    Result := ret;
  except
     ERunTimeError.Create('Invalid operation for class ' + Ret.ClassName);
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
     ERunTimeError.Create('Invalid operation for class ' + Ret.ClassName);
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
     ERunTimeError.Create('Invalid operation for class ' + ARes.ClassName);
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
            ' forbidden for type ' + LeftClass);
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
         ERunTimeError.Create('Can''t compare instances of type ' + LeftClass);
    end
    else
       ERunTimeError.Create('Can''t compare different types ' +
        LeftClass + ' and ' + RightClass + ' implicitly.');
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
  {if LeftStr = T_ID then
    AResL := VisitVariableReference(TVariableReference(ANode.PLeft))
  else
    }AResL := Visit(Anode.PLeft);
  RightStr := ANode.PRight.PToken.PType;
  {if RightStr = T_ID then
    AResR := VisitVariableReference(TVariableReference(ANode.PRight))
  else
    }AResR := Visit(Anode.PRight);
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
        ' between strings');
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
      ' for type ' + LeftClass)
  else
     ERunTimeError.Create('Can''t perform opertions between different types ' +
      LeftClass + ' and ' + RightClass + ' implicitly.');

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
    //for AnItem in Anode.PArgs do
    for i:=0 to len2-1 do
    begin
      len := len + 1;
      SetLength(AList, len);
      //AList[len - 1] := Visit(AnItem);
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
  else
     ERunTimeError.Create('Foridden type for index using');
  ASrc := Visit(Anode.PList);
  if ASrc.ClassNameIs('TListInstance') then
    ARet := TListInstance(ASrc).GetItem(AIndexInt)
  else if ASrc.ClassNameIs('TStringInstance') then
  begin
    if AIndex.ClassNameIs('TIntegerInstance') then
      ARet := TStringInstance(ASrc).GetChar(AIndexInt)
    else
       ERunTimeError.Create('Foridden type for index using with List');
  end
  else if ASrc.ClassNameIs('TActRecInstance') then
  begin
    if AIndex.ClassNameIs('TStringInstance') then
      ARet := TActRecInstance(ASrc).PValue.GetMember(AindexStr.PValue)
    else
       ERunTimeError.Create('Foridden type for index using with Dict');
  end
  else
     ERunTimeError.Create('Foridden type for indexing as list');
  Result := ARet;
end;

function Tinterpreter.VisitBoolean(ANode: TBoolean): TBooleanInstance;
begin
  if (ANode.PValue = T_LANG_TRUE) then
    Result := TBooleanInstance.Create(True)
  else if (ANode.PValue = T_LANG_FALSE) then
    Result := TBooleanInstance.Create(False);

end;

end.
