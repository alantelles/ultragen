unit InterpreterClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, ASTClass,
      BinOpClass, NumClass, UnaryOpClass, BinLogicOpClass, UnaryLogicopClass,
      ImpParserClass, StrUtils, LoggingClass, FlowControlASTClass,
      StackClass, ARClass, InstanceofClass,
      StringInstanceClass,
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
      procedure BootStrapRegister;


    public
      property PTree:TAST read FTree;
      property PLive: string read FLiveOutput;
      constructor Create(var ATree: TAST);
      function GetLive:string;
      procedure PassCallStack(var ACallStack: TStack);
      procedure CleanStack;
      function Interpret(DontPush:boolean=False):string;

      function Visit(ANode:TAST; ASrcInstance: TInstanceOf = nil):TInstanceOf;
      {$INCLUDE 'interpreter_visitations_declarations.pp'}

	end;

implementation

uses
  Math, ExceptionsClasses, TokenClass, Tokens, CoreFunctionsClass, LexerClass;


constructor TInterpreter.Create(var ATree: TAST);
begin
  FTree := ATree;
  FCallStack := TStack.Create;
  FBreakSignal := False;
end;

procedure TInterpreter.BootstrapRegister;
const
  ST_ACCESS = ':';
var
  AActRec: TActivationRecord;
  ACoreType, AStrType, AIntType, AFloatType, AListType, ABoolType,AFuncType: TFunctionInstance;
begin
  ACoreType := TFunctionInstance.Create('BuiltIn', nil, nil, 'CoreFunction');
  AStrType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TStringInstance');
  AIntType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TIntegerInstance');
  AFloatType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFloatInstance');
  AListType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TListInstance');
  AFuncType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFunctionInstance');
  ABoolType := TFunctionInstance.Create('BuiltIn', nil, nil, 'TBooleanInstance');
  AActRec := FCallStack.GetFirst();
  {$INCLUDE 'builtin_functions/register_builtins.pp' }
end;

procedure TInterpreter.CleanStack;
begin
  FCallStack.Pop();
end;

procedure TInterpreter.PassCallStack(var ACallStack: TStack);
begin
  FCallStack := ACallStack;
end;

function TInterpreter.Interpret(DontPush: boolean = False):string;
var
  Ret:TInstanceOf;
begin
  FDontPush := DontPush;
  Ret := Visit(FTree);
  Result := '';
end;

function TInterpreter.GetLive:string;
var
  AActRec: TActivationRecord;
  AVal: TStringInstance;
begin
  AActrec := FCallStack.Peek();
  Aval := TStringInstance(AActRec.GetMember('$LIVE'));
  Result := AVal.PValue;
end;

procedure TInterpreter.VisitPlainTextEmbed(ANode: TPlainTextEmbed);
var
  AState: TAST;
begin
  for AState in Anode.PNodes do
    Visit(AState);
end;

procedure TInterpreter.VisitLiveOutput(ANode: TLiveOutput);
var
  AVal: TInstanceOf;
  AActRec: TActivationRecord;
  aLive, ASet: string;
  ALiveVal:TStringInstance;
begin
  AVal := Visit(ANode.PValue);
  AActRec := FCallStack.Peek();
  ALiveVal := TStringInstance(AActrec.GetMember('$LIVE'));
  ALiveVal.PValue := ALiveVal.PValue + AVal.AsString;
  AActRec.AddMember('$LIVE', ALiveVal);
end;

function TInterpreter.VisitLivePrint(ANode: TLivePrint):TStringInstance;
var
  AActRec: TActivationRecord;
begin
  AActRec := FCallStack.Peek();
  Result := TStringInstance(AActrec.GetMember('$LIVE'));
end;

procedure TInterpreter.VisitBreak(Anode: TBreakLoop);
begin
  FBreakSignal := True;
end;

procedure TInterpreter.VisitContinue(Anode: TContinueLoop);
begin
  FContinueSignal := True;
end;

function TInterpreter.VisitReturn(ANode: TReturnFunction):TInstanceOf;
var
  AVal:TInstanceOf;
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
  ALive := TStringInstance(AActrec.GetMember('$LIVE')).PValue;
  TStringInstance(AActrec.GetMember('$LIVE')).PValue := ALive + ANode.PValue;
end;

procedure TInterpreter.VisitInterpolation(ANode: TInterpolation);
var
  AActRec: TActivationRecord;
  ALive: string;
begin
  AActRec := FCallStack.Peek();
  ALive := AActrec.GetMember('$LIVE').AsString;
  TStringInstance(AActrec.GetMember('$LIVE')).PValue := ALive + Visit(ANode.POper).AsString;
end;

function TInterpreter.VisitFunctionDefinition(ANode: TFunctionDefinition):TInstanceOf;
var
  i, len: integer;
  AValue: TFunctionInstance;
  AActRec: TActivationRecord;
  ABlock: TAST;
begin
  logtext('INTER', 'Interpreter', 'Visiting function definition');
  AActRec := FCallStack.Peek;
  AValue := TFunctionInstance.Create(ANode.PName, ANode.PParamList, ANode.PBlock, ANode.ptype);
	AActrec.AddMember(ANode.PName, AValue);
  Result := TInstanceOf.Create;
end;

procedure TInterpreter.VisitConditional(ANode: TConditional);
var
  ACondition: TAST;
  Return: TBooleanInstance;
begin
  for ACondition in ANode.PConditions do
  begin
    Return := VisitIfCondition(TIfConditionBlock(ACondition));
    if Return.PValue then
      break;
  end;
end;

function TInterpreter.VisitIfCondition(ANode: TIfConditionBlock): TBooleanInstance;
var
  AEval: TBooleanInstance = nil;
  AState: TAST;
begin
  if ANode.PCondition <> nil then
  begin
    AEval := TBooleanInstance(Visit(ANode.PCondition));
    if AEval.PValue or (ANode.PCondition = nil) then
    begin
      for AState in ANode.PBLock do
      begin
        if FBreakSignal or FContinueSignal then
          break
        else if FReturnSignal then
          break
				else
          Visit(AState);
      end;
    end;
  end
  else
  begin
    AEval := TBooleanInstance.Create(False);
    for AState in ANode.PBLock do
    begin
      if FBreakSignal or FContinueSignal then
      begin
        break
      end
      else if FReturnSignal then
          break
      else
        Visit(AState);
    end;
  end;
  Result := AEval;
end;

procedure TInterpreter.VisitWhileLoop(ANode: TWhileLoop);
var
  AState:TAST;
begin
  while TBooleanInstance(Visit(ANode.PCondition)).PValue do
  begin
    for AState in ANode.PBLock do
    begin
      if FBreakSignal or FContinueSignal then
      begin
        FContinueSignal := False;
        break
      end
      else
        Visit(AState);
    end;
    if FBreakSignal then
      break;
  end;
  FBreakSignal := False;
end;

procedure TInterpreter.VisitForLoop(ANode: TForLoop);
var
  AState:TAST;
  AList,AVar: TAST;
  AInst, AListRes: TInstanceOf;
  AInt, AIndex: TIntegerInstance;
  AActRec: TActivationRecord;
  i: integer = 0;
  len: integer;
  ACandidate: TListInstance;
  AStr:string;
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
       (StrToInt(AListRes.AsString) > -1 )
  then
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
      ACandidate.Free;
      raise ERunTimeError.Create(AListRes.ClassName + ' is not iterable')
    end;
  end
  else
    raise ERunTimeError.Create(AListRes.ClassName + ' is not iterable');
  for AInst in ACandidate.PValue do
  begin
    AActRec.AddMember(Anode.PVar.PVarName.PValue, AInst);
    AIndex := TIntegerInstance.Create(i);
    AActRec.AddMember('_'+Anode.PVar.PVarName.PValue, AIndex);
    for AState in ANode.PBlock do
    begin
      if FBreakSignal or FContinueSignal then
      begin
        AIndex.Free;
        FContinueSignal := False;
        break
      end
      else
        Visit(AState);
		end;
		i := 1 + i;
    if FBreakSignal then
    begin
      AIndex.Free;
      break;
    end;
    AIndex.Free;
  end;
  for i:=0 to ACandidate.Count - 1 do
  begin
    ACandidate.PValue[i].Free;
  end;
  ACandidate.Free;
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
  AActrec := FCallStack.Peek;
  AActRec.AddMember(AName, AValue);
end;

function TInterpreter.VisitVariableReference(ANode: TVariableReference):TInstanceOf;
var
  AName:string;
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
    raise ERunTimeError.Create('Referenced variable "'+Aname+'" does not exist');
	LogText(INTER, 'Interpreter', 'Getting value of "'+ ANode.PToken.PValue+'" from type '+Ret.ClassName);
  Result := Ret;
end;

function TInterpreter.VisitMethodCall(ANode: TMethodCall):TInstanceOf;
begin
  LogText(INTER, 'Interpreter', 'Visiting method ' + ANode.PToken.PValue);
  Result := Visit(ANode.Poper, Visit(ANode.PSrc));
end;

function TInterpreter.VisitFunctionCall(ANode: TFunctionCall; ASrcInstance: TInstanceOf = nil):TInstanceOf;
const
  ST_ACCESS = ':';
var
  ACoreExec: TCoreFunction;
  AFuncName, ASrcName, compl:string;
  AActRec: TActivationRecord;
  AParamName: string;
  AState: TAST;
  AParamState: TParam;
  FuncDef: TFunctioninstance;
  ADef: TInstanceOf = nil;
  AReturn: TInstanceOf;
  LenArgs, LenParams, Len, i: integer;
  ArgsList: TInstanceList;
begin
  AFuncName := ANode.PFuncName;
  ASrcName := AFuncName;
  if ASrcInstance <> nil then
    ASrcName := ASrcInstance.ClassName + ST_ACCESS + AFuncName;
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
    if FuncDef.PType = 'RunTimeFunction' then
    begin
      AActRec := TActivationRecord.Create(FuncDef.PName, AR_FUNCTION, FCallStack.PLevel+1);
      AActRec.AddMember('$LIVE', TStringInstance.Create(''));
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
              raise EArgumentsError.Create('Wrong number of arguments to call this function');
          end
          else if i > (LenParams - 1) then
          begin
            raise EArgumentsError.Create('Wrong number of arguments to call this function');
          end
          else
            ADef := Visit(ANode.PEvalParams[i]);
          AParamName := TParam(FuncDef.PParams[i]).PNode.PValue;
          AActRec.AddMember(AParamName, ADef);
        end;
      end;
      FCallStack.Push(AActRec);
      for AState in FuncDef.PBlock do
      begin
        if FReturnSignal then
        begin
          FReturnSignal := False;
          FCallStack.Pop();
		      Result := FReturnValue;
          exit
        end
        else
          Visit(AState);
      end;
      AReturn := AActRec.GetMember('$LIVE');
      FCallStack.Pop();
      Result := AReturn;
      exit
    end
    else
    begin

      ACoreExec := TCoreFunction.Create;
      Len := 0;
      SetLength(ArgsList, 0);
      for AState in ANode.PEvalParams do
      begin
        Len := Len + 1;
        SetLength(ArgsList, len);
        ArgsList[len - 1] := Visit(AState, ASrcInstance);
      end;
      AReturn := ACoreExec.Execute(Self, AFuncName, ArgsList, ASrcInstance);
      ACoreExec.Free;
      Result := AReturn;
      exit
    end;
  end
  else
  begin

    raise ERunTimeError.Create('Referenced function "' + AFuncName + '" does not exist.');
  end;
  // end of new
end;


procedure TInterpreter.VisitProgram(ANode: TProgram);
var
  AChild: TAST;
  AActRec: TActivationRecord;
begin
  LogText(INTER, 'Interpreter', 'Visiting a program');
  AActRec := TActivationRecord.Create('PROGRAM', AR_PROGRAM, 1);
  AActRec.AddMember('$LIVE', TStringInstance.Create(''));
  if not FDontPush then
  begin
    FCallStack.Push(AActRec);
    BootStrapRegister;
  end;

  for AChild in ANode.PChildren do
  begin
    LogText(INTER, 'Interpreter', 'Visiting a child ' + AChild.ToString);
    Visit(AChild);
  end;
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
begin
  AFileName := TStringInstance(Visit(ANode.PFilename)).PValue;
  ALexer := TLexer.Create(AFileName);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode;
  AInter := TInterpreter.Create(ATree);
  AInter.PassCallStack(FCallStack);
  AInter.Interpret(True);

  AInter.Free;
end;

function TInterpreter.Visit(ANode:TAST; ASrcInstance: TInstanceOf = nil):TInstanceOf;
var
  AuxBool:boolean;
begin
  {$INCLUDE 'interpreter_node_switcher.pp'}
end;

function TInterpreter.VisitUnaryOp(ANode: TUnaryOp):TFloatInstance;
var
  AOper:string;
  Ret: TFloatInstance;
begin
  try
    Ret := TFloatinstance(Visit(ANode.PExpr));
    AOper := ANode.POper.PType;
		if AOper = T_PLUS then
		  Ret.PValue := Ret.PValue
		else if AOper = T_MINUS then
		begin
		  Ret.PValue := Ret.PValue * (-1);
		end;
    Result := ret;
  except
    raise ERunTimeError.Create('Invalid operation for class ' + Ret.ClassName);
	end;
end;

function TInterpreter.VisitUnaryOp(ANode: TUnaryOp):TIntegerInstance;
var
  AOper:string;
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
    raise ERunTimeError.Create('Invalid operation for class ' + Ret.ClassName);
	end;
end;

function TInterpreter.VisitUnaryLogicOp(ANode: TUnaryLogicOp):TBooleanInstance;
var
  AOper:string;
  AuxExt:boolean;
  ARes: TBooleanInstance;
begin
  try
    ARes := TBooleanInstance(Visit(ANode.PExpr));
    ARes.PValue := not ARes.PValue;
    Result := ARes;

	except
    raise ERunTimeError.Create('Invalid operation for class '+ARes.ClassName);
	end;
end;

function TInterpreter.VisitBinLogicOp(ANode: TBinLogicOp): TBooleanInstance;
var
  LeftExt, RightExt:Extended;
  LeftInt, RightInt:integer;
  LeftBool, RightBool:boolean;
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
          raise ERunTimeError.Create('Logic operation '+ANode.POper.PType+' forbidden for type '+LeftClass);
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
        raise ERunTimeError.Create('Can''t compare instances of type '+LeftClass);
		end
    else
      raise ERunTimeError.Create('Can''t compare different types '+LeftClass+' and '+RightClass+' implicitly.');
  except
	end;
end;

function TInterpreter.VisitBinOp(ANode:TBinOp): TInstanceOf;
var
  LeftExt, RightExt, ResExt:Extended;
  LeftInt, RightInt:integer;
  LeftBool, RightBool, Leftnum, RightNum:boolean;
  LeftStr, RightStr: string;
  LeftClass, RightClass: string;
  AResl, AResR: TInstanceOf;
  StrRes:string;
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
      raise ERunTimeError.Create('Invalid operation '+Anode.POper.PType+' between strings');
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
    raise ERunTimeError.Create('Invalid operation '+ ANode.POper.ptype + ' for type ' + LeftClass)
	else
    raise ERunTimeError.Create('Can''t perform opertions between different types '+LeftClass+' and '+RightClass+' implicitly.');

end;

function TInterpreter.VisitNumFloat(ANode:TNumFloat):TFloatInstance;
begin

  Result := TFloatInstance.Create(ANode.PValue.ToExtended);
end;

function TInterpreter.VisitNull(ANode:TNull):TNullInstance;
begin

  Result := TNullInstance.Create;
end;

function TInterpreter.VisitNumInt(ANode:TNumInt):TIntegerInstance;
begin

  Result := TIntegerInstance.Create(ANode.PValue.ToInteger);
end;

function TInterpreter.VisitString(ANode:TString):TStringInstance;
begin

  Result := TStringInstance.Create(ANode.PValue);
end;

function TInterpreter.VisitList(ANode: TListAST): TListInstance;
var
  AnItem: TAST;
  AList: TInstanceList;
  len: integer = 0;
begin
  SetLength(AList, 0);
  for AnItem in Anode.PArgs do
  begin                             
    len := len + 1;
    SetLength(AList, len);
    AList[len - 1] := Visit(AnItem);
  end;
  Result := TListInstance.Create(AList);
end;

function TInterpreter.VisitListAccess(ANode: TListAccessAST):TInstanceOf;
var
  AList: TListInstance;
  AStr: TStringInstance;
  AIndex: TIntegerInstance;
  AVarRef: TVariableReference;
  ARet, ASrc: TInstanceOf;
begin
  AIndex := TIntegerInstance(Visit(ANode.PIndex));
  ASrc := Visit(Anode.PList);
  if ASrc.ClassNameIs('TListInstance') then
    ARet := TListInstance(ASrc).GetItem(AIndex)
  else if ASrc.ClassNameIs('TStringInstance') then
    ARet := TStringInstance(ASrc).GetChar(AIndex)
  else
    raise ERunTimeError.Create('Foridden type for indexing as list');
  Result := ARet;
end;

function Tinterpreter.VisitBoolean(ANode:TBoolean):TBooleanInstance;
begin
  if (ANode.PValue = T_LANG_TRUE) then
    Result := TBooleanInstance.Create(True)
  else if (ANode.PValue = T_LANG_FALSE) then
    Result := TBooleanInstance.Create(False);

end;

end.

