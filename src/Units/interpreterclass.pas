unit InterpreterClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, ASTClass,
      BinOpClass, NumClass, UnaryOpClass, BinLogicOpClass, UnaryLogicopClass,
      Tokens, ImpParserClass, StrUtils, LoggingClass,
      StackClass, ARClass, InstanceofClass;

type
  TInterpreter = class
    private
      FTree: TAST;
      FCallStack: TStack;
    public
      property PTree:TAST read FTree;
      constructor Create(var ATree: TAST);
      function Visit(ANode:TAST):TInstanceOf;
      procedure VisitProgram(ANode: TProgram);
      procedure VisitNoOp(ANode: TNoOp);
      procedure VisitVarAssign(ANode: TVarAssign);
      function VisitBinOp(ANode:TBinOp):TInstanceOf;
      function VisitBinLogicOp(ANode: TBinLogicOp): TInstanceOf;
      function VisitNum(ANode:TNum):TInstanceOf;
      function VisitUnaryOp(ANode: TUnaryOp):TInstanceOf;
      function VisitUnaryLogicOp(ANode: TUnaryLogicOp):TInstanceOf;
      function Interpret:string;

	end;

implementation

uses
  Math;


constructor TInterpreter.Create(var ATree: TAST);
begin
  FTree := ATree;
  FCallStack := TStack.Create;
end;

function TInterpreter.Interpret:string;
var
  Ret:TInstanceOf;
begin
  Ret := Visit(FTree);
  Result := '';
end;

procedure TInterpreter.VisitVarAssign(ANode: TVarAssign);
var
  ASolve: Extended;
  AName: string;

begin
  LogText(INTER, 'Interpreter', 'VarAssign visitation');
  //LogText(DEBUG, 'Interpreter', 'Assigning var '+ ANode.PLeft.PToken.AsString +' to symbol table');
  //ASolve := visit(ANode.PRight);
  // FGlobals.Add(ANode.PLeft.PToken.PValue+'='+FloatToStr(ASolve));
  //LogText(DEBUG, 'Interpreter', 'Added value :'+ FGlobals.Values[ANode.PLeft.PToken.PValue]);
end;

procedure TInterpreter.VisitProgram(ANode: TProgram);
var
  AChild: TAST;
  AActRec: TActivationRecord;
begin
  LogText(INTER, 'Interpreter', 'Visiting a program');
  AActRec := TActivationRecord.Create('PROGRAM', AR_PROGRAM, 1);
  FCallStack.Push(AActRec);
  for AChild in ANode.PChildren do
  begin
    LogText(INTER, 'Interpreter', 'Visiting a child ' + AChild.ToString);
    Visit(AChild);
  end;
  FCallStack.Pop();
end;

procedure TInterpreter.VisitNoOp(ANode: TNoOp);
begin

end;

function TInterpreter.Visit(ANode:TAST):TInstanceOf;
var
  AuxBool:boolean;
begin
  if ANode.ClassNameIs('TProgram') then
  begin
    VisitProgram(TProgram(ANode));
  end
  else if ANode.ClassNameIs('TVarAssign') then
    VisitVarAssign(TVarAssign(ANode))
  else if Anode.ClassNameIs('TNum') then
    Result := VisitNum(TNum(ANode))
  else if ANode.ClassNameIs('TUnaryOp') then
    Result := VisitUnaryOp(TUnaryOp(ANode))
  else if ANode.ClassNameIs('TBinOp') then
    Result := VisitBinOp(TBinOp(ANode))
  else if ANode.ClassNameIs('TBinLogicOp') then
  begin
    Result := VisitBinLogicOp(TBinLogicOp(ANode));
  end
  else if ANode.ClassNameIs('TUnaryLogicOp') then
  begin
    Result := VisitUnaryLogicOp(TUnaryLogicOp(ANode));
  end;
end;

function TInterpreter.VisitUnaryOp(ANode: TUnaryOp):TInstanceOf;
var
  AOper:string;
  Ret: TFloatInstance;
begin
  Ret := TFloatInstance(Visit(ANode.PExpr));
  AOper := ANode.POper.PType;
  if AOper = T_PLUS then
    Result := Ret
  else if AOper = T_MINUS then
  begin
    Ret.PValue := Ret.PValue * (-1);
    Result := Ret;
  end;
end;

function TInterpreter.VisitUnaryLogicOp(ANode: TUnaryLogicOp):TInstanceOf;
var
  AOper:string;
  AuxExt:Extended;
begin
  {todo}
  AuxExt := Visit(ANode.PExpr) * (-1);
  if AuxExt >= 0 then
    Result := True
  else
    Result := False;
end;

function TInterpreter.VisitBinLogicOp(ANode: TBinLogicOp): boolean;
var
  LeftRes, RightRes:Extended;
  AuxBool, LeftBool, RightBool:boolean;
begin

  if ANode.PLeft.ClassNameIs('TNum') then
    LeftRes := VisitNum(TNum(ANode.PLeft))
  else if ANode.PLeft.ClassNameIs('TUnaryOp') then
    LeftRes := VisitUnaryOp(TUnaryOp(ANode.PLeft))
  else if ANode.PLeft.ClassNameIs('TBinOp') then
    LeftRes := VisitBinOp(TBinOp(ANode.PLeft))
  else if ANode.PLeft.ClassNameIs('TBinLogicOp') then
  begin
    AuxBool := VisitBinLogicOp(TBinLogicOp(ANode.PLeft));
    if AuxBool then
      LeftRes := 1
    else
      LeftRes := 0;
  end
  else if ANode.PLeft.ClassNameIs('TUnaryLogicOp') then
  begin
    AuxBool := VisitUnaryLogicOp(TUnaryLogicOp(ANode.PLeft));
    if AuxBool then
      LeftRes := 1
    else
      LeftRes := 0;
  end;

  if ANode.PRight.ClassNameIs('TNum') then
    RightRes := VisitNum(TNum(ANode.PRight))
  else if ANode.PRight.ClassNameIs('TUnaryOp') then
    RightRes := VisitUnaryOp(TUnaryOp(ANode.PRight))
  else if ANode.PRight.ClassNameIs('TBinOp') then
    RightRes := VisitBinOp(TBinOp(ANode.PRight))
  else if ANode.PRight.ClassNameIs('TBinLogicOp') then
  begin
    AuxBool := VisitBinLogicOp(TBinLogicOp(ANode.PRight));
    if AuxBool then
      RightRes := 1
    else
      RightRes := 0;
  end
  else if ANode.PRight.ClassNameIs('TUnaryLogicOp') then
  begin
    AuxBool := VisitUnaryLogicOp(TUnaryLogicOp(ANode.PRight));
    if AuxBool then
      RightRes := 1
    else
      RightRes := 0;
  end;

  LeftBool := LeftRes > 0;
  RightBool := RightRes > 0;

  if (ANode.POper.PType = T_GT) then
    Result := LeftRes > RightRes
  else if (ANode.POper.PType = T_LT) then
    Result := LeftRes < RightRes
  else if (ANode.POper.PType = T_EQ) then
    Result := LeftRes = RightRes
  else if (ANode.POper.PType = T_LEQ) then
    Result := LeftRes <= RightRes
  else if (ANode.POper.PType = T_GEQ) then
    Result := LeftRes >= RightRes
  else if (ANode.POper.PType = T_NEQ) then
    Result := LeftRes <> RightRes
  else if (ANode.POper.PType = T_AND) then
    Result := LeftBool and RightBool
  else if (ANode.POper.PType = T_OR) then
    Result := LeftBool or RightBool;
end;

function TInterpreter.VisitBinOp(ANode:TBinOp): Extended;
var
  LeftRes, RightRes:Extended;
  AuxBool: boolean;
begin

  if ANode.PLeft.ClassNameIs('TNum') then
    LeftRes := VisitNum(TNum(ANode.PLeft))
  else if ANode.PLeft.ClassNameIs('TUnaryOp') then
    LeftRes := VisitUnaryOp(TUnaryOp(ANode.PLeft))
  else if ANode.PLeft.ClassNameIs('TBinOp') then
    LeftRes := VisitBinOp(TBinOp(ANode.PLeft))
  else if ANode.PLeft.ClassNameIs('TBinLogicOp') then
  begin
    AuxBool := VisitBinLogicOp(TBinLogicOp(ANode.PLeft));
    if AuxBool then
      LeftRes := 1
    else
      LeftRes := 0;
  end;

  if ANode.PRight.ClassNameIs('TNum') then
    RightRes := VisitNum(TNum(ANode.PRight))
  else if ANode.PRight.ClassNameIs('TUnaryOp') then
    RightRes := VisitUnaryOp(TUnaryOp(ANode.PRight))
  else if ANode.PRight.ClassNameIs('TBinOp') then
    RightRes := VisitBinOp(TBinOp(ANode.PRight))
  else if ANode.PRight.ClassNameIs('TBinLogicOp') then
  begin
    AuxBool := VisitBinLogicOp(TBinLogicOp(ANode.PRight));
    if AuxBool then
      RightRes := 1
    else
      RightRes := 0;
  end;

  if (ANode.POper.PType = T_PLUS) then
    Result := LeftRes + RightRes
  else if (ANode.POper.PType = T_MINUS) then
    Result := LeftRes - RightRes
  else if (ANode.POper.PType = T_MULT) then
    Result := LeftRes * RightRes
  else if (ANode.POper.PType = T_INT_DIV) then
    Result := Floor(LeftRes / RightRes)
  else if (ANode.POper.PType = T_MODULUS) then
    Result := Floor(LeftRes) mod Floor(RightRes)
  else if (ANode.POper.PType = T_DIV) then
    Result := LeftRes / RightRes;
end;

function TInterpreter.VisitNum(ANode:TNum):Extended;
begin

  Result := ANode.PValue.ToExtended;
end;


end.

