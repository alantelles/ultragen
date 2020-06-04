unit SymbolTableCreatorClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, ASTClass,
      BinOpClass, NumClass, UnaryOpClass, BinLogicOpClass, UnaryLogicopClass,
      Tokens, ImpParserClass, StrUtils, LoggingClass,
      SymbolTableClass, SymbolsClass, CoreFunctionsClass;

type
  TSymbolTableBuilder = class
    private
      FCore:TCoreFunction;
      FCurrentScope: TSymbolTable;
      FTree: TAST;
    public
      FParser: TTParser;

      property PCurrentScope:TSymbolTable read FCurrentScope;
      // constructor Create(AParser:TTParser);
      constructor Create(var ATree: TAST);
      function Visit(ANode:TAST):Extended;
      procedure VisitProgram(ANode: TProgram);
      procedure VisitFunctionDefinition(ANode: TFunctionDefinition);
      procedure VisitNoOp(ANode: TNoOp);
      procedure VisitFunctionCall(ANode: TFunctionCall);
      function VisitVarAssign(ANode: TVarAssign):extended;
      procedure VisitVariableReference(ANode: TVariableReference);
      function VisitBinOp(ANode:TBinOp):Extended;
      function VisitBinLogicOp(ANode: TBinLogicOp):boolean;
      function VisitNum(ANode:TNumFloat):Extended;
      function VisitUnaryOp(ANode: TUnaryOp):Extended;
      function VisitUnaryLogicOp(ANode: TUnaryLogicOp):boolean;
      function VisitString(ANode: TString):Extended;
      function Interpret:string;

	end;

implementation

uses
  Math, ExceptionsClasses;


{constructor TSymbolTableBuilder.Create(AParser:TTParser);
begin
  FCurrentScope := nil;
  FParser := AParser;
end;}

constructor TSymbolTableBuilder.Create(var ATree: TAST);
begin
  FCore := TCoreFunction.Create;
  Visit(ATree);
end;

function TSymbolTableBuilder.Interpret:string;
var
  ATree:TAST;
  Ret:Extended;
begin
  ATree := FParser.ParseCode;
  Ret := Visit(ATree);
  Result := FloatToStr(Ret);
end;

procedure TSymbolTableBuilder.VisitFunctionDefinition(ANode: TFunctionDefinition);
var
  AIter: TAST;
  AVarNode: TVarAssign;
  ASymbolTable: TSymbolTable;
  AProcSymbol: TProcedureSymbol;
  AVar: TVariableSymbol;
  AParam: TAST;
begin
  LogDebug('Visiting function definition', 'SymbolTableBuilder');
  AProcSymbol := TProcedureSymbol.Create(ANode.PName, ANode.PParamList);
  FCurrentScope.Define(AProcSymbol);
  ASymbolTable := TSymbolTable.Create(ANode.PName, FCurrentScope.PLevel + 1, FCurrentScope);
  FCurrentScope := ASymbolTable;
  logdebug('Entering scope ---->'+ uppercase(ASymbolTable.PParentScope.PScope)+'/'+ uppercase(ASymbolTable.PScope), 'SymbolTableBuilder');
  for AIter in ANode.PParamList do
  begin
    LogDebug('Adding parameter ' +TVarAssign(AIter).PVarName.PValue+ ' to symbol table', 'SymbolTableBuilder');
    AVar := TVariableSymbol.Create(TVarAssign(AIter).PVarName.PValue);
    FCurrentScope.Define(AVar);
  end;
  for AIter in ANode.PBlock do
    visit(AIter);
  FCurrentScope := ASymbolTable.PParentScope;
  FCurrentScope.PLevel := FCurrentScope.PLevel - 1;
  logdebug('Leaving scope <----'+ uppercase(ASymbolTable.PScope), 'SymbolTableBuilder');
  // ASymbolTable.AsString;
end;

function TSymbolTableBuilder.VisitVarAssign(ANode: TVarAssign):extended;
var
  ASolve: Extended;
  AVarSym: TVariableSymbol;
begin
  LogText(DEBUG, 'SymbolTableBuilder', 'VarAssign visitation');
  AVarSym := TVariableSymbol.Create(ANode.PVarName.PValue);
  if ANode.PValue.ClassNameIs('TFunctionCall') then
    VisitFunctionCall(TFunctionCall(ANode.PValue))
  else if ANode.PValue.PToken.PType = T_ID then
    VisitVariableReference(TVariableReference(Anode.PValue))
  else
    Visit(ANode.PValue);
  if (FCurrentScope.Define(AVarSym)) then
    LogText(DEBUG, 'SymbolTableBuilder', 'Assigning var '+ ANode.PVarName.AsString +' to symbol table')
  else
    LogDebug('Variable '+ANode.PVarName.AsString+' already subscripted', 'SymbolTableBuilder');

end;

procedure TSymbolTableBuilder.VisitVariableReference(ANode: TVariableReference);

begin
  LogText(DEBUG, 'SymbolTableBuilder', 'Executing "'+ ANode.PToken.PValue +'" variable lookup');

  if FCurrentScope.Lookup(Anode.PToken.PValue) = nil then
    raise ESemanticError.Create('Trying to reference undefined variable "'+ ANode.PToken.PValue +'".');
end;

procedure TSymbolTableBuilder.VisitFunctionCall(ANode: TFunctionCall);
var
  AnArg:TAST;
  ASymb:TProcedureSymbol;
begin
  LogText(DEBUG, 'SymbolTableBuilder', 'Visiting a function call named ' + ANode.PToken.PValue);
  ASymb := TProcedureSymbol(FCurrentScope.Lookup(ANode.PToken.PValue));
  {for AnArg in Anode.PEvalParams do
    logdebug('Showing arg '+AnArg.PToken.PValue+' from '+ANode.PToken.PValue, 'SymbolTableBuilder');}
  if ANode.ClassNameIs('TFunctionCall') and  FCore.FunctionExists(ANode.PToken.PValue) then
  begin

	end
  else if ANode.ClassNameIs('TVarAssign') and  FCore.FunctionExists(ANode.PToken.PValue) then
  begin

	end
	else if ASymb = nil then
    raise ESemanticError.Create(ANode.PToken.PValue + ' function doesn''t exists')
  else
  begin
  	if Length(ASymb.PParams) <> Length(ANode.PEvalParams) then
      raise ESemanticError.Create('Wrong number of parameters for calling '+ANode.PToken.PValue);
    for AnArg in ANode.PEvalParams do
      Visit(AnArg);
	end;


end;

procedure TSymbolTableBuilder.VisitProgram(ANode: TProgram);
var
  AChild: TAST;
  ASymbolTable: TSymbolTable;
begin
  LogText(DEBUG, 'SymbolTableBuilder', 'Visiting a program');
  ASymbolTable := TSymbolTable.Create('GLOBAL', 1, FCurrentScope);
  logdebug('Entering ---->' + uppercase(ASymbolTable.PScope), 'SymbolTableBuilder');
  FCurrentScope := ASymbolTable;
  for AChild in ANode.PChildren do
  begin
    LogText(DEBUG, 'SymbolTableBuilder', 'Visiting a child ' + AChild.ToString);
    Visit(AChild);
  end;
  FCurrentScope := FCurrentScope.PParentScope;
  logdebug('Leaving <----' + ASymbolTable.PScope, 'SymbolTableBuilder');
  // ASymbolTable.AsString;
end;

procedure TSymbolTableBuilder.VisitNoOp(ANode: TNoOp);
begin

end;

function TSymbolTableBuilder.Visit(ANode:TAST):Extended;
var
  AuxBool:boolean;
begin
  if ANode.ClassNameIs('TProgram') then
  begin
    VisitProgram(TProgram(ANode));
    Result := 1;
  end
  else if ANode.ClassNameIs('TString') then
  begin
    Result := VisitString(TString(ANode));
  end
  else if ANode.ClassNameIs('TFunctionCall') then
  begin
    VisitFunctionCall(TFunctionCall(ANode));
    Result := 1;
  end
  else if Anode.ClassNameIs('TFunctionDefinition')  then
  begin
    VisitFunctionDefinition(TFunctionDefinition(ANode));
    Result := 1;
  end
  else if ANode.ClassNameIs('TVarAssign') then
    Result := VisitVarAssign(TVarAssign(ANode))
  else if ANode.ClassNameIs('TVariableReference') then
  begin
    VisitVariableReference(TVariableReference(Anode));
    Result := 1;
	end;
	{else if Anode.ClassNameIs('TNum') then
    Result := VisitNum(TNumFloat(ANode))
  else if ANode.ClassNameIs('TUnaryOp') then
    Result := VisitUnaryOp(TUnaryOp(ANode))
  else if ANode.ClassNameIs('TBinOp') then
    Result := VisitBinOp(TBinOp(ANode))
  else if ANode.ClassNameIs('TBinLogicOp') then
  begin
    AuxBool := VisitBinLogicOp(TBinLogicOp(ANode));
    if AuxBool then
      Result := 1
    else
      Result := 0;
  end
  else if ANode.ClassNameIs('TUnaryLogicOp') then
  begin
    AuxBool := VisitUnaryLogicOp(TUnaryLogicOp(ANode));
    if AuxBool then
      Result := 1
    else
      Result := 0;
  end;}
end;

function TSymbolTableBuilder.VisitString(ANode: TString):Extended;
begin
  logdebug('Visiting string node', 'SymbolTableClass');
  Result := 1;
end;

function TSymbolTableBuilder.VisitUnaryOp(ANode: TUnaryOp):Extended;
var
  AOper:string;
begin

  AOper := ANode.POper.PType;
  if AOper = T_PLUS then
    Result := Visit(ANode.PExpr)
  else if AOper = T_MINUS then
    Result := Visit(ANode.PExpr) * (-1);
end;

function TSymbolTableBuilder.VisitUnaryLogicOp(ANode: TUnaryLogicOp):boolean;
var
  AOper:string;
  AuxExt:Extended;
begin

  AuxExt := Visit(ANode.PExpr) * (-1);
  if AuxExt >= 0 then
    Result := True
  else
    Result := False;
end;

function TSymbolTableBuilder.VisitBinLogicOp(ANode: TBinLogicOp): boolean;
var
  LeftRes, RightRes:Extended;
  AuxBool, LeftBool, RightBool:boolean;
begin

  if Anode.ClassNameIs('TNumFloat') then
    LeftRes := VisitNum(TNumFloat(ANode))
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
    RightRes := VisitNum(TNumFloat(ANode.PRight))
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

function TSymbolTableBuilder.VisitBinOp(ANode:TBinOp): Extended;
var
  LeftRes, RightRes:Extended;
  AuxBool: boolean;
begin

  if ANode.PLeft.ClassNameIs('TNum') then
    LeftRes := VisitNum(TNumFloat(ANode.PLeft))
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
    RightRes := VisitNum(TNumFloat(ANode.PRight))
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

function TSymbolTableBuilder.VisitNum(ANode:TNumFloat):Extended;
begin

  Result := ANode.PValue.ToExtended;
end;


end.

