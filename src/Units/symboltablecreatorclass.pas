unit SymbolTableCreatorClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, ASTClass,
      BinOpClass, NumClass, UnaryOpClass, BinLogicOpClass, UnaryLogicopClass,
      FlowControlASTClass,

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
      procedure Visit(ANode:TAST);
      procedure VisitProgram(ANode: TProgram);
      procedure VisitFunctionDefinition(ANode: TFunctionDefinition);
      procedure VisitNoOp(ANode: TNoOp);
      procedure VisitFunctionCall(ANode: TFunctionCall);
      procedure VisitVarAssign(ANode: TVarAssign);
      procedure VisitVariableReference(ANode: TVariableReference);

      procedure VisitListAccess(ANode: TListAccessAST);
      procedure VisitList(ANode: TListAST);
      procedure VisitConditional(ANode: TConditional);
      procedure VisitIfCondition(ANode: TIfConditionBlock);
      procedure VisitForLoop(ANode:TForLoop);

      procedure VisitWhileLoop(ANode: TWhileLoop);

      procedure Interpret;

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

procedure TSymbolTableBuilder.Interpret;
var
  ATree:TAST;
  Ret:Extended;
begin
  ATree := FParser.ParseCode;
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

procedure TSymbolTableBuilder.VisitVarAssign(ANode: TVarAssign);
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
    for AnArg in ANode.PEvalParams do
      Visit(AnArg);
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

procedure TSymbolTableBuilder.Visit(ANode:TAST);
var
  AuxBool:boolean;
begin
  if ANode.ClassNameIs('TProgram') then
  begin
    VisitProgram(TProgram(ANode));
  end
  else if ANode.ClassNameIs('TVarAssign') then
    VisitVarAssign(TVarAssign(ANode))
  else if ANode.ClassNameIs('TVariableReference') then
    VisitVariableReference(TVariableReference(ANode))
  else if ANode.ClassNameIs('TFunctionCall') then
    VisitFunctionCall(TFunctionCall(Anode))
  else if Anode.ClassNameIs('TListAST') then
    VisitList(TListAST(ANode))
  else if Anode.ClassNameIs('TListAccessAST') then
    VisitListAccess(TListAccessAST(ANode))
  else if ANode.ClassNameIs('TConditional') then
    VisitConditional(TConditional(ANode))
  else if ANode.ClassNameIs('TIfConditionBlock') then
    VisitIfCondition(TIfConditionBlock(ANode))
  else if ANode.ClassNameIs('TWhileLoop') then
    VisitWhileLoop(TWhileLoop(ANode))
  else if ANode.ClassNameIs('TForLoop') then
    VisitForLoop(TForLoop(ANode));
end;

procedure TSymbolTableBuilder.VisitListAccess(ANode: TListAccessAST);
begin

end;

procedure TSymbolTableBuilder.VisitList(ANode: TListAST);
begin

end;
procedure TSymbolTableBuilder.VisitConditional(ANode: TConditional);
begin

end;
procedure TSymbolTableBuilder.VisitIfCondition(ANode: TIfConditionBlock);
begin

end;
procedure TSymbolTableBuilder.VisitForLoop(ANode:TForLoop);
begin

end;

procedure TSymbolTableBuilder.VisitWhileLoop(ANode: TWhileLoop);
begin

end;


end.

