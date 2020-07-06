unit ImpParserClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TokenClass, Tokens,
  LexerClass, ASTClass, LoggingClass,
  BinOpClass, NumClass, UnaryOpClass,
  FlowControlASTClass,
  BinLogicOpClass, UnaryLogicOpClass;

type
  TTParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    FInArgsDef: boolean;
  public
    constructor Create(var ALexer: TLexer);
    procedure EParseError;
    procedure Eat(AType: string);
    function ParseCode: TAST;

    // rules
    function LogicEval: TAST;
    function LogicExpr: TAST;
    function Expr: TAST;
    function Term: TAST;
    function Factor: TAST;
    function SourceProgram: TAST;
    function Statements: TASTList;
    function Statement: TAST;
    function VarAssign(AToken: TToken): TAST;
    function VarAssign(AToken: TToken; ANull: TAST): TAST;
    function Variable(AToken: TToken): TAST;
    function FunctionBlock: TAST;
    function DefParams: TASTList;
    function DefParam: TAST;
    function FunctionCall(AToken: TToken): TAST;
    function MethodCall: TAST;
    function Args: TASTList;
    function ListArgs: TASTList;
    function IfBlock: TAST;
    function ElseBlock: TAST;
    function Conditional: TAST;
    function WhileLoop: TAST;
    function ForLoop: TAST;
    function List: TAST;
    function ListAccess: TAST;
    function LiveOutput: TAST;
    function Interpolated: TAST;
    function PlainTextEmbed: TAST;
    function IncludeScript: TAST;

  end;

implementation

uses
  ExceptionsClasses;

constructor TTParser.Create(var ALexer: TLexer);
begin
  FLexer := ALexer;
  FCurrentToken := ALexer.GetNextToken;
end;

function TTParser.IncludeScript: TAST;
var
  AFileName: TAST;
  ANamespace: string = '';
begin
  Eat(T_INCLUDE);
  AFileName := MethodCall();
  if FCurrentToken.PType = T_NAMESPACE then
  begin
    Eat(T_NAMESPACE);
    ANamespace := FCurrentToken.PValue;
    Eat(T_ID);
  end;
  Result := TIncludeScript.Create(AFileName, FCurrentToken, ANamespace);
end;

function TTParser.FunctionBlock: TAST;
var
  AStrId: string;
  ParamList: TASTList;
  InBlock: TASTList;
  AToken: TToken;
  AType:string = '';
begin
  SetLength(ParamList, 0);
  Eat(T_FUNC_DEF);
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
  AStrId := FCurrentToken.PValue;
  Eat(T_ID);
  Eat(T_LPAREN);
  FInArgsDef := True;
  ParamList := DefParams();
  Eat(T_RPAREN);
  if FCurrentToken.PType = T_NAMESPACE then
  begin
    Eat(T_NAMESPACE);
    AType := FCurrentToken.PValue;
    Eat(T_ID);
  end;
  FInArgsDef := False;
  if FLexer.PExtension <> '.ultra' then
    FLexer.PScriptMode := False;
  Eat(T_NEWLINE);
  InBlock := Statements();
  Eat(T_END + T_FUNC_DEF);
  logtext('PARSER', 'Parser', 'Creating function block node');
  if AType <> '' then
    AStrId := AType + ':' + AStrId;
  Result := TFunctionDefinition.Create(AToken, AStrId, InBlock, ParamList, AType);
end;

function TTParser.PlainTextEmbed: TAST;
var
  Ret: TASTList;
  len: integer;
  AToken: TToken;
begin
  SetLength(Ret, 0);
  len := 0;
  AToken := TToken.Create;
  while (FCurrentToken.PType <> T_NEWLINE) and (FCurrentToken.PType <> EOF) do
  begin
    AToken.SetValue(FCurrentToken.PType, FCurrentToken.PValue);
    len := len + 1;
    SetLength(Ret, len);
    if FCurrentToken.PType = T_PLAIN_TEXT then
    begin
      Ret[len - 1] := TPlaintext.Create(AToken.PValue, AToken);
      Eat(T_PLAIN_TEXT);
    end
    else if FCurrentToken.PType = T_INTERPOLATION_START then
    begin
      Ret[len - 1] := Interpolated();
    end;
  end;
  {len := len + 1;
  SetLength(Ret, Len);
  Ret[len - 1] := TLiveOutput.Create(TString.Create(TToken.Create(TYPE_STRING, sLineBreak)), FCurrentToken);}
  Result := TPlainTextEmbed.Create(Ret, AToken);
end;

function TTParser.ElseBlock: TAST;
begin
  Eat(T_NEWLINE);
  logtext('PARSER', 'Parser', 'Creating else block node');
  Result := TIfConditionBlock.Create(Statements());
end;

function TTParser.Interpolated: TAST;
var
  AOper: TAST;
begin
  Eat(T_INTERPOLATION_START);
  AOper := MethodCall();
  Eat(T_INTERPOLATION_END);
  logtext('PARSER', 'Parser', 'Creating inter node');
  Result := TInterpolation.Create(AOper, FCurrentToken);
end;

function TTParser.LiveOutput: TAST;
begin
  Result := TLiveOutput.Create(MethodCall(), FCurrentToken);
end;

function TTParser.IfBlock: TAST;
var
  InBlock: TASTList;
  AExpr: TAST;
begin
  Eat(T_LPAREN);
  FInArgsDef := True;
  AExpr := LogicEval();
  FInArgsDef := False;
  Eat(T_RPAREN);
  if FLexer.PExtension <> '.ultra' then
    FLexer.PScriptMode := False;
  Eat(T_NEWLINE);

  logtext('PARSER', 'Parser', 'Creating if block node');
  Result := TIfConditionBlock.Create(Statements(), AExpr);
end;

function TTParser.WhileLoop: TAST;
var
  AExpr: TAST;
begin
  Eat(T_WHILE_LOOP);
  Eat(T_LPAREN);
  FInArgsDef := True;
  AExpr := LogicEval();
  FInArgsDef := False;
  Eat(T_RPAREN);
  if FLexer.PExtension <> '.ultra' then
    FLexer.PScriptMode := False;
  Eat(T_NEWLINE);
  Result := TWhileLoop.Create(Statements(), AExpr);
end;

function TTParser.ForLoop: TAST;
var
  AList: TAST;
  Avar: TAST;
  AToken: TToken;
begin
  Eat(T_FOR_LOOP);
  Eat(T_LPAREN);
  FInArgsDef := True;
  AList := MethodCall();
  Eat(T_COMMA);
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
  AVar := VarAssign(AToken, TNull.Create(TToken.Create(TYPE_NULL, T_LANG_NULL)));
  FInArgsDef := False;
  Eat(T_RPAREN);
  if FLexer.PExtension <> '.ultra' then
    FLexer.PScriptMode := False;
  Eat(T_NEWLINE);
  logtext('PARSER', 'Parser', 'Creating for node');
  Result := TForLoop.Create(Statements(), AList, TVarAssign(AVar));
end;

function TTParser.Conditional: TAST;
var
  Conditions: array of TAST;
  len: integer;
begin
  Eat(T_IF_START);
  SetLength(Conditions, 1);
  len := 1;
  //AnIfBlock := Statements();
  Conditions[0] := IfBlock();
  while FCurrentToken.PType = T_ELSE_IF do
  begin

    Eat(T_ELSE_IF);
    len := len + 1;
    SetLength(Conditions, len);
    Conditions[len - 1] := IfBlock();
  end;
  if FCurrentToken.PType = T_ELSE then
  begin
    Eat(T_ELSE);
    len := len + 1;
    SetLength(Conditions, len);
    Conditions[len - 1] := ElseBlock();
  end;
  logtext('PARSER', 'Parser', 'Creating if block node');
  Result := TConditional.Create(Conditions);
end;

function TTParser.Args: TASTList;
var
  AArgs: TASTList;
  len: integer;
begin
  // if (FCurrentToken.PType = T_NEWLINE) then
  //  Eat(T_NEWLINE);
  SetLength(AArgs, 0);
  len := 0;
  while (FCurrentToken.PType <> T_RPAREN) do
  begin
    if (FCurrentToken.PType = T_NEWLINE) then
      Eat(T_NEWLINE)
    else if (FCurrentToken.PType = T_COMMA) then
      Eat(T_COMMA);
    len := len + 1;
    SetLength(AArgs, len);
    AArgs[len - 1] := MethodCall();

    if (FCurrentToken.PType <> T_RPAREN) then
    begin
      if (FCurrentToken.PType = T_NEWLINE) then
        Eat(T_NEWLINE);
      if (FCurrentToken.PType = T_COMMA) then
        Eat(T_COMMA);
    end;
  end;
  logtext('PARSER', 'Parser', 'Creating args node');
  Result := AArgs;
end;

function TTParser.ListArgs: TASTList;
var
  AArgs: TASTList;
  len: integer;
begin
  // if (FCurrentToken.PType = T_NEWLINE) then
  //  Eat(T_NEWLINE);
  SetLength(AArgs, 0);
  len := 0;
  while (FCurrentToken.PType <> T_LIST_END) do
  begin
    if (FCurrentToken.PType = T_NEWLINE) then
      Eat(T_NEWLINE)
    else if (FCurrentToken.PType = T_COMMA) then
      Eat(T_COMMA);
    len := len + 1;
    SetLength(AArgs, len);
    AArgs[len - 1] := MethodCall();
    if (FCurrentToken.PType <> T_LIST_END) then
    begin
      if (FCurrentToken.PType = T_NEWLINE) then
        Eat(T_NEWLINE);
      if (FCurrentToken.PType = T_COMMA) then
        Eat(T_COMMA);
    end;
  end;
  logtext('PARSER', 'Parser', 'Creating list args node');
  Result := AArgs;
end;


function TTParser.ListAccess(): TAST;
var
  AIndex: TAST;
  AList: TAST;
  Ret: TAST;
begin
  Eat(T_LIST_START);
  //Ret := TListAccessAST.Create(ASrc, MethodCall(), FCurrentToken);
  Eat(T_LIST_END);
  //Ret := MethodCall();
  logtext('PARSER', 'Parser', 'Creating list access node');
  while (FCurrentToken.PType = T_LIST_START) do
  begin
    Eat(T_LIST_START);
    Ret := TListAccessAST.Create(Ret, MethodCall(), FCurrentToken);
    Eat(T_LIST_END);
  end;
  Result := Ret;
end;

function TTParser.List: TAST;
var
  Ret: TAST;
  AToken: TToken;
begin
  Eat(T_LIST_START);
  AToken := TToken.Create(FcurrentToken.PType, FCurrentToken.PValue);
  Ret := TListAST.Create(AToken, ListArgs());
  Eat(T_LIST_END);
  while FCurrentToken.PType = T_LIST_START do
  begin
    Eat(T_LIST_START);
    Ret := TListAccessAST.Create(Ret, MethodCall(), FCurrentToken);
    Eat(T_LIST_END);
  end;
  Result := Ret;
end;

function TTParser.MethodCall: TAST;
var
  Ret: TAST;
begin
  // Eat(T_ATTR_ACCESSOR);
  Ret := LogicEval();
  logtext('PARSER', 'Parser', 'Creating method call node');
  while (FCurrentToken.PType = T_ATTR_ACCESSOR) or
    (FCurrentToken.PType = T_LIST_START) do
  begin
    if (FCurrentToken.PType = T_ATTR_ACCESSOR) then
    begin
      Eat(T_ATTR_ACCESSOR);
      if FCurrentToken.ptype = T_NEWLINE then
        Eat(T_NEWLINE);
      Ret := TMethodCall.Create(Ret, Factor(), FCurrentToken);
      continue;
    end;
    if (FCurrentToken.PType = T_LIST_START) then
    begin
      Eat(T_LIST_START);
      if FCurrentToken.ptype = T_NEWLINE then
        Eat(T_NEWLINE);
      Ret := TListAccessAST.Create(Ret, MethodCall(), FCurrentToken);
      if FCurrentToken.ptype = T_NEWLINE then
        Eat(T_NEWLINE);
      Eat(T_LIST_END);
    end
  end;
  Result := Ret;
end;

function TTParser.FunctionCall(AToken: TToken): TAST;
var
  AFuncName: string;
  AArgs: TASTList;
  Ret: TAST;
begin
  AFuncName := AToken.PValue;
  Eat(T_LPAREN);
  //if (FCurrentToken.PType = T_NEWLINE) then
  //  Eat(T_NEWLINE);
  AArgs := Args();
  Eat(T_RPAREN);

  logtext('PARSER', 'Parser', 'Creating function call node');
  Result := TFunctionCall.Create(AFuncName, AArgs, AToken);
end;


function TTParser.DefParam: TAST;
var
  Ret, ADef: TAST;
  AToken: TToken;
  AVarAssign: TAST;
begin
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
  ADef := nil;
  //AVarAssign := TVarAssign.Create(AToken, nil);
  // Ret := TParam.Create(AToken);
  Eat(T_ID);
  if FCurrentToken.PType = T_ASSIGN then
  begin
    Eat(T_ASSIGN);
    ADef := MethodCall();
  end;
  Ret := TParam.Create(AToken, ADef);
  logtext('PARSER', 'Parser', 'Creating param node');
  Result := Ret;
end;

function TTParser.DefParams: TASTList;
var
  AList: TASTList;
  len: integer = 0;
begin
  SetLength(AList, 0);
  while FCurrentToken.PType <> T_RPAREN do
  begin
    if FCurrentToken.PType = T_ID then
    begin
      // suruba para T_ID
      len := len + 1;
      SetLength(AList, len);
      AList[len - 1] := DefParam();
      if FCurrentToken.PType <> T_RPAREN then
      begin
        Eat(T_COMMA);
      end;
    end;
  end;
  logtext('PARSER', 'Parser', 'Creating params node');
  Result := AList;
end;

function TTParser.Variable(AToken: TToken): TAST;
var
  ret: TAST;
  //AToken: TToken;
begin

  //AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
  Ret := TVariableReference.Create(AToken);
  // Eat(T_ID);
  Result := Ret;
end;

function TTParser.VarAssign(AToken: TToken): TAST;
var
  Aleft, ARight: TAST;
  AVar: TToken;
begin
  Eat(T_ASSIGN);
  ARight := MethodCall();
  Logdebug('Creating a VarAssign to ' + AToken.AsString, 'Parser');
  logtext('PARSER', 'Parser', 'Creating var assign node to ' + AToken.PValue);
  Result := TVarAssign.Create(AToken, ARight);
end;

function TTParser.VarAssign(AToken: TToken; Anull: TAST): TAST;
var
  Aleft, ARight: TAST;
  AVar: TToken;
begin
  Eat(T_ID);
  ARight := ANull;
  Logdebug('Creating a VarAssign to ' + AToken.AsString, 'Parser');
  logtext('PARSER', 'Parser', 'Creating var assign node');
  Result := TVarAssign.Create(AToken, ARight);
end;

function TTParser.Statement: TAST;
var
  AToken: TToken;
  AStrId: string;
  Ret: TAST;
begin

  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
  logtext('PARSER', 'Parser', 'Creating statement node');
  if (AToken.PType = T_ID) then
  begin
    Eat(T_ID);
    if (FCurrentToken.PType = T_ASSIGN) then
    begin
      Ret := VarAssign(AToken);
    end
    else if (FCurrentToken.PType = T_LPAREN) then
    begin
      Ret := FunctionCall(AToken);
    end
    else
      EParseError;
  end
  else if (ATOken.PType = T_INCLUDE) then
  begin
    Logtext('PARSER', 'Parser', 'Creating include node');
    Ret := IncludeScript();
  end
  else if (AToken.PType = T_PLAIN_TEXT) then
  begin
    LogText('PARSER', 'Parser', 'Creating plaintext node');
    Ret := PlainTextEmbed();
  end
  else if AToken.PType = T_INTERPOLATION_START then
  begin
    LogText('PARSER', 'Parser', 'Creating interpol node');
    Ret := PlainTextEmbed();
  end
  else if AToken.PType = T_LIVE_OUTPUT then
  begin
    Eat(T_LIVE_OUTPUT);
    Ret := LiveOutput();
  end
  else if (AToken.PType = T_LINE_SCRIPT_EMBED) then
  begin
    Eat(T_LINE_SCRIPT_EMBED);
    Ret := Statement();
    FLexer.PScriptMode := False;
  end
  else if AToken.PType = T_RETURN then
  begin
    Eat(T_RETURN);
    Ret := TReturnFunction.Create(MethodCall());
  end
  else if (AToken.PType = T_BREAK) then
  begin
    Eat(T_BREAK);
    Ret := TBreakLoop.Create(AToken);
  end
  else if (AToken.PType = T_CONTINUE) then
  begin
    Eat(T_CONTINUE);
    Ret := TContinueLoop.Create(AToken);
  end
  else if (AToken.PType = T_COMMENT) then
  begin
    Eat(T_COMMENT);
    Ret := TNoOp.Create;
  end
  else if (AToken.PType = T_END + T_FUNC_DEF) then
  begin
    Ret := TNoOp.Create;
  end
  else if (AToken.PType = T_FUNC_DEF) then
  begin
    Ret := FunctionBlock();
  end
  else if (AToken.PType = T_IF_START) then
  begin
    Ret := Conditional();
  end
  else if (AToken.PType = T_WHILE_LOOP) then
  begin
    Ret := WhileLoop();
  end
  else if (AToken.PType = T_WHILE_LOOP) then
  begin
    Ret := ForLoop();
  end
  else if (AToken.PType = T_FOR_LOOP) then
  begin
    Ret := ForLoop();
  end
  else
    Ret := TNoOp.Create;
  Result := Ret;
end;

function TTParser.Statements: TASTList;
var
  Ret: TAST;
  Results: TASTList;
  len: integer;
begin
  Ret := Statement();
  SetLength(Results, 1);
  len := Length(Results);
  Results[len - 1] := Ret;
  while FCurrentToken.PType = T_NEWLINE do
  begin
    Eat(T_NEWLINE);

    if FCurrentToken.PType = T_NEWLINE then
    begin
      continue;
    end;
    len := len + 1;
    SetLength(Results, Len);
    Results[len - 1] := Statement();
    if FCurrentToken.PType = T_END + T_FUNC_DEF then
    begin
      break;
    end
    else if FCurrentToken.PType = T_RETURN then
    begin
      Eat(T_RETURN);
      writeln('return');
    end
    else if FCurrentToken.PType = T_ELSE_IF then
    begin
      // Eat(T_ELSE_IF);
      break;
    end
    else if FCurrentToken.PType = T_ELSE then
    begin
      // Eat(T_ELSE);
      Break;
    end
    else if FCurrentToken.PType = T_END + T_WHILE_LOOP then
    begin
      Eat(T_END + T_WHILE_LOOP);
      Break;
    end
    else if FCurrentToken.PType = T_END + T_FOR_LOOP then
    begin
      Eat(T_END + T_FOR_LOOP);
      Break;
    end
    else if FCurrentToken.PType = T_END + T_IF_START then
    begin
      Eat(T_END + T_IF_START);
      Break;
    end;
  end;
  logtext('PARSER', 'Parser', 'Creating statement list node');
  Result := Results;
end;

function TTParser.SourceProgram: TAST;
var
  AList: TASTList;
  AProgram: TProgram;
  Ret: TAST;
begin
  AList := Statements();
  AProgram := TProgram.Create;
  for Ret in AList do
  begin
    Aprogram.Add(Ret);
  end;
  Eat(EOF);
  logtext('PARSER', 'Parser', 'Creating program node');
  Result := AProgram;
end;

function TTParser.ParseCode: TAST;
var
  Ret: TAST;
  AToken: TToken;
begin
  Ret := SourceProgram();
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
  if AToken.PType <> EOF then
    EParseError;
  Result := Ret;
end;

function TTParser.Factor: TAST;
var
  AToken, ParToken: TToken;
  Ret: TAST;

begin
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
  ParToken := TToken.Create();
  if (FCurrentToken.PType = T_NEWLINE) then
    Eat(T_NEWLINE);
  if (AToken.PType = T_MINUS) then
  begin
    Eat(T_MINUS);
    logtext('PARSER', 'Parser', 'Creating unary op node');
    Ret := TUnaryOp.Create(AToken, Factor());
  end
  else if (AToken.PType = TYPE_NULL) then
  begin
    Eat(TYPE_NULL);
    logtext('PARSER', 'Parser', 'Creating null node');
    Ret := TNull.Create(AToken);
  end
  else if (AToken.PType = TYPE_STRING) then
  begin
    Eat(TYPE_STRING);
    logtext('PARSER', 'Parser', 'Creating string node');
    Ret := TString.Create(AToken);
  end
  else if (AToken.PType = T_PLUS) then
  begin
    Eat(T_PLUS);
    logtext('PARSER', 'Parser', 'Creating unary op node');
    Ret := TUnaryOp.Create(AToken, Factor());
  end

  else if (AToken.PType = T_NOT) then
  begin
    Eat(T_NOT);
    logtext('PARSER', 'Parser', 'Creating logic not node');
    Ret := TUnaryLogicOp.Create(AToken, Factor());
  end

  // INNER ATRIBUTTES
  else if (AToken.PType = T_LIVE_PRINT) then
  begin
    Eat(T_LIVE_PRINT);
    Ret := TLivePrint.Create(AToken);
  end
  // END INNER ATTRIBUTES

  else if (AToken.PType = TYPE_INTEGER) then
  begin
    Eat(TYPE_INTEGER);
    logtext('PARSER', 'Parser', 'Creating integer node');
    Ret := TNumInt.Create(AToken);
  end

  else if (AToken.PType = T_LPAREN) then
  begin
    Eat(T_LPAREN);
    FInArgsDef := True;
    Ret := MethodCall();
    Eat(T_RPAREN);
    FInArgsDef := False;
    logtext('PARSER', 'Parser', 'Creating closing paren node');
  end
  else if (AToken.PType = TYPE_FLOAT) then
  begin
    Eat(TYPE_FLOAT);
    logtext('PARSER', 'Parser', 'Creating float node');
    Ret := TNumFloat.Create(AToken);
  end
  else if (AToken.PType = TYPE_BOOLEAN) then
  begin
    Eat(TYPE_BOOLEAN);
    logtext('PARSER', 'Parser', 'Creating boolean node');
    Ret := TBoolean.Create(AToken);
  end
  else if (AToken.PType = T_LIST_START) then
  begin
    logtext('PARSER', 'Parser', 'creating list node');
    Ret := List();

  end
  else if (AToken.PType = T_ID) then
  begin
    Eat(T_ID);
    if (FCurrentToken.PType = T_LPAREN) then
    begin
      Ret := FunctionCall(AToken);
    end
    else
      Ret := Variable(AToken);
  end;
  Result := Ret;
end;

function TTParser.Term: TAST;
var
  AToken: TToken;
  Ret: TAST;
begin
  Ret := Factor();
  while (FCurrentToken <> nil) and ((FCurrentToken.PType = T_MULT) or
      (FCurrentToken.PType = T_DIV) or (FCurrentToken.PType = T_MODULUS) or
      (FCurrentToken.PType = T_INT_DIV)) do
  begin
    AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
    if AToken.PType = T_MULT then
      Eat(T_MULT)
    else if AToken.PType = T_DIV then
      Eat(T_DIV)
    else if (ATOken.PType = T_INT_DIV) then
      Eat(T_INT_DIV)
    else if (ATOken.PType = T_MODULUS) then
      Eat(T_MODULUS);
    logtext('PARSER', 'Parser', 'Creating bin op node ' + AToken.AsString);
    Ret := TBinOp.Create(Ret, Factor(), AToken);
  end;
  Result := Ret;
end;

function TTParser.Expr: TAST;
var
  AToken: TToken;
  Ret: TAST;
begin
  Ret := Term();
  while (FCurrentToken <> nil) and ((FCurrentToken.PType = T_PLUS) or
      (FCurrentToken.PType = T_MINUS)) do
  begin
    AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
    if AToken.PType = T_PLUS then
      Eat(T_PLUS)
    else if AToken.PType = T_MINUS then
      Eat(T_MINUS);
    logtext('PARSER', 'Parser', 'Creating bin op node');
    Ret := TBinOp.Create(Ret, Term(), AToken);
  end;
  Result := Ret;
end;

function TTParser.LogicExpr: TAST;
var
  AToken: TToken;
  Ret: TAST;
begin
  Ret := Expr();
  while (FCurrentToken <> nil) and ((FCurrentToken.PType = T_AND) or
      (FCurrentToken.PType = T_OR)) do
  begin
    AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
    Eat(AToken.PType);
    logtext('PARSER', 'Parser', 'Creating bin logic op node');
    Ret := TBinLogicOp.Create(Ret, Expr(), AToken);
  end;
  Result := Ret;
end;

function TTParser.LogicEval: TAST;
var
  AToken: TToken;
  Ret: TAST;
begin
  Ret := LogicExpr();
  while (FCurrentToken <> nil) and ((FCurrentToken.PType = T_GT) or
      (FCurrentToken.PType = T_LT) or (FCurrentToken.PType = T_EQ) or
      (FCurrentToken.PType = T_GEQ) or (FCurrentToken.PType = T_NEQ) or
      (FCurrentToken.PType = T_LEQ)) do
  begin
    AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue);
    Eat(AToken.PType);
    logtext('PARSER', 'Parser', 'Creating bin logic op node');
    Ret := TBinLogicOp.Create(Ret, LogicExpr(), AToken);
  end;
  Result := Ret;
end;

procedure TTParser.EParseError;
var
  msg: string;
begin
  msg := 'Unexpected token "' + FCurrentToken.PType + '" at < Line: ' +
    IntToStr(FLexer.PScriptLine) + ', Char: ' + IntToStr(FLexer.PLineChar - 1) + ' >';
  raise EParserError.Create(msg);
end;

procedure TTParser.Eat(AType: string);
var
  now: string;
begin
  now := FCurrentToken.AsString;

  if (FCurrentToken.PType = AType) then
  begin
    FCurrentToken := FLexer.GetNextToken;
  end
  else
    EParseError;

  now := FCurrentToken.AsString;

end;


end.









