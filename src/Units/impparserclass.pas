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
    destructor Destroy; override;
    procedure EParseError(addMsg: string = '');
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
    function VarShortAssign(AToken: TToken; ShortCutType: string): TAST;
    function Variable(AToken: TToken): TAST;
    function FunctionBlock(Anony, IsDecorator:boolean): TAST;

    function DefParams: TASTList;
    function DefParam: TAST;
    function FunctionCall(var AToken: TToken): TAST;
    function FunctionCallByInstance(AFuncNode: TAST; var AToken: TToken): TAST;
    function MethodCall: TAST;
    function LoadType: TAST;
    function MethodCallState: TAST;
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
    //function NamespaceGet: TAST;
    //function NamespaceState: TAST;
    function InstaLiteral: TAST;
    function NewObject: TAST;
    function Dict: TAST;
    function DictKey: TAST;
    function ListAssign(ASrc, AKey: TAST; var CToken: TToken): TAST;
    function ImportModule: TAST;
    function ClassDefinition: TAST;
  end;

implementation

uses
  ExceptionsClasses;

constructor TTParser.Create(var ALexer: TLexer);
begin
  FLexer := ALexer;
  FCurrentToken := ALexer.GetNextToken;
end;

destructor TTParser.Destroy;
begin
  FLexer.Free;
  inherited;
end;

function TTParser.NewObject: TAST;
var
  ConstArgs: TASTList;
  AName:string;
  AToken: TToken;
begin
  eat(T_NEW_OBJECT);
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  AName := AToken.PValue;
  Eat(T_ID);
  SetLength(ConstArgs, 0);
  logtext('PARSER', 'Parser', 'Creating new object node');
  if FCurrentToken.PType = T_LPAREN then
  begin
    Eat(T_LPAREN);
    ConstArgs := Args();
    Eat(T_RPAREN);
  end;
  Result := TNewObject.Create(ConstArgs, AName, AToken);
end;

function TTParser.ClassDefinition: TAST;
var
  AToken: TToken;
begin
  Eat(T_CLASS_DEF);
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Eat(T_ID);
  Result := TClassDefinition.Create(AToken);
end;

function TTParser.LoadType: TAST;
var
  AParams: TASTList;
  len: integer;
  AToken: TToken;
begin
  AToken := TToken.Create(
        FCurrentToken.PType,
        FCurrentToken.PValue,
        FCurrentToken.PLineNo,
        FCurrentToken.PCharNo,
        FCurrentToken.PScriptName
      );
  Eat(T_LOAD_TYPE);
  len := 1;
  SetLength(AParams, len);
  AParams[len-1] := TAST.Create(
    TToken.Create(
      FCurrentToken.PType,
      FCurrentToken.PValue,
      FCurrentToken.PLineNo,
      FCurrentToken.PCharNo,
      FCurrentToken.PScriptName
    )
  );
  Eat(T_ID);
  while (FCurrentToken.PType <> T_NEWLINE) and (FCurrentToken.PType <> EOF) do
  begin
    Eat(T_COMMA);
    len := len + 1;
    SetLength(AParams, len);
    AParams[len-1] := TAST.Create(
      TToken.Create(
        FCurrentToken.PType,
        FCurrentToken.PValue,
        FCurrentToken.PLineNo,
        FCurrentToken.PCharNo,
        FCurrentToken.PScriptName
      )
    );
    Eat(T_ID);
  end;

  Result :=  TLoadType.Create(AParams, AToken);
end;

function TTParser.IncludeScript: TAST;
var
  AFileName: TAST;
  ANamespace: string = '';
  AToken: TToken;
  ModuleName: string = '';
  IsModule: boolean = False;
begin
  Eat(T_INCLUDE);
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  if FCurrentToken.PType <> T_MODULE_PATH then
  begin
    AFileName := MethodCall();

	end
  else
  begin
    IsModule := True;
    ModuleName := FCurrentToken.PValue;
    Eat(T_MODULE_PATH);
	end;
  if FCurrentToken.PType = T_DICT_ASSIGN then
  begin
    Eat(T_DICT_ASSIGN);
    if FCurrentToken.PType = T_ID then
    begin
      ANamespace := FCurrentToken.PValue;
      Eat(T_ID);
    end
    else
      ANamespace := '0';
  end;

  Result := TIncludeScript.Create(AFileName, AToken, ANamespace, IsModule, ModuleName);
end;

function TTParser.ImportModule: TAST;
begin

end;

function TTParser.ListAssign(ASrc, AKey: TAST; var CToken: TToken): TAST;
var
  AVal: TAST;
  AToken: TToken;
begin
  AToken := TToken.Create(
    CToken.PType,
    CToken.PValue,
    CToken.PLineNo,
    CToken.PCharNo,
    CToken.PScriptName
  );
  Eat(T_ASSIGN);
  AVal := MethodCall();
  Result := TListAssign.Create(ASrc, Akey, AVal, AToken);
end;

function TTParser.Dict: TAST;
var
  AllKeys: TASTList;
  ret: TAST;
  len: integer = 0;
  AToken: TToken;
begin
  eat(T_DICT_START);
  while (FCurrentToken.PType = T_NEWLINE) do
    Eat(T_NEWLINE);
  SetLength(AllKeys, 0);
  if FCurrentToken.PType <> T_DICT_END then
  begin
    len := len + 1;
    SetLength(Allkeys, len);
    Allkeys[len - 1] := DictKey();
  end;
  while (FCurrentToken.PType = T_COMMA) do
  begin
    Eat(T_COMMA);
    while (FCurrentToken.PType = T_NEWLINE) do
      Eat(T_NEWLINE);
    len := len + 1;
    SetLength(Allkeys, len);
    Allkeys[len - 1] := DictKey();
  end;
  while (FCurrentToken.PType = T_NEWLINE) do
    Eat(T_NEWLINE);
  Eat(T_DICT_END);
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Result := TDictNode.Create(AllKeys, AToken);
end;

function TTParser.DictKey: TAST;
var
  AKey: TAST;
  AVal: TAST;
  AToken: TToken;
begin
  // Akey := FCurrentToken.PValue;
  // Eat(T_ID);
  Akey := MethodCall();
  Logtext('PARSER', 'Parser', 'Setting a dict key');
  Eat(T_DICT_ASSIGN);
  while (FCurrentToken.PType = T_NEWLINE) do
    Eat(T_NEWLINE);
  AVal := MethodCall();
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Result := TDictKeyNode.Create(AKey, AVal, AToken);
end;
{
function TTParser.NamespaceGet: TAST;
var
  Oper: TAST;
  AName: string;
  AToken: TToken;
begin
  Eat(T_DICT_ASSIGN);
  AName := FCurrentToken.PValue;
  Eat(T_ID);
  if FcurrentToken.PType = T_ATTR_ACCESSOR then
  begin
    Eat(T_ATTR_ACCESSOR);
    Oper := MethodCall();
    AToken := TToken.Create(
      FCurrentToken.PType,
      FCurrentToken.PValue,
      FCurrentToken.PLineNo,
      FCurrentToken.PCharNo,
      FCurrentToken.PScriptName
    );
    Result := TNamespaceGet.Create(Aname, Oper, AToken);
  end
  else if FCurrentToken.PType = T_DICT_ASSIGN then
  begin
    AToken := TToken.Create(
      FCurrentToken.PType,
      FCurrentToken.PValue,
      FCurrentToken.PLineNo,
      FCurrentToken.PCharNo,
      FCurrentToken.PScriptName
    );
    Result := TnamespaceGet.Create(AName, NamespaceGet(), AToken);
  end;
end;

function TTParser.NamespaceState: TAST;
var
  AName: string;
  Aoper: TAST;
  AToken: TToken;
begin
  Eat(T_DICT_ASSIGN);
  AName := FCurrentToken.PValue;
  Eat(T_ID);
  if (FcurrentToken.PType = T_ATTR_ACCESSOR) or
     (FCurrentToken.PType = T_FUNC_DEF) then
  begin
    if FcurrentToken.PType = T_ATTR_ACCESSOR then
      Eat(T_ATTR_ACCESSOR);
    AOper := Statement();
    AToken := TToken.Create(
      FCurrentToken.PType,
      FCurrentToken.PValue,
      FCurrentToken.PLineNo,
      FCurrentToken.PCharNo,
      FCurrentToken.PScriptName
    );
    Result := TNamespaceState.Create(Aname, AOper, AToken);
  end
  else if FCurrentToken.PType = T_DICT_ASSIGN then
  begin
    AToken := TToken.Create(
      FCurrentToken.PType,
      FCurrentToken.PValue,
      FCurrentToken.PLineNo,
      FCurrentToken.PCharNo,
      FCurrentToken.PScriptName
    );
    Result := TnamespaceState.Create(AName, NamespaceState(), AToken);
  end;
end;
    }

function TTParser.InstaLiteral: TAST;
var
  AStr: string;
  Aoper: TAST;
  AToken: TToken;
begin
  Eat(T_DICT_ASSIGN);
  AStr := FCurrentToken.PValue;
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Eat(T_ID);
  Result := TString.Create(AToken);
end;


function TTParser.FunctionBlock(Anony, IsDecorator:boolean): TAST;
var
  AStrId: string;
  ParamList: TASTList;
  InBlock: TASTList;
  AToken: TToken;
  AType:string = '';
  AName:string;
  OpenType: string;
  AccVar: boolean=False;
begin
  SetLength(ParamList, 0);
  if FCurrentToken.PType = T_DECOR_DEF then
  begin
    OpenType := T_DECOR_DEF;
    Eat(T_DECOR_DEF)
  end
  else
  begin
    OpenType := T_FUNC_DEF;
    Eat(T_FUNC_DEF);
  end;
  if Anony then
    AName := FormatDateTime('yyyymmddhhnnsszzz', Now)
  else
    AName := FCurrentToken.PValue;
  AToken := TToken.Create(FCurrentToken.PType, AName,
         FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
  AStrId := AName;
  if not Anony then
    Eat(T_ID);
  Eat(T_LPAREN);
  FInArgsDef := True;
  ParamList := DefParams();
  Eat(T_RPAREN);
  if (OpenType = T_DECOR_DEF) and (Length(ParamList) = 0) then
    EParseError('Decorators must have at least one parameter');
  if FCurrentToken.PType = T_MULT then
  begin
    AccVar := True;
    if OpenType = T_DECOR_DEF then
      EParseError('Decorator cannot accept variable arguments');
    Eat(T_MULT);
  end;
  if FCurrentToken.PType = T_DICT_ASSIGN then
  begin
    Eat(T_DICT_ASSIGN);
    AType := FCurrentToken.PValue;
    Eat(T_ID);
  end;
  FInArgsDef := False;
  if FLexer.PExtension <> '.ultra' then
    FLexer.PScriptMode := False;
  Eat(T_NEWLINE);
  InBlock := Statements();
  Eat(T_END + OpenType);
  logtext('PARSER', 'Parser', 'Creating function block node');
  {if AType <> '' then
    AStrId := AType + ':' + AStrId;}
  if not IsDecorator then
    Result := TFunctionDefinition.Create(AToken, AStrId, InBlock, ParamList, AType, IsDecorator, AccVar)
  else
    Result := TDecoratorDefinition.Create(AToken, AStrId, InBlock, ParamList, AType, IsDecorator, AccVar);
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
    AToken.SetValue(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
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
  Result := TPlainTextEmbed.Create(Ret, AToken);
end;

function TTParser.ElseBlock: TAST;
var
  AToken: TToken;
begin
  Eat(T_NEWLINE);
  logtext('PARSER', 'Parser', 'Creating else block node');
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Result := TIfConditionBlock.Create(Statements(), nil, AToken);
end;

function TTParser.Interpolated: TAST;
var
  AOper: TAST;
  AToken: TToken;
begin
  Eat(T_INTERPOLATION_START);
  AOper := MethodCall();
  Eat(T_INTERPOLATION_END);
  logtext('PARSER', 'Parser', 'Creating inter node');
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Result := TInterpolation.Create(AOper, AToken);
end;

function TTParser.LiveOutput: TAST;
var
  AToken: TToken;
begin
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Result := TLiveOutput.Create(MethodCall(), AToken);
end;

function TTParser.IfBlock: TAST;
var
  InBlock: TASTList;
  AExpr: TAST;
  AToken: TToken;
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
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Result := TIfConditionBlock.Create(Statements(), AExpr, AToken);
end;

function TTParser.WhileLoop: TAST;
var
  AExpr: TAST;
  AToken: TToken;
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
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Result := TWhileLoop.Create(Statements(), AExpr, AToken);
end;

function TTParser.ForLoop: TAST;
var
  AList: TAST;
  AToken: TToken;
  AControlVar: string;
begin
  Eat(T_FOR_LOOP);
  Eat(T_LPAREN);
  FInArgsDef := True;
  AList := MethodCall();
  Eat(T_COMMA);
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine,
         FLexer.PLineChar, FLexer.PFileName);
  Eat(T_ID);
  FInArgsDef := False;
  Eat(T_RPAREN);
  if FLexer.PExtension <> '.ultra' then
    FLexer.PScriptMode := False;
  Eat(T_NEWLINE);
  logtext('PARSER', 'Parser', 'Creating for node');
  Result := TForLoop.Create(Statements(), AList, AToken.PValue, AToken);
end;

function TTParser.Conditional: TAST;
var
  Conditions: array of TAST;
  len: integer;
  AToken: TToken;
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
    if FLexer.PExtension <> '.ultra' then
      FLexer.PScriptMode := False;
    Eat(T_ELSE);
    len := len + 1;
    SetLength(Conditions, len);
    Conditions[len - 1] := ElseBlock();
  end;
  logtext('PARSER', 'Parser', 'Creating if block node');
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  Result := TConditional.Create(Conditions, AToken);
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
  while ((FCurrentToken.PType <> T_RPAREN) and
        (FCurrentToken.PType <> EOF))do
  begin
    if (FCurrentToken.PType = T_NEWLINE) then
      Eat(T_NEWLINE);
    len := len + 1;
    SetLength(AArgs, len);
    AArgs[len - 1] := MethodCall();

    if (FCurrentToken.PType <> T_RPAREN) then
    begin
      if (FCurrentToken.PType = T_COMMA) then
        Eat(T_COMMA);
      if (FCurrentToken.PType = T_NEWLINE) then
        Eat(T_NEWLINE);
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
  AToken, BToken: TToken;
begin
  Eat(T_LIST_START);
  AToken := TToken.Create(
    FCurrentToken.PType,
    FCurrentToken.PValue,
    FCurrentToken.PLineNo,
    FCurrentToken.PCharNo,
    FCurrentToken.PScriptName
  );
  //Ret := TListAccessAST.Create(ASrc, MethodCall(), AToken);
  Eat(T_LIST_END);
  //Ret := MethodCall();
  logtext('PARSER', 'Parser', 'Creating list access node');
  while (FCurrentToken.PType = T_LIST_START) do
  begin
    Eat(T_LIST_START);
    Ret := TListAccessAST.Create(Ret, MethodCall(), TToken.Create(
        FCurrentToken.PType,
        FCurrentToken.PValue,
        FCurrentToken.PLineNo,
        FCurrentToken.PCharNo,
        FCurrentToken.PScriptName
      )
    );
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
  AToken := TToken.Create(FcurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
  Ret := TListAST.Create(AToken, ListArgs());
  Eat(T_LIST_END);
  if FCurrentToken.PType = T_ASSIGN then
  begin
    Ret := ListAssign(Ret, Ret, AToken);
  end
  else
  begin
    while FCurrentToken.PType = T_LIST_START do
    begin
      Eat(T_LIST_START);
      Ret := TListAccessAST.Create(Ret, MethodCall(), TToken.Create(
        FCurrentToken.PType,
        FCurrentToken.PValue,
        FCurrentToken.PLineNo,
        FCurrentToken.PCharNo,
        FCurrentToken.PScriptName
      ));
      Eat(T_LIST_END);
    end;
  end;

  Result := Ret;
end;

function TTParser.MethodCallState: TAST;
var
  Ret, IndexOrAcc, ASrc: TAST;
  AVarToken: TToken;
begin
  // Eat(T_ATTR_ACCESSOR);
  AVarToken := TToken.Create(
      FCurrentToken.PType,
      FCurrentToken.PValue,
      FCurrentToken.PLineNo,
      FCurrentToken.PCharNo,
      FCurrentToken.PScriptName
  );
  Ret := LogicEval();

  logtext('PARSER', 'Parser', 'Creating method call node');
  if FCurrentToken.PType = T_ASSIGN then
  begin
    Ret := VarAssign(Ret.PToken);
  end
  else if (FCurrentToken.PType = T_SHORT_INC) or
          (FCurrentToken.PType = T_SHORT_DEC) or
          (FCurrentToken.PType = T_SHORT_MULT) or
          (FCurrentToken.PType = T_SHORT_DIV) then
  begin
    Ret := VarShortAssign(Ret.PToken, FCurrentToken.PType)
	end
	else
  begin
    while (FCurrentToken.PType = T_ATTR_ACCESSOR) or
      (FCurrentToken.PType = T_LIST_START) do
    begin
      if (FCurrentToken.PType = T_ATTR_ACCESSOR) then
      begin
        Eat(T_ATTR_ACCESSOR);
        if FCurrentToken.ptype = T_NEWLINE then
          Eat(T_NEWLINE);
        Ret := TMethodCall.Create(Ret, LogicEval(),TToken.Create(
              FCurrentToken.PType,
              FCurrentToken.PValue,
              FCurrentToken.PLineNo,
              FCurrentToken.PCharNo,
              FCurrentToken.PScriptName
            ));
        continue;
      end;
      if (FCurrentToken.PType = T_LIST_START) then
      begin
        ASrc := Ret;
        Eat(T_LIST_START);
        if FCurrentToken.ptype = T_NEWLINE then
          Eat(T_NEWLINE);
        IndexOrAcc := MethodCall();
        Ret := TListAccessAST.Create(Ret, IndexOrAcc, TToken.Create(
              FCurrentToken.PType,
              FCurrentToken.PValue,
              FCurrentToken.PLineNo,
              FCurrentToken.PCharNo,
              FCurrentToken.PScriptName
            ));
        if FCurrentToken.ptype = T_NEWLINE then
          Eat(T_NEWLINE);
        Eat(T_LIST_END);
        if FCurrentToken.PType = T_ASSIGN then
        begin
          Ret := ListAssign(ASrc, IndexOrAcc, AVarToken);
        end
        else if FCurrentToken.PType = T_LPAREN then
          ret := FunctionCallByInstance(Ret, AVarToken);
      end
    end;
  end;

  Result := Ret;
end;

function TTParser.MethodCall: TAST;
var
  Ret: TAST;
  AToken: ttoken;
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
      Ret := TMethodCall.Create(Ret, LogicEval(), TToken.Create(
            FCurrentToken.PType,
            FCurrentToken.PValue,
            FCurrentToken.PLineNo,
            FCurrentToken.PCharNo,
            FCurrentToken.PScriptName
          ));
      continue;
    end;
    if (FCurrentToken.PType = T_LIST_START) then
    begin
      Eat(T_LIST_START);
      if FCurrentToken.ptype = T_NEWLINE then
        Eat(T_NEWLINE);
      Ret := TListAccessAST.Create(Ret, MethodCall(), TToken.Create(
              FCurrentToken.PType,
              FCurrentToken.PValue,
              FCurrentToken.PLineNo,
              FCurrentToken.PCharNo,
              FCurrentToken.PScriptName
            ));
      if FCurrentToken.ptype = T_NEWLINE then
        Eat(T_NEWLINE);
      Eat(T_LIST_END);
      if FCurrentToken.PType = T_LPAREN then
      begin
          AToken := TToken.Create(
              FCurrentToken.PType,
              FCurrentToken.PValue,
              FCurrentToken.PLineNo,
              FCurrentToken.PCharNo,
              FCurrentToken.PScriptName
            );
          ret := FunctionCallByInstance(Ret, AToken);
      end;
    end
  end;
  Result := Ret;
end;

function TTParser.FunctionCall(var AToken: TToken): TAST;
var
  AFuncName: string;
  AArgs: TASTList;
  Ret: TAST;
  CToken: TTOken;
begin
  CToken := TToken.Create(
        FCurrentToken.PType,
        AToken.PValue,
        FCurrentToken.PLineNo,
        FCurrentToken.PCharNo,
        FCurrentToken.PScriptName
      );
  AFuncName := AToken.PValue;
  Eat(T_LPAREN);
  AArgs := Args();
  if (FCurrentToken.PType = T_NEWLINE) then
        Eat(T_NEWLINE);
  Eat(T_RPAREN);

  logtext('PARSER', 'Parser', 'Creating function call node');
  Result := TFunctionCall.Create(AFuncName, AArgs, CToken);
end;

function TTParser.FunctionCallByInstance(AFuncNode: TAST; var AToken: TToken): TAST;
var
  AFuncName: string;
  AArgs: TASTList;
  Ret: TAST;
  CToken: TTOken;
begin
  CToken := TToken.Create(
        FCurrentToken.PType,
        AToken.PValue,
        FCurrentToken.PLineNo,
        FCurrentToken.PCharNo,
        FCurrentToken.PScriptName
      );
  Eat(T_LPAREN);
  AArgs := Args();

  Eat(T_RPAREN);

  logtext('PARSER', 'Parser', 'Creating function call node');
  Result := TFunctionCallByInstance.Create(AFuncNode, AArgs, CToken);
end;


function TTParser.DefParam: TAST;
var
  Ret, ADef: TAST;
  AToken: TToken;
  AVarAssign: TAST;
begin
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
  ADef := nil;
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
begin
  Ret := TVariableReference.Create(AToken);
  Result := Ret;
end;

function TTParser.VarAssign(AToken: TToken): TAST;
var
  Aleft, ARight: TAST;
begin
  Eat(T_ASSIGN);
  //if FCurrentToken.PType = T_NEW_OBJECT then
  //begin
    //ARight := NewObject();
  //end
  //else
    ARight := MethodCall();
  Logdebug('Creating a VarAssign to ' + AToken.AsString, 'Parser');
  logtext('PARSER', 'Parser', 'Creating var assign node to ' + AToken.PValue);
  Result := TVarAssign.Create(AToken, ARight);
end;

function TTParser.VarShortAssign(AToken: TToken; ShortCutType: string): TAST;
var
  Aleft, ARight: TAST;
  OperToken: TTOken;
begin
  Eat(ShortCutType);
  if FCurrentToken.PType = T_NEW_OBJECT then
  begin
    ARight := NewObject();
  end
  else
    ARight := MethodCall();
  Logdebug('Creating a VarAssign to ' + AToken.AsString, 'Parser');
  logtext('PARSER', 'Parser', 'Creating var assign node to ' + AToken.PValue);
  if ShortCutType = T_SHORT_INC then
    OperToken := TToken.Create(
        T_PLUS,
        '+',
        AToken.PLineNo,
        AToken.PCharNo,
        AToken.PScriptName
      )
  else if ShortCutType = T_SHORT_DEC then
    OperToken := TToken.Create(
        T_MINUS,
        '-',
        AToken.PLineNo,
        AToken.PCharNo,
        AToken.PScriptName
      )
  else if ShortCutType = T_SHORT_MULT then
    OperToken := TToken.Create(
        T_MULT,
        '*',
        AToken.PLineNo,
        AToken.PCharNo,
        AToken.PScriptName
      )
  else if ShortCutType = T_SHORT_DIV then
    OperToken := TToken.Create(
        T_DIV,
        '/',
        AToken.PLineNo,
        AToken.PCharNo,
        AToken.PScriptName
      );

  Result := TVarAssign.Create(
    AToken,
    TBinOp.Create(
      TVariableReference.Create(AToken),
      ARight,
      OperToken
    )
  );
end;


function TTParser.Statement: TAST;
var
  AToken: TToken;
  AStrId: string;
  Ret: TAST;
begin

  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
  logtext('PARSER', 'Parser', 'Creating statement node');
  if (AToken.PType = T_ID) then
  begin
    Ret := MethodCallState();
  end


  else if (ATOken.PType = T_INCLUDE) then
  begin
    Logtext('PARSER', 'Parser', 'Creating include node');
    Ret := IncludeScript();
  end
	{else if (AToken.PType = T_DICT_ASSIGN) then
  begin
    Logtext('PARSER', 'Parser', 'Creating namespace');
    Ret := NamespaceState();
  end}
  else if (AToken.PType = T_LOAD_TYPE) then
  begin
    Ret := LoadType();
  end
  else if (AToken.PType = T_CLASS_DEF) then
  begin
    ret := ClassDefinition();
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
  end
  else if AToken.PType = T_RETURN then
  begin
    Eat(T_RETURN);
    Ret := TReturnFunction.Create(MethodCall(), AToken);
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
  {else if (AToken.PType = T_COMMENT) then
  begin
    Eat(T_COMMENT);
//    FLexer.PassLineComment;
    Ret := TNoOp.Create(AToken);
  end}
  else if (AToken.PType = T_END + T_FUNC_DEF) then
  begin
    Ret := TNoOp.Create(AToken);
  end
  else if (AToken.PType = T_END + T_DECOR_DEF) then
  begin
    Ret := TNoOp.Create(AToken);
  end

  else if (AToken.PType = T_FUNC_DEF) then
  begin
    Ret := FunctionBlock(False, False);
  end
  else if (AToken.PType = T_DECOR_DEF) then
  begin
    Ret := FunctionBlock(False, True);
  end
  else if (AToken.PType = T_IF_START) then
  begin
    Ret := Conditional();
  end
  else if (AToken.PType = T_WHILE_LOOP) then
  begin
    Ret := WhileLoop();
  end
  else if (AToken.PType = T_FOR_LOOP) then
  begin
    Ret := ForLoop();
  end
  else
    Ret := TNoOp.Create(AToken);

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
    if FLexer.PExtension <> '.ultra' then
        FLexer.PScriptMode := False;
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
    else if FCurrentToken.PType = T_END + T_DECOR_DEF then
    begin
      break;
    end
    else if FCurrentToken.PType = T_RETURN then
    begin
      Eat(T_RETURN);

    end
    else if FCurrentToken.PType = T_ELSE_IF then
    begin
      break;
    end
    else if FCurrentToken.PType = T_ELSE then
    begin
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
      if FLexer.PExtension <> '.ultra' then
        FLexer.PScriptMode := False;
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
  AToken: TToken;
begin
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
  AList := Statements();
  AProgram := TProgram.Create(AToken);
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
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
  if AToken.PType <> EOF then
    EParseError;
  Result := Ret;
end;

function TTParser.Factor: TAST;
var
  AToken, ParToken: TToken;
  Ret: TAST;

begin
  AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
  ParToken := TToken.Create();
  if (FCurrentToken.PType = T_NEWLINE) then
    Eat(T_NEWLINE);
  if (AToken.PType = T_MINUS) then
  begin
    Eat(T_MINUS);
    logtext('PARSER', 'Parser', 'Creating unary op node');
    Ret := TUnaryOp.Create(AToken, Factor());
  end
  else if (AToken.PType = T_DICT_ASSIGN) then
  begin
    Ret := InstaLiteral();
  end
  else if (AToken.PType = T_FUNC_DEF) then
  begin
    Ret := FunctionBlock(True, False);
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

  else if (AToken.PType = T_DICT_START) then
  begin
    logtext('PARSER', 'Parser', 'Creating dict node');
    ret := Dict();
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
  else if FCurrentToken.PType = T_NEW_OBJECT then
  begin
    Ret := NewObject();
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
    else if (FCurrentToken.PType = T_ASSIGN) then
    begin
      Ret := VarAssign(AToken);
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
    AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
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
    AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
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
    AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue, FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
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
    AToken := TToken.Create(FCurrentToken.PType, FCurrentToken.PValue,
      FLexer.PScriptLine, FLexer.PLineChar, FLexer.PFileName);
    Eat(AToken.PType);
    logtext('PARSER', 'Parser', 'Creating bin logic op node');
    Ret := TBinLogicOp.Create(Ret, LogicExpr(), AToken);
  end;
  Result := Ret;
end;

procedure TTParser.EParseError(addMsg: string='');
var
  msg: string;
begin

  msg := 'Unexpected token "' + FCurrentToken.PValue + '" from type "'+FCurrentToken.PType+'"';
  if addMsg <> '' then
    msg := msg + sLineBreak + AddMsg;
  EParserError.Create(msg, FLexer.PFileName, FLexer.PScriptLine, FLexer.PLineChar);
end;

procedure TTParser.Eat(AType: string);
begin
  if (FCurrentToken.PType = AType) then
  begin
    //FCurrentToken.Free;
    FreeAndNil(FCurrentToken);
    FCurrentToken := FLexer.GetNextToken;
  end
  else
    EParseError;
end;


end.









