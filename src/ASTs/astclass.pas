unit ASTClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TokenClass, Tokens, LoggingClass;

type

  TAST = class
  protected
    FNodeLine: integer;
    FNodeChar: integer;
    FToken: TToken;
  public
    property PToken: TToken read FToken write FToken;
    function ToString: string; override;
    constructor Create(AToken: TToken);
    destructor Destroy; override;
  end;



  TASTList = array of TAST;

  TProgram = class(TAST)
  protected
    FChildren: TASTList;
    FPreludes: TASTList;
  public
    property PChildren: TASTList read FChildren write FChildren;
    property PPreludes: TASTList read FPreludes write FPreludes;
    constructor Create(AToken: TToken);
    destructor Destroy; override;
    procedure Add(ANode: TAST);
    procedure AddPrelude(ANode: TAST);
    function Count: integer;
  end;

  TAssignedTest = class(TAST)
  protected
    FValue: TAST;
  public
    property PValue: TAST read FValue;
    destructor Destroy;
    constructor Create(AValue: TAST; AToken: TToken);
  end;

  TDictKeyNode = class(TAST)
  protected
    FKey: TAST;
    FValue: TAST;
  public
    property PKey: TAST read Fkey;
    property PValue: TAST read FValue;
    constructor Create(AKey: TAST; AValue: TAST; AToken: TToken);
    destructor Destroy; override;
  end;

  TDictNode = class(TAST)
  protected
    FKeys: TASTList;
  public
    property PKeys: TASTList read Fkeys;
    destructor Destroy; override;
    constructor Create(AKeys: TASTList; AToken: TToken);
  end;

  TLoadType = class (TAST)
    protected
      FTypeNames: TASTList;
    public
      property PTypeNames: TASTList read FTypeNames;
      destructor Destroy; override;
      constructor Create(ATypeNames: TASTList; AToken: TToken);
  end;

  TNewObject = class(TAST)
  private
    FArgs: TASTList;
    FName: string;
    FRef: TAST;
  public
    property PArgs: TASTList read FArgs write FArgs;
    property PName: string read Fname write FName;
    property PRef: TAST read FRef;
    constructor Create(AArgs: TASTList; AName: string; AToken: TToken);
    constructor Create(AArgs: TASTList; ARef: TAST; AToken: TToken);
    destructor Destroy; override;
  end;



  TNamespaceGet = class(TAST)
  protected
    FName: string;
    FOper: TAST;
  public
    property PName: string read FName;
    property POper: TAST read FOper;
    constructor Create(AName: string; AOper: TAST; AToken: TToken);
    destructor Destroy; override;
  end;

  TNamespaceState = class(TAST)
  protected
    FName: string;
    FOper: TAST;
  public
    property PName: string read FName;
    property POper: TAST read FOper;
    constructor Create(AName: string; AOper: TAST; AToken: TToken);
    destructor Destroy; override;
  end;

  TClassDefinition = class (TAST)
    protected
      FName: string;
    public
      constructor Create(AToken: TToken);
	end;

  TIncludeScript = class(TAST)
  protected
    FFileName: TAST;
    FNamespace: string;
    FIsmodule: boolean;
    FModulePath: string;
  public
    property PFilename: TAST read FFileName;
    property PNamespace: string read FNamespace;
    property PIsModule: boolean read FIsModule;
    property PModulePath: string read FModulePath;
    constructor Create(AFileName: TAST; AToken: TToken; ANamespace: string; IsModule: boolean; AModulePath: string);
    destructor Destroy; override;
  end;

  TListAST = class(TAST)
  private
    FArgs: TASTList;
  public
    property PArgs: TASTList read FArgs write FArgs;
    constructor Create(AToken: TToken; AArgs: TASTList);
    destructor Destroy; override;
  end;

  TListAccessAST = class(TAST)
  private
    FList: TAST;
    FIndex: TAST;
  public
    property PIndex: TAST read FIndex write FIndex;
    property PList: TAST read FList write FList;
    constructor Create(AList: TAST; AIndex: TAST; AToken: TToken);
    destructor Destroy; override;
  end;

  TPlainText = class(TAST)
  protected
    FValue: string;
  public
    property PValue: string read FValue;
    constructor Create(AValue: string; AToken: TToken);
  end;

  TFunctionDefinition = class(TAST)
  protected
    FName: string;
    FBlock: TASTList;
    FParamList: TASTList;
    FType: string;
    FIsInstance: boolean;
    FIsDecorator: boolean;
    FAcceptVarargs: boolean;
  public
    property PAcceptVarargs: boolean read FAcceptVarargs write FAcceptVarargs;
    property PType: string read FType;
    property PParamList: TASTList read FParamList;
    property PBlock: TASTList read FBlock;
    property PName: string read FName;
    property PIsDecorator: boolean read FIsDecorator;
    property PIsInstanceFunction: boolean read FIsInstance;
    constructor Create(AToken: TToken; AName: string; ABlock, AParamList: TASTList;
      AType: string; IsDecorator: boolean; AcceptVarargs: boolean; IsInstance: boolean);
    destructor Destroy; override;
  end;

  TDecoratorDefinition = class(TFunctionDefinition)

  end;

  TFunctionCall = class(TAST)
  protected
    FFuncName: string;
    FEvalParams: TASTList;
  public
    property PFuncName: string read FFuncName;
    property PEvalParams: TASTList read FEvalParams;

    constructor Create(AFuncName: string; AEvalParams: TASTList; AToken: TToken);
    destructor Destroy; override;
  end;

  TFunctionCallByInstance = class(TAST)
  protected
    FFuncInst: TAST;
    FEvalParams: TASTList;
  public
    property PFuncInst: TAST read FFuncInst;
    property PEvalParams: TASTList read FEvalParams;

    constructor Create(AFuncInst: TAST; AEvalParams: TASTList; AToken: TToken);
    destructor Destroy; override;
  end;

  TMethodCall = class(TAST)
  protected
    FSrc: TAST;
    FOper: TAST;
  public
    property PSrc: TAST read FSrc write FSrc;
    property POper: TAST read FOper write FOper;
    constructor Create(ASrc, AOper: TAST; AToken: TToken);
    destructor Destroy; override;
  end;

  TInterpolation = class(TAST)
  protected
    FOper: TAST;
  public
    property POper: TAST read FOper write FOper;
    constructor Create(AOper: TAST; AToken: TToken);
    destructor Destroy; override;

  end;

  TPlainTextEmbed = class(TAST)
  protected
    FNodes: TASTList;
  public
    property PNodes: TASTList read FNodes;
    constructor Create(ANodes: TASTList; AToken: TToken);
    destructor Destroy; override;
  end;

  TExpandArgs = class(TAST)
    protected
      FSrcList: TAST;
    public
      property PSrcList: TAST read FSrcList write FSrcList;
      constructor Create(ASrcList: TAST; AToken: TToken);
  end;

  TParam = class(TAST)
  protected
    FNode: TToken;
    FDefValue: TAST;
    FArgType: TAST;
  public
    property PNode: TToken read FNode write FNode;
    property PDefValue: TAST read FDefValue write FDefValue;
    property PArgType: TAST read FArgType write FArgType;
    constructor Create(ANode: TToken; ADefValue: TAST; ATyped: TAST = nil);
    destructor Destroy; override;

  end;

  TString = class(TAST)
  protected
    FValue: string;
  public
    property PValue: string read FValue write FValue;
    constructor Create(AToken: TToken);

  end;

  TNull = class(TAST)
  protected
    FValue: string;
  public
    property PValue: string read FValue write FValue;
    constructor Create(AToken: TToken);

  end;

  TBoolean = class(TAST)
  protected
    FValue: string;
  public
    property PValue: string read FValue write FValue;
    constructor Create(AToken: TToken);
  end;

  TVarAssign = class(TAST)
  protected
    // FLeft: TAST;
    // FRight: TAST;
    // FOper: TToken;
    FVarName: TToken;
    FValue: TAST;
  public
    property PVarName: TToken read FVarName;
    property PValue: TAST read FValue;
    constructor Create(AVarName: TToken; AValue: TAST);
    destructor Destroy; override;
  end;

  TLiveOutput = class(TAST)
  protected
    FValue: TAST;
  public
    property PValue: TAST read FValue write FValue;
    constructor Create(AValue: TAST; AToken: TToken);
    destructor Destroy; override;
  end;

  TLivePrint = class(TAST)
    constructor Create(AToken: TToken);
  end;

  TVariableReference = class(TAST)
  protected
    FValue: string;
  public
    constructor Create(AToken: TToken);
  end;

  TNoOp = class(TAST)
    constructor Create(AToken: TToken);
  end;

  TListAssign = class(TAST)
  protected
    FEntry: TAST;
    FSrc: TAST;
    FValue: TAST;
  public
    property PEntry: TAST read FEntry;
    property PValue: TAST read FValue;
    property PSrc: TAST read FSrc;
    constructor Create(ASrc, AEntry, AValue: TAST; AToken: TToken);
    destructor Destroy; override;
  end;

implementation

constructor TExpandArgs.Create(ASrcList: TAST; AToken: TToken);
begin
  FToken := AToken;
  FSrcList := ASrcList;
end;

function TAST.ToString: string;
begin
  if FToken <> nil then
    Result := ClassName + ': "' + FToken.PValue + '" at ' +
      IntToStr(FToken.PLineNo) + ':' + IntToStr(FToken.PCharNo) +
      ', in ' + FToken.PScriptName
  else
    Result := ClassName + ' (no token available)';
end;

constructor TClassDefinition.Create(AToken: TToken);
begin
  FToken := AToken;
end;

destructor TProgram.Destroy;
var
  len, i: integer;
  a: string;
begin
  //writeln('destruindo programa');
  len := Length(FPreludes);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      FPreludes[i].Free;
    end;
  end;
  len := Length(FChildren);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      FChildren[i].Free;
    end;
  end;
  inherited;
end;

constructor TAssignedTest.Create(AValue: TAST; AToken: TToken);
begin
  FValue := AValue;
  FToken := AToken;
end;

destructor TAssignedTest.Destroy;
begin
  FValue.Free;
  inherited;
end;

constructor TDictKeyNode.Create(AKey: TAST; AValue: TAST; AToken: TToken);
begin
  FKey := AKey;
  FValue := AValue;
  FToken := AToken;
end;

destructor TDictKeyNode.Destroy;
begin
  //writeln('destruindo dict node');
  FValue.Free;
  FKey.Free;
  inherited;
end;

constructor TListAssign.Create(ASrc, AEntry, AValue: TAST; AToken: TToken);
begin
  FSrc := ASrc;
  FEntry := AEntry;
  FValue := AValue;
end;

destructor TListAssign.Destroy;
begin
  FSrc.Free;
  FEntry.Free;
  FValue.Free;
  inherited;
end;

constructor TDictNode.Create(Akeys: TASTList; AToken: TToken);
begin
  FKeys := AKeys;
  FToken := AToken;
end;

destructor TDictNode.Destroy;
var
  len, i: integer;
begin
  //writeln('destruindo dicionario');
  len := Length(FKeys);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
      FKeys[i].Free;
  end;
  inherited;
end;

constructor TIncludeScript.Create(AFilename: TAST; AToken: TToken; ANamespace: string; IsModule: boolean; AModulePath: string);
begin
  FFileName := AFileName;
  FNamespace := ANamespace;
  FToken := AToken;
  FIsModule := IsModule;
  FModulePath := AModulePath;
end;

destructor TIncludeScript.Destroy;
begin
  FFileName.Free;
  inherited;
end;

constructor TNamespaceGet.Create(AName: string; AOper: TAST; AToken: TToken);
begin
  FName := AName;
  FOper := AOper;
  FToken := AToken;
end;

destructor TNamespaceGet.Destroy;
begin
  FOper.Free;
  inherited;
end;

constructor TNamespaceState.Create(AName: string; AOper: TAST; AToken: TToken);
begin
  FName := AName;
  FOper := AOper;
  FToken := AToken;
end;

destructor TNamespaceState.Destroy;
begin
  FOper.Free;
  inherited;
end;

constructor TNewObject.Create(AArgs: TASTList; AName: string; AToken: TToken);
begin
  FArgs := AArgs;
  FName := AName;
end;

constructor TNewObject.Create(AArgs: TASTList; ARef: TAST; AToken: TToken);
begin
  FArgs := AArgs;
  FRef := Aref;
end;

destructor TNewObject.Destroy;
var
  i, len: integer;
begin
  len := Length(FArgs);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      FArgs[i].Free;
    end;
  end;
  inherited;
end;

constructor TPlainText.Create(AValue: string; AToken: TToken);
begin
  FValue := AValue;
  FToken := AToken;
end;

constructor TAST.Create(AToken: TToken);
begin
  FToken := AToken;
end;

destructor TAST.Destroy;
begin
  //writeln('destruindo node: ' + self.classname);
  //writeln('tentando destruir token: < ' + FToken.PType + ' : '+ FToken.PValue + ' >');
  //FToken.Free();
  FreeAndNil(FToken);
  inherited;
end;

constructor TLivePrint.Create(AToken: TToken);
begin
  FToken := AToken;
end;

constructor TInterpolation.Create(AOper: TAST; AToken: TToken);
begin
  FOper := AOper;
  FToken := AToken;
end;

destructor TInterpolation.Destroy;
begin
  FOper.Free;
  inherited;
end;

constructor TPlainTextEmbed.Create(ANodes: TASTList; AToken: TToken);
begin
  FNodes := ANodes;
  FToken := AToken;
end;

destructor TPlainTextEmbed.Destroy;
var
  len, i: integer;
begin
  len := Length(FNodes);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      try
        FNodes[i].Free;
      finally
      end;

    end;
  end;
  inherited;
end;

constructor TLiveOutput.Create(AValue: TAST; AToken: TToken);
begin
  FValue := AValue;
  FToken := AToken;
end;

destructor TLiveOutput.Destroy;
begin
  FValue.Free;
  inherited;
end;

constructor TParam.Create(ANode: TToken; ADefValue: TAST; ATyped: TAST = nil);
begin
  FToken := ANode;
  FNode := ANode;
  FDefValue := ADefValue;
  FArgType := ATyped;
  logDebug('Creating a parameter node', 'AST');
end;

destructor TParam.Destroy;
begin
  try
    try
      FDefValue.Free;
    except
    end;
  finally
    inherited;
  end;
end;

constructor TListAccessAST.Create(AList: TAST; AIndex: TAST; AToken: TToken);
begin
  FToken := AToken;
  FList := AList;
  FIndex := AIndex;
end;

destructor TListAccessAST.Destroy;
begin
  FList.Free;
  FIndex.Free;
  inherited;
end;

constructor TListAST.Create(AToken: TToken; AARgs: TASTList);
begin
  FToken := AToken;
  FArgs := AArgs;
end;

destructor TListAST.Destroy;
var
  len, i: integer;
begin
  len := Length(FArgs);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      FArgs[i].Free;
    end;
  end;
  inherited;
end;

constructor TString.Create(AToken: TToken);
begin
  logdebug('Creating a string node', 'AST');
  FToken := AToken;
  FValue := AToken.PValue;
end;

constructor TNull.Create(AToken: TToken);
begin
  logdebug('Creating a null node', 'AST');
  FToken := AToken;
  FValue := AToken.PValue;
end;

constructor TBoolean.Create(AToken: TToken);
begin
  logdebug('Creating a boolean node', 'AST');
  FToken := AToken;
  FValue := AToken.PValue;
end;

constructor TMethodCall.Create(ASrc, AOper: TAST; AToken: TToken);
begin
  FSrc := ASrc;
  FOper := AOper;
  FToken := AToken;
end;

destructor TMethodCall.Destroy;
begin
  FSrc.Free;
  FOper.Free;
  inherited;
end;

constructor TFunctionCall.Create(AFuncName: string; AEvalParams: TASTList; AToken: TToken);
begin
  FToken := AToken;
  FFuncName := AFuncName;
  FEvalParams := AEvalParams;
end;

destructor TFunctionCall.Destroy;
var
  len, i: integer;
begin
  len := Length(FEvalParams);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      FEvalParams[i].Free;
    end;
  end;
  inherited;
end;

constructor TFunctionCallByInstance.Create(AFuncInst: TAST; AEvalParams: TASTList; AToken: TToken);
begin
  FToken := AToken;
  FFuncInst := AFuncInst;
  FEvalParams := AEvalParams;
end;

destructor TFunctionCallByInstance.Destroy;
var
  len, i: integer;
begin
  len := Length(FEvalParams);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      FEvalParams[i].Free;
    end;
  end;
  inherited;
end;

constructor TLoadType.Create(ATypeNames: TASTList; AToken: TToken);
begin
  FTypeNames := ATypeNames;
  FToken := AToken;
end;

destructor TLoadType.Destroy;
var
  AT: TAST;
begin
  for AT in FTypeNames do
    AT.Free;
  inherited;
end;

constructor TFunctionDefinition.Create(AToken: TToken; AName: string;
  ABlock, AParamList: TASTList; AType: string; IsDecorator: boolean; AcceptVarargs:boolean; IsInstance: boolean);
begin
  FName := AName;
  FBlock := ABlock;
  FToken := AToken;
  FType := AType;
  FAcceptVarargs := AcceptVarargs;
  FIsInstance := IsInstance;
  FIsDecorator := IsDecorator;
  logDebug('Creating a function definition named ' + AName, 'AST');
  FParamList := AParamList;
end;

destructor TFunctionDefinition.Destroy;
var
  len, i: integer;
begin
  len := Length(FBlock);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
      FBlock[i].Free;
  end;
  len := Length(FParamList);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
      FParamList[i].Free;
  end;
  inherited;
end;

constructor TNoOp.Create(AToken: TToken);
begin
  FToken := AToken;
  LogText(DEBUG, 'AST', 'Creating a NoOp Node');
end;

// --------- Program

constructor TProgram.Create(AToken: TToken);
begin
  FToken := AToken;
  SetLength(FChildren, 0);
  LogText(DEBUG, 'AST', 'Starting a new program');
end;

function TProgram.Count: integer;
begin
  Result := Length(FChildren);
end;

procedure TProgram.AddPrelude(ANode: TAST);
var
  len: integer;
begin
  len := Length(FPreludes);
  len := len + 1;
  SetLength(FPreludes, len);
  FPreludes[len - 1] := ANode;
end;

procedure TProgram.Add(ANode: TAST);
var
  Len: integer;
begin
  Len := Length(FChildren);
  SetLength(FChildren, Len + 1);
  FChildren[Len] := ANode;
end;

// -------------- VarAssign

constructor TVarAssign.Create(AVarName: TToken; AValue: TAST);
begin
  {FLeft := ALeft;
  FRight := ARight;
  FOper := AOper;
  FToken := AOper;}
  FToken := AVarName;
  FVarName := AVarName;
  FValue := AValue;
  // LogText(DEBUG, 'AST', 'Assigning '+ FRight.ClassName +' to var '+ FLeft.ClassName );
end;

destructor TVarAssign.Destroy;
begin
  //writeln('destruindo varass');
  FValue.Free;
  inherited;
end;

// ------------- Variable

constructor TVariableReference.Create(AToken: TToken);
begin
  FToken := AToken;
  FValue := AToken.PValue;
  LogText(DEBUG, 'AST', 'Creating a Variable reference node for ' + FToken.AsString);
end;



end.
