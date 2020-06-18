unit ASTClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TokenClass, Tokens, LoggingClass;

type

  TAST = class
  protected
    FToken: TToken;
  public
    property PToken: TToken read FToken write FToken;
  end;

  TASTList = array of TAST;

  TProgram = class(TAST)
  protected
    FChildren: TASTList;
  public
    property PChildren: TASTList read FChildren write FChildren;
    constructor Create;
    procedure Add(ANode: TAST);
    function Count: integer;
  end;

  TListAST = class (TAST)
    private
      FArgs: TASTList;
    public
      property PArgs: TASTList read FArgs write FArgs;
      constructor Create(AToken: TToken; AArgs: TASTList);
  end;

  TListAccessAST = class (TAST)
    private
      FList: TAST;
      FIndex: TAST;
    public
      property PIndex: TAST read FIndex write FIndex;
      property PList: TAST read FList write FList;
      constructor Create(AList: TAST; AIndex: TAST; AToken: TToken);
  end;

  TFunctionDefinition = class (TAST)
    protected
      FName: string;
      FBlock: TASTList;
      FParamList: TASTList;
    public
      property PParamList: TASTList read FParamList;
      property PBlock: TASTList read FBlock;
      property PName:string read FName;
      constructor Create(AToken: TToken; AName:string; ABlock: TASTList; AParamList: TASTList);
  end;

  TFunctionCall = class(TAST)
    protected
      FFuncName:string;
      FEvalParams:TASTList;
    public
      property PFuncName:string read FFuncName;
      property PEvalParams: TASTList read FEvalParams;
      constructor Create(AFuncName:string; AEvalParams: TASTList; AToken:TToken);
  end;

  TMethodObjCall = class(TAST)
    protected
      FParent: TAST;
      FFunction: TAST;
    public
      property PParent: TAST read FParent write FParent;
      property PFunction: TAST read FFunction write FFunction;
      constructor Create(AParent: TAST; AFunction: TAST);
  end;

  TParam = class (TAST)
    protected
      FNode: TToken;
    public
      constructor Create(ANode: TToken);

  end;

  TString = class(TAST)
    protected
      FValue: string;
    public
      property PValue:string read FValue write FValue;
      constructor Create(AToken: TToken);
  end;

  TNull = class(TAST)
    protected
      FValue: string;
    public
      property PValue:string read FValue write FValue;
      constructor Create(AToken: TToken);
  end;

  TBoolean = class (TAST)
    protected
      FValue: string;
    public
      property PValue:string read FValue write FValue;
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
  end;

  TVariableReference = class(TAST)
  protected
    FValue: string;
  public
    constructor Create(AToken: TToken);
  end;

  TNoOp = class (TAST)
     constructor Create;
  end;

implementation

constructor TParam.Create(ANode:TToken);
begin
  FNode := ANode;
  logDebug('Creating a parameter node', 'AST');
end;

constructor TListAccessAST.Create(AList: TAST; AIndex: TAST; AToken: TToken);
begin
  FToken := AToken;
  FList := AList;
  FIndex := AIndex;
end;

constructor TListAST.Create(AToken: TToken; AARgs: TASTList);
begin
  FToken := AToken;
  FArgs := AArgs;
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

constructor TFunctionCall.Create(AFuncName:string; AEvalParams: TASTList; AToken:TToken);
begin
  FToken := AToken;
  FFuncName := AFuncName;
  FEvalParams := AEvalParams;
end;

constructor TMethodObjCall.Create(AParent: TAST; AFunction: TAST);
begin
  FParent := AParent;
  FFunction := AFunction;
end;

constructor TFunctionDefinition.Create(AToken: TToken; AName:string; ABlock: TASTList; AParamList: TASTList);
begin
  FName := AName;
  FBlock := ABlock;
  FToken := AToken;
  logDebug('Creating a function definition named ' + AName, 'AST');
  FParamList := AParamList;
end;

constructor TNoOp.Create;
begin
  LogText(DEBUG, 'AST', 'Creating a NoOp Node');
end;

// --------- Program

constructor TProgram.Create;
begin
  SetLength(FChildren, 0);
  LogText(DEBUG, 'AST', 'Starting a new program');
end;

function TProgram.Count: integer;
begin
  Result := Length(FChildren);
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

// ------------- Variable

constructor TVariableReference.Create(AToken: TToken);
begin
  FToken := AToken;
  FValue := AToken.PValue;
  LogText(DEBUG, 'AST', 'Creating a Variable reference node for '+ FToken.AsString);
end;



end.

