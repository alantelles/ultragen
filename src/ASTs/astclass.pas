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
    constructor Create(AToken: TToken);
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

  TIncludeScript = class (TAST)
    protected
      FFileName: TAST;
    public
      property PFilename: TAST read FFileName;
      constructor Create(AFileName: TAST; AToken: TToken);
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

  TPlainText = class (TAST)
    protected
      FValue: string;
    public
      property PValue: string read FValue;
      constructor Create(AValue: string; AToken: TToken);
	end;

  TFunctionDefinition = class (TAST)
    protected
      FName: string;
      FBlock: TASTList;
      FParamList: TASTList;
      FType: string;
    public
      property PType: string read FType;
      property PParamList: TASTList read FParamList;
      property PBlock: TASTList read FBlock;
      property PName:string read FName;
      constructor Create(AToken: TToken; AName:string; ABlock, AParamList: TASTList; AType: string);
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

  TMethodCall = class(TAST)
    protected
      FSrc: TAST;
      FOper: TAST;
    public
      property PSrc: TAST read FSrc write FSrc;
      property POper: TAST read FOper write FOper;
      constructor Create(ASrc, AOper: TAST; AToken:TToken);
  end;

  TInterpolation = class (TAST)
    protected
      FOper: TAST;
    public
      property POper: TAST read FOper write FOper;
      constructor Create(AOper: TAST; AToken: TToken);

	end;

  TPlainTextEmbed = class (TAST)
    protected
      FNodes: TASTList;
    public
      property PNodes: TASTList read FNodes;
      constructor Create(ANodes: TASTList; AToken: TToken);
	end;

  TParam = class (TAST)
    protected
      FNode: TToken;
      FDefValue: TAST;
    public
      property PNode: TToken read FNode;
      property PDefValue: TAST read FDefValue;
      constructor Create(ANode: TToken; ADefValue: TAST = nil);

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

  TLiveOutput = class (TAST)
  protected
    FValue: TAST;
  public
    property PValue: TAST read FValue write FValue;
    constructor Create(AValue: TAST; AToken: TToken);
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

  TNoOp = class (TAST)
     constructor Create;
  end;

implementation

constructor TIncludeScript.Create(AFilename: TAST; AToken: TToken);
begin
  FFileName := AFileName;
  FToken := AToken;
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

constructor TLivePrint.Create(AToken: TToken);
begin
  FToken := AToken;
end;

constructor TInterpolation.Create(AOper: TAST; AToken: TToken);
begin
  FOper := AOper;
  FToken := AToken;
end;

constructor TPlainTextEmbed.Create(ANodes: TASTList; AToken: TToken);
begin
  FNodes := ANodes;
  FToken := AToken;
end;

constructor TLiveOutput.Create(AValue: TAST; AToken: TToken);
begin
  FValue := AValue;
  FToken := AToken;
end;

constructor TParam.Create(ANode:TToken; ADefValue: TAST = nil);
begin
  FNode := ANode;
  FDefValue := ADefValue;
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

constructor TMethodCall.Create(ASrc, AOper: TAST; AToken: TToken);
begin
  FSrc := ASrc;
  FOper:= AOper;
  FToken := AToken;
end;

constructor TFunctionCall.Create(AFuncName:string; AEvalParams: TASTList; AToken:TToken);
begin
  FToken := AToken;
  FFuncName := AFuncName;
  FEvalParams := AEvalParams;
end;


constructor TFunctionDefinition.Create(AToken: TToken; AName:string; ABlock, AParamList: TASTList; AType: string);
begin
  FName := AName;
  FBlock := ABlock;
  FToken := AToken;
  FType := AType;
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

