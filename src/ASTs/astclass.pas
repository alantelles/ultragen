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
    function ToString:string; override;
    constructor Create(AToken: TToken);
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
    procedure Add(ANode: TAST);
    procedure AddPrelude(ANode: TAST);
    function Count: integer;
  end;

  TDictKeyNode = class (TAST)
    protected
      FKey: TAST;
      FValue: TAST;
    public
      property PKey: TAST read Fkey;
      property PValue: TAST read FValue;
      constructor Create(AKey: TAST; AValue: TAST; AToken: TToken);
  end;

  TDictNode = class (TAST)
    protected
      FKeys: TASTList;
    public
      property PKeys: TASTList read Fkeys;
      constructor Create(AKeys: TASTList; AToken: TToken);
  end;

  TNewObject = class (TAST)
    private
      FArgs: TASTList;
      FName: string;
    public
      property PArgs: TASTList read FArgs write FArgs;
      property PName: string read Fname write FName;
      constructor Create(AArgs: TASTList; AName: string; AToken: TToken);
  end;

  TNamespaceGet = class (TAST)
    protected
      FName: string;
      FOper: TAST;
    public
      property PName: string read FName;
      property POper: TAST read FOper;
      constructor Create(AName:string; AOper: TAST; AToken: TToken);
  end;

  TNamespaceState = class (TAST)
    protected
      FName: string;
      FOper: TAST;
    public
      property PName: string read FName;
      property POper: TAST read FOper;
      constructor Create(AName:string; AOper: TAST; AToken: TToken);
  end;

  TIncludeScript = class (TAST)
    protected
      FFileName: TAST;
      FNamespace: string;
    public
      property PFilename: TAST read FFileName;
      property PNamespace: string read FNamespace;
      constructor Create(AFileName: TAST; AToken: TToken; ANamespace:string);
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
     constructor Create(AToken: TToken);
  end;

  TListAssign = class (TAST)
  protected
    FEntry: TAST;
    FSrc: TAST;
    FValue: TAST;
  public
    property PEntry: TAST read FEntry;
    property PValue: TAST read FValue;
    property PSrc: TAST read FSrc;
    constructor Create(ASrc, AEntry, AValue: TAST; AToken: TToken);
  end;

implementation

function TAST.ToString:string;
begin
  if FToken <> nil then
    Result := ClassName + ': "' + FToken.PValue + '" at ' +
      IntToStr(FToken.PLineNo) + ':'+ IntToStr(FToken.PCharNo) + ', in ' + FToken.PScriptName
  else
    Result := ClassName + ' (no token available)';
end;

constructor TDictKeyNode.Create(AKey: TAST; AValue: TAST; AToken: TToken);
begin
  FKey := AKey;
  FValue := AValue;
  FToken := AToken;
end;

constructor TListAssign.Create(ASrc, AEntry, AValue: TAST; AToken: TToken);
begin
  FSrc := ASrc;
  FEntry := AEntry;
  FValue := AValue;
end;

constructor TDictNode.Create(Akeys: TASTList; AToken: TToken);
begin
  FKeys := AKeys;
  FToken := AToken;
end;

constructor TIncludeScript.Create(AFilename: TAST; AToken: TToken; ANamespace: string);
begin
  FFileName := AFileName;
  FNamespace := ANamespace;
  FToken := AToken;
end;

constructor TNamespaceGet.Create(AName: string;AOper: TAST; AToken: TToken);
begin
  FName := AName;
  FOper := AOper;
  FToken := AToken;
end;

constructor TNamespaceState.Create(AName: string;AOper: TAST; AToken: TToken);
begin
  FName := AName;
  FOper := AOper;
  FToken := AToken;
end;

constructor TNewObject.Create(AArgs: TASTList; AName: string; AToken: TToken);
begin
  FArgs := AArgs;
  FName := AName;
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
  FToken := ANode;
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



constructor TFunctionDefinition.Create(AToken: TToken; AName:string; ABlock, AParamList: TASTList; AType:string);
begin
  FName := AName;
  FBlock := ABlock;
  FToken := AToken;
  FType := AType;
  logDebug('Creating a function definition named ' + AName, 'AST');
  FParamList := AParamList;
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

// ------------- Variable

constructor TVariableReference.Create(AToken: TToken);
begin
  FToken := AToken;
  FValue := AToken.PValue;
  LogText(DEBUG, 'AST', 'Creating a Variable reference node for '+ FToken.AsString);
end;



end.

