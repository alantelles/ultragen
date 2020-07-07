procedure VisitProgram(ANode: TProgram);
procedure VisitNoOp(ANode: TNoOp);
procedure VisitIncludeScript(ANode: TIncludeScript);

procedure VisitLiveOutput(Anode: TLiveOutput);
function VisitLivePrint(ANode: TLivePrint):TStringInstance;
procedure VisitPlainText(ANode: TPlainText);
procedure VisitPlainTextEmbed(ANode: TPlainTextEmbed);
procedure VisitInterpolation(ANode: TInterpolation);
function VisitNameSpaceGet(Anode: TNamespaceGet):TInstanceOf;
function VisitNamespaceState(ANode: TNamespaceState): TInstanceOf;

// references
procedure VisitVarAssign(ANode: TVarAssign);
function VisitVariableReference(ANode: TVariableReference):TInstanceOf;
function VisitFunctionCall(ANode: TFunctionCall; ASrcInstance: TInstanceOf = nil):TInstanceOf;
function VisitMethodCall(ANode: TMethodCall):TInstanceOf;
function VisitListAccess(ANode: TListAccessAST): TInstanceOf;


// operations
function VisitBinOp(ANode:TBinOp):TInstanceOf;
function VisitBinLogicOp(ANode: TBinLogicOp): TBooleanInstance;

// types
function VisitBoolean(ANode:TBoolean):TBooleanInstance;
function VisitNumInt(ANode:TNumInt):TIntegerInstance;
function VisitNull(ANode:TNull):TNullInstance;
function VisitNumFloat(ANode:TNumFloat):TFloatInstance;
function VisitString(ANode:TString):TStringInstance;
function VisitFunctionDefinition(ANode: TFunctionDefinition): TInstanceOf;
function VisitList(ANode: TListAST): TListInstance;



//overloads
function VisitUnaryOp(ANode: TUnaryOp):TFloatInstance;
function VisitUnaryOp(ANode: TUnaryOp):TIntegerInstance;
function VisitUnaryLogicOp(ANode: TUnaryLogicOp):TBooleanInstance;


// flow
function VisitReturn(ANode: TReturnFunction):TInstanceOf;
function VisitIfCondition(ANode: TIfConditionBlock):TBooleanInstance;
procedure VisitConditional(ANode: TConditional);
procedure VisitWhileLoop(ANode: TWhileLoop);
procedure VisitForLoop(ANode: TForLoop);
procedure VisitBreak(Anode: TBreakLoop);
procedure VisitContinue(Anode: TContinueLoop);
