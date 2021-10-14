procedure VisitProgram(ANode: TProgram);
procedure VisitNoOp(ANode: TNoOp);
function VisitIncludeScript(ANode: TIncludeScript): TInstanceOf;

procedure VisitLiveOutput(Anode: TLiveOutput);
function VisitLivePrint(ANode: TLivePrint):TStringInstance;
procedure VisitPlainText(ANode: TPlainText);
procedure VisitPlainTextEmbed(ANode: TPlainTextEmbed);
procedure VisitInterpolation(ANode: TInterpolation);
function VisitNameSpaceGet(Anode: TNamespaceGet):TInstanceOf;
function VisitNamespaceState(ANode: TNamespaceState): TInstanceOf;
function VisitNewObject(ANode: TNewObject): TInstanceOf;
function VisitDict(ANode: TDictNode): TDictionaryInstance;
procedure VisitClassDefinition(ANode: TClassDefinition);
procedure VisitLoadType(ANode: TLoadType);
function VisitExpandArgs(ANode: TExpandArgs): TListInstance;
function VisitAssignedTest(ANode: TAssignedTest): TBooleanInstance;

// references
procedure VisitVarAssign(ANode: TVarAssign; ASrc: TInstanceOf);
procedure VisitListAssign(ANode: TListAssign);
function VisitVariableReference(ANode: TVariableReference; ASrc: TInstanceOf{ = nil}):TInstanceOf;
function VisitFunctionCall(ANode: TFunctionCall; ASrcInstance: TInstanceOf = nil):TInstanceOf;
function VisitDecoratorCall(AFunctionInstance: TFunctionInstance; ADecorated: TASTList):TInstanceOf;
function VisitFunctionCallByInstance(ANode: TFunctionCallByinstance; ASrcInstance: TInstanceOf = nil):TInstanceOf;
function VisitMethodCall(ANode: TMethodCall):TInstanceOf;
function VisitListAccess(ANode: TListAccessAST): TInstanceOf;


// operations
function VisitBinOp(ANode:TBinOp):TInstanceOf;
function VisitBinLogicOp(ANode: TBinLogicOp): TBooleanInstance;

// types
function VisitBoolean(ANode:TBoolean):TBooleanInstance;
function VisitNumInt(ANode:TNumInt):TIntegerInstance;
function VisitByte(ANode:TByte): TByteInstance;
function VisitNull(ANode:TNull):TNullInstance;
function VisitNumFloat(ANode:TNumFloat):TFloatInstance;
function VisitString(ANode:TString):TStringInstance;
function VisitFunctionDefinition(ANode: TFunctionDefinition): TFunctionInstance;
function VisitDecoratorDefinition(ANode: TDecoratorDefinition): TDecoratorInstance;

function VisitList(ANode: TListAST): TListInstance;



//overloads
function VisitUnaryOp(ANode: TUnaryOp):TIntegerInstance;
function VisitUnaryOpFloat(ANode: TUnaryOp):TFloatInstance;
function VisitUnaryLogicOp(ANode: TUnaryLogicOp):TBooleanInstance;


// flow
function VisitReturn(ANode: TReturnFunction):TInstanceOf;
function VisitIfCondition(ANode: TIfConditionBlock):TBooleanInstance;
procedure VisitConditional(ANode: TConditional);
procedure VisitWhileLoop(ANode: TWhileLoop);
procedure VisitForLoop(ANode: TForLoop);
procedure VisitBreak(Anode: TBreakLoop);
procedure VisitContinue(Anode: TContinueLoop);
