  if ANode.ClassNameIs('TProgram') then
  begin
    VisitProgram(TProgram(ANode));
  end
  else if ANode.ClassNameIs('TVarAssign') then   
    VisitVarAssign(TVarAssign(ANode), ASrcInstance)

  else if ANode.ClassNameIs('TExpandArgs') then
    Ret := VisitExpandArgs(TExpandArgs(ANode))

  else if ANode.ClassNameIs('TListAssign') then
    VisitListAssign(TListAssign(ANode))

  else if Anode.ClassNameIs('TLoadType') then
    VisitLoadType(TLoadType(ANode))

  else if ANode.ClassNameIs('TVariableReference') then
    Ret := VisitVariableReference(TVariableReference(ANode), ASrcInstance)

  else if ANode.ClassNameIs('TIncludeScript') then
    VisitIncludeScript(TIncludeScript(ANode))

  else if ANode.ClassNameIs('TNamespaceGet') then
    Ret := VisitNamespaceGet(TNamespaceGet(Anode))

  else if ANode.ClassNameIs('TClassDefinition') then
    VisitClassDefinition(TClassDefinition(ANode))



  else if ANode.ClassNameIs('TDictNode') then
    Ret := VisitDict(TDictNode(ANode))

  else if ANode.ClassNameIs('TNamespaceState') then
    Ret := VisitNamespaceState(TNamespaceState(Anode))

  else if ANode.ClassNameIs('TNewObject') then
    Ret := VisitNewObject(TNewObject(ANode))

  else if ANode.ClassNameIs('TPlainTextEmbed') then
    VisitPlainTextEmbed(TPlainTextEmbed(Anode))
  else if ANode.ClassNameIs('TInterpolation') then
    VisitInterpolation(TInterpolation(ANode))
  else if ANode.ClassNameIs('TLiveOutput') then
    VisitLiveOutput(TLiveOutput(Anode))
  else if ANode.ClassNameIs('TPlainText') then
    VisitPlainText(TPlainText(Anode))

  else if ANode.ClassNameIs('TLivePrint') then
    Ret := VisitLivePrint(TLivePrint(ANode))
  else if ANode.ClassNameIs('TMethodCall') then
    Ret := VisitMethodCall(TMethodCall(Anode))
  else if ANode.ClassNameIs('TFunctionCall') then
    Ret := VisitFunctionCall(TFunctionCall(Anode), ASrcInstance)

  else if ANode.ClassNameIs('TFunctionCallByInstance') then
    Ret := VisitFunctionCallByInstance(TFunctionCallByInstance(Anode), ASrcInstance)

  else if ANode.ClassNameIs('TReturnFunction') then
    Ret := VisitReturn(TReturnFunction(ANode))

  else if ANode.ClassNameIs('TFunctionDefinition') then
    Ret := VisitFunctionDefinition(TFunctionDefinition(Anode))

  else if ANode.ClassNameIs('TDecoratorDefinition') then
    Ret := VisitDecoratorDefinition(TDecoratorDefinition(Anode))

  else if Anode.ClassNameIs('TNumInt') then
    Ret := VisitNumInt(TNumInt(ANode))
  else if Anode.ClassNameIs('TNull') then
    Ret := VisitNull(TNull(ANode))
  else if Anode.ClassNameIs('TNumFloat') then
    Ret := VisitNumFloat(TNumFloat(ANode))
  else if Anode.ClassNameIs('TListAST') then
    Ret := VisitList(TListAST(ANode))
  else if Anode.ClassNameIs('TListAccessAST') then
    Ret := VisitListAccess(TListAccessAST(ANode))
  else if Anode.ClassNameIs('TString') then
    Ret := VisitString(TString(ANode))
  else if Anode.ClassNameIs('TBoolean') then
    Ret := VisitBoolean(TBoolean(ANode))
  else if ANode.ClassNameIs('TUnaryOp') then
  begin
      Ret := VisitUnaryOp(TUnaryOp(ANode))
  end
  else if ANode.ClassNameIs('TBinOp') then
    Ret := VisitBinOp(TBinOp(ANode))
  else if ANode.ClassNameIs('TBinLogicOp') then
    Ret := VisitBinLogicOp(TBinLogicOp(ANode))
  else if ANode.ClassNameIs('TUnaryLogicOp') then
    Ret := VisitUnaryLogicOp(TUnaryLogicOp(ANode))
  else if ANode.ClassNameIs('TConditional') then
    VisitConditional(TConditional(ANode))
  else if ANode.ClassNameIs('TIfConditionBlock') then
    VisitIfCondition(TIfConditionBlock(ANode))
  else if ANode.ClassNameIs('TWhileLoop') then
    VisitWhileLoop(TWhileLoop(ANode))
  else if ANode.ClassNameIs('TForLoop') then
    VisitForLoop(TForLoop(ANode))
  else if ANode.ClassNameIs('TBreakLoop') then
    VisitBreak(TBreakLoop(ANode))
  else if ANode.ClassNameIs('TContinueLoop') then
    VisitContinue(TContinueLoop(ANode));
