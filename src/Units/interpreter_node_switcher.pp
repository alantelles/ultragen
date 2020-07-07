  if ANode.ClassNameIs('TProgram') then
  begin
    VisitProgram(TProgram(ANode));
  end
  else if ANode.ClassNameIs('TVarAssign') then   
    VisitVarAssign(TVarAssign(ANode))
  else if ANode.ClassNameIs('TVariableReference') then
    Result := VisitVariableReference(TVariableReference(ANode))

  else if ANode.ClassNameIs('TIncludeScript') then
    VisitIncludeScript(TIncludeScript(ANode))

  else if ANode.ClassNameIs('TNamespaceGet') then
    Result := VisitNamespaceGet(TNamespaceGet(Anode))

  else if ANode.ClassNameIs('TNamespaceState') then
    Result := VisitNamespaceState(TNamespaceState(Anode))

  else if ANode.ClassNameIs('TPlainTextEmbed') then
    VisitPlainTextEmbed(TPlainTextEmbed(Anode))
  else if ANode.ClassNameIs('TInterpolation') then
    VisitInterpolation(TInterpolation(ANode))
  else if ANode.ClassNameIs('TLiveOutput') then
    VisitLiveOutput(TLiveOutput(Anode))
  else if ANode.ClassNameIs('TPlainText') then
    VisitPlainText(TPlainText(Anode))

  else if ANode.ClassNameIs('TLivePrint') then
    Result := VisitLivePrint(TLivePrint(ANode))
  else if ANode.ClassNameIs('TMethodCall') then
    Result := VisitMethodCall(TMethodCall(Anode))
  else if ANode.ClassNameIs('TFunctionCall') then
    Result := VisitFunctionCall(TFunctionCall(Anode), ASrcInstance)
  else if ANode.ClassNameIs('TReturnFunction') then
    Result := VisitReturn(TReturnFunction(ANode))
  else if ANode.ClassNameIs('TFunctionDefinition') then
    VisitFunctionDefinition(TFunctionDefinition(Anode))
  else if Anode.ClassNameIs('TNumInt') then
    Result := VisitNumInt(TNumInt(ANode))
  else if Anode.ClassNameIs('TNull') then
    Result := VisitNull(TNull(ANode))
  else if Anode.ClassNameIs('TNumFloat') then
    Result := VisitNumFloat(TNumFloat(ANode))
  else if Anode.ClassNameIs('TListAST') then
    Result := VisitList(TListAST(ANode))
  else if Anode.ClassNameIs('TListAccessAST') then
    Result := VisitListAccess(TListAccessAST(ANode))
  else if Anode.ClassNameIs('TString') then
    Result := VisitString(TString(ANode))
  else if Anode.ClassNameIs('TBoolean') then
    Result := VisitBoolean(TBoolean(ANode))
  else if ANode.ClassNameIs('TUnaryOp') then
  begin
      Result := VisitUnaryOp(TUnaryOp(ANode))
  end
  else if ANode.ClassNameIs('TBinOp') then
    Result := VisitBinOp(TBinOp(ANode))
  else if ANode.ClassNameIs('TBinLogicOp') then
    Result := VisitBinLogicOp(TBinLogicOp(ANode))
  else if ANode.ClassNameIs('TUnaryLogicOp') then
    Result := VisitUnaryLogicOp(TUnaryLogicOp(ANode))
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
