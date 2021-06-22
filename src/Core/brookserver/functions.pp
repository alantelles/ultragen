//Brook
procedure TCoreFunction.RunBrookServer(AObj: TBrookServerInstance);
begin
  if Length(FParams) <> 1 then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  AObj.RunServer(FPArams[0].PBoolValue);
end;
