//Brook
procedure TCoreFunction.RunBrookServer(AObj: TBrookServerInstance);
begin
  if Length(FParams) <> 1 then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  if FPArams[0].PBoolValue then
    AObj.RunServerReloading
  else
    AObj.RunServer;
end;
