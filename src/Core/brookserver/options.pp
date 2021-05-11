//Brook
else if AType = 'TBrookServerInstance' then
begin
  if FName = 'run' then
    RunBrookServer(TBrookServerInstance(FObj))
  else if FName = 'redirect' then
  begin
    if (Length(FParams) > 2) or (Length(FParams) = 0) then
      FInter.RaiseException(E_INVALID_ARGS, 'Argumets');
    if Length(FParams) > 0 then
      TBrookServerInstance(FObj).PMembers.Add('redirect', TStringInstance(FParams[0]));

    if Length(FParams) = 2 then
      TBrookServerInstance(FObj).PMembers.Add('redirect', TStringInstance(FParams[1]));
  end;
end
