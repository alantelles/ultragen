//Brook
else if AType = 'TBrookServerInstance' then
begin
  if FName = 'run' then
    RunBrookServer(TBrookServerInstance(FObj));
end
