// teste
else if AType = 'TServerInstance' then
begin
  if FName = 'init' then
    CreateServer(TServerInstance(FObj))
  else if FName = 'setPort' then
    SetServerPort(TServerInstance(FObj))
  else if FName = 'setTitle' then
    SetAppTitle(TServerInstance(FObj))
  else if FName = 'setRootFile' then
    SetRootFile(TServerInstance(FObj))
  else if FName = 'setStopRoute' then
    SetStopServerRoute(TServerInstance(FObj))
  else if FName = 'run' then
    RunServer(TServerInstance(FObj));
end
