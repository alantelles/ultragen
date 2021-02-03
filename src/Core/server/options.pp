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
  else if FName = 'setStaticPath' then
    TServerInstance(FObj).SetStaticPath(
      TStringInstance(FParams[0]).PValue,
      TStringInstance(FParams[1]).PValue
    )
  else if FName = 'setStaticPaths' then
    TServerInstance(FObj).SetStaticPaths(TDictionaryInstance(FParams[0]).PValue)
  else if FName = 'setMimeTypesFile' then
    TServerInstance(FObj).SetMimeTypesFile(TStringInstance(FParams[0]).PValue)

  else if FName = 'run' then
    RunServer(TServerInstance(FObj));
end
