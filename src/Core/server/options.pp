// teste
else if AType = 'TServerInstance' then
begin
  if FName = 'setStaticPath' then
    TServerInstance(FObj).SetStaticPath(
      TStringInstance(FParams[0]).PValue,
      TStringInstance(FParams[1]).PValue
    )
  else if FName = 'setStaticPaths' then
    TServerInstance(FObj).SetStaticPaths(TDictionaryInstance(FParams[0]).PValue)
  //else if FName = 'setMimeTypesFile' then
  //  TServerInstance(FObj).SetMimeTypesFile(TStringInstance(FParams[0]).PValue)

  else if FName = 'run' then
    RunServer(TServerInstance(FObj));
end
