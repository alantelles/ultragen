// options
else if AType = 'THttpClientInstance' then
begin
  if FName = 'get' then
  begin
    if Length(FParams) = 1 then
      Ret := THttpClientInstance.RequestGet(TStringInstance(FParams[0]).PValue)
    else if Length(FParams) = 2 then
      Ret := THttpClientInstance.RequestGet(
        TStringInstance(FParams[0]).PValue,
        TDictionaryInstance(FParams[1])
        )
    else
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  end
  else if FName = 'post' then
  begin
    if Length(FParams) = 2 then
      Ret := THttpClientInstance.RequestPost(TStringInstance(FParams[0]).PValue,FParams[1])
    else if Length(FParams) = 3 then
      Ret := THttpClientInstance.RequestPost(
        TStringInstance(FParams[0]).PValue,
        FParams[1],
        TDictionaryInstance(FParams[2])
        )
    else
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  end;
end
