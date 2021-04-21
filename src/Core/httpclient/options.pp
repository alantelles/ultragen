// options
else if AType = 'THttpClientInstance' then
begin
  if FName = 'run' then
  begin
    if Length(FParams) = 2 then
    begin
      Ret := THttpClientInstance(FObj).DoRequest(TStringInstance(FParams[0]).PValue, TStringInstance(FParams[1]).PValue);
    end
    else
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  end
  else if FName = 'get' then
  begin
    if Length(FParams) = 1 then
      Ret := THttpClientInstance.RequestGet('get', TStringInstance(FParams[0]).PValue)
    else if Length(FParams) = 2 then
      Ret := THttpClientInstance.RequestGet(
        'get',
        TStringInstance(FParams[0]).PValue,
        TDictionaryInstance(FParams[1])
        )
    else
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  end
  else if FName = 'put' then
  begin
    if Length(FParams) = 2 then
      Ret := THttpClientInstance.RequestPost('put', TStringInstance(FParams[0]).PValue,FParams[1])
    else if Length(FParams) = 3 then
      Ret := THttpClientInstance.RequestPost(
        'put',
        TStringInstance(FParams[0]).PValue,
        FParams[1],
        TDictionaryInstance(FParams[2])
        )
    else
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  end
  else if FName = 'delete' then
  begin
    if Length(FParams) = 1 then
      Ret := THttpClientInstance.RequestGet('delete', TStringInstance(FParams[0]).PValue)
    else if Length(FParams) = 2 then
      Ret := THttpClientInstance.RequestGet(
        'delete',
        TStringInstance(FParams[0]).PValue,
        TDictionaryInstance(FParams[2])
        )
    else
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  end
  else if FName = 'post' then
  begin
    if Length(FParams) = 2 then
      Ret := THttpClientInstance.RequestPost('post', TStringInstance(FParams[0]).PValue,FParams[1])
    else if Length(FParams) = 3 then
      Ret := THttpClientInstance.RequestPost(
        'post',
        TStringInstance(FParams[0]).PValue,
        FParams[1],
        TDictionaryInstance(FParams[2])
        )
    else
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  end;
end
