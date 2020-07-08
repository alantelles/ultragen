// teste
else if AType = 'TActRecInstance' then
begin
  if FName = 'set' then
    SetItem(TActRecInstance(AObj))
  else if FName = 'get' then
    Ret := GetItem(TActRecInstance(AObj))
  else if FName = 'keys' then
    Ret := GetKeys(TActRecInstance(AObj));
end
