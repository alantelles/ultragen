// teste
else if AType = 'TDictionaryInstance' then
begin
  if FName = 'set' then
    SetItem(TDictionaryInstance(AObj))
  else if FName = 'get' then
    Ret := GetItem(TDictionaryInstance(AObj))
  else if FName = 'keys' then
    Ret := GetKeys(TDictionaryInstance(AObj));
end
