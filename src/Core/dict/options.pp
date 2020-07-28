// teste
else if AType = 'TDictionaryInstance' then
begin
  if FName = 'set' then
    SetItem(TDictionaryInstance(AObj))
  else if FName = 'get' then
    Ret := GetItem(TDictionaryInstance(AObj))
  else if FName = 'hasKey' then
    Ret := DictHasKey(TDictionaryInstance(AObj))
  else if FName = 'keys' then
    Ret := GetKeys(TDictionaryInstance(AObj))
  else if FName = 'drop' then
    Ret := TDictionaryInstance(AObj).Pvalue.DropItem(FParams[0].AsString);

end
