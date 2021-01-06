// teste
else if AType = 'TDictionaryInstance' then
begin
  if FName = 'set' then
    SetItem(TDictionaryInstance(FObj))
  else if FName = 'get' then
    Ret := GetItem(TDictionaryInstance(FObj))
  else if FName = 'hasKey' then
    Ret := DictHasKey(TDictionaryInstance(FObj))
  else if FName = 'keys' then
    Ret := GetKeys(TDictionaryInstance(FObj))
  else if FName = 'addLock' then
    TDictionaryInstance(FObj).PAddLocked := True
  else if FName = 'changeLock' then
    TDictionaryInstance(FObj).PChangeLocked := True
  else if Fname = 'lock' then
  begin
    TDictionaryInstance(FObj).PChangeLocked := True;
    TDictionaryInstance(FObj).PAddLocked := True
	end
  else if FName = 'localize' then
    LocalizeDict(TDictionaryInstance(FObj))
	else if FName = 'drop' then
    Ret := TDictionaryInstance(FObj).Pvalue.DropItem(FParams[0].AsString);

end
