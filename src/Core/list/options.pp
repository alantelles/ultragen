{else if FName = 'map' then
  Result := MapList}
else if Fname = 'length' then
  Result := TIntegerInstance.Create(TListInstance(FObj).Count)
