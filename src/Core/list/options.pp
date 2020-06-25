{else if FName = 'map' then
  Result := MapList}

else if AType = 'TListInstance' then
begin
     if Fname = 'length' then
        Ret := TIntegerInstance.Create(TListInstance(FObj).Count);
end
