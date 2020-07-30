{else if FName = 'map' then
  Result := MapList}

else if AType = 'TListInstance' then
begin
     if Fname = 'length' then
        Ret := TIntegerInstance.Create(TListInstance(FObj).Count)
     else if FName = 'append' then
        Ret := AppendToList(TListInstance(FObj))
     else if FName = 'set' then
        Ret := SetItem(TListInstance(FObj))
		 else if FName = 'addLock' then
		    TListInstance(AObj).PAddLocked := True
		 else if FName = 'changeLock' then
		    TListInstance(AObj).PChangeLocked := True
		 else if Fname = 'lock' then
		 begin
		   TListInstance(AObj).PChangeLocked := True;
		   TListInstance(AObj).PAddLocked := True
		end
     else if FName = 'pop' then
        Ret := PopItem(TListInstance(FObj))
     else
      raise ERunTimeError.Create('Referenced function "' + FName + '" does not exist.');
end
