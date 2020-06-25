else if FName = 'split' then
  Result := SplitString(TStringInstance(FObj))
else if FName = 'upper' then
  Result := TStringInstance.Create(UTF8UpperCase(TStringInstance(FObj).PValue))
else if FName = 'lower' then
  Result := TStringInstance.Create(UTF8LowerCase(TStringInstance(FObj).PValue))
else if FName = 'join' then
  Result := JoinString(TStringInstance(FObj))
else if FName = 'capital' then
  Result := CapitalString(TStringInstance(FObj))
else if FName = 'replace' then
  Result := TStringInstance.Create(
    ReplaceStr(
      TStringInstance(FObj).PValue,
      TStringInstance(FParams[0]).PValue,
      TStringInstance(FParams[1]).PValue
      )
  )
else if FName = 'length' then
  Result := TIntegerInstance.Create(UTF8Length(TStringInstance(FObj).PValue))
