else if FName = 'split' then
  Result := SplitString(TStringInstance(FObj))
else if FName = 'upper' then
  Result := TStringInstance.Create(AnsiUpperCase(TStringInstance(FObj).PValue))
else if FName = 'lower' then
  Result := TStringInstance.Create(AnsiLowerCase(TStringInstance(FObj).PValue))
else if FName = 'join' then
  Result := JoinString(TStringInstance(FObj))
else if FName = 'capital' then
  Result := CapitalString(TStringInstance(FObj))
else if FName = 'replace' then
  Result := TStringInstance.Create(
    ReplaceStr(
      TStringInstance(FObj).PValue,
      TStringInstance(FParams[1]).PValue,
      TStringInstance(FParams[2]).PValue
      )
  )
