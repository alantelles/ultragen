else if FName = 'split' then
  Result := SplitString
else if FName = 'upper' then
  Result := TStringInstance.Create(AnsiUpperCase(TStringInstance(FParams[0]).PValue))
else if FName = 'lower' then
  Result := TStringInstance.Create(AnsiLowerCase(TStringInstance(FParams[0]).PValue))
else if FName = 'join' then
  Result := JoinString
else if FName = 'capital' then
  Result := CapitalString
else if FName = 'replace' then
  Result := TStringInstance.Create(
    ReplaceStr(
      TStringInstance(FParams[0]).PValue,
      TStringInstance(FParams[1]).PValue,
      TStringInstance(FParams[2]).PValue
      )
  )
