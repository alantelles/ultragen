// teste
else if AType = 'TDateTimeInstance' then
begin
  if FName = 'now' then
    Ret := TDateTimeInstance.CreateNowDateTime
  else if FName = 'parse' then
  begin
    Ret := TDateTimeInstance.ParseDateFromString(
        TStringInstance(FParams[0]).PValue,
        TSTringInstance(FPArams[1]).PValue
    );
  end
  else if (FName = 'hour') or
          (FName = 'minute') or
          (FName = 'second') or
          (FName = 'year') or
          (FName = 'month') or
          (FName = 'day') or
          (FName = 'milli') then
    Ret := TDateTimeInstance(FObj).GetPartOfDate(FName);

end
