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
  else if FName = 'unixTime' then
    Ret := TIntegerInstance.Create(DateTimeToUnix(TDateTimeInstance(FObj).PValue))

  else if FName = 'format' then
  begin
    Ret := TStringInstance.Create(
        FormatDateTime(TStringInstance(FParams[0]).PValue, TDateTimeInstance(FObj).PValue)
    );
  end
  else if FName = 'compare' then
  begin
    Ret := TIntegerInstance.Create(CompareDateTime(TDateTimeInstance(FObj).PValue, TDateTimeInstance(FParams[0]).PValue));
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
