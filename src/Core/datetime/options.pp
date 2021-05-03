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
  else if FName = 'compareTime' then
  begin
    Ret := TIntegerInstance.Create(CompareTime(TDateTimeInstance(FObj).PValue, TDateTimeInstance(FParams[0]).PValue));
  end
  else if FName = 'compareDate' then
  begin
    Ret := TIntegerInstance.Create(CompareDate(TDateTimeInstance(FObj).PValue, TDateTimeInstance(FParams[0]).PValue));
  end
  else if FName = 'addDays' then
  begin
    Ret := TDateTimeInstance.Create(IncDay(TDateTimeInstance(FObj).PValue, TIntegerInstance(FParams[0]).PValue));
  end
  else if FName = 'addMinutes' then
  begin
    Ret := TDateTimeInstance.Create(IncMinute(TDateTimeInstance(FObj).PValue, TIntegerInstance(FParams[0]).PValue));
  end
  else if FName = 'addSeconds' then
  begin
    Ret := TDateTimeInstance.Create(IncSecond(TDateTimeInstance(FObj).PValue, TIntegerInstance(FParams[0]).PValue));
  end
  else if FName = 'addMonths' then
  begin
    Ret := TDateTimeInstance.Create(IncMonth(TDateTimeInstance(FObj).PValue, TIntegerInstance(FParams[0]).PValue));
  end
  else if FName = 'addYears' then
  begin
    Ret := TDateTimeInstance.Create(IncYear(TDateTimeInstance(FObj).PValue, TIntegerInstance(FParams[0]).PValue));
  end
  else if FName = 'addMillis' then
  begin
    Ret := TDateTimeInstance.Create(IncMilliSecond(TDateTimeInstance(FObj).PValue, TIntegerInstance(FParams[0]).PValue));
  end
  else if FName = 'addHours' then
  begin
    Ret := TDateTimeInstance.Create(IncHour(TDateTimeInstance(FObj).PValue, TIntegerInstance(FParams[0]).PValue));
  end
  else if FName = 'addWeeks' then
  begin
    Ret := TDateTimeInstance.Create(IncWeek(TDateTimeInstance(FObj).PValue, TIntegerInstance(FParams[0]).PValue));
  end
  else if FName = 'isBetween' then
  begin
    if (CompareDateTime(TDateTimeInstance(FParams[0]).PValue, TDateTimeInstance(FParams[1]).PValue) > 0) then
      FInter.RaiseException('Start date must be less than end date for evaluating if a date is in that range', 'DateTime');
    if Length(FParams) = 2 then
      Ret := TBooleanInstance.Create(DateTimeInRange(TDateTimeInstance(FObj).PValue, TDateTimeInstance(FParams[0]).PValue, TDateTimeInstance(FParams[1]).PValue))
    else if Length(FParams) = 3 then
      Ret := TBooleanInstance.Create(DateTimeInRange(TDateTimeInstance(FObj).PValue, TDateTimeInstance(FParams[0]).PValue, TDateTimeInstance(FParams[1]).PValue, TBooleanInstance(FParams[2]).PValue))
    else
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');

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
