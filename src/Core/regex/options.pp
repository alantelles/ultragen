// options
else if AType = 'TRegexInstance' then
begin
  if FName = 'match' then
  begin
    Ret := TBooleanInstance.Create(TRegexInstance(FObj).MatchExpr(FParams[0].PStrValue));
  end
end
