// teste
else if AType = 'TIntegerInstance' then
begin
  if FName = 'cycle' then
    Ret := Cycle(TIntegerInstance(AObj))
  else if FName = 'left_zeros' then
    Ret := LeftZeros(TIntegerInstance(AObj));
end
