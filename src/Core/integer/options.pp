// teste
else if AType = 'TIntegerInstance' then
begin
  if FName = 'cycle' then
    Ret := Cycle(TIntegerInstance(AObj))
  else if FName = 'leftZeros' then
    Ret := LeftZeros(TIntegerInstance(AObj))
  else if FName = 'random' then
    Ret := TIntegerInstance.Create(Random(TIntegerInstance(FParams[0]).PValue))
  else if Fname = 'fixed' then
    Ret := FixedChars(TIntegerInstance(AObj));
end
