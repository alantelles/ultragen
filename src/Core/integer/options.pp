// teste
else if AType = 'TIntegerInstance' then
begin
  if FName = 'cycle' then
    Ret := Cycle(TIntegerInstance(AObj))
  else if FName = 'leftZeros' then
    Ret := LeftZeros(TIntegerInstance(AObj))
  else if Fname = 'fixed' then
    Ret := FixedChars(TIntegerInstance(AObj));
end
