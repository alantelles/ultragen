else if AType = 'TByteStreamInstance' then
begin
  if FName = 'save' then
    Ret := SaveByteStream(TByteStreamInstance(FObj))
  else if FName = 'read' then
    Ret := readByteStreamPart(TByteStreamInstance(FObj))
  else if FName = 'write' then
    WriteByteStreamPart(TByteStreamInstance(FObj))
  else if FName = 'length' then
    Ret := TIntegerInstance.Create(Length(TByteStreamInstance(FObj).PValue));
end
