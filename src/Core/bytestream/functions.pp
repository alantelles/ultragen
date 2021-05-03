function TCoreFunction.SaveByteStream(AObj: TByteStreamInstance): TBooleanInstance;
begin
  Result := AObj.SaveStream(TStringInstance(FParams[0]));
end;

procedure TCorefunction.WriteByteStreamPart(AObj: TByteStreamInstance);
begin
  AObj.WriteStream(TByteStreamInstance(FParams[0]).PValue);
end;

function TCorefunction.ReadByteStreamPart(AObj: TByteStreamInstance): TByteStreamInstance;
begin
  if Length(FParams) = 0 then
    Result := AObj.ReadStream
  else if Length(FParams) = 1 then
    Result := AObj.ReadStream(TIntegerInstance(FParams[0]).PValue)
  else
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
end;


