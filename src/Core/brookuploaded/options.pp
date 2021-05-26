// uploaded
else if AType = 'TBrookUploadedInstance' then
begin
  if FName = 'save' then
  begin
    if Length(FParams) <> 1 then
      Finter.RaiseException(E_INVALID_ARGS, 'Arguments');
    Ret := TBrookUploadedInstance(FObj).SaveFile(TStringInstance(FParams[0]).PValue);
  end;
end
