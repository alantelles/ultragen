// uploaded
else if AType = 'TUploadedInstance' then
begin
  if FName = 'save' then
  begin
    if Length(FParams) <> 1 then
      Finter.RaiseException(E_INVALID_ARGS, 'Arguments');
    Ret := TUploadedInstance(FObj).SaveFile(TStringInstance(FParams[0]).PValue);
  end;
end
