// uploaded
else if AType = 'TBrookUploadedInstance' then
begin
  if FName = 'save' then
  begin
    if Length(FParams) > 1 then
      Finter.RaiseException(E_INVALID_ARGS, 'Arguments');
    if Length(FParams) = 0 then
      AuxBool := TBrookUploadedInstance(FObj).PValue.Save(AuxStr);
    if Length(Fparams) = 1 then
      AuxBool := TBrookUploadedInstance(FObj).PValue.SaveAs(FParams[0].AsString, AuxStr);
    if not AuxBool then
      FInter.RaiseException(AuxStr, 'Brook');
  end
  else if FName = 'name' then
    Ret := TStringInstance.Create(TBrookUploadedInstance(FObj).PValue.Name)
  else if FName = 'size' then
    Ret := TIntegerInstance.Create(TBrookUploadedInstance(FObj).PValue.Size)
  else if FName = 'contentType' then
    Ret := TStringInstance.Create(TBrookUploadedInstance(FObj).PValue.Mime);
end
