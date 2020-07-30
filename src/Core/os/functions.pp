function TCoreFunction.GetEnvVar(): TStringInstance;
var
  RetVal: string;
begin
  if (Length(FParams) > 2) or (Length(FParams) < 1) then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  RetVal := GetEnv(TStringInstance(FParams[0]).PValue);
  if (RetVal = '') and (Length(FParams) = 2) then
    RetVal := FParams[1].AsString;
  Result := TStringInstance.Create(Retval);
end;
