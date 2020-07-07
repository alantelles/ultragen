function TCoreFunction.Cycle(AObj: TIntegerInstance):TInstanceOf; 
var
  step, len, i:integer;
begin
  len := Length(FParams);
  if len > 0 then
  begin
    step := AObj.PValue;
    Result := FParams[step mod (len)];
	end
  else
    raise EArgumentsError.Create(E_INVALID_ARGS);
end;

function TCoreFunction.LeftZeros(AObj: TIntegerInstance): TStringInstance;
var
  left, i: integer;
  output: string = '';
begin
  if Length(FParams) <> 1 then
    raise EArgumentsError.Create(E_INVALID_ARGS);
  Left := TIntegerInstance(FParams[0]).PValue;
  if left > 0 then
    for i:=0 to left-1 do
      output := output + '0';
  Result := TStringInstance.Create(output + IntToStr(AObj.PValue));
end;
