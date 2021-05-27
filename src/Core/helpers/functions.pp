function TCoreFunction.HelpersGetRandomToken: TStringInstance;
var
  i, lenStr, lenParams:integer;
  useNumbers: boolean = True;
  useSpecial: boolean = True;
  useLetters: boolean = True;
  Ret: string = '';
begin
  lenParams := Length(FParams);
  if (LenParams < 1) or (LenParams > 4) then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  lenStr := FParams[0].PIntValue;
  if LenParams > 1 then
    useLetters := FParams[1].PBoolValue;
  if LenParams > 2 then
    useNumbers := FParams[2].PBoolValue;
  if LenParams > 3 then
    useSpecial := FParams[3].PBoolValue;
  if not (useLetters or useNumbers or useSpecial) then
    FInter.RaiseException('A random token must have at least one of three types of elements: numbers, letters or special characters', 'RunTime');
  while Length(Ret) < lenStr do
  begin
    // 33 - 126
    i := 33 + random(94);
    if not useNumbers then
      if (i >= 48) and (i <= 57) then
        continue;
    if not useSpecial then
      if ((i >= 33) and (i <= 47)) or ((i >= 58) and (i <= 64)) or ((i >= 91) and (i <= 96)) or ((i >= 123) and (i <= 126)) then
        continue;
    if not useLetters then
      if ((i >= 65) and (i <= 90)) or ((i >= 97) and (i <= 122)) then
        continue;

    Ret := Ret + chr(i);
  end;
  Result := TStringInstance.Create(Ret);
end;
function TCoreFunction.HelpersGetRandomToken(faster: boolean): TStringInstance;
var
  i, lenStr, lenParams:integer;
  Ret: string = '';
begin
  lenParams := Length(FParams);
  if (LenParams < 1) then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  lenStr := FParams[0].PIntValue;
  while Length(Ret) < lenStr do
  begin
    // 33 - 126
    i := 33 + random(94);
    Ret := Ret + chr(i);
  end;
  Result := TStringInstance.Create(Ret);
end;
