function TCoreFunction.MapList:TListInstance;
var
  AList, AArgsList: TListInstance;
  AFunc: TFunctionInstance;
  AArgs, Ret: TInstanceList;
  AArg: TInstanceOf;
  len:integer = 0;
begin
  AList := TListInstance(FParams[0]);
  AArgsList := TListInstance(FParams[2]);
  AFunc := TFunctionInstance(FParams[1]);
  SetLength(AArgs, 0);
  for AArg in AArgsList.PValue do
  begin
    len := len + 1;
    SetLength(AArgs, len);
    AArgs[len - 1] := AArg;
  end;
  len := 0;
  SetLength(Ret, 0);
  for AArg in AList.PValue do
  begin
    len := len + 1;
    AArgs[0] := AArg;
    SetLength(Ret, len);
    Ret[len - 1] := Execute(FInter, AFunc.PName, AArgs);
  end;
  Result := TListInstance.Create(Ret);
end;

function TCoreFunction.AppendToList(var AObj: TListInstance): TListInstance;
var
  len, i: integer;
begin
  len := Length(FParams);
  if len = 0 then
    raise EArgumentsError.Create(E_INVALID_ARGS);
  for i:=0 to len - 1 do
  begin
    AObj.Add(FParams[i]);
  end;
  Result := AObj
end;
