function TCoreFunction.MapList:TListInstance;
var
  AList, AArgsList: TListInstance;
  AFunc: TFunctionInstance;
  AArgs: TInstanceList;
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
  for AArg in AList.PValue do
  begin
    AArgs[0] := AArg;
    Execute(AFunc.PName, AArgs);
  end;
end;
