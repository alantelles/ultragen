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
    Ret[len - 1] := Execute(FInter, AFunc.PName, AArgs, FObj);
  end;
  Result := TListInstance.Create(Ret);
end;

function TCoreFunction.AppendToList(var AObj: TListInstance): TListInstance;
var
  len, i: integer;
begin
  if AObj.PAddLocked then
    FInter.RaiseException('Can''t add values to add locked List', 'Value');
  len := Length(FParams);
  if len = 0 then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  for i:=0 to len - 1 do
  begin
    AObj.Add(FParams[i]);
  end;
  Result := AObj
end;

function TCoreFunction.PrependToList(var AObj: TListInstance): TListInstance;
var
  len, i: integer;
begin
  if AObj.PAddLocked then
    FInter.RaiseException('Can''t add values to add locked List', 'Value');
  len := Length(FParams);
  if len = 0 then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  for i:=0 to len - 1 do
  begin
    AObj.Prepend(FParams[i]);
  end;
  Result := AObj
end;

function TCoreFunction.SetItem(var AObj: TListInstance): TListInstance;
begin
  if Length(FParams) <> 2 then
    raise ERunTimeError.Create(E_INVALID_ARGS);
  AObj.SetItem(TIntegerInstance(FParams[0]).PValue, FParams[1]);
  Result := AObj;
end;

function TCoreFunction.PopItem(var AObj: TListInstance): TInstanceOf;
var
  Ret: TInstanceOf;
begin
  if Length(FParams) <> 0 then
    raise ERunTimeError.Create(E_INVALID_ARGS);
  Result := AObj.PopItem;
end;

function TCoreFunction.PathJoin(var AObj: TListInstance): TStringInstance;
var
  OStr: string = '';
  i, len: integer;
begin
  len := AObj.Count;
  if len > 0 then
  begin
    for i := 0 to len-1 do
    begin
      if i > 0 then
        OStr := OStr + ESCAPE_SYMBOL + DirectorySeparator;
      OStr := OStr + AObj.PValue[i].AsString;
    end;
  end;
  Result := TStringInstance.Create(OStr);
end;
