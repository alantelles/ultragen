procedure TCoreFunction.DistributeList(var AObj: TListInstance);
var
  lenArgs, lenList, i: integer;
  AActRec: TActivationRecord;
begin
  AActRec := FInter.PCallStack.Peek;
  lenArgs := Length(FParams);
  if lenArgs > 1 then
  begin
    if (lenArgs) > AObj.Count then
      ERunTimeError.Create('More names than objects in the list');
    for i:=0 to lenArgs - 1 do
      AActRec.AddMember(FParams[i].PStrValue, AObj.GetItem(i));
  end
  else
    EArgumentsError.Create(E_INVALID_ARGS);
  {FInter.RaiseException(E_INVALID_ARGS_TYPE+ ', must be String', 'Arguments');}
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
        {$IFDEF Windows}OStr := OStr + ESCAPE_SYMBOL + DirectorySeparator;{$ELSE}
        OStr := OStr + DirectorySeparator;{$ENDIF}

      OStr := OStr + AObj.PValue[i].AsString;
    end;
  end;
  Result := TStringInstance.Create(OStr);
end;
