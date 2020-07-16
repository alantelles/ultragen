procedure TCoreFunction.InitString(AObj: TStringInstance);
begin
  AObj.PValue := TStringInstance(FParams[0]).PValue;
end;

function TCoreFunction.Substring(AObj: TStringInstance): TStringInstance;
var
  ret: string;
  start: integer;
  _end: integer;
begin
  if (Length(FParams) > 2) or (Length(FParams) < 1) then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  Ret := AObj.PValue;
  start := TIntegerInstance(FParams[0]).PValue;
  if Length(FParams) = 2 then
  begin
    start := TIntegerInstance(FParams[0]).PValue;
    _end := TIntegerInstance(FParams[1]).PValue
  end
  else
  begin
    _end := TIntegerInstance(FParams[0]).PValue;
    start := 0;
  end;
  Ret := Copy(Ret, start+1, _end);
  Result := TStringInstance.Create(Ret);
end;

function TCoreFunction.JoinString(AObj: TStringInstance):TStringInstance;
var
  len, i: integer;
  joiner, part: string;
  AList: TListInstance;
  AStr: TStringInstance;
begin
  AList := TListInstance(FParams[0]);
  len := AList.Count;
  // joiner := TStringInstance(FParams[0]).PValue;
  joiner := AObj.PValue;
  part := '';
  if len > 0 then
  begin
    for i:= 0 to len-1 do
    begin
      if AList.GetItem(i).ClassNameIs('TStringInstance') then
      begin
        AStr := TStringInstance(AList.GetItem(i));
        part := part + AStr.PValue;
        if i < (len-1) then
          part := part + joiner;
      end
      else
        raise ERunTimeError.Create('Only string instances can be joined');
    end;
  end;
  Result := TStringInstance.Create(part);
end;

function TCoreFunction.GetFileName(AObj: TStringInstance):TStringInstance;
var
  AFile, Ares: string;
  WithExt: boolean = True;
  dotpos, len: integer;

begin
  len := Length(FParams);
  AFile := AObj.PValue;
  if (len > 1) then
    raise EArgumentsError.Create(E_INVALID_ARGS);
  if (len = 1) then
  begin
    if FParams[0].ClassNameIs('TBooleanInstance') then
      WithExt := TBooleanInstance(FParams[0]).PValue
    else
      raise EArgumentsError.Create(E_INVALID_ARGS_TYPE);
  end;
  ARes := Copy(AFile, RPos(DirectorySeparator, AFile) + 1, Length(AFile));
  if not WithExt then
  begin
    dotpos := Pos('.', ARes);
    if dotpos > 0 then
      ARes := Copy(ARes, 1, dotpos-1);
  end;
  Result := TStringInstance.Create(ARes);
end;

function TCoreFunction.CapitalString(AObj: TStringInstance):TStringInstance;
var

  s,last, part: string;
  i: integer;
  AStr: string;
begin
  AStr := AObj.PValue;
  if Length(AStr) > 0 then
  begin
    if Length(FParams) > 0 then
    begin
      if TBooleanInstance(FParams[0]).PValue then
      begin
        part := '';
        last := '';
        if Length(AStr) > 0 then
        begin
          for i:=1 to Length(AStr) do
          begin
            s := AStr[i];
            if last = '' then
            begin
              part := part + UpperCase(s);
              last := 'up'
            end
            else if s = ' ' then
            begin
              part := part + s;
              last := '';
            end
            else if last = 'up' then
            begin
              part := part + LowerCase(s);
              last := 'up';
            end;
          end;
        end;
      end
      else
        part := AnsiUpperCase(AStr[1]) + AnsiLowerCase(Copy(AStr, 2, Length(AStr)));

    end
    else
      part := AnsiUpperCase(AStr[1]) + AnsiLowerCase(Copy(AStr, 2, Length(AStr)));
  end;
  Result := TStringInstance.Create(part);
end;

function TCoreFunction.SplitString(AObj: TStringInstance):TListInstance;
var
  ASep: string = '';
  s: string;
  AList: TStringList;
  AInsList: TInstanceList;
  len: integer;
begin
  if Length(FParams) > 0 then
  begin
    ASep := TStringInstance(FParams[0]).PValue;
    AList := TStringList.Create;
    AList.SkipLastLineBreak := True;
    AList.LineBreak := ASep;
    AList.Text := AObj.PValue;
    SetLength(AInsList, 0);
    len := 0;
    for s in AList do
    begin
      len := len + 1;
      SetLength(AInsList, len);
      AInsList[len - 1] := TStringInstance.Create(s);
    end;
  end
  else
  begin
    SetLength(AInsList, 0);
    len := 0;
    for s in AObj.PValue do
    begin
      len := len + 1;
      SetLength(AInsList, len);
      AInsList[len - 1] := TStringInstance.Create(s);
    end;
  end;
  Result := TListInstance.Create(AInsList);
end;

