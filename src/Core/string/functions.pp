procedure TCoreFunction.InitString(AObj: TStringInstance);
begin
  checkArgCount([1]);
  checkArgTypes(['TStringInstance']);
  AObj.PValue := TStringInstance(FParams[0]).PValue;
end;

procedure TCoreFunction.SaveStringStream(AObj: TStringInstance);
var
  AStream: TStringStream;
  FOut: string;
begin
  checkArgCount([1]);
  checkArgTypes(['TStringInstance']);
  AStream := TStringStream.Create(AObj.PValue);
  FOut := TStringInstance(FParams[0]).PValue;
  AStream.SaveToFile(FOut);
  AStream.Free;
end;

function TCoreFunction.SliceString(AObj: TStringInstance):TStringInstance;
var
  Return: string;
  Astr: string;
  StrStart, StrEnd: integer;
begin
  checkArgCount([1, 2]);
  checkArgTypes(['TIntegerInstance', 'TIntegerInstance']);
  Return := '';
  if (Length(FParams) > 2) or (Length(FParams) < 1) then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  AStr := AObj.PValue;
  StrStart := TIntegerInstance(FParams[0]).PValue;
  if LEngth(FParams) = 2 then
    StrEnd := TIntegerInstance(FParams[1]).PValue
  else
    StrEnd := Length(AStr);
  if (StrStart >= 0) and (StrEnd > StrStart) then
  //Slice('abcdefgh',1,5) = bcde
    Return := UTF8Copy(AStr,StrStart+1,StrEnd-StrStart)
  else if (StrStart >= 0) and (StrEnd < 0) then
  //Slice('abcdefgh',2,-1) = cdefg
  //Slice('abcdefgh',4,-2) = ef
    Return := UTF8Copy(AStr,StrStart+1,Length(AStr)+StrEnd-StrStart)
  else if (StrStart < 0) and (StrEnd <= 0) then
  //Slice('abcdefgh',-6,-3) = cde
    Return := UTF8Copy(AStr,Length(AStr)+StrStart+1,(StrStart-StrEnd)*(-1));
  Result := TStringInstance.Create(Return);

end;

function TCoreFunction.JoinString(AObj: TStringInstance):TStringInstance;
var
  len, i: integer;
  valid, joiner, part: string;
  AList: TListInstance;
  AStr: TStringInstance;

begin
  CheckArgCount([1]);
  CheckArgTypes(['TListInstance']);
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
        FInter.RaiseException('Only string instances can be joined', 'Type');
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

