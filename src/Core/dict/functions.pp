procedure TCoreFunction.SetItem(AObj: TDictionaryInstance);
begin
//  precisa mudar
  Aobj.PValue.AddMember(TStringInstance(FParams[0]).PValue, FParams[1]);
end;

function TCoreFunction.GetItem(AObj: TDictionaryInstance): TInstanceOf;
begin
//  precisa mudar
  Result := Aobj.PValue.GetMember(TStringInstance(FParams[0]).PValue);

end;

function TCoreFunction.DictHasKey(AObj: TDictionaryInstance): TBooleanInstance;
var
  i: integer;
begin
  i := AObj.PValue.PMembers.FindIndexOf(TStringInstance(FParams[0]).PValue);
  if i > -1 then
    Result := TBooleanInstance.Create(True)
  else
    Result := TBooleanInstance.Create(False);
end;

function TCoreFunction.RouteMatch(AObj: TDictionaryInstance): TInstanceOf;
var
  i, j, DotPos: integer;
  Exp, Inp: TStringList;
  //APair: TKVPair;
  Match: boolean = False;
  Return, ParamType: string;
  ASearch: string;
begin
  ASearch := TStringInstance(FParams[0]).PValue;
  Inp := TStringList.Create;
  Inp.SkipLastLineBreak := True;
  Inp.Delimiter := '/';
  Inp.StrictDelimiter := True;
  Inp.DelimitedText := ASearch;
  Exp := TStringList.Create;
  Exp.SkipLastLineBreak := True;
  Exp.Delimiter := '/';
  Exp.StrictDelimiter := True;
end;

function TCoreFunction.GetKeys(AObj: TDictionaryInstance): TListInstance;
var
  ret: TListInstance;
begin
  Ret := TListInstance.Create();
  AObj.PValue.GetKeys(Ret);
  Result := Ret;
end;
