procedure TCoreFunction.SetItem(AObj: TActRecInstance);
begin
//  precisa mudar
  Aobj.PValue.AddMember(TStringInstance(FParams[0]).PValue, FParams[1]);
end;

function TCoreFunction.GetItem(AObj: TActRecInstance): TInstanceOf;
begin
//  precisa mudar
  Result := Aobj.PValue.GetMember(TStringInstance(FParams[0]).PValue);

end;

function TCoreFunction.RouteMatch(AObj: TActRecInstance): TInstanceOf;
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

function TCoreFunction.GetKeys(AObj: TActRecInstance): TListInstance;
var
  ret: TListInstance;
begin
  Ret := TListInstance.Create();
  AObj.PValue.GetKeys(Ret);
  Result := Ret;
end;
