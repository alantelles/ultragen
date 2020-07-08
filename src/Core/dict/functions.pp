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

function TCoreFunction.GetKeys(AObj: TActRecInstance): TListInstance;
var
  ret: TListInstance;
begin
  Ret := TListInstance.Create();
  AObj.PValue.GetKeys(Ret);
  Result := Ret;
end;
