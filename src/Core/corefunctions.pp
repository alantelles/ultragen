
constructor TCoreFunction.Create;
begin
  FFuncList := TStringList.Create;
  FFuncList.Add('print');
  FFuncList.Add('inline');
  FFuncList.Add('str');
  FFuncList.Add('int');
  FFuncList.Add('typeof');
  FFuncList.Add('split');
end;
