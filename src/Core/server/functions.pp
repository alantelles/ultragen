procedure TCoreFunction.CreateServer(var AObj: TServerInstance);
begin
  AObj.Create(TIntegerInstance(FParams[0]).PValue, TBooleanInstance(FParams[1]).PValue);
end;

procedure TCoreFunction.SetRootFile(var AObj: TServerInstance);
begin
  AObj.PRootFile := TStringInstance(FParams[0]).PValue;
end;

procedure TCoreFunction.SetStopServerRoute(var AObj: TServerInstance);
begin
  AObj.PStopRoute := TStringInstance(FParams[0]).PValue;
end;

procedure TCoreFunction.SetServerPort(var AObj: TServerInstance);
begin
  AObj.PPort := TIntegerInstance(FParams[0]).PValue;
end;

procedure TCoreFunction.SetAppTitle(var AObj: TServerInstance);
begin
  AObj.PTitle := TStringInstance(FParams[0]).PValue;
end;

procedure TCoreFunction.RunServer(var AObj: TServerInstance);
begin
  AObj.RunServer;
end;
