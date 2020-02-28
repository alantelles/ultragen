program UltraGen;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    Classes, sysutils, StrUtils, LazUTF8, FileUtil, Process,
    { you can add units after this }

    { Classes }
     WorkClass, WebServerClass, TemplateClass, QueueListClass,
     ThreadedTemplateClass, GenFileSetClass, ParserClass,

    { Globals }
    TypesGlobals, VariablesGlobals, ConstantsGlobals,

    { Utils }
    FileHandlingUtils;




var
  iter,iterA, ADefault:string;
  Start:TDateTime;
  FlowLines,AuxGens,AuxGroup,AuxTemp:TStringList;
  AWork:TWork;
  LookSub, Live, AsString, IsGenSetCall, IsGenpathCall:boolean;
  i, interval:integer;
  Server:TUltraGenServer;
  ATemplate:TTemplate;
  AGenSet:TGenFileSet;

begin
  Start := now;
  GlobalQueue := TQueueSet.Create;
  AVerifyThread := TThreadVerify.Create;
  if ParamCount > 2 then
  begin
    if (ParamStr(ParamCount-1) = ENABLE_THREADS) then
    begin
      interval := StrToInt(Trim(ParamStr(ParamCount)));
      if interval < 1 then
        interval := 1;
      GlobalQueue.Interval := interval*1000;
      AVerifyThread.Start;
    end;
  end;

  randomize;
  AuxGens :=  TStringList.Create;
  AuxGroup :=  TStringList.Create;
  //calling modes
  //a set of gens example
  //ultragen -set src1.gen|src2.gen -templates teste.ultra.txt|teste2.ultra.txt
  //new call        f
  //ultragen [template name] [genmode] [genpath]

  // server mode
  //--serve appName mode port
  if (ParamStr(1) = '--serve') then
  begin
    IsGenSetCall := False;
    IsGenPathCall := False;
    if not FileExists(GetFileName(ParamStr(2),False)+'.gen') then
    begin
      WriteLn('Config App Gen ',GetFileName(ParamStr(2),False),' not found.');
      WriteLn('See docs for details. Exitting');
      Exit;
    end;
    if ParamCount = 2 then
    begin
    //-- serve appName mode

      Server := TUltraGenServer.Create(2020, ParamStr(2), '--dev')
    end
    else if ParamCount = 3 then
    //-- serve appName mode
      Server := TUltraGenServer.Create(StrToInt(ParamStr(3)), ParamStr(2), '--dev')
    else if ParamCount = 4 then
      if ParamStr(4) = '--prod' then
        Server := TUltraGenServer.Create(StrToInt(ParamStr(3)), ParamStr(2), '--prod')
      else
        Server := TUltraGenServer.Create(StrToInt(ParamStr(3)), ParamStr(2), '--dev');
      if not Server.RunServer then
        exit;
  end;
  Live := True;


	if (ParamStr(2) = GENSET_CALL) then
    IsGenSetCall := True
  else if (ParamStr(2) = GENPATH_CALL) then
    IsGenPathCall := True;

  if (ParamStr(ParamCount) = LIVE_CALL) or (ParamStr(ParamCount) = LIVE_CALL_S) then
    Live := False;

  if IsGenSetCall then
  begin
    if ParamStr(2) = GENSET_CALL then
    begin
      AuxGens.SkipLastLineBreak := True;
      AuxGens.StrictDelimiter := True;
      AuxGens.Delimiter := SET_SEP;
      AuxGens.DelimitedText := ParamStr(3);
      ATemplate := TTemplate.Create(ParamStr(1));
      AGenSet := TGenFileSet.Create;
      for iter in AuxGens do
      begin
        AuxGroup.Clear;
        AuxGroup.SkipLastLineBreak := True;
        AuxGroup.StrictDelimiter := True;
        AuxGroup.Delimiter := SET_GROUP;
        AuxGroup.DelimitedText := iter;
        AGenSet.ClearSet;
        for iterA in AuxGroup do
          AGenSet.Add(iterA);
        ATemplate.ParseTemplate(AGenSet);
        if Live then
          ATemplate.PrintParsed
        else
          ATemplate.Save;
      end;
      ATemplate.Free;
    end;
  end
  else if IsGenPathCall then
  begin
    //temp.ultra --genpath [folder]
    //dummy
    Live := False;
    FindAllFiles(AuxGens,ParamStr(3),'*.GEN;*.gen',False);
    AuxGens.Sort;
    if AuxGens.Count > 0 then
    begin
      ATemplate := TTemplate.Create(ParamStr(1));
      AGenSet := TGenFileSet.Create;
      for i:=0 to AuxGens.Count-1 do
      begin
        AGenSet.ClearSet;
        AGenSet.Add(AuxGens[i]);
        ATemplate.ParseTemplate(AGenSet);
        ATemplate.Save;
      end;
      ATemplate.Free;
    end;
  end;

  if not Live then
    WriteLn('UltraGen processed ',AuxGens.Count,' files in: ',FormatDateTime('ss.zzz',Now-Start));
  AuxGens.Free;
  AuxGroup.Free;

  GlobalQueue.Free;
  AVerifyThread.Terminate;

end.

