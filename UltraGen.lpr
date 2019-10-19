program SuperGen;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Classes, sysutils, StrUtils, LazUTF8, FileUtil, Process,
    { you can add units after this }

    { Classes }
     WorkClass,

    { Globals }
    TypesGlobals, VariablesGlobals, ConstantsGlobals,

    { Utils }
    FileHandlingUtils
    ;


var
  GenPath, UFlow,iter,iterA,iterB, ADefault:string;
  Start:TDateTime;
  FlowLines,AuxGens,AuxGroup,AuxTemp:TStringList;
  AWork:TWork;
  UltraFlowCall, Unique, LookSub, SrcIsPath, Live, AsString, IsGenSetCall, IsGenpathCall:boolean;
  i:integer;
begin
  Start := now;
  randomize;
  AuxGens :=  TStringList.Create;
  AuxGroup :=  TStringList.Create;
  AuxTemp := TStringList.Create;
  AuxGens.StrictDelimiter := True;
  AuxGens.Delimiter := SET_SEP;
  AuxGroup.StrictDelimiter := True;
  AuxGroup.Delimiter := SET_GROUP;
  AuxTemp.StrictDelimiter := True;
  AuxTemp.Delimiter := SET_SEP;
  //calling modes
  //a set of gens example
  //ultragen -set src1.gen|src2.gen -templates teste.ultra.txt|teste2.ultra.txt
  UltraFlowCall := False;
  if (ParamStr(1) = GENSET_CALL) or (ParamStr(1) = '-templates') then
    IsGenSetCall := True
  else if (ParamStr(1) = GENPATH_CALL) then
    IsGenPathCall := True;
  if (ParamStr(ParamCount) = LIVE_CALL) or (ParamStr(ParamCount) = LIVE_CALL_S) then
    Live := True;

  if IsGenSetCall then
  begin
    if ParamStr(1) = GENSET_CALL then
    begin
      //-gens "f1.gen+g1.gen" -templates teste.ultra.html -l
      //-gens "f1.gen+g1.gen|f2.gen+g2.gen" -templates teste.ultra.txt
      //-gens "f1.gen+g1.gen|f2.gen+g2.gen" -templates "t1.ultra.txt|t2.ultra.txt" -default "a string"
      AuxGens.DelimitedText := ParamStr(2);
      //f1+g1
      //f2+g2
      AuxTemp.DelimitedText := ParamStr(4);
      ADefault := DEF_IF_NOT;
      if ParamCount > 5 then
      begin
        if ParamStr(5) = PARAM_SET_DEFAULT then
          ADefault := ParamStr(6);
      end;
      for iterA in AuxGens do
      begin
        AuxGroup.Clear;
        AuxGroup.DelimitedText := iterA;
        AWork := TWork.Create(AuxGroup);
        AWork.Live := Live;
        AWork.DoWork(GENSET_TEMPLATE,AuxGroup,AuxTemp,ADefault);
        AWork.Free;
      end;
    end
    else if ParamStr(1) = TEMPSET_CALL then
    begin
      AuxGens.Clear;
      AuxGens.DelimitedText := '';
      AuxTemp.DelimitedText := ParamStr(2);
      AWork := TWork.Create(AuxGens);
      AWork.Live := Live;
      AWork.DoWork(GENSET_TEMPLATE,AuxGens,AuxTemp,ADefault);
      AWork.Free;
    end;
  end
  else if IsGenPathCall then
  begin
    //-genpath gens -templates "teste.ultra.txt|teste2.ultra.txt" -sub -default thomas
    AuxGens.Clear;
    LookSub := False;
    ADefault := DEF_IF_NOT;
    AuxTemp.DelimitedText := ParamStr(4);
    if ParamStr(5) = LOOK_SUB_FLAG then
      LookSub := True;
    if (ParamCount > 5) then
    begin
      if (ParamStr(5) = PARAM_SET_DEFAULT) then
        ADefault := ParamStr(6)
      else if (ParamStr(6) = PARAM_SET_DEFAULT) then
        ADefault := ParamStr(7);
    end;
    FindAllFiles(AuxGens,ParamStr(2),'*.GEN;*.gen',LookSub);
    for iter in AuxGens do
    begin
      AuxGroup.Clear;
      AuxGroup.Add(iter);
      AWork := TWork.Create(AuxGroup);
      AWork.Live := Live;
      AWork.DoWork(GENSET_TEMPLATE,AuxGroup,AuxTemp,ADefault);
      AWork.Free;
    end;
  end;


  if UltraflowCall then
  begin
    GenPath := ParamStr(1);
    UFlow := ParamStr(2);
    FlowLines := TStringList.Create;
    FlowLines.LoadFromFile(UFlow);
    if SrcIsPath then
    begin
      if ParamCount = 3 then
        AWork := TWork.Create(GenPath,ParamStr(3))
      else
        AWork := TWork.Create(GenPath,'.');
    end
    else
    begin
      if ParamCount = 3 then
        AWork := TWork.Create(True,GenPath,ParamStr(3))
      else
        AWork := TWork.Create(True,GenPath,'.');
    end;
    try
      if FileExists (UFlow) then
      begin
        FlowLines.LoadFromFile(UFlow);
        for iter in FlowLines do
        begin
          if (Length(Trim(iter)) > 0) and (Trim(iter)[1]<>'#') then
          begin
            AWork.SetWork(iter);
          end;
        end
      end
      else
        WriteLn('Ultraflow file not found.');
    except
      WriteLn('Some error occurred');
    end;
    FlowLines.Free;
    AWork.Free;
  end;
  if not Live then
    WriteLn('UltraGen processed files in: ',FormatDateTime('ss.zzz',Now-Start));
  AuxGens.Free;
  AuxGroup.Free;
  AuxTemp.Free;
end.

