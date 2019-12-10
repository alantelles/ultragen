program UltraGen;

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
  //new call
  //ultragen [template name] [genmode] [genpath]
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
      //-gens "f1.gen+g1.gen" -templates teste.ultra.html -l
      //-gens "f1.gen+g1.gen|f2.gen+g2.gen" -templates teste.ultra.txt
      //-gens "f1.gen+g1.gen|f2.gen+g2.gen" -templates "t1.ultra.txt|t2.ultra.txt" -default "a string"
      //[template] [genmode] [genlist] [def] [str def]
      AuxGens.DelimitedText := ParamStr(3);
      //f1+g1
      //f2+g2
      AuxTemp.DelimitedText := ParamStr(1);
      ADefault := DEF_IF_NOT;
      if ParamCount > 3 then
      begin
        if ParamStr(4) = PARAM_SET_DEFAULT then
          ADefault := ParamStr(5);
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
  end
  else
  begin
    AuxGens.Clear;
    AuxGens.DelimitedText := '';
    AuxTemp.DelimitedText := ParamStr(1);
    AWork := TWork.Create(AuxGens);
    AWork.Live := Live;
    AWork.DoWork(GENSET_TEMPLATE,AuxGens,AuxTemp,ADefault);
    AWork.Free;
  end;



  if not Live then
    WriteLn('UltraGen processed files in: ',FormatDateTime('ss.zzz',Now-Start));
  AuxGens.Free;
  AuxGroup.Free;
  AuxTemp.Free;

  for i:=1 to ParamCount do
  begin
    if ParamStr(i) = '-debug' then
      readln;
  end;
end.

