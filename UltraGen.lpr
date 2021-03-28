program UltraGen;

{$mode objfpc}{$H+}
// git push test

uses
  {$IFDEF UNIX}
  cthreads,{$ENDIF}
  Classes, SysUtils,
  { you can add units after this }
  ASTClass, LexerClass, ImpParserClass, InterpreterClass, StrUtils,
  LoggingClass, UltraGenInterfaceClass, DateTimeInstanceClass, Dos;
var
  BTree: TAST;
  LiveOut, UHome: string;
  i: integer;
  ParamsNodes: TStringList;
  {bT: string;
  bB: array of byte;
  lenBb: integer = 0;
  BStream: TMemoryStream;}
{$R *.res}

begin

  {bt := 'um teste côm carãc;te*4&212';
  SetLength(bB, lenBb);
  for i:=1 to Length(bt) do
  begin
    lenBb := lenBb + 1;
    SetLength(bB, lenBb);
    bB[i - 1] := ord(bt[i]);
  end;
  BStream := TMemoryStream.Create;
  for i := 0 to Length(bB) - 1 do
  begin
    Writeln(bb[i]);
    BStream.WriteByte(bb[i]);
  end;
  BStream.SaveToFile('teste.txt');}


  // serious
  LogLevel := '';
  {$IFDEF Windows}DecimalSeparator := '.';{$ENDIF}
  if ParamCount > 0 then
  begin
    {if ParamCount > 1 then
    begin
      if ParamStr(2) = '--debug' then
        LogLevel := DEBUG
      else if ParamStr(2) = '--inter' then
        LogLevel := INTER
      else if ParamStr(2) = '--parser' then
        LogLevel := 'PARSER';
    end;}
    ParamsNodes := TStringList.Create;
    UHome := GetEnv('ULTRAGEN_HOME');
    {$IFDEF Windows}
    UHome := ReplaceStr(UHome, DirectorySeparator, '\' + DirectorySeparator);
    {$ENDIF}

    if Trim(UHome) <> '' then
    begin
      ParamsNodes.Add('addModulePath(["'+UHome + '", "modules"].path())');
      ParamsNodes.Add('include @Core');
		end;

    if FileExists('./_INCLUDE.ultra') then
      ParamsNodes.Add('include ".\/_INCLUDE.ultra"');
    ParamsNodes.Add('$params = []');
    if ParamCount > 1 then
    begin

      if ParamStr(1) = '--serve' then
      begin
        // start a server
      end
      else
      begin
        i := 2;
        while ((Copy(ParamStr(i), 1, 2) <> '--')) and
              (i <= ParamCount) do
        begin
          ParamsNodes.Add('$params.append("' + ParamStr(i) + '")');
          i := i + 1;
        end;
        ParamsNodes.Add('$params.lock()');
      end;
		end;
    BTree := TUltraInterface.ParseStringList(ParamsNodes);
    ParamsNodes.Free;
    LiveOut := TUltraInterface.InterpretScript(ParamStr(1), TProgram(BTree), nil, '', nil, nil, nil);
    if Trim(LiveOut) <> '' then
      Writeln(LiveOut);
    {if (ParamStr(2) = '--persist') then
    begin
      AOut := TStringList.Create;
      AOut.SkipLastLineBreak := True;
      AOut.Text := LiveOut;
      AOut.SaveToFile(ParamStr(3));
		end;}
    //ATree.Free;
  end
  else
  begin
    WriteLn('UltraGen - Desktop/Web Template engine/Scripting language');
    WriteLn('Version: 0.4');
    WriteLn('Usage: ultragen [script path] [...params] [(--...OPTIONS)]');
    WriteLn('Created by Alan Telles');
  end;
end.
