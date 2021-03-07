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

{$R *.res}

begin
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


      i := 2;
      while ((Copy(ParamStr(i), 1, 2) <> '--')) and
            (i <= ParamCount) do
      begin
        ParamsNodes.Add('$params.append("' + ParamStr(i) + '")');
        i := i + 1;
      end;
      ParamsNodes.Add('$params.lock()');

		end;
    BTree := TUltraInterface.ParseStringList(ParamsNodes);
    ParamsNodes.Free;
    LiveOut := TUltraInterface.InterpretScript(ParamStr(1), TProgram(BTree), nil, '', nil, nil);
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
