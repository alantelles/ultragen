program UltraGen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,{$ENDIF}
  Classes, SysUtils,
  { you can add units after this }
  ASTClass, LexerClass, ImpParserClass, InterpreterClass,
  StrUtils, LoggingClass, UltraGenInterfaceClass;
var
  AParser: TTParser;
  ALexer: TLexer;
  ATree, BTree: TAST;
  AInter: TInterpreter;
  AOut: TStringList;
  LiveOut: string;
  i, len: integer;
  ParamsNodes: TStringList;
begin
  LogLevel := '';
  DecimalSeparator := '.';
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
    if ParamCount > 1 then
    begin
      ParamsNodes := TStringList.Create;
      ParamsNodes.Add('$params = []');
      i := 2;
      while ((Copy(ParamStr(i), 1, 2) <> '--')) and
            (i <= ParamCount) do
      begin
        ParamsNodes.Add('$params.append("' + ParamStr(i) + '")');
        i := i + 1;
      end;
      ParamsNodes.Add('$params.lock()');
      BTree := TUltraInterface.ParseStringList(ParamsNodes);
      ParamsNodes.Free;
		end;

		ALexer := TLexer.Create(ParamStr(1));
    AParser := TTParser.Create(ALexer);
    ATree := AParser.ParseCode();
    if ParamCount > 1 then
    begin
      len := Length(TProgram(BTree).PChildren);
      if len > 0 then
      begin
        for i:=0 to len-1 do
          TProgram(ATree).AddPrelude(TProgram(BTree).PChildren[i]);
      end;
		end;

    AParser.Free;
    AInter := TInterpreter.Create(ATree);
    AInter.Interpret;
    LiveOut := AInter.PLive;
    AInter.Free;
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
    WriteLn('Version: 0.7');
    WriteLn('Usage: ultragen [script path] [...params] [(--...OPTIONS)]');
    WriteLn('Created by Alan Telles');
  end;
end.

