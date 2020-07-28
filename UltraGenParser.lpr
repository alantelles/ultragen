program UltraGenParser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,{$ENDIF}
  Classes, SysUtils,
  { you can add units after this }
  ASTClass, LexerClass, ImpParserClass, InterpreterClass,
  StrUtils, LoggingClass;
var
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
  AInter: TInterpreter;
  AOut: TStringList;
  LiveOut: string;
begin
  LogLevel := '';
  DecimalSeparator := '.';
  if ParamCount > 0 then
  begin
    if ParamCount > 1 then
    begin
      if ParamStr(2) = '--debug' then
        LogLevel := DEBUG
      else if ParamStr(2) = '--inter' then
        LogLevel := INTER
      else if ParamStr(2) = '--parser' then
        LogLevel := 'PARSER';
    end;
    ALexer := TLexer.Create(ParamStr(1));
    AParser := TTParser.Create(ALexer);
    ATree := AParser.ParseCode();
    AParser.Free;
    AInter := TInterpreter.Create(ATree);
    AInter.Interpret;
    LiveOut := AInter.PLive;
    AInter.Free;
    if Trim(LiveOut) <> '' then
      Writeln(LiveOut);
    if (ParamStr(2) = '--persist') then
    begin
      AOut := TStringList.Create;
      AOut.SkipLastLineBreak := True;
      AOut.Text := LiveOut;
      AOut.SaveToFile(ParamStr(3));
		end;
    //ATree.Free;
  end;
end.

