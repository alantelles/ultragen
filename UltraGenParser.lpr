program UltraGenParser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  { you can add units after this }
  ASTClass, LexerClass, ImpParserClass, InterpreterClass,
  StrUtils, SymbolTableClass, SymbolsClass,TOkens, crt,
  LoggingClass, StackClass, ARClass,
  InstanceOfClass, StringInstanceClass;
var
  ASource: TStringList;
  AOut:string;
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
  AInter: TInterpreter;
  AStack: TStack;
  AnAr: TActivationRecord;
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
    ASource := TStringList.Create;
    ASource.SkipLastLineBreak := False;
    ASource.LoadFromFile(ParamStr(1));
    ALexer := TLexer.Create(ASource.Text);
    AParser := TTParser.Create(ALexer);
    ATree := AParser.ParseCode();
    AInter := TInterpreter.Create(ATree);
    AOut := AInter.Interpret;
    AInter.Free;
  end;

end.

