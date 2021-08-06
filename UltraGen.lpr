program UltraGen;

{$mode objfpc}{$H+}
// git push test

uses
  {$IFDEF UNIX}
  cthreads,{$ENDIF}
  Classes, SysUtils,
  { you can add units after this }
  ASTClass, LexerClass, ImpParserClass, InterpreterClass, StrUtils,
  ListInstanceClass, LoggingClass, UltraGenInterfaceClass,
  DateTimeInstanceClass, ServerClass, Dos, StringInstanceClass, Coreutils;
var
  BTree: TAST;
  LiveOut, UHome, Arg, ProgramPath, Option: string;
  i: integer;
  ParamsNodes, ArgParse: TStringList;
  Adapter: TUltraAdapter;
  ParamsList: TListInstance;
{$R *.res}

begin
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
    //for i:=1 to ParamCount do
    UHome := GetEnv('ULTRAGEN_HOME');

    i := 1;
    ParamsList := TListInstance.Create();
    Option := GetOption(ParamStr(1));
    if option <> '' then
    begin
      while i < ParamCount do
      begin
        Arg := ParamStr(i);
        if Arg = '-' then
        begin
          i := i + 1;
          break;
        end;
        option := GetOption(Arg);
        if option <> '' then
        begin
          if option = 'home' then
          begin
            i := i + 1;
            UHome := ParamStr(i);
            i := i + 1;
          end
          else if option = 'logparse' then
          begin
            LogLevel := 'PARSER';
            i := i + 1;
          end;
        end
        else
          i := i + 1;
      end;

    end;
    ProgramPath := ParamStr(i);
    i := i + 1;
    while i <= ParamCount do
    begin
      ParamsList.Add(TStringInstance.Create(ParamStr(i)));
      i := i + 1;
    end;
    Adapter := TUltraAdapter.Create('params');
    Adapter.ActRec.AddMember('$params', ParamsList);
    Adapter.ActRec.AddMember('$scriptName', TStringInstance.Create(ProgramPath));
    Adapter.ActRec.AddMember('$ULTRAGEN_HOME', TStringInstance.Create(UHome));
    ParamsNodes := TStringList.Create;
    //{$IFDEF Windows}
    //UHome := ReplaceStr(UHome, DirectorySeparator, '\' + DirectorySeparator);
    //{$ENDIF}


    ParamsNodes.Add('params.localize()');
    ParamsNodes.Add('addModulePath([$ULTRAGEN_HOME, "modules"].path())');
    ParamsNodes.Add('include @Core');
    BTree := TUltraInterface.ParseStringList(ParamsNodes);

    LiveOut := TUltraInterface.InterpretScript(ProgramPath, ParamsNodes, Adapter);
    ParamsNodes.Free;
    ParamsList.Free;
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
    WriteLn(SizeOf(Pointer) shl 3, '-bit executable');
    WriteLn('Version: 0.7.0');
    WriteLn('Usage: ultragen [(--...OPTIONS)] [- (needed if any options)] [script path] [...params]');
    WriteLn('Created by Alan Telles');
  end;
end.
