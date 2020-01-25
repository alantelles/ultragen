unit ThreadedTemplateClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Classes }
  GenFileSetClass, TypesGlobals, TemplateClass;

const
  TASK_DONE = 2;


type
  TThreadedTemplate = class(TThread)
  private
    FFunction: TUserFunction;
    FGenFileSet: TGenFileSet;
    FIndex:integer;
    FQueueName:string;
    FParams:TStringList;
  protected
    procedure Execute; override;
  public
    Constructor Create(AFunction:TUserFunction; AGenFileSet:TGenFileSet; TaskIndex:integer; var AParams:TStringList; AQueueName:string);
  end;

implementation

uses
  { Classes }
  ParserClass, GenFileClass;

constructor TThreadedTemplate.Create(AFunction:TUserFunction; AGenFileSet:TGenFileSet; TaskIndex:integer; var AParams:TStringList; AQueueName:string);
begin
  inherited Create(True);
  FGenFileSet := AGenFileSet;
  FreeOnTerminate := True;
  FIndex := TaskIndex;
  FParams := AParams;
  FQueueName := AQueueName;
  FFunction := AFunction;
end;


procedure TThreadedTemplate.Execute;
var
  i, all:integer;
  ATemplate:TTemplate;
  Line:string;
  AGenFile: TGenFile;
begin
  //FCaller^.Consume(FIndex, FGenFileSet);
  //Randomize;
  //writeln('executing task ',FIndex);
  AGenFile := TGenFile.Create;
  AGenFile.Create;
  AGenFile.SetValue('TASK_INDEX',IntToStr(FIndex));
  AGenFile.SetValue('QUEUE_NAME',FQueueName);
  FGenFileSet.Add(AGenFile,'INTERNALS');
  ATemplate := TTemplate.Create;
  ATemplate.GenFileSet := FGenFileSet;
  ATemplate.MakeFunctionsRoom;

  ATemplate.UserFunctions[0] := FFunction;
  ATemplate.ExecuteFunction(FFunction.FunctionName,FFunction.HasReturn,FParams);
  ATemplate.Free;
  //writeln('ending task ',FIndex);


  {all := 5000+Random(5000);
  //writeln('comecou ',IntToStr(FIndex),' rodando por ',(all div 1000),' segundos na fila ',FQueueName);
  //writeln('funcao ',FFunction.FunctionName);
  sleep(all);
  //writeln('acabou ',IntToStr(FIndex),' na fila ',FQueueName);}
  ReturnValue := TASK_DONE;
  //FCaller^.Queue[FIndex].Done := TASK_DONE;
end;

end.

