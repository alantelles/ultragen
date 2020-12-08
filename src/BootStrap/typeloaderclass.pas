unit TypeLoaderClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, InterpreterClass, ARClass;

type TTypeLoader = class
  public
    class procedure LoadType(AName: string; var AInter: Tinterpreter; var AActRec: TActivationRecord);
    class procedure LoadRequest(var AActRec: TActivationRecord);
    class procedure LoadFileSystem(var AActRec: TActivationRecord);
    class procedure LoadOS(var AActRec: TActivationRecord);
    class procedure LoadDateTime(var AActRec: TActivationRecord);
end;

implementation

class procedure TTypeLoader.LoadOS(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AType := TDataType.Create('TOSInstance', 'OS');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TOSInstance', True);
  AType.PMembers.Add('getEnv', AFunc);
  AActRec.AddMember('OS', AType);
end;

class procedure TTypeLoader.LoadDateTime(var AActRec: TActivationRecord);
var
  ADateTimeFunc: TFunctionInstance;
  ADateTimeType: TDataType;
begin
  ADateTimeFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TDateTimeInstance', True);
  ADateTimeType := TDataType.Create('TDateTimeInstance', 'DateTime');
  ADateTimeType.PMembers.Add('now', ADateTimeFunc);
  ADateTimeType.PMembers.Add('year', ADateTimeFunc);
  ADateTimeType.PMembers.Add('month', ADateTimeFunc);
  ADateTimeType.PMembers.Add('day', ADateTimeFunc);
  ADateTimeType.PMembers.Add('hour', ADateTimeFunc);
  ADateTimeType.PMembers.Add('minute', ADateTimeFunc);
  ADateTimeType.PMembers.Add('second', ADateTimeFunc);
  ADateTimeType.PMembers.Add('milli', ADateTimeFunc);
  AActRec.AddMember('DateTime', ADateTimeType);
end;

class procedure TTypeLoader.LoadRequest(var AActRec: TActivationRecord);
var
  AHttpClientFunc: TFunctionInstance;
  AHttpClientType, AHttpResponseType: TDataType;
begin
  AHttpClientFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'THttpClientInstance', True);
  AHttpClientType := TDataType.Create('THttpClientInstance', 'Request');
  AHttpClientType.PMembers.Add('get', AHttpClientFunc);
  AHttpClientType.PMembers.Add('post', AHttpClientFunc);
  AHttpClientType.PMembers.Add('put', AHttpClientFunc);
  AHttpClientType.PMembers.Add('delete', AHttpClientFunc);
  AActrec.AddMember('Request', AHttpClientType);

  AHttpResponseType := TDataType.Create('THttpResponseInstance', 'Response');
  AActrec.AddMember('Response', AHttpResponseType);
end;

class procedure TTypeLoader.LoadFileSystem(var AActRec: TActivationRecord);
var
  AFSType: TDataType;
  AFSFunc: TFunctionInstance;
begin
  AFSType := TDataType.Create('TFileSystemInstance', 'FileSystem');
  AFSFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFileSystemInstance', True);
  AFSType.PMembers.Add('loadText', AFSFunc);
  AFSType.PMembers.Add('mkdir', AFSFunc);
  AFSType.PMembers.Add('isFile', AFSFunc);
  AFSType.PMembers.Add('isDir', AFSFunc);
  AFSType.PMembers.Add('getAllFiles', AFSFunc);
  AActRec.AddMember('FileSystem', AFSType);
end;

class procedure TTypeLoader.LoadType(AName: string; var AInter: Tinterpreter; var AActRec: TActivationRecord);
begin
  if AName = 'Request' then
    TTypeLoader.LoadRequest(AActRec)
  else if AName = 'FileSystem' then
    TTypeLoader.LoadFileSystem(AActRec)
  else if AName = 'OS' then
    TTypeLoader.LoadOS(AActRec)
  else if AName = 'DateTime' then
    TTypeLoader.LoadDateTime(AActRec)
  else
    AInter.RaiseException('Type "'+AName+'" does not exist and can''t be loaded', 'RunTime');
end;

end.

