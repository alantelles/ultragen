{AFSType := TDataType.Create('TFileSystemInstance', 'FileSystem');
AFSFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFileSystemInstance', True);
AFSType.PMembers.Add('loadText', AFSFunc);
AFSType.PMembers.Add('mkdir', AFSFunc);
AFSType.PMembers.Add('isFile', AFSFunc);
AFSType.PMembers.Add('isDir', AFSFunc);
AFSType.PMembers.Add('getAllFiles', AFSFunc);
AActRec.AddMember('FileSystem', AFSType);}

