AStrType := TDataType.Create('TStringInstance', 'String');
AStrFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TStringInstance', True, False, False);
AStrType.PMembers.Add('split', AStrFunc);
AStrType.PMembers.Add('upper', AStrFunc);
AStrType.PMembers.Add('lower', AStrFunc);
AStrType.PMembers.Add('replace', AStrFunc);
AStrType.PMembers.Add('capital', AStrFunc);
AStrType.PMembers.Add('slice', AStrFunc);
AStrType.PMembers.Add('pos', AStrFunc);
AStrType.PMembers.Add('fileName', AStrFunc);
AStrType.PMembers.Add('join', AStrFunc);
AStrType.PMembers.Add('length', AStrFunc);
AStrType.PMembers.Add('length', AStrFunc);
AStrType.PMembers.Add('save', AStrFunc);
AStrType.PMembers.Add('indexOf', AStrFunc);

AActRec.AddMember('String', AStrType);
