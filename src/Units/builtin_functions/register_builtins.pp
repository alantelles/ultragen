{$INCLUDE 'stringmethods.pp'}

{$INCLUDE 'listmethods.pp'}
{$INCLUDE 'integermethods.pp'}
{$INCLUDE 'servermethods.pp'}
{$INCLUDE 'dictmethods.pp'}

// builtins
AActRec.AddMember('locals', ACoreFunc);
AActRec.AddMember('raise', ACoreFunc);
AActRec.AddMember('addModulePath', ACoreFunc);
AActRec.AddMember('members', ACoreFunc);
AActRec.AddMember('print', ACoreFunc);
AActRec.AddMember('inline', ACoreFunc);
AActRec.AddMember('str', ACoreFunc);
AActRec.AddMember('urlEncode', ACoreFunc);
AActRec.AddMember('urlDecode', ACoreFunc);
AActRec.AddMember('int', ACoreFunc);
AActRec.AddMember('typeof', ACoreFunc);
AActRec.AddMember('range', ACoreFunc);
AActRec.AddMember('concat', ACoreFunc);
AActRec.AddMember('clear', ACoreFunc);
AActRec.AddMember('pause', ACoreFunc);
AActRec.AddMember('input', ACoreFunc);
AActRec.AddMember('saveLive', ACoreFunc);
