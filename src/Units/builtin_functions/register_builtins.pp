{$INCLUDE 'stringmethods.pp'}
{$INCLUDE 'listmethods.pp'}
{$INCLUDE 'integermethods.pp'}
{$INCLUDE 'osmethods.pp'}

// builtins
AActRec.AddMember('print', ACoreType);
AActRec.AddMember('inline', ACoreType);
AActRec.AddMember('str', ACoreType);
AActRec.AddMember('int', ACoreType);
AActRec.AddMember('typeof', ACoreType);
AActRec.AddMember('range', ACoreType);
AActRec.AddMember('clear', ACoreType);
AActRec.AddMember('pause', ACoreType);
AActRec.AddMember('input', ACoreType);
AActRec.AddMember('saveLive', ACoreType);
