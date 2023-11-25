unit Utils;

{$mode ObjFPC}{$H+}

interface
  procedure ShowException(O: TObject);


implementation

uses
  Classes, SysUtils, LazLogger;


procedure ShowException(O: TObject);
var
  E : Exception;
begin
    if (O is Exception) then
    begin
        E := Exception(O);
        DebugLn('Exception Type: ', E.ClassName);
        DebugLn('Exception Msg:  ', E.Message);
    end;
end;
end.

