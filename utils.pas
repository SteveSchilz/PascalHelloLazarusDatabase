unit Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SQLDB, SysUtils, LazLogger;

  procedure DeleteUser(Q: TSQLQuery; SelectedID: Integer);
  procedure QueryPhones(Q: TSQLQuery; SelectedID: Integer);
  function GetLastInsertID(Q: TSQLQuery) : Integer;
  procedure ShowException(O: TObject);


implementation



//==============================================================================
// Database Functions
//==============================================================================

procedure DeleteUser(Q: TSQLQuery; SelectedID: Integer);
var
   SqlString : String;

begin
try
    Q.Close();      // In Delphi you would set Active = false here
    Q.sql.Clear();
    SqlString := FORMAT('DELETE FROM People WHERE Id = %d', [SelectedID]);
    DebugLn(SqlString);
    Q.sql.text := SqlString;
    Q.execSql;
    // Delete query is automatically committed, no need for Transaction.commit
except
    on E: Exception do
    begin
        ShowException(E);
    end;
end;

end;

procedure QueryPhones(Q: TSQLQuery; SelectedId : Integer);
var
  filterString : String;
  queryString : String;

begin
     try
       Q.Close();      // In Delphi you would set Active = false here
       Q.sql.Clear();
       filterString := Format('PhoneNumbers.PersonId = %d', [SelectedId]);
       queryString := 'SELECT [PhoneNumbers].Id, ' +
               '[PhoneNumbers].PersonId, ' +
               '[PhoneTypes].Type, ' +
               '[PhoneNumbers].Number ' +
               'FROM PhoneNumbers ' +
               'INNER JOIN PhoneTypes ' +
               'ON [PhoneTypes].Id = [PhoneNumbers].PhoneTypeID ' +
               'WHERE ' + filterString;
       DebugLn('Filtering phone list on ', filterString);
       Q.sql.text := queryString;
       Q.execSql;
       Q.Open();    //In Delphi you would set active:= true here


     except
       on E: Exception do
         begin
           ShowException(E);
         end;
     end;
end;

function GetLastInsertID(Q : TSQLQuery) : Integer;

begin
    GetLastInsertID := -1;   // If no insert was found this will be result.
 try
    if Assigned(Q) and (Q.RowsAffected > 0) then
    begin
      Q.Close;
      Q.SQL.Text := 'SELECT last_insert_rowid() AS LastInsertID';
      Q.Open;

      if not Q.EOF then
        Result := Q.FieldByName('LastInsertID').AsInteger;

      Q.Close;
    end;
  except
    on E: Exception do
      begin
        ShowException(E);
      end;
  end;
end;

//==============================================================================
// Exception Handlers
//==============================================================================

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

