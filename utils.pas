unit Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SQLDB, StrUtils, SysUtils, Types, LazLogger;

  function SplitName(FullName : String) : TStringDynArray;
  function ValidatePhone(number: String) : Int64;

  function AddUser(Q: TSQLQuery; First: String; Last: String): Integer;
  procedure EditUser(Q: TSQLQuery; Id: Integer; First: String; Last: String);
  procedure DeleteUser(Q: TSQLQuery; SelectedID: Integer);

  procedure QueryPhones(Q: TSQLQuery; SelectedID: Integer);
  function AddPhone(Q: TSQLQuery; PersonId: Integer; Number: String; PhoneTypeId:Integer) : Integer;
  procedure EditPhone(Q: TSQLQuery; Id: Integer; PersonId: Integer; Number: String; PhoneTypeId: Integer);
  procedure DeletePhone(Q: TSQLQuery; SelectedID: Integer);

  function GetLastInsertID(Q: TSQLQuery) : Integer;

  procedure ShowException(O: TObject);


implementation
//==============================================================================
// General Use Functions
//==============================================================================
(*
 *  Split a spring name into individual words, guaranteed to return two entries,
 *  either or both of which may be null.
 *)

function SplitName(FullName : String) : TStringDynArray;
var
NamesArray: TStringDynArray;

begin
    NamesArray := SplitString(FullName, ' ');
    if (Length(NamesArray) = 0) then
        begin
        SetLength(NamesArray, 2);
        NamesArray[0] := '';
        NamesArray[1] := '';
        end
     else if (Length(NamesArray) = 1) then
        begin
        SetLength(NamesArray, 2);
        NamesArray[1] := '';
        end
     else if (Length(NamesArray) > 2) then
         begin
         SetLength(NamesArray, 2);
     end;

     SplitName := NamesArray;
end;

(* ValidatePhone
 * Simple Validation of a phone number
 *
 * @param: number - The phone number (String with no formatting)
 *
 * @returns: The phone number as an integer
 *)
function ValidatePhone(number: String) : Int64;
var
    isOK : Boolean;
    phNumber : Int64;
begin
    isOk := TryStrToInt64(number, phNumber);
    if (isOk = true) then
        begin
        // In a real app you would split out the prefix and
        // Validate the area code, and perhaps check for invalid
        // numbers as well.
        //
        // This is just a simple check that we have 10 digits
        //             9168490226
        if (phNumber <= 999999999)  then
           begin
           isOk := false;
           end;
        // More than 10 digits is also an error
        //               9168490226
        if (phNumber >= 10000000000) then
           begin
           isOk := false;
           end;
    end;
    if (isOk) then
       begin
       ValidatePhone := phNumber;
       end
    else
        begin
        ValidatePhone := -1;
        end;

end;
//==============================================================================
// Database Functions
//==============================================================================
(* AddUser
 * Adds a new user to the database, returns newly created ID or -1 if failed.
 * It is expected that at least one of First or Last is non-null
 *
 *)
function AddUser(Q: TSQLQuery; First: String; Last: String) : Integer;

begin
    AddUser := -1;  // Default value if we fail.
 try
   try
      Q.Close();
      Q.Sql.Clear();
      // NOTES:
      //   To "Open" a query, Sql.text must be set to a select statment.
      //   This is because open makes the result set available, and insert
      //   statements do not have a result set.
      //
      //   The InsertSql.Text property also exists, apparently use Insert()
      //    instead of ExecSQL
      Q.Sql.Text := 'INSERT INTO People(First, Last) ' +
                  'VALUES(' + QuotedStr(First) + ',' +
                              QuotedStr(Last) + ')';
      DebugLn(Q.Sql.text);
      Q.ExecSql();           // Insert/Update use execSQL

      // ApplyUpdates(); - Needed to save when editing an existing record
      if (Q.Transaction.Active) then
          begin
          // Cast the transaction property of the query to TSQLTr...
          TSQLTransaction(Q.Transaction).Commit();
          end;


      // Return the value to caller if succeeded
      AddUser := Utils.GetLastInsertID(Q);
   except
     on E: Exception do
     begin
       if (Q.Transaction.Active) then
           begin
           TSQLTransaction(Q.Transaction).Rollback();
           end;
       Utils.ShowException(E);
     end;
   end;
 finally
   // Nothing needed here.
 end;

end;

(*
 * Edit an Existing User
 * It is expected that at least one of First or Last is non-null
 *
 *)
procedure EditUser(Q: TSQLQuery; Id:Integer; First: String; Last: String);

begin
 try
   try
      Q.Close();
      Q.Sql.Clear();
      // NOTES:
      //   To "Open" a query, Sql.text must be set to a select statment.
      //   This is because open makes the result set available, and insert
      //   statements do not have a result set.
      //
      //   The InsertSql.Text property also exists, apparently use Insert()
      //    instead of ExecSQL
      Q.Sql.Text := 'UPDATE People ' +
                  'SET First = ' + QuotedStr(First) + ',' +
                       'Last = ' + QuotedStr(Last) +
                  'WHERE Id =' + IntToStr(Id);
      DebugLn(Q.Sql.text);
      Q.ExecSql();           // Insert/Update use execSQL

      // ApplyUpdates(); - Needed to save when editing an existing record
      if (Q.Transaction.Active) then
          begin
          // Cast the transaction property of the query to TSQLTr...
          TSQLTransaction(Q.Transaction).Commit();
          end;

   except
     on E: Exception do
     begin
       if (Q.Transaction.Active) then
           begin
           TSQLTransaction(Q.Transaction).Rollback();
           end;
       Utils.ShowException(E);
     end;
   end;
 finally
   // Nothing needed here.
 end;
end;


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
               '[PhoneNumbers].PhoneTypeId, ' +
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

(* AddPhone
 * Adds a new Phone Number to the database, returns newly created ID or -1 if failed.
 *
 *)
function AddPhone(Q: TSQLQuery; PersonId: Integer; Number: String; PhoneTypeId:Integer) : Integer;

begin
    AddPhone := -1;  // Default value if we fail.
 try
   try
      Q.Close();
      Q.Sql.Clear();
      // NOTES:
      //   To "Open" a query, Sql.text must be set to a select statment.
      //   This is because open makes the result set available, and insert
      //   statements do not have a result set.
      //
      //   The InsertSql.Text property also exists, apparently use Insert()
      //    instead of ExecSQL
      Q.Sql.Text := 'INSERT INTO PHONENUMBERS(PersonId, PhoneTypeId, Number) ' +
                  'VALUES(' + IntToStr(PersonId) + ',' +
                              IntToStr(PhoneTypeId) + ',' +
                              QuotedStr(Number) + ')';
      DebugLn(Q.Sql.text);
      Q.ExecSql();           // Insert/Update use execSQL

      // ApplyUpdates(); - Needed to save when editing an existing record
      if (Q.Transaction.Active) then
          begin
          // Cast the transaction property of the query to TSQLTr...
          TSQLTransaction(Q.Transaction).Commit();
          end;


      // Return the value to caller if succeeded
      AddPhone:= Utils.GetLastInsertID(Q);
   except
     on E: Exception do
     begin
       if (Q.Transaction.Active) then
           begin
           TSQLTransaction(Q.Transaction).Rollback();
           end;
       Utils.ShowException(E);
     end;
   end;
 finally
   // Nothing needed here.
 end;
end;


(*
 * Edit an Existing Phone
 * @Param Q           SQL Query
 * @param Id          ID for PhoneNumbers table
 * @param PersonId    ID for Person this number belongs to.
 * @param PhoneTypeID ID to look up phone type in the PhoneTypes table
 *
 * It is expected that the inputs have already been validated
 *)
procedure EditPhone(Q: TSQLQuery; Id: Integer; PersonId: Integer; Number: String; PhoneTypeId: Integer);

begin
 try
   try
      Q.Close();
      Q.Sql.Clear();
      // NOTES:
      //   To "Open" a query, Sql.text must be set to a select statment.
      //   This is because open makes the result set available, and insert
      //   statements do not have a result set.
      //
      //   The InsertSql.Text property also exists, apparently use Insert()
      //    instead of ExecSQL
      Q.Sql.Text := 'UPDATE PhoneNumbers ' +
                  'SET PersonId = ' + IntToStr(PersonId) + ', ' +
                       'PhoneTypeId = ' + IntToStr(PhoneTypeId) + ', ' +
                       'Number = ' + QuotedStr(Number) + ' ' +
                  'WHERE Id = ' + IntToStr(Id);
      DebugLn(Q.Sql.text);
      Q.ExecSql();           // Insert/Update use execSQL

      // ApplyUpdates(); - Needed to save when editing an existing record
      if (Q.Transaction.Active) then
          begin
          // Cast the transaction property of the query to TSQLTr...
          TSQLTransaction(Q.Transaction).Commit();
          end;

   except
     on E: Exception do
     begin
       if (Q.Transaction.Active) then
           begin
           TSQLTransaction(Q.Transaction).Rollback();
           end;
       Utils.ShowException(E);
     end;
   end;
 finally
   // Nothing needed here.
 end;
end;

procedure DeletePhone(Q: TSQLQuery; SelectedID: Integer);
var
   SqlString : String;

begin
    try
       Q.Close();      // In Delphi you would set Active = false here
       Q.sql.Clear();
       SqlString := FORMAT('DELETE FROM PhoneNumbers WHERE Id = %d', [SelectedID]);
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

