unit unitData;

{$mode ObjFPC}{$H+}

interface

uses
  Utils,
  Classes, SysUtils, LCLIntf, SQLite3Conn, SQLDB, DB,
  LazLogger, LazLoggerBase;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    DSPeople: TDataSource;
    DSPhones: TDataSource;
    QueryPeople: TSQLQuery;
    QueryPhones: TSQLQuery;
    QueryPhoneType: TSQLQuery;
    SQLite3Connection1: TSQLite3Connection;
    QueryInsert: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure InitializePhoneTypesQuery();
    procedure EnsureDatabasePresent();
    procedure EnsureMainQueriesActive();
    procedure RefreshAllData();
  private

  public
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
     EnsureDatabasePresent();

     InitializePhoneTypesQuery();
end;

procedure TDataModule1.EnsureDatabasePresent();
const
  DBFileName = 'helloContacts.db';
var
  AppConfigDir: String;
  DBPathName: String;
  i : Integer;
  ResourceStream : TResourceStream;
  SQLScript: TStringList;          // Entire script, one line per SQL statement
  QueryCreate: TSQLQuery;

begin
     AppConfigDir := GetAppConfigDir(False);
     DBPathName := AppConfigDir + DBFileName;
     DebugLn('DataBase File: ', DBPathName);

     try

        if (not DirectoryExists(AppConfigDir)) then
        begin
             CreateDir(AppConfigDir);
             DebugLn('Created App Config Directory', AppConfigDir);
        end;

        SQLite3Connection1.DatabaseName := DBPathName;
        SQLite3Connection1.Transaction := SQLTransaction1;
        if (FileExists(DBPathName)) then
            begin
            SQLite3Connection1.Connected:= true;
            end
        else
            begin
            SQLite3Connection1.Connected:= true;
            if (SQLite3Connection1.Connected = true) then
                begin
                try
                    DebugLn('Successfully created Database File: ', DBPathName);

                    // Now Load the Database Schema from .SQL Resource File.
                    //
                    // Note: the SQL Resource file must be one SQL statement per
                    //       line to simplify string processing.  This is because
                    //       the ExecSQL command will only execute a single statement.
                    //
                    ResourceStream := TResourceStream.Create(HInstance, 'DATABASE_SCHEMA', RT_RCDATA);
                    SQLScript := TStringList.Create();
                    SQLScript.LoadFromStream(ResourceStream);

                    QueryCreate := TSQLQuery.Create(nil);
                    QueryCreate.Database := SQLite3Connection1;
                    QueryCreate.Transaction := SQLTransaction1;

                    DebugLn('------------');
                    for i:= 0 to (SQLScript.Count - 1) do
                        begin
                        QueryCreate.SQL.Clear();
                        QueryCreate.SQL.Text := SQLSCript[i];
                        Debugln('[%d]: %s', [i, SQLScript[i] ]);
                        QueryCreate.ExecSQL();
                        SQLite3Connection1.Transaction.Commit();
                        end;
                    DebugLn('------------');
                finally
                    ResourceStream.Free();
                    QueryCreate.Free();
                    SQLScript.Free();
                end;
                end
            else
                begin
                DebugLn('Database File Not Connected!!: ', DBPathName);
                end;
        end;

       except
         on E: Exception do
             Utils.ShowException(E);
       end;
end;

// Called when main form activates to be sure queries are active
procedure TDataModule1.EnsureMainQueriesActive();
begin

     if (not QueryPeople.active) then
     begin
       QueryPeople.Active := true;
     end;

     if (not QueryPhones.active) then
     begin
       QueryPhones.Active := true;
     end;

end;


procedure TDataModule1.InitializePhoneTypesQuery();
var
  Q: TSQLQuery;
begin
    Q := QueryPhoneType;
    Q.Database := SQLite3Connection1;
    Q.Close();
    Q.SQL.Clear();
    Q.SQL.Text := 'SELECT Id, Type FROM PhoneTypes';
    try
        Q.ExecSQL();
        Q.Open();
        DebugLn(Format('Record Count %d', [Q.RecordCount]));

    except
       on E: Exception do
         begin
           Utils.ShowException(E);
         end;
     end;    end;



// Allows customers to update all data fields without needing to know our
// Internal Details
procedure TDataModule1.RefreshAllData();

begin
     QueryPeople.Refresh();
     DSPeople.DataSet.Refresh();

     QueryPhones.Refresh();
     DSPhones.DataSet.Refresh();
end;

end.

