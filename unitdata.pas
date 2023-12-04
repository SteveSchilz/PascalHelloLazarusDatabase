unit unitData;

{$mode ObjFPC}{$H+}

interface

uses
  Utils,
  Classes, SysUtils, LazLogger, LCLIntf, SQLite3Conn, SQLDB, DB;

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

        if (not FileExists(DBPathName)) then
        begin
            SQLite3Connection1.DatabaseName := DBPathName;
            SQLite3Connection1.Connected:= true;
            if (SQLite3Connection1.Connected = true) then
            begin
                DebugLn('Successfully created Database File: ', DBPathName);
                // TODO: Import schema for database
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
begin
    queryPhoneType.Database := SQLite3Connection1;
    queryPhoneType.SQL.Text := 'SELECT Type FROM PhoneTypes';
end;

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

