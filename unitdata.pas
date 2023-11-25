unit unitData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    DSPeople: TDataSource;
    DSPhones: TDataSource;
    QueryPeople: TSQLQuery;
    QueryPhones: TSQLQuery;
    SQLite3Connection1: TSQLite3Connection;
    QueryInsert: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
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

