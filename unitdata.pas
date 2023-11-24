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
    DSTypes: TDataSource;
    QueryPeople: TSQLQuery;
    QueryPhones: TSQLQuery;
    QueryTypes: TSQLQuery;
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
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

end;


end.

