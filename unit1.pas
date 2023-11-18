unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, DBGrids, SQLDB,
  DB, SQLite3Conn;

type

  { TForm1 }

  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBGridPeople: TDBGrid;
    DSPeople: TDataSource;
    DSPhones: TDataSource;
    DSTypes: TDataSource;
    MainMenu1: TMainMenu;
    SQLite3Connection1: TSQLite3Connection;
    QueryPeople: TSQLQuery;
    QueryPhones: TSQLQuery;
    QueryTypes: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure SQLite3Connection1AfterConnect(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SQLite3Connection1AfterConnect(Sender: TObject);
begin

end;

end.

