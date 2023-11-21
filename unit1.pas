unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, DBGrids,
  StdCtrls, SQLDB, DB, SQLite3Conn;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonSearch: TButton;
    ButtonAdd: TButton;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBGridPeople: TDBGrid;
    DSPeople: TDataSource;
    DSPhones: TDataSource;
    DSTypes: TDataSource;
    EditSearch: TEdit;
    LabelContact: TLabel;
    MainMenu1: TMainMenu;
    SQLite3Connection1: TSQLite3Connection;
    QueryPeople: TSQLQuery;
    QueryPhones: TSQLQuery;
    QueryTypes: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure ButtonSearchClick(Sender: TObject);
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

procedure TForm1.ButtonSearchClick(Sender: TObject);
var
   FilterString : String;
begin
     (*
      * String escaping and concatenation:
      *   * double quotes escapes a quote:  '' => ",
      *   * % is a normal character
      *   * plus sign concatenates elements
      * This ends up something like
      *
      *      'First Like "%Ste%"'
      *)
     FilterString := 'Name Like ''%' + EditSearch.Text + '%''';
     WriteLn(FilterString);

     (* Server Filter filters on the database side, as opposed to .Filter which
      * Filters the result table
      *)
     QueryPeople.ServerFilter := FilterString;
     QueryPeople.ServerFiltered := true;

     QueryPeople.refresh();
     DBGridPeople.refresh();
end;

end.

