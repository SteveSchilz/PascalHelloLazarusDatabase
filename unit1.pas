unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, DBGrids,
  StdCtrls, SQLDB, DB, SQLite3Conn,
  LazLogger, LazLoggerBase;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonSearch: TButton;
    ButtonAdd: TButton;
    DBGridPhones: TDBGrid;
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
    procedure DBGridPeopleCellClick(Column: TColumn);
    procedure QueryPeopleAfterOpen(DataSet: TDataSet);
    procedure QueryPhonesAfterOpen(DataSet: TDataSet);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


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

procedure TForm1.DBGridPeopleCellClick(Column: TColumn);
var
   RowIndex: Integer;
   ColIndex: Integer;
   SelectedRow: Integer;
   SelectedId: Integer;
   CellValue: Variant;

begin
     RowIndex := DBGridPeople.DataSource.DataSet.RecNo;
     ColIndex := Column.Index;
     CellValue := DBGridPeople.DataSource.DataSet.Fields[ColIndex].Value;
     SelectedRow := DBGridPhones.DataSource.Dataset.RecNo;
     if (SelectedRow >= 1) and (SelectedRow <= DBGridPeople.DataSource.DataSet.RecordCount) then
     begin
         SelectedId := DBGridPeople.Datasource.DataSet.FieldByName('Id').AsInteger;
         DebugLn(Format('Clicked Cell: Row %d, Column %d - Value: %s', [RowIndex, ColIndex, CellValue]));
         DebugLn('Corresponding Record for Row %d = %d', [SelectedRow, SelectedId]);
     end;

     QueryPhones.ServerFilter := Format('PersonId = %d', [SelectedId]);
     DebugLn('Filtering Phone list on %s', [QueryPhones.ServerFilter]);
     QueryPhones.ServerFiltered := true;
     QueryPhones.Refresh();
     DBGridPhones.Refresh();
end;

procedure TForm1.QueryPeopleAfterOpen(DataSet: TDataSet);
begin
     if (DBGridPeople.Columns.Count >= 1) and (DBGridPeople.Columns[0].Visible) then
     begin
          DBGridPeople.Columns[0].Visible := false;
          DBGridPeople.Refresh();
     end;

end;

procedure TForm1.QueryPhonesAfterOpen(DataSet: TDataSet);
begin
     if (DBGridPhones.Columns.Count >= 1) and (DBGridPhones.Columns[0].Visible) then
     begin
          DBGridPhones.Columns[0].Visible := false;  // Hide Phone Id Column
          DBGridPhones.Columns[1].Visible := false;  // Hide PersonId column
          DBGridPhones.Refresh();
     end;

end;


end.

