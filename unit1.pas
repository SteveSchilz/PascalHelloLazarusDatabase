unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  unitData,
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
    EditSearch: TEdit;
    LabelContact: TLabel;
    MainMenu1: TMainMenu;
    procedure ButtonSearchClick(Sender: TObject);
    procedure DBGridPeopleCellClick(Column: TColumn);
    procedure FormShow(Sender: TObject);
    procedure HideIds();
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
     DataModule1.QueryPeople.ServerFilter := FilterString;
     DataModule1.QueryPeople.ServerFiltered := true;

     DataModule1.QueryPeople.refresh();
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
  try
    try
          RowIndex := DBGridPeople.DataSource.DataSet.RecNo;
          ColIndex := Column.Index;
          CellValue := DBGridPeople.DataSource.DataSet.Fields[ColIndex].Value;
          SelectedRow := DBGridPhones.DataSource.Dataset.RecNo;
          if (SelectedRow >= 1) and (SelectedRow <= DBGridPeople.DataSource.DataSet.RecordCount) then
          begin
             SelectedId := DBGridPeople.Datasource.DataSet.FieldByName('Id').AsInteger;
             DebugLn(Format('Clicked Cell: Row %d, Column %d - Value: %s', [RowIndex, ColIndex, CellValue]));
             DebugLn('Corresponding Record for Row %d = %d', [SelectedRow, SelectedId]);
             if (SelectedId >= 1) then
             begin
                 DataModule1.QueryPhones.ServerFilter := Format('[PhoneNumbers].PersonId=%d', [SelectedId]);
                 DebugLn('Filtering Phone list on %s', [DataModule1.QueryPhones.ServerFilter]);

                 // TODO: This blows up on following line,
                 //       but says problem is in the SQLite3Connection, near "Where"
                 // TRIED:
                 //       QueryPhones.ServerFilter := Format('PersonId = %d', [SelectedId]);
                 //       QueryPhones.ServerFilter := Format('[PhoneNumbers].PersonId = %d', [SelectedId]);
                 //       QueryPhones.ServerFilter := Format('[PhoneTypes].Id = %d', [SelectedId]);
                 //
                 DataModule1.QueryPhones.ServerFiltered := true;
             end;
          end;
    except
        on E: Exception do
        begin
          DebugLn('Exception Type: ', E.ClassName);
          DebugLn('Exception Msg:  ', E.Message);
          DataModule1.QueryPhones.ServerFiltered := false;
          DataModule1.QueryPhones.ServerFilter := '';
          end;
    end;
  finally
       DebugLn('Finallly!');
       DataModule1.QueryPhones.Refresh();
       DBGridPhones.Refresh();
       HideIds();
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
     HideIds();
end;

(* HideIds
 * The Database grids contain IDs so we can extract the ID values to do filtering
 * and matching on them.
 *
 * This procedure hides those columns
 *)
procedure TForm1.HideIds();
begin

    if (DBGridPeople.Columns.Count >= 1) and (DBGridPeople.Columns[0].Visible) then
    begin
         DBGridPeople.Columns[0].Visible := false;
         DBGridPeople.Refresh();
     end;
     if (DBGridPhones.Columns.Count >= 1) and (DBGridPhones.Columns[0].Visible) then
     begin
         DBGridPhones.Columns[0].Visible := false;  // Hide Phone Id Column
         DBGridPhones.Columns[1].Visible := false;  // Hide PersonId column
         DBGridPhones.Refresh();
     end;

end;

end.

