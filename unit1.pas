unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  unitData,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, DBGrids,
  StdCtrls, SQLDB, DB, SQLite3Conn,
  LazLogger, LazLoggerBase;

type

  { TFormContacts }

  TFormContacts = class(TForm)
    ButtonSearch: TButton;
    ButtonAdd: TButton;
    DBGridPhones: TDBGrid;
    DBGridPeople: TDBGrid;
    EditSearch: TEdit;
    LabelContact: TLabel;
    procedure ButtonSearchClick(Sender: TObject);
    procedure DBGridPeopleCellClick(Column: TColumn);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HideIds();
  private

  public

  end;

var
  FormContacts: TFormContacts;

implementation

{$R *.lfm}

{ TFormContacts }


procedure TFormContacts.ButtonSearchClick(Sender: TObject);
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

procedure TFormContacts.DBGridPeopleCellClick(Column: TColumn);
var
   RowIndex: Integer;
   ColIndex: Integer;
   SelectedRow: Integer;
   SelectedId: Integer;
   CellValue: Variant;
   QueryString: String;
   FilterString: String;
begin
  try
    try
          RowIndex := DBGridPeople.DataSource.DataSet.RecNo;
          ColIndex := Column.Index;
          if (RowIndex >= 1) and (RowIndex <= DBGridPeople.DataSource.DataSet.RecordCount) then
          begin
             SelectedId := DBGridPeople.Datasource.DataSet.FieldByName('Id').AsInteger;
             DebugLn('Corresponding Record for Cell(%d, %d) = %d', [RowIndex, ColIndex, SelectedId]);
             if (SelectedId >= 1) then
             begin
                with DataModule1.QueryPhones do
                 begin
                     Close();      // In Delphi you would set Active = false here
                     sql.Clear();
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
                     sql.text := queryString;
                     execSql;
                     Open();    //In Delphi you would set active:= true here

                 end;
             end;
          end;
    except
        on E: Exception do
        begin
          DebugLn('Exception Type: ', E.ClassName);
          DebugLn('Exception Msg:  ', E.Message);
          end;
    end;
  finally
       DebugLn('Finallly!');
       DataModule1.QueryPhones.Refresh();
       DBGridPhones.Refresh();
       HideIds();
  end;
end;

procedure TFormContacts.FormActivate(Sender: TObject);
begin
     DBGridPeople.Refresh();
     DBGridPhones.Refresh();
end;

procedure TFormContacts.FormShow(Sender: TObject);
begin
     HideIds();
end;

(* HideIds
 * The Database grids contain IDs so we can extract the ID values to do filtering
 * and matching on them.
 *
 * This procedure hides those columns
 *)
procedure TFormContacts.HideIds();
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

