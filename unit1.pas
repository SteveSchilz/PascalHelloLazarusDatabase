unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  UnitData, UnitAddContact, Utils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, DBGrids,
  StdCtrls, SQLDB, DB, SQLite3Conn,
  StrUtils, Types,
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
    procedure ButtonAddClick(Sender: TObject);
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

procedure TFormContacts.ButtonAddClick(Sender: TObject);
var
   NamesArray : TStringDynArray;
   First:  String;
   Last: String;
begin
    First := '';
    Last := '';
    if (EditSearch.Text <> '') then
    begin
        NamesArray := SplitString(EditSearch.Text, ' ');
        if (Length(NamesArray) = 1) then
        begin
            First := NamesArray[0];
        end
        else if (Length(NamesArray) = 2) then
        begin
            First := NamesArray[0];
            Last := NamesArray[1];
        end;

        frmAddContact.EditId.Text := '';
        frmAddContact.EditFirst.Text := First;
        frmAddContact.EditLast.Text := Last;
        frmAddContact.ShowModal();
    end;

end;

procedure TFormContacts.DBGridPeopleCellClick(Column: TColumn);
var
   RowIndex: Integer;
   ColIndex: Integer;
   SelectedId: Integer;
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
                Utils.QueryPhones(DataModule1.QueryPhones, SelectedId);
             end;
          end;
    except
        on E: Exception do
        begin
          Utils.ShowException(E);
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
     // Queries used on this page need to be set to active when activating.
     // Apparently something in the system deactivates them when another form
     // receives focus.
     DataModule1.EnsureMainQueriesActive();
     DataModule1.RefreshAllData();

     DBGridPeople.Refresh();
     DBGridPhones.Refresh();
     HideIds();
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

