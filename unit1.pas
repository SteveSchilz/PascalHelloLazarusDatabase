unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  UnitData, UnitAddContact, UnitAddPhone, Utils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, DBGrids,
  StdCtrls, ExtCtrls, SQLDB, DB, SQLite3Conn,
  Types,
  LazLogger, LazLoggerBase;


type

  { TFormContacts }

  TFormContacts = class(TForm)
    ButtonSearch: TButton;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    ButtonAddPhone: TButton;
    ButtonEditPhone: TButton;
    ButtonDeletePhone: TButton;
    DBGridPhones: TDBGrid;
    DBGridPeople: TDBGrid;
    EditSearch: TEdit;
    Image1: TImage;
    LabelContact: TLabel;
    LabelDetails: TLabel;
    PanelDetails: TPanel;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonAddPhoneClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonDeletePhoneClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonEditPhoneClick(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure DBGridPeopleCellClick(Column: TColumn);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowDBGridPhones(enable : Boolean);
    function GetSelectedId(Grid : TDBGrid) : Integer;
    function GetSelectedFieldByName(Grid : TDBGrid; Field: String) : String;
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
begin
    NamesArray := Utils.SplitName(EditSearch.Text);

    frmAddContact.EditId.Text := '';
    frmAddContact.EditFirst.Text := NamesArray[0];
    frmAddContact.EditLast.Text := NamesArray[1];
    frmAddContact.SetEditMode(false);
    frmAddContact.ShowModal();

end;

procedure TFormContacts.ButtonAddPhoneClick(Sender: TObject);
var
   personId : Integer;

begin
    personId := GetSelectedId(DBGridPeople);
    if (personId = -1) then
    begin
       exit;
     end;
     frmAddPhone.SetEditMode(false);
     frmAddPhone.SetPersonId(personId);
     frmAddPhone.SetPhoneTypeId(1);
     frmAddPhone.ShowModal();

end;

procedure TFormContacts.ButtonEditClick(Sender: TObject);
var
SelectedId: Integer;
SelectedName: String;
NamesArray : TStringDynArray;

begin
  SelectedId := GetSelectedId(DBGridPeople);
  if (SelectedId = -1) then
  begin
     MessageDlg('No User Selected to Edit!', mtInformation, mbOKCancel, 0);
  end
  else
  begin
    SelectedName := GetSelectedFieldByName(DBGridPeople, 'Name');
    NamesArray := Utils.SplitName(SelectedName);
    frmAddContact.EditId.Text := IntToStr(SelectedId);
    frmAddContact.EditFirst.Text := NamesArray[0];
    frmAddContact.EditLast.Text := NamesArray[1];
    frmAddContact.SetEditMode(true);
    frmAddContact.ShowModal();

  end;
end;

procedure TFormContacts.ButtonEditPhoneClick(Sender: TObject);
var
   SelectedId: Integer;
   SelectedPhone: String;

begin
  SelectedId := GetSelectedId(DBGridPhones);
  if (SelectedId = -1) then
  begin
     MessageDlg('No Phone Number Selected to Edit!', mtInformation, mbOKCancel, 0);
  end
  else
  begin
    frmAddPhone.SetPhoneId(SelectedId);

    SelectedPhone := GetSelectedFieldByName(DBGridPhones, 'Number');
    frmAddPhone.EditNumber.Text := SelectedPhone;

    SelectedId := StrToInt(GetSelectedFieldByName(DBGridPhones, 'PersonId'));
    frmAddPhone.SetPersonId(SelectedId);

    SelectedId := StrToInt(GetSelectedFieldByName(DBGridPhones, 'PhoneTypeId'));
    frmAddPhone.SetPhoneTypeId(SelectedId);

    frmAddPhone.SetEditMode(true);
    frmAddPhone.ShowModal();

  end;

end;

procedure TFormContacts.ButtonDeleteClick(Sender: TObject);
var
   SelectedId: Integer;
   SelectedName: String;
   Var OkToDelete: Integer;

begin
    try
        try
        begin
            SelectedId := GetSelectedId(DBGridPeople);
            if (SelectedId = -1) then
            begin
               MessageDlg('No User Selected to delete!', mtInformation, mbOKCancel, 0);
            end
            else
            begin
                SelectedName := GetSelectedFieldByName(DBGridPeople, 'Name');
                OkToDelete := MessageDlg('OK to Delete ' + SelectedName  + '?' + #13#10#13#10 +
                                         '(All of their phone numbers will be deleted!)',
                                         mtConfirmation, mbOKCancel, 0);
                if (OkToDelete = mrOk) then
                   begin
                   Utils.DeleteUser(DataModule1.QueryInsert, SelectedId);
                   ShowDBGridPhones(false);
                   end;
            end;
         end;
     except
         on E: Exception do
         begin
             Utils.ShowException(E);
         end;
     end;
    finally
        DataModule1.QueryPeople.Refresh();
        DBGridPeople.Refresh();
        HideIds();
    end;
end;

procedure TFormContacts.ButtonDeletePhoneClick(Sender: TObject);
   var
      SelectedId: Integer;
      SelectedNumber: String;
      Var OkToDelete: Integer;

   begin
       try
           try
           begin
               SelectedId := GetSelectedId(DBGridPhones);
               if (SelectedId = -1) then
               begin
                  MessageDlg('No Phone Selected to delete!', mtInformation, mbOKCancel, 0);
               end
               else
               begin
                   SelectedNumber := GetSelectedFieldByName(DBGridPhones, 'Number');
                   OkToDelete := MessageDlg('OK to Delete ' + SelectedNumber  + '? ',
                                            mtConfirmation, mbOKCancel, 0);
                   if (OkToDelete = mrOk) then
                      begin
                      Utils.DeletePhone(DataModule1.QueryInsert, SelectedId);
                      DBGridPhones.Refresh();
                      end;
               end;
            end;
        except
            on E: Exception do
            begin
                Utils.ShowException(E);
            end;
        end;
       finally
           DataModule1.QueryPhones.Refresh();
           DBGridPhones.Refresh();
           HideIds();
       end;

end;

// When the DB Grid is clicked, it filters the phones list to the selected
// person only.
procedure TFormContacts.DBGridPeopleCellClick(Column: TColumn);
var
   SelectedId: Integer;
begin
  try
    try
          begin
             SelectedId := GetSelectedId(DBGridPeople);
             if (SelectedId <> -1) then
             begin
                ShowDBGridPhones(true);
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
     ShowDBGridPhones(false);
end;

procedure TFormContacts.FormCreate(Sender: TObject);
var
   rs: TResourceStream;
   img: TPicture;
begin
  try
    begin
    try
        begin
          // TODO:  Consider moving this functionality to UTILS, especially if
          //        I need to load more pictures
          //
          // NOTES On Resources
          // Resource file is resources.rc, defines which files are available
          // This is compiled by {$R resources.rc} statement in hellocontacts.lpr
          //
          // On OSX I had to install the windres rc compiler, see Lazarus Install
          // Notes in Obsidian.
          //
          rs := TResourceStream.Create(HInstance, 'CONTACTS_ICON', RT_RCDATA);
          img := TPicture.Create();
          img.LoadFromStream(rs);
          Image1.Picture.assign(img);

          // This is a simpler way to accomplish the same thing, but if it fails
          // We only get an EAccessViolation, instead of A missing resource exception
//          Image1.Picture.LoadFromResourceName(HInstance, 'CONTACTS_ICON');
        end
      finally
      begin
        img.Free();
      end;
    end

  end;
  except
    on E: Exception Do
    begin
    Utils.ShowException(E);
    end;

  end;
end;

procedure TFormContacts.FormShow(Sender: TObject);
begin

     HideIds();
end;

procedure TFormContacts.ShowDBGridPhones(Enable : Boolean);
begin
     if (Enable) then
     begin
          LabelDetails.Caption := 'Phone List For: ' +  GetSelectedFieldByName(DBGridPeople, 'Name');
          PanelDetails.Visible := true;
     end
     else
     begin
         PanelDetails.Visible:= false;
     end;
end;

//Returns the current selected ID for the given DB Grid, or -1 if none.
function TFormContacts.GetSelectedId(Grid : TDBGrid) : Integer;
var
   RowIndex: Integer;
begin
  GetSelectedId := -1;  // Default if nothing found

  RowIndex := grid.DataSource.DataSet.RecNo;
  if (RowIndex >= 1) and (RowIndex <= grid.DataSource.DataSet.RecordCount) then
    begin
    GetSelectedId := grid.Datasource.DataSet.FieldByName('Id').AsInteger;
    end;
end;

// Returns the current selected Field for the given DB Grid, or empty string ('') if none.
function TFormContacts.GetSelectedFieldByName(Grid : TDBGrid; Field: String) : String;
var
   RowIndex: Integer;
begin
  GetSelectedFieldByName := '';  // Default if nothing found

  RowIndex := grid.DataSource.DataSet.RecNo;
  if (RowIndex >= 1) and (RowIndex <= grid.DataSource.DataSet.RecordCount) then
    begin
    GetSelectedFieldByName := grid.Datasource.DataSet.FieldByName(Field).AsString;
    end;
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
         DBGridPhones.Columns[1].Visible := false;  // Hide PersonId Column
         DBGridPhones.Columns[2].Visible := false;  // Hide PhoneTypeId Column
         DBGridPhones.Refresh();
     end;

end;

end.

