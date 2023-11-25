unit unitAddContact;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFrmAddContact }

  TFrmAddContact = class(TForm)
    ButtonSave: TButton;
    ButtonCancel: TButton;
    EditID: TEdit;
    EditFirst: TEdit;
    EditLast: TEdit;
    Label_ID: TLabel;
    LabelFirst: TLabel;
    LabelLast: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
  private

  public

  end;

var
  FrmAddContact: TFrmAddContact;

implementation

{$R *.lfm}

{ TFrmAddContact }


end.

