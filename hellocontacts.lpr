program helloContacts;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, unitData, unitAddContact, Utils;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormContacts, FormContacts);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFrmAddContact, FrmAddContact);
  Application.Run;
end.

