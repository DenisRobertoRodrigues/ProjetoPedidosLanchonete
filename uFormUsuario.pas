unit uFormUsuario;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.StrUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.UITypes,
  Vcl.Grids, Vcl.DBGrids, Data.DB, Vcl.ComCtrls, System.Generics.Collections,
  uUsuario, uUsuarioDAO, Math;

type
  TFormUsuario = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    lblTitulo: TLabel;
    PageControl1: TPageControl;
    tsLista: TTabSheet;
    tsCadastro: TTabSheet;
    GridUsuarios: TStringGrid;
    pnlBotoesLista: TPanel;
    btnNovo: TButton;
    btnEditar: TButton;
    btnExcluir: TButton;
    btnAtualizar: TButton;
    lblNome: TLabel;
    edtNome: TEdit;
    lblEmail: TLabel;
    edtEmail: TEdit;
    lblSenha: TLabel;
    edtSenha: TEdit;
    lblConfirmaSenha: TLabel;
    edtConfirmaSenha: TEdit;
    lblTipo: TLabel;
    cmbTipo: TComboBox;
    chkAtivo: TCheckBox;
    pnlBotoesCadastro: TPanel;
    btnSalvar: TButton;
    btnCancelar: TButton;
    pnlFiltro: TPanel;
    lblFiltro: TLabel;
    edtFiltro: TEdit;
    chkApenasAtivos: TCheckBox;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure GridUsuariosDblClick(Sender: TObject);
    procedure edtFiltroChange(Sender: TObject);
    procedure chkApenasAtivosClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    
  private
    FUsuarioDAO: TUsuarioDAO;
    FUsuarioAtual: TUsuario;
    FListaUsuarios: TObjectList<TUsuario>;
    
    procedure ConfigurarGrid;
    procedure CarregarUsuarios;
    procedure PreencherGrid;
    procedure LimparCampos;
    procedure CarregarUsuario(Usuario: TUsuario);
    procedure HabilitarEdicao(Habilitar: Boolean);
    function ValidarCampos: Boolean;
    
  public
    { Public declarations }
  end;

var
  FormUsuario: TFormUsuario;

implementation

{$R *.dfm}

procedure TFormUsuario.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormUsuario.FormCreate(Sender: TObject);
begin
  FUsuarioAtual := TUsuario.Create;
  FListaUsuarios := TObjectList<TUsuario>.Create;

  ConfigurarGrid;
  CarregarUsuarios;

  PageControl1.ActivePage := tsLista;

  cmbTipo.Items.Clear;
  cmbTipo.Items.Add('Normal');
  cmbTipo.Items.Add('Administrador');
  cmbTipo.ItemIndex := 0;
end;

procedure TFormUsuario.FormDestroy(Sender: TObject);
begin
  FListaUsuarios.Free;
  FUsuarioAtual.Free;
end;

procedure TFormUsuario.ConfigurarGrid;
begin
  GridUsuarios.ColCount := 6;
  GridUsuarios.RowCount := 1;

  GridUsuarios.Cells[0, 0] := 'ID';
  GridUsuarios.Cells[1, 0] := 'Nome';
  GridUsuarios.Cells[2, 0] := 'E-mail';
  GridUsuarios.Cells[3, 0] := 'Tipo';
  GridUsuarios.Cells[4, 0] := 'Data Cadastro';
  GridUsuarios.Cells[5, 0] := 'Ativo';
  
  GridUsuarios.ColWidths[0] := 50;
  GridUsuarios.ColWidths[1] := 200;
  GridUsuarios.ColWidths[2] := 200;
  GridUsuarios.ColWidths[3] := 100;
  GridUsuarios.ColWidths[4] := 120;
  GridUsuarios.ColWidths[5] := 60;
end;

procedure TFormUsuario.CarregarUsuarios;
begin
  FListaUsuarios.Clear;
  FListaUsuarios.Free;
  FListaUsuarios := FUsuarioDAO.ListarTodos(chkApenasAtivos.Checked);
  PreencherGrid;
end;

procedure TFormUsuario.PreencherGrid;
var
  I: Integer;
  Usuario: TUsuario;
  Filtro: string;
begin
  GridUsuarios.RowCount := 1;
  Filtro := UpperCase(Trim(edtFiltro.Text));
  
  for I := 0 to FListaUsuarios.Count - 1 do
  begin
    Usuario := FListaUsuarios[I];

    if (Filtro <> '') and 
       (Pos(Filtro, UpperCase(Usuario.Nome)) = 0) and
       (Pos(Filtro, UpperCase(Usuario.Email)) = 0) then
      Continue;
    
    GridUsuarios.RowCount := GridUsuarios.RowCount + 1;
    GridUsuarios.Cells[0, GridUsuarios.RowCount - 1] := IntToStr(Usuario.ID);
    GridUsuarios.Cells[1, GridUsuarios.RowCount - 1] := Usuario.Nome;
    GridUsuarios.Cells[2, GridUsuarios.RowCount - 1] := Usuario.Email;
    GridUsuarios.Cells[3, GridUsuarios.RowCount - 1] := 
      IfThen(Usuario.Tipo = tuAdministrador, 'Administrador', 'Normal');
    GridUsuarios.Cells[4, GridUsuarios.RowCount - 1] := 
      FormatDateTime('dd/mm/yyyy hh:nn', Usuario.DataCadastro);
    GridUsuarios.Cells[5, GridUsuarios.RowCount - 1] := 
      IfThen(Usuario.Ativo, 'Sim', 'Não');
  end;
end;

procedure TFormUsuario.LimparCampos;
begin
  FUsuarioAtual.Clear;
  edtNome.Clear;
  edtEmail.Clear;
  edtSenha.Clear;
  edtConfirmaSenha.Clear;
  cmbTipo.ItemIndex := 0;
  chkAtivo.Checked := True;
end;

procedure TFormUsuario.CarregarUsuario(Usuario: TUsuario);
begin
  FUsuarioAtual.ID := Usuario.ID;
  FUsuarioAtual.Nome := Usuario.Nome;
  FUsuarioAtual.Email := Usuario.Email;
  FUsuarioAtual.Tipo := Usuario.Tipo;
  FUsuarioAtual.Ativo := Usuario.Ativo;
  
  edtNome.Text := Usuario.Nome;
  edtEmail.Text := Usuario.Email;
  edtSenha.Clear;
  edtConfirmaSenha.Clear;
  cmbTipo.ItemIndex := Integer(Usuario.Tipo);
  chkAtivo.Checked := Usuario.Ativo;
end;

procedure TFormUsuario.HabilitarEdicao(Habilitar: Boolean);
begin
  edtNome.Enabled := Habilitar;
  edtEmail.Enabled := Habilitar;
  edtSenha.Enabled := Habilitar;
  edtConfirmaSenha.Enabled := Habilitar;
  cmbTipo.Enabled := Habilitar;
  chkAtivo.Enabled := Habilitar;
  btnSalvar.Enabled := Habilitar;
end;

function TFormUsuario.ValidarCampos: Boolean;
begin
  Result := False;
  
  if Trim(edtNome.Text) = '' then
  begin
    ShowMessage('Informe o nome do usuário!');
    edtNome.SetFocus;
    Exit;
  end;
  
  if Trim(edtEmail.Text) = '' then
  begin
    ShowMessage('Informe o e-mail do usuário!');
    edtEmail.SetFocus;
    Exit;
  end;
  
  if (FUsuarioAtual.ID = 0) and (Trim(edtSenha.Text) = '') then
  begin
    ShowMessage('Informe a senha do usuário!');
    edtSenha.SetFocus;
    Exit;
  end;
  
  if Trim(edtSenha.Text) <> Trim(edtConfirmaSenha.Text) then
  begin
    ShowMessage('As senhas não conferem!');
    edtConfirmaSenha.SetFocus;
    Exit;
  end;
  
  if (Trim(edtSenha.Text) <> '') and (Length(edtSenha.Text) < 6) then
  begin
    ShowMessage('A senha deve ter no mínimo 6 caracteres!');
    edtSenha.SetFocus;
    Exit;
  end;
  
  Result := True;
end;

procedure TFormUsuario.btnNovoClick(Sender: TObject);
begin
  LimparCampos;
  PageControl1.ActivePage := tsCadastro;
  HabilitarEdicao(True);
end;

procedure TFormUsuario.btnEditarClick(Sender: TObject);
var
  ID: Integer;
  Usuario: TUsuario;
begin
  if GridUsuarios.Row < 1 then
  begin
    ShowMessage('Selecione um usuário para editar!');
    Exit;
  end;
  
  ID := StrToIntDef(GridUsuarios.Cells[0, GridUsuarios.Row], 0);
  
  if ID > 0 then
  begin
    Usuario := FUsuarioDAO.BuscarPorID(ID);
    try
      if Assigned(Usuario) then
      begin
        CarregarUsuario(Usuario);
        PageControl1.ActivePage := tsCadastro;
        HabilitarEdicao(True);
      end;
    finally
      Usuario.Free;
    end;
  end;
end;

procedure TFormUsuario.btnExcluirClick(Sender: TObject);
var
  ID: Integer;
begin
  if GridUsuarios.Row < 1 then
  begin
    ShowMessage('Selecione um usuário para excluir!');
    Exit;
  end;
  
  if MessageDlg('Deseja realmente excluir este usuário?', 
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ID := StrToIntDef(GridUsuarios.Cells[0, GridUsuarios.Row], 0);
    
    try
      if FUsuarioDAO.Excluir(ID) then
      begin
        ShowMessage('Usuário excluído com sucesso!');
        CarregarUsuarios;
      end;
    except
      on E: Exception do
        ShowMessage('Erro ao excluir usuário: ' + E.Message);
    end;
  end;
end;

procedure TFormUsuario.btnAtualizarClick(Sender: TObject);
begin
  CarregarUsuarios;
end;

procedure TFormUsuario.btnSalvarClick(Sender: TObject);
begin
  if not ValidarCampos then
    Exit;
  
  FUsuarioAtual.Nome := edtNome.Text;
  FUsuarioAtual.Email := edtEmail.Text;
  FUsuarioAtual.Senha := edtSenha.Text;
  FUsuarioAtual.Tipo := TTipoUsuario(cmbTipo.ItemIndex);
  FUsuarioAtual.Ativo := chkAtivo.Checked;
  
  try
    if FUsuarioAtual.ID = 0 then
    begin
      // Inserir novo usuário
      if FUsuarioDAO.Inserir(FUsuarioAtual) then
      begin
        ShowMessage('Usuário cadastrado com sucesso!');
        CarregarUsuarios;
        PageControl1.ActivePage := tsLista;
      end;
    end
    else
    begin
      // Atualizar usuário existente
      if FUsuarioDAO.Atualizar(FUsuarioAtual) then
      begin
        ShowMessage('Usuário atualizado com sucesso!');
        CarregarUsuarios;
        PageControl1.ActivePage := tsLista;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao salvar usuário: ' + E.Message);
  end;
end;

procedure TFormUsuario.btnCancelarClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsLista;
  LimparCampos;
end;

procedure TFormUsuario.GridUsuariosDblClick(Sender: TObject);
begin
  btnEditarClick(Sender);
end;

procedure TFormUsuario.edtFiltroChange(Sender: TObject);
begin
  PreencherGrid;
end;

procedure TFormUsuario.chkApenasAtivosClick(Sender: TObject);
begin
  CarregarUsuarios;
end;

end.
