unit uFormCliente;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.StrUtils, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, Data.DB, Vcl.ComCtrls, System.Generics.Collections,
  uCliente, uClienteDAO, FireDAC.Comp.Client;

type
  TFormCliente = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    lblTitulo: TLabel;
    PageControl1: TPageControl;
    tsLista: TTabSheet;
    tsCadastro: TTabSheet;
    GridClientes: TStringGrid;
    pnlBotoesLista: TPanel;
    btnNovo: TButton;
    btnEditar: TButton;
    btnExcluir: TButton;
    btnAtualizar: TButton;
    lblNome: TLabel;
    edtNome: TEdit;
    lblContato: TLabel;
    edtContato: TEdit;
    lblDocumento: TLabel;
    edtDocumento: TEdit;
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
    procedure GridClientesDblClick(Sender: TObject);
    procedure edtFiltroChange(Sender: TObject);
    procedure chkApenasAtivosClick(Sender: TObject);
    procedure edtDocumentoExit(Sender: TObject);
    procedure edtDocumentoKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    
  private
    FClienteDAO: TClienteDAO;
    FClienteAtual: TCliente;
    FListaClientes: TObjectList<TCliente>;
    
    procedure ConfigurarGrid;
    procedure CarregarClientes;
    procedure PreencherGrid;
    procedure LimparCampos;
    procedure CarregarCliente(Cliente: TCliente);
    procedure HabilitarEdicao(Habilitar: Boolean);
    function ValidarCampos: Boolean;
    function FormatarDocumento(const Documento: string): string;
    function LimparDocumento(const Documento: string): string;
    
  public
    { Public declarations }
  end;

var
  FormCliente: TFormCliente;

implementation

{$R *.dfm}

uses UDMDTO;

procedure TFormCliente.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormCliente.FormCreate(Sender: TObject);
begin
  FClienteAtual := TCliente.Create;
  FListaClientes := TObjectList<TCliente>.Create;
  
  ConfigurarGrid;
  CarregarClientes;
  
  PageControl1.ActivePage := tsLista;
end;

procedure TFormCliente.FormDestroy(Sender: TObject);
begin
  FListaClientes.Free;
  FClienteAtual.Free;
  FClienteDAO.Free;
end;

procedure TFormCliente.ConfigurarGrid;
begin
  GridClientes.ColCount := 6;
  GridClientes.RowCount := 1;

  GridClientes.Cells[0, 0] := 'ID';
  GridClientes.Cells[1, 0] := 'Nome';
  GridClientes.Cells[2, 0] := 'Contato';
  GridClientes.Cells[3, 0] := 'Documento';
  GridClientes.Cells[4, 0] := 'Data Cadastro';
  GridClientes.Cells[5, 0] := 'Ativo';
  
  GridClientes.ColWidths[0] := 50;
  GridClientes.ColWidths[1] := 250;
  GridClientes.ColWidths[2] := 150;
  GridClientes.ColWidths[3] := 150;
  GridClientes.ColWidths[4] := 120;
  GridClientes.ColWidths[5] := 60;
end;

procedure TFormCliente.CarregarClientes;
begin
  FListaClientes.Clear;
  FListaClientes.Free;
  FListaClientes := FClienteDAO.ListarTodos(chkApenasAtivos.Checked);
  PreencherGrid;
end;

procedure TFormCliente.PreencherGrid;
var
  I: Integer;
  Cliente: TCliente;
  Filtro: string;
begin
  GridClientes.RowCount := 1;
  Filtro := UpperCase(Trim(edtFiltro.Text));
  
  for I := 0 to FListaClientes.Count - 1 do
  begin
    Cliente := FListaClientes[I];

    if (Filtro <> '') and
       (Pos(Filtro, UpperCase(Cliente.Nome)) = 0) and
       (Pos(Filtro, UpperCase(Cliente.Contato)) = 0) and
       (Pos(Filtro, UpperCase(Cliente.Documento)) = 0) then
      Continue;
    
    GridClientes.RowCount := GridClientes.RowCount + 1;
    GridClientes.Cells[0, GridClientes.RowCount - 1] := IntToStr(Cliente.ID);
    GridClientes.Cells[1, GridClientes.RowCount - 1] := Cliente.Nome;
    GridClientes.Cells[2, GridClientes.RowCount - 1] := Cliente.Contato;
    GridClientes.Cells[3, GridClientes.RowCount - 1] := FormatarDocumento(Cliente.Documento);
    GridClientes.Cells[4, GridClientes.RowCount - 1] := 
      FormatDateTime('dd/mm/yyyy hh:nn', Cliente.DataCadastro);
    GridClientes.Cells[5, GridClientes.RowCount - 1] := 
      IfThen(Cliente.Ativo, 'Sim', 'Não');
  end;
end;

procedure TFormCliente.LimparCampos;
begin
  FClienteAtual.Clear;
  edtNome.Clear;
  edtContato.Clear;
  edtDocumento.Clear;
  chkAtivo.Checked := True;
end;

procedure TFormCliente.CarregarCliente(Cliente: TCliente);
begin
  FClienteAtual.ID := Cliente.ID;
  FClienteAtual.Nome := Cliente.Nome;
  FClienteAtual.Contato := Cliente.Contato;
  FClienteAtual.Documento := Cliente.Documento;
  FClienteAtual.Ativo := Cliente.Ativo;
  
  edtNome.Text := Cliente.Nome;
  edtContato.Text := Cliente.Contato;
  edtDocumento.Text := FormatarDocumento(Cliente.Documento);
  chkAtivo.Checked := Cliente.Ativo;
end;

procedure TFormCliente.HabilitarEdicao(Habilitar: Boolean);
begin
  edtNome.Enabled := Habilitar;
  edtContato.Enabled := Habilitar;
  edtDocumento.Enabled := Habilitar;
  chkAtivo.Enabled := Habilitar;
  btnSalvar.Enabled := Habilitar;
end;

function TFormCliente.ValidarCampos: Boolean;
begin
  Result := False;
  
  if Trim(edtNome.Text) = '' then
  begin
    ShowMessage('Informe o nome do cliente!');
    edtNome.SetFocus;
    Exit;
  end;
  
  if Trim(edtContato.Text) = '' then
  begin
    ShowMessage('Informe o contato do cliente!');
    edtContato.SetFocus;
    Exit;
  end;
  
  if Trim(edtDocumento.Text) = '' then
  begin
    ShowMessage('Informe o documento do cliente!');
    edtDocumento.SetFocus;
    Exit;
  end;
  
  Result := True;
end;

function TFormCliente.LimparDocumento(const Documento: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Documento) do
  begin
    if CharInSet(Documento[I], ['0'..'9']) then
      Result := Result + Documento[I];
  end;
end;

function TFormCliente.FormatarDocumento(const Documento: string): string;
var
  Doc: string;
begin
  Doc := LimparDocumento(Documento);
  
  // CPF: 000.000.000-00
  if Length(Doc) = 11 then
  begin
    Result := Copy(Doc, 1, 3) + '.' +
              Copy(Doc, 4, 3) + '.' +
              Copy(Doc, 7, 3) + '-' +
              Copy(Doc, 10, 2);
  end
  // CNPJ: 00.000.000/0000-00
  else if Length(Doc) = 14 then
  begin
    Result := Copy(Doc, 1, 2) + '.' +
              Copy(Doc, 3, 3) + '.' +
              Copy(Doc, 6, 3) + '/' +
              Copy(Doc, 9, 4) + '-' +
              Copy(Doc, 13, 2);
  end
  else
    Result := Documento;
end;

procedure TFormCliente.btnNovoClick(Sender: TObject);
begin
  LimparCampos;
  PageControl1.ActivePage := tsCadastro;
  HabilitarEdicao(True);
end;

procedure TFormCliente.btnEditarClick(Sender: TObject);
var
  ID: Integer;
  Cliente: TCliente;
begin
  if GridClientes.Row < 1 then
  begin
    ShowMessage('Selecione um cliente para editar!');
    Exit;
  end;
  
  ID := StrToIntDef(GridClientes.Cells[0, GridClientes.Row], 0);
  
  if ID > 0 then
  begin
    Cliente := FClienteDAO.BuscarPorID(ID);
    try
      if Assigned(Cliente) then
      begin
        CarregarCliente(Cliente);
        PageControl1.ActivePage := tsCadastro;
        HabilitarEdicao(True);
      end;
    finally
      Cliente.Free;
    end;
  end;
end;

procedure TFormCliente.btnExcluirClick(Sender: TObject);
var
  ID: Integer;
begin
  if GridClientes.Row < 1 then
  begin
    ShowMessage('Selecione um cliente para excluir!');
    Exit;
  end;
  
  if MessageDlg('Deseja realmente excluir este cliente?', 
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ID := StrToIntDef(GridClientes.Cells[0, GridClientes.Row], 0);
    
    try
      if FClienteDAO.Excluir(ID) then
      begin
        ShowMessage('Cliente excluído com sucesso!');
        CarregarClientes;
      end;
    except
      on E: Exception do
        ShowMessage('Erro ao excluir cliente: ' + E.Message);
    end;
  end;
end;

procedure TFormCliente.btnAtualizarClick(Sender: TObject);
begin
  CarregarClientes;
end;

procedure TFormCliente.btnSalvarClick(Sender: TObject);
begin
  if not ValidarCampos then
    Exit;
  
  FClienteAtual.Nome := edtNome.Text;
  FClienteAtual.Contato := edtContato.Text;
  FClienteAtual.Documento := LimparDocumento(edtDocumento.Text);
  FClienteAtual.Ativo := chkAtivo.Checked;
  
  try
    if FClienteAtual.ID = 0 then
    begin
      // Inserir novo cliente
      if FClienteDAO.Inserir(FClienteAtual) then
      begin
        ShowMessage('Cliente cadastrado com sucesso!');
        CarregarClientes;
        PageControl1.ActivePage := tsLista;
      end;
    end
    else
    begin
      // Atualizar cliente existente
      if FClienteDAO.Atualizar(FClienteAtual) then
      begin
        ShowMessage('Cliente atualizado com sucesso!');
        CarregarClientes;
        PageControl1.ActivePage := tsLista;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao salvar cliente: ' + E.Message);
  end;
end;

procedure TFormCliente.btnCancelarClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsLista;
  LimparCampos;
end;

procedure TFormCliente.GridClientesDblClick(Sender: TObject);
begin
  btnEditarClick(Sender);
end;

procedure TFormCliente.edtFiltroChange(Sender: TObject);
begin
  PreencherGrid;
end;

procedure TFormCliente.chkApenasAtivosClick(Sender: TObject);
begin
  CarregarClientes;
end;

procedure TFormCliente.edtDocumentoExit(Sender: TObject);
begin
  // Formatar documento ao sair do campo
  edtDocumento.Text := FormatarDocumento(edtDocumento.Text);
end;

procedure TFormCliente.edtDocumentoKeyPress(Sender: TObject; var Key: Char);
begin
  // Permitir apenas números e teclas de controle
  if not (CharInSet(Key, ['0'..'9', #8, #9])) then
    Key := #0;
end;

end.
