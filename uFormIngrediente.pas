unit uFormIngrediente;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.StrUtils, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, Data.DB, Vcl.ComCtrls, System.Generics.Collections, Vcl.ExtDlgs,
  Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, Vcl.Imaging.GIFImg,
  uIngrediente, uIngredienteDAO, FireDAC.Comp.Client;

type
  TFormIngrediente = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    lblTitulo: TLabel;
    PageControl1: TPageControl;
    tsLista: TTabSheet;
    tsCadastro: TTabSheet;
    GridIngredientes: TStringGrid;
    pnlBotoesLista: TPanel;
    btnNovo: TButton;
    btnEditar: TButton;
    btnExcluir: TButton;
    btnAtualizar: TButton;
    lblNome: TLabel;
    edtNome: TEdit;
    lblValor: TLabel;
    edtValor: TEdit;
    chkAtivo: TCheckBox;
    pnlBotoesCadastro: TPanel;
    btnSalvar: TButton;
    btnCancelar: TButton;
    pnlFiltro: TPanel;
    lblFiltro: TLabel;
    edtFiltro: TEdit;
    chkApenasAtivos: TCheckBox;
    pnlImagem: TPanel;
    imgIngrediente: TImage;
    btnSelecionarImagem: TButton;
    btnRemoverImagem: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    lblImagem: TLabel;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure GridIngredientesDblClick(Sender: TObject);
    procedure edtFiltroChange(Sender: TObject);
    procedure chkApenasAtivosClick(Sender: TObject);
    procedure btnSelecionarImagemClick(Sender: TObject);
    procedure btnRemoverImagemClick(Sender: TObject);
    procedure edtValorKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    
  private
    FIngredienteDAO: TIngredienteDAO;
    FIngredienteAtual: TIngrediente;
    FListaIngredientes: TObjectList<TIngrediente>;
    FCaminhoImagemSelecionada: string;
    
    procedure ConfigurarGrid;
    procedure CarregarIngredientes;
    procedure PreencherGrid;
    procedure LimparCampos;
    procedure CarregarIngrediente(Ingrediente: TIngrediente);
    procedure HabilitarEdicao(Habilitar: Boolean);
    function ValidarCampos: Boolean;
    procedure ExibirImagem(Ingrediente: TIngrediente);
    procedure LimparImagem;
    function FormatarValor(const Valor: string): Double;
    
  public
    { Public declarations }
  end;

var
  FormIngrediente: TFormIngrediente;

implementation

uses
  uDMDTO; // Unit do DataModule onde está a conexão

{$R *.dfm}

procedure TFormIngrediente.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormIngrediente.FormCreate(Sender: TObject);
begin
  // Criar o DAO usando a conexão existente
  //FIngredienteDAO := TIngredienteDAO.Create(DMDTO.FDConexao);
  FIngredienteAtual := TIngrediente.Create;
  FListaIngredientes := TObjectList<TIngrediente>.Create;
  
  ConfigurarGrid;
  CarregarIngredientes;
  
  PageControl1.ActivePage := tsLista;
  
  // Configurar o diálogo de abertura de imagem
  OpenPictureDialog.Filter := 'Imagens|*.jpg;*.jpeg;*.png;*.bmp;*.gif|Todos os arquivos|*.*';
  OpenPictureDialog.Title := 'Selecionar Imagem do Ingrediente';
  
  // Configurar propriedades da imagem
  imgIngrediente.Stretch := True;
  imgIngrediente.Proportional := True;
  imgIngrediente.Center := True;
end;

procedure TFormIngrediente.FormDestroy(Sender: TObject);
begin
  FListaIngredientes.Free;
  FIngredienteAtual.Free;
  FIngredienteDAO.Free;
end;

procedure TFormIngrediente.ConfigurarGrid;
begin
  GridIngredientes.ColCount := 6;
  GridIngredientes.RowCount := 1;
  
  GridIngredientes.Cells[0, 0] := 'ID';
  GridIngredientes.Cells[1, 0] := 'Nome';
  GridIngredientes.Cells[2, 0] := 'Valor';
  GridIngredientes.Cells[3, 0] := 'Imagem';
  GridIngredientes.Cells[4, 0] := 'Data Cadastro';
  GridIngredientes.Cells[5, 0] := 'Ativo';
  
  GridIngredientes.ColWidths[0] := 50;
  GridIngredientes.ColWidths[1] := 300;
  GridIngredientes.ColWidths[2] := 100;
  GridIngredientes.ColWidths[3] := 80;
  GridIngredientes.ColWidths[4] := 120;
  GridIngredientes.ColWidths[5] := 60;
end;

procedure TFormIngrediente.CarregarIngredientes;
begin
  FListaIngredientes.Clear;
  FListaIngredientes.Free;
  FListaIngredientes := FIngredienteDAO.ListarTodos(chkApenasAtivos.Checked);
  PreencherGrid;
end;

procedure TFormIngrediente.PreencherGrid;
var
  I: Integer;
  Ingrediente: TIngrediente;
  Filtro: string;
begin
  GridIngredientes.RowCount := 1;
  Filtro := UpperCase(Trim(edtFiltro.Text));
  
  for I := 0 to FListaIngredientes.Count - 1 do
  begin
    Ingrediente := FListaIngredientes[I];
    
    // Aplicar filtro se houver
    if (Filtro <> '') and 
       (Pos(Filtro, UpperCase(Ingrediente.Nome)) = 0) then
      Continue;
    
    GridIngredientes.RowCount := GridIngredientes.RowCount + 1;
    GridIngredientes.Cells[0, GridIngredientes.RowCount - 1] := IntToStr(Ingrediente.ID);
    GridIngredientes.Cells[1, GridIngredientes.RowCount - 1] := Ingrediente.Nome;
    GridIngredientes.Cells[2, GridIngredientes.RowCount - 1] := 
      FormatFloat('#,##0.00', Ingrediente.Valor);
    GridIngredientes.Cells[3, GridIngredientes.RowCount - 1] := 
      IfThen(Ingrediente.TemImagem, 'Sim', 'Não');
    GridIngredientes.Cells[4, GridIngredientes.RowCount - 1] := 
      FormatDateTime('dd/mm/yyyy hh:nn', Ingrediente.DataCadastro);
    GridIngredientes.Cells[5, GridIngredientes.RowCount - 1] := 
      IfThen(Ingrediente.Ativo, 'Sim', 'Não');
  end;
end;

procedure TFormIngrediente.LimparCampos;
begin
  FIngredienteAtual.Clear;
  edtNome.Clear;
  edtValor.Text := '0,00';
  chkAtivo.Checked := True;
  LimparImagem;
end;

procedure TFormIngrediente.LimparImagem;
begin
  imgIngrediente.Picture := nil;
  FCaminhoImagemSelecionada := '';
  btnRemoverImagem.Enabled := False;
end;

procedure TFormIngrediente.CarregarIngrediente(Ingrediente: TIngrediente);
begin
  FIngredienteAtual.ID := Ingrediente.ID;
  FIngredienteAtual.Nome := Ingrediente.Nome;
  FIngredienteAtual.Valor := Ingrediente.Valor;
  FIngredienteAtual.Ativo := Ingrediente.Ativo;
  
  // Copiar imagem
  FIngredienteAtual.Imagem.Clear;
  if Ingrediente.TemImagem then
  begin
    Ingrediente.Imagem.Position := 0;
    FIngredienteAtual.Imagem.CopyFrom(Ingrediente.Imagem, Ingrediente.Imagem.Size);
  end;
  
  edtNome.Text := Ingrediente.Nome;
  edtValor.Text := FormatFloat('0.00', Ingrediente.Valor);
  chkAtivo.Checked := Ingrediente.Ativo;
  
  ExibirImagem(Ingrediente);
end;

procedure TFormIngrediente.ExibirImagem(Ingrediente: TIngrediente);
var
  JPEGImage: TJPEGImage;
  PNGImage: TPngImage;
  Bitmap: TBitmap;
begin
  LimparImagem;
  
  if Ingrediente.TemImagem then
  begin
    try
      Ingrediente.Imagem.Position := 0;
      
      // Tentar carregar como JPEG
      try
        JPEGImage := TJPEGImage.Create;
        try
          JPEGImage.LoadFromStream(Ingrediente.Imagem);
          imgIngrediente.Picture.Assign(JPEGImage);
          btnRemoverImagem.Enabled := True;
          Exit;
        finally
          JPEGImage.Free;
        end;
      except
        // Não é JPEG
      end;
      
      // Tentar carregar como PNG
      Ingrediente.Imagem.Position := 0;
      try
        PNGImage := TPngImage.Create;
        try
          PNGImage.LoadFromStream(Ingrediente.Imagem);
          imgIngrediente.Picture.Assign(PNGImage);
          btnRemoverImagem.Enabled := True;
          Exit;
        finally
          PNGImage.Free;
        end;
      except
        // Não é PNG
      end;
      
      // Tentar carregar como Bitmap
      Ingrediente.Imagem.Position := 0;
      try
        Bitmap := TBitmap.Create;
        try
          Bitmap.LoadFromStream(Ingrediente.Imagem);
          imgIngrediente.Picture.Assign(Bitmap);
          btnRemoverImagem.Enabled := True;
        finally
          Bitmap.Free;
        end;
      except
        ShowMessage('Formato de imagem não suportado!');
      end;
      
    except
      on E: Exception do
        ShowMessage('Erro ao carregar imagem: ' + E.Message);
    end;
  end;
end;

procedure TFormIngrediente.HabilitarEdicao(Habilitar: Boolean);
begin
  edtNome.Enabled := Habilitar;
  edtValor.Enabled := Habilitar;
  chkAtivo.Enabled := Habilitar;
  btnSelecionarImagem.Enabled := Habilitar;
  btnSalvar.Enabled := Habilitar;
end;

function TFormIngrediente.ValidarCampos: Boolean;
begin
  Result := False;
  
  if Trim(edtNome.Text) = '' then
  begin
    ShowMessage('Informe o nome do ingrediente!');
    edtNome.SetFocus;
    Exit;
  end;
  
  if FormatarValor(edtValor.Text) < 0 then
  begin
    ShowMessage('O valor não pode ser negativo!');
    edtValor.SetFocus;
    Exit;
  end;
  
  Result := True;
end;

function TFormIngrediente.FormatarValor(const Valor: string): Double;
var
  ValorFormatado: string;
begin
  // Remove pontos e substitui vírgula por ponto
  ValorFormatado := StringReplace(Valor, '.', '', [rfReplaceAll]);
  ValorFormatado := StringReplace(ValorFormatado, ',', '.', [rfReplaceAll]);
  
  try
    Result := StrToFloat(ValorFormatado);
  except
    Result := 0;
  end;
end;

procedure TFormIngrediente.btnNovoClick(Sender: TObject);
begin
  LimparCampos;
  PageControl1.ActivePage := tsCadastro;
  HabilitarEdicao(True);
end;

procedure TFormIngrediente.btnEditarClick(Sender: TObject);
var
  ID: Integer;
  Ingrediente: TIngrediente;
begin
  if GridIngredientes.Row < 1 then
  begin
    ShowMessage('Selecione um ingrediente para editar!');
    Exit;
  end;
  
  ID := StrToIntDef(GridIngredientes.Cells[0, GridIngredientes.Row], 0);
  
  if ID > 0 then
  begin
    Ingrediente := FIngredienteDAO.BuscarPorID(ID);
    try
      if Assigned(Ingrediente) then
      begin
        CarregarIngrediente(Ingrediente);
        PageControl1.ActivePage := tsCadastro;
        HabilitarEdicao(True);
      end;
    finally
      Ingrediente.Free;
    end;
  end;
end;

procedure TFormIngrediente.btnExcluirClick(Sender: TObject);
var
  ID: Integer;
begin
  if GridIngredientes.Row < 1 then
  begin
    ShowMessage('Selecione um ingrediente para excluir!');
    Exit;
  end;
  
  if MessageDlg('Deseja realmente excluir este ingrediente?', 
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ID := StrToIntDef(GridIngredientes.Cells[0, GridIngredientes.Row], 0);
    
    try
      if FIngredienteDAO.Excluir(ID) then
      begin
        ShowMessage('Ingrediente excluído com sucesso!');
        CarregarIngredientes;
      end;
    except
      on E: Exception do
        ShowMessage('Erro ao excluir ingrediente: ' + E.Message);
    end;
  end;
end;

procedure TFormIngrediente.btnAtualizarClick(Sender: TObject);
begin
  CarregarIngredientes;
end;

procedure TFormIngrediente.btnSalvarClick(Sender: TObject);
begin
  if not ValidarCampos then
    Exit;
  
  FIngredienteAtual.Nome := edtNome.Text;
  FIngredienteAtual.Valor := FormatarValor(edtValor.Text);
  FIngredienteAtual.Ativo := chkAtivo.Checked;
  
  // Carregar imagem selecionada, se houver
  if FCaminhoImagemSelecionada <> '' then
  begin
    try
      FIngredienteAtual.CarregarImagemDeArquivo(FCaminhoImagemSelecionada);
    except
      on E: Exception do
      begin
        ShowMessage('Erro ao carregar imagem: ' + E.Message);
        Exit;
      end;
    end;
  end;
  
  try
    if FIngredienteAtual.ID = 0 then
    begin
      // Inserir novo ingrediente
      if FIngredienteDAO.Inserir(FIngredienteAtual) then
      begin
        ShowMessage('Ingrediente cadastrado com sucesso!');
        CarregarIngredientes;
        PageControl1.ActivePage := tsLista;
      end;
    end
    else
    begin
      // Atualizar ingrediente existente
      if FIngredienteDAO.Atualizar(FIngredienteAtual) then
      begin
        ShowMessage('Ingrediente atualizado com sucesso!');
        CarregarIngredientes;
        PageControl1.ActivePage := tsLista;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao salvar ingrediente: ' + E.Message);
  end;
end;

procedure TFormIngrediente.btnCancelarClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsLista;
  LimparCampos;
end;

procedure TFormIngrediente.GridIngredientesDblClick(Sender: TObject);
begin
  btnEditarClick(Sender);
end;

procedure TFormIngrediente.edtFiltroChange(Sender: TObject);
begin
  PreencherGrid;
end;

procedure TFormIngrediente.chkApenasAtivosClick(Sender: TObject);
begin
  CarregarIngredientes;
end;

procedure TFormIngrediente.btnSelecionarImagemClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    try
      FCaminhoImagemSelecionada := OpenPictureDialog.FileName;
      imgIngrediente.Picture.LoadFromFile(FCaminhoImagemSelecionada);
      btnRemoverImagem.Enabled := True;
    except
      on E: Exception do
      begin
        ShowMessage('Erro ao carregar imagem: ' + E.Message);
        LimparImagem;
      end;
    end;
  end;
end;

procedure TFormIngrediente.btnRemoverImagemClick(Sender: TObject);
begin
  if MessageDlg('Deseja realmente remover a imagem?', 
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    LimparImagem;
    FIngredienteAtual.Imagem.Clear;
  end;
end;

procedure TFormIngrediente.edtValorKeyPress(Sender: TObject; var Key: Char);
begin
  // Permitir apenas números, vírgula, ponto e teclas de controle
  if not (CharInSet(Key, ['0'..'9', ',', '.', #8, #9])) then
    Key := #0;
end;

end.
