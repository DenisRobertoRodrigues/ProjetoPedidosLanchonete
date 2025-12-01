unit uFormProduto;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.StrUtils, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, Data.DB, Vcl.ComCtrls, System.Generics.Collections, Vcl.ExtDlgs,
  Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, Vcl.Imaging.GIFImg, Vcl.CheckLst,
  uProduto, uProdutoDAO, uIngrediente, uIngredienteDAO, FireDAC.Comp.Client;

type
  TFormProduto = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    lblTitulo: TLabel;
    PageControl1: TPageControl;
    tsLista: TTabSheet;
    tsCadastro: TTabSheet;
    GridProdutos: TStringGrid;
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
    imgProduto: TImage;
    btnSelecionarImagem: TButton;
    btnRemoverImagem: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    lblImagem: TLabel;
    lblIngredientes: TLabel;
    pnlIngredientes: TPanel;
    chkListIngredientes: TCheckListBox;
    btnAdicionarIngrediente: TButton;
    lstIngredientesSelecionados: TListBox;
    lblIngredientesSelecionados: TLabel;
    btnRemoverIngrediente: TButton;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure GridProdutosDblClick(Sender: TObject);
    procedure edtFiltroChange(Sender: TObject);
    procedure chkApenasAtivosClick(Sender: TObject);
    procedure btnSelecionarImagemClick(Sender: TObject);
    procedure btnRemoverImagemClick(Sender: TObject);
    procedure edtValorKeyPress(Sender: TObject; var Key: Char);
    procedure btnAdicionarIngredienteClick(Sender: TObject);
    procedure btnRemoverIngredienteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    
  private
    FProdutoDAO: TProdutoDAO;
    FIngredienteDAO: TIngredienteDAO;
    FProdutoAtual: TProduto;
    FListaProdutos: TObjectList<TProduto>;
    FListaIngredientes: TObjectList<TIngrediente>;
    FCaminhoImagemSelecionada: string;
    
    procedure ConfigurarGrid;
    procedure CarregarProdutos;
    procedure PreencherGrid;
    procedure LimparCampos;
    procedure CarregarProduto(Produto: TProduto);
    procedure HabilitarEdicao(Habilitar: Boolean);
    function ValidarCampos: Boolean;
    procedure ExibirImagem(Produto: TProduto);
    procedure LimparImagem;
    function FormatarValor(const Valor: string): Double;
    procedure CarregarIngredientesDisponiveis;
    procedure AtualizarListaIngredientesSelecionados;
    
  public
    { Public declarations }
  end;

var
  FormProduto: TFormProduto;

implementation

uses
  uDMDTO;

{$R *.dfm}

procedure TFormProduto.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormProduto.FormCreate(Sender: TObject);
begin
  FProdutoAtual := TProduto.Create;
  FListaProdutos := TObjectList<TProduto>.Create;
  FListaIngredientes := TObjectList<TIngrediente>.Create;
  
  ConfigurarGrid;
  CarregarProdutos;
  
  PageControl1.ActivePage := tsLista;

  OpenPictureDialog.Filter := 'Imagens|*.jpg;*.jpeg;*.png;*.bmp;*.gif|Todos os arquivos|*.*';
  OpenPictureDialog.Title := 'Selecionar Imagem do Produto';

  imgProduto.Stretch := True;
  imgProduto.Proportional := True;
  imgProduto.Center := True;

  CarregarIngredientesDisponiveis;
end;

procedure TFormProduto.FormDestroy(Sender: TObject);
begin
  FListaIngredientes.Free;
  FListaProdutos.Free;
  FProdutoAtual.Free;
  FIngredienteDAO.Free;
  FProdutoDAO.Free;
end;

procedure TFormProduto.ConfigurarGrid;
begin
  GridProdutos.ColCount := 7;
  GridProdutos.RowCount := 1;

  GridProdutos.Cells[0, 0] := 'ID';
  GridProdutos.Cells[1, 0] := 'Nome';
  GridProdutos.Cells[2, 0] := 'Valor';
  GridProdutos.Cells[3, 0] := 'Imagem';
  GridProdutos.Cells[4, 0] := 'Ingredientes';
  GridProdutos.Cells[5, 0] := 'Data Cadastro';
  GridProdutos.Cells[6, 0] := 'Ativo';
  
  GridProdutos.ColWidths[0] := 50;
  GridProdutos.ColWidths[1] := 250;
  GridProdutos.ColWidths[2] := 100;
  GridProdutos.ColWidths[3] := 80;
  GridProdutos.ColWidths[4] := 100;
  GridProdutos.ColWidths[5] := 120;
  GridProdutos.ColWidths[6] := 60;
end;

procedure TFormProduto.CarregarProdutos;
begin
  FListaProdutos.Clear;
  FListaProdutos.Free;
  FListaProdutos := FProdutoDAO.ListarTodos(chkApenasAtivos.Checked);
  PreencherGrid;
end;

procedure TFormProduto.PreencherGrid;
var
  I: Integer;
  Produto: TProduto;
  Filtro: string;
begin
  GridProdutos.RowCount := 1;
  Filtro := UpperCase(Trim(edtFiltro.Text));
  
  for I := 0 to FListaProdutos.Count - 1 do
  begin
    Produto := FListaProdutos[I];

    if (Filtro <> '') and 
       (Pos(Filtro, UpperCase(Produto.Nome)) = 0) then
      Continue;
    
    GridProdutos.RowCount := GridProdutos.RowCount + 1;
    GridProdutos.Cells[0, GridProdutos.RowCount - 1] := IntToStr(Produto.ID);
    GridProdutos.Cells[1, GridProdutos.RowCount - 1] := Produto.Nome;
    GridProdutos.Cells[2, GridProdutos.RowCount - 1] := 
      FormatFloat('#,##0.00', Produto.Valor);
    GridProdutos.Cells[3, GridProdutos.RowCount - 1] := 
      IfThen(Produto.TemImagem, 'Sim', 'Não');
    GridProdutos.Cells[4, GridProdutos.RowCount - 1] := 
      IntToStr(Produto.Ingredientes.Count);
    GridProdutos.Cells[5, GridProdutos.RowCount - 1] := 
      FormatDateTime('dd/mm/yyyy hh:nn', Produto.DataCadastro);
    GridProdutos.Cells[6, GridProdutos.RowCount - 1] := 
      IfThen(Produto.Ativo, 'Sim', 'Não');
  end;
end;

procedure TFormProduto.CarregarIngredientesDisponiveis;
var
  I: Integer;
  Ingrediente: TIngrediente;
begin
  chkListIngredientes.Items.Clear;
  FListaIngredientes.Free;
  FListaIngredientes := FIngredienteDAO.ListarTodos(True);
  
  for I := 0 to FListaIngredientes.Count - 1 do
  begin
    Ingrediente := FListaIngredientes[I];
    chkListIngredientes.Items.AddObject(Ingrediente.Nome, TObject(Ingrediente.ID));
  end;
end;

procedure TFormProduto.AtualizarListaIngredientesSelecionados;
var
  ProdutoIngrediente: TProdutoIngrediente;
begin
  lstIngredientesSelecionados.Items.Clear;
  
  for ProdutoIngrediente in FProdutoAtual.Ingredientes do
  begin
    lstIngredientesSelecionados.Items.AddObject(
      ProdutoIngrediente.NomeIngrediente,
      TObject(ProdutoIngrediente.IDIngrediente)
    );
  end;
  
  btnRemoverIngrediente.Enabled := lstIngredientesSelecionados.Items.Count > 0;
end;

procedure TFormProduto.LimparCampos;
begin
  FProdutoAtual.Clear;
  edtNome.Clear;
  edtValor.Text := '0,00';
  chkAtivo.Checked := True;
  LimparImagem;
  lstIngredientesSelecionados.Items.Clear;
  btnRemoverIngrediente.Enabled := False;
end;

procedure TFormProduto.LimparImagem;
begin
  imgProduto.Picture := nil;
  FCaminhoImagemSelecionada := '';
  btnRemoverImagem.Enabled := False;
end;

procedure TFormProduto.CarregarProduto(Produto: TProduto);
var
  ProdutoIngrediente: TProdutoIngrediente;
begin
  FProdutoAtual.ID := Produto.ID;
  FProdutoAtual.Nome := Produto.Nome;
  FProdutoAtual.Valor := Produto.Valor;
  FProdutoAtual.Ativo := Produto.Ativo;

  FProdutoAtual.Imagem.Clear;
  if Produto.TemImagem then
  begin
    Produto.Imagem.Position := 0;
    FProdutoAtual.Imagem.CopyFrom(Produto.Imagem, Produto.Imagem.Size);
  end;

  FProdutoAtual.Ingredientes.Clear;
  for ProdutoIngrediente in Produto.Ingredientes do
  begin
    FProdutoAtual.AdicionarIngrediente(
      ProdutoIngrediente.IDIngrediente,
      ProdutoIngrediente.NomeIngrediente
    );
  end;
  
  edtNome.Text := Produto.Nome;
  edtValor.Text := FormatFloat('0.00', Produto.Valor);
  chkAtivo.Checked := Produto.Ativo;
  
  ExibirImagem(Produto);
  AtualizarListaIngredientesSelecionados;
end;

procedure TFormProduto.ExibirImagem(Produto: TProduto);
var
  JPEGImage: TJPEGImage;
  PNGImage: TPngImage;
  Bitmap: TBitmap;
begin
  LimparImagem;
  
  if Produto.TemImagem then
  begin
    try
      Produto.Imagem.Position := 0;
      
      // Tentar carregar como JPEG
      try
        JPEGImage := TJPEGImage.Create;
        try
          JPEGImage.LoadFromStream(Produto.Imagem);
          imgProduto.Picture.Assign(JPEGImage);
          btnRemoverImagem.Enabled := True;
          Exit;
        finally
          JPEGImage.Free;
        end;
      except
        // Não é JPEG
      end;
      
      // Tentar carregar como PNG
      Produto.Imagem.Position := 0;
      try
        PNGImage := TPngImage.Create;
        try
          PNGImage.LoadFromStream(Produto.Imagem);
          imgProduto.Picture.Assign(PNGImage);
          btnRemoverImagem.Enabled := True;
          Exit;
        finally
          PNGImage.Free;
        end;
      except
        // Não é PNG
      end;
      
      // Tentar carregar como Bitmap
      Produto.Imagem.Position := 0;
      try
        Bitmap := TBitmap.Create;
        try
          Bitmap.LoadFromStream(Produto.Imagem);
          imgProduto.Picture.Assign(Bitmap);
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

procedure TFormProduto.HabilitarEdicao(Habilitar: Boolean);
begin
  edtNome.Enabled := Habilitar;
  edtValor.Enabled := Habilitar;
  chkAtivo.Enabled := Habilitar;
  btnSelecionarImagem.Enabled := Habilitar;
  chkListIngredientes.Enabled := Habilitar;
  btnAdicionarIngrediente.Enabled := Habilitar;
  btnSalvar.Enabled := Habilitar;
end;

function TFormProduto.ValidarCampos: Boolean;
begin
  Result := False;
  
  if Trim(edtNome.Text) = '' then
  begin
    ShowMessage('Informe o nome do produto!');
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

function TFormProduto.FormatarValor(const Valor: string): Double;
var
  ValorFormatado: string;
begin
  ValorFormatado := StringReplace(Valor, '.', '', [rfReplaceAll]);
  ValorFormatado := StringReplace(ValorFormatado, ',', '.', [rfReplaceAll]);
  
  try
    Result := StrToFloat(ValorFormatado);
  except
    Result := 0;
  end;
end;

procedure TFormProduto.btnNovoClick(Sender: TObject);
begin
  LimparCampos;
  PageControl1.ActivePage := tsCadastro;
  HabilitarEdicao(True);
end;

procedure TFormProduto.btnEditarClick(Sender: TObject);
var
  ID: Integer;
  Produto: TProduto;
begin
  if GridProdutos.Row < 1 then
  begin
    ShowMessage('Selecione um produto para editar!');
    Exit;
  end;
  
  ID := StrToIntDef(GridProdutos.Cells[0, GridProdutos.Row], 0);
  
  if ID > 0 then
  begin
    Produto := FProdutoDAO.BuscarPorID(ID);
    try
      if Assigned(Produto) then
      begin
        CarregarProduto(Produto);
        PageControl1.ActivePage := tsCadastro;
        HabilitarEdicao(True);
      end;
    finally
      Produto.Free;
    end;
  end;
end;

procedure TFormProduto.btnExcluirClick(Sender: TObject);
var
  ID: Integer;
begin
  if GridProdutos.Row < 1 then
  begin
    ShowMessage('Selecione um produto para excluir!');
    Exit;
  end;
  
  if MessageDlg('Deseja realmente excluir este produto?', 
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ID := StrToIntDef(GridProdutos.Cells[0, GridProdutos.Row], 0);
    
    try
      if FProdutoDAO.Excluir(ID) then
      begin
        ShowMessage('Produto excluído com sucesso!');
        CarregarProdutos;
      end;
    except
      on E: Exception do
        ShowMessage('Erro ao excluir produto: ' + E.Message);
    end;
  end;
end;

procedure TFormProduto.btnAtualizarClick(Sender: TObject);
begin
  CarregarProdutos;
end;

procedure TFormProduto.btnSalvarClick(Sender: TObject);
begin
  if not ValidarCampos then
    Exit;
  
  FProdutoAtual.Nome := edtNome.Text;
  FProdutoAtual.Valor := FormatarValor(edtValor.Text);
  FProdutoAtual.Ativo := chkAtivo.Checked;


  if FCaminhoImagemSelecionada <> '' then
  begin
    try
      FProdutoAtual.CarregarImagemDeArquivo(FCaminhoImagemSelecionada);
    except
      on E: Exception do
      begin
        ShowMessage('Erro ao carregar imagem: ' + E.Message);
        Exit;
      end;
    end;
  end;
  
  try
    if FProdutoAtual.ID = 0 then
    begin
      if FProdutoDAO.Inserir(FProdutoAtual) then
      begin
        ShowMessage('Produto cadastrado com sucesso!');
        CarregarProdutos;
        PageControl1.ActivePage := tsLista;
      end;
    end
    else
    begin
      if FProdutoDAO.Atualizar(FProdutoAtual) then
      begin
        ShowMessage('Produto atualizado com sucesso!');
        CarregarProdutos;
        PageControl1.ActivePage := tsLista;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao salvar produto: ' + E.Message);
  end;
end;

procedure TFormProduto.btnCancelarClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsLista;
  LimparCampos;
end;

procedure TFormProduto.GridProdutosDblClick(Sender: TObject);
begin
  btnEditarClick(Sender);
end;

procedure TFormProduto.edtFiltroChange(Sender: TObject);
begin
  PreencherGrid;
end;

procedure TFormProduto.chkApenasAtivosClick(Sender: TObject);
begin
  CarregarProdutos;
end;

procedure TFormProduto.btnSelecionarImagemClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    try
      FCaminhoImagemSelecionada := OpenPictureDialog.FileName;
      imgProduto.Picture.LoadFromFile(FCaminhoImagemSelecionada);
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

procedure TFormProduto.btnRemoverImagemClick(Sender: TObject);
begin
  if MessageDlg('Deseja realmente remover a imagem?', 
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    LimparImagem;
    FProdutoAtual.Imagem.Clear;
  end;
end;

procedure TFormProduto.btnAdicionarIngredienteClick(Sender: TObject);
var
  I: Integer;
  IDIngrediente: Integer;
  NomeIngrediente: string;
begin
  for I := 0 to chkListIngredientes.Items.Count - 1 do
  begin
    if chkListIngredientes.Checked[I] then
    begin
      IDIngrediente := Integer(chkListIngredientes.Items.Objects[I]);
      NomeIngrediente := chkListIngredientes.Items[I];
      
      FProdutoAtual.AdicionarIngrediente(IDIngrediente, NomeIngrediente);
      chkListIngredientes.Checked[I] := False;
    end;
  end;
  
  AtualizarListaIngredientesSelecionados;
end;

procedure TFormProduto.btnRemoverIngredienteClick(Sender: TObject);
var
  IDIngrediente: Integer;
begin
  if lstIngredientesSelecionados.ItemIndex < 0 then
  begin
    ShowMessage('Selecione um ingrediente para remover!');
    Exit;
  end;
  
  IDIngrediente := Integer(lstIngredientesSelecionados.Items.Objects[lstIngredientesSelecionados.ItemIndex]);
  FProdutoAtual.RemoverIngrediente(IDIngrediente);
  
  AtualizarListaIngredientesSelecionados;
end;

procedure TFormProduto.edtValorKeyPress(Sender: TObject; var Key: Char);
begin
  if not (CharInSet(Key, ['0'..'9', ',', '.', #8, #9])) then
    Key := #0;
end;

end.
