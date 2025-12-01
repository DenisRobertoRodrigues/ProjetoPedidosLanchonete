unit uFormMenu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, System.Generics.Collections,
  uProduto, uProdutoDAO, FireDAC.Comp.Client;

type
  TPainelProduto = class(TPanel)
  private
    FIDProduto: Integer;
    FImgProduto: TImage;
    FLblNome: TLabel;
    FLblValorReal: TLabel;
    FLblValorDolar: TLabel;
    FMemoIngredientes: TMemo;
    FPnlImagem: TPanel;
    FPnlInfo: TPanel;
    FPnlValores: TPanel;
  public
    constructor Create(AOwner: TComponent; Produto: TProduto; CotacaoDolar: Double); reintroduce;
    property IDProduto: Integer read FIDProduto;
  end;

  TFormMenu = class(TForm)
    pnlTop: TPanel;
    lblTitulo: TLabel;
    pnlBottom: TPanel;
    ScrollBox: TScrollBox;
    pnlCotacao: TPanel;
    btnAtualizar: TButton;
    pnlFiltro: TPanel;
    lblFiltro: TLabel;
    edtFiltro: TEdit;
    chkApenasAtivos: TCheckBox;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure edtFiltroChange(Sender: TObject);
    procedure chkApenasAtivosClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    
  private
    FProdutoDAO: TProdutoDAO;
    FListaProdutos: TObjectList<TProduto>;
    FCotacaoDolar: Double;
    FListaPaineis: TList;
    
    procedure CarregarProdutos;
    procedure ExibirProdutos;
    procedure LimparPaineis;
    function ConverterParaDolar(ValorReal: Double): Double;
    procedure AtualizarValoresDolar;
    
  public
    { Public declarations }
  end;

var
  FormMenu: TFormMenu;

implementation

uses
  uDMDTO, Math;

{$R *.dfm}

{ TPainelProduto }

constructor TPainelProduto.Create(AOwner: TComponent; Produto: TProduto; CotacaoDolar: Double);
var
  ProdutoIngrediente: TProdutoIngrediente;
  Ingredientes: string;
  JPEGImage: TJPEGImage;
  PNGImage: TPngImage;
  Bitmap: TBitmap;
  ValorDolar: Double;
begin
  inherited Create(AOwner);
  
  FIDProduto := Produto.ID;
  
  // Configurar painel principal
  Parent := AOwner as TWinControl;
  Width := 350;
  Height := 420;
  BevelOuter := bvNone;
  BorderStyle := bsSingle;
  Color := clWhite;
  Padding.Left := 8;
  Padding.Top := 8;
  Padding.Right := 8;
  Padding.Bottom := 8;
  
  // Painel da imagem
  FPnlImagem := TPanel.Create(Self);
  FPnlImagem.Parent := Self;
  FPnlImagem.Align := alTop;
  FPnlImagem.Height := 220;
  FPnlImagem.BevelOuter := bvNone;
  FPnlImagem.Color := clWhite;
  FPnlImagem.BorderStyle := bsSingle;
  
  // Imagem do produto
  FImgProduto := TImage.Create(Self);
  FImgProduto.Parent := FPnlImagem;
  FImgProduto.Align := alClient;
  FImgProduto.Stretch := True;
  FImgProduto.Proportional := True;
  FImgProduto.Center := True;
  
  // Carregar imagem se existir
  if Produto.TemImagem then
  begin
    try
      Produto.Imagem.Position := 0;
      
      // Tentar JPEG
      try
        JPEGImage := TJPEGImage.Create;
        try
          JPEGImage.LoadFromStream(Produto.Imagem);
          FImgProduto.Picture.Assign(JPEGImage);
        finally
          JPEGImage.Free;
        end;
      except
        // Tentar PNG
        Produto.Imagem.Position := 0;
        try
          PNGImage := TPngImage.Create;
          try
            PNGImage.LoadFromStream(Produto.Imagem);
            FImgProduto.Picture.Assign(PNGImage);
          finally
            PNGImage.Free;
          end;
        except
          // Tentar Bitmap
          Produto.Imagem.Position := 0;
          try
            Bitmap := TBitmap.Create;
            try
              Bitmap.LoadFromStream(Produto.Imagem);
              FImgProduto.Picture.Assign(Bitmap);
            finally
              Bitmap.Free;
            end;
          except
            // Imagem padrão se falhar
            FPnlImagem.Color := $00F0F0F0;
          end;
        end;
      end;
    except
      FPnlImagem.Color := $00F0F0F0;
    end;
  end
  else
  begin
    // Sem imagem - fundo cinza claro
    FPnlImagem.Color := $00F0F0F0;
  end;
  
  // Painel de informações
  FPnlInfo := TPanel.Create(Self);
  FPnlInfo.Parent := Self;
  FPnlInfo.Align := alClient;
  FPnlInfo.BevelOuter := bvNone;
  FPnlInfo.Color := clWhite;
  FPnlInfo.Padding.Left := 8;
  FPnlInfo.Padding.Top := 8;
  FPnlInfo.Padding.Right := 8;
  FPnlInfo.Padding.Bottom := 8;
  
  // Nome do produto
  FLblNome := TLabel.Create(Self);
  FLblNome.Parent := FPnlInfo;
  FLblNome.Align := alTop;
  FLblNome.Caption := Produto.Nome;
  FLblNome.Font.Size := 14;
  FLblNome.Font.Style := [fsBold];
  FLblNome.Font.Color := clNavy;
  FLblNome.Height := 30;
  FLblNome.Layout := tlCenter;
  FLblNome.WordWrap := True;
  
  // Painel de valores
  FPnlValores := TPanel.Create(Self);
  FPnlValores.Parent := FPnlInfo;
  FPnlValores.Align := alTop;
  FPnlValores.Height := 50;
  FPnlValores.BevelOuter := bvNone;
  FPnlValores.Color := clWhite;
  
  // Valor em Real
  FLblValorReal := TLabel.Create(Self);
  FLblValorReal.Parent := FPnlValores;
  FLblValorReal.Align := alTop;
  FLblValorReal.Caption := FormatFloat('R$ #,##0.00', Produto.Valor);
  FLblValorReal.Font.Size := 16;
  FLblValorReal.Font.Style := [fsBold];
  FLblValorReal.Font.Color := $00008000; // Verde escuro
  FLblValorReal.Height := 25;
  
  // Valor em Dólar
  ValorDolar := Produto.Valor / CotacaoDolar;
  FLblValorDolar := TLabel.Create(Self);
  FLblValorDolar.Parent := FPnlValores;
  FLblValorDolar.Align := alTop;
  FLblValorDolar.Caption := FormatFloat('US$ #,##0.00', ValorDolar);
  FLblValorDolar.Font.Size := 10;
  FLblValorDolar.Font.Style := [];
  FLblValorDolar.Font.Color := clGray;
  FLblValorDolar.Height := 20;
  
  // Ingredientes
  Ingredientes := '';
  for ProdutoIngrediente in Produto.Ingredientes do
  begin
    if Ingredientes <> '' then
      Ingredientes := Ingredientes + ', ';
    Ingredientes := Ingredientes + ProdutoIngrediente.NomeIngrediente;
  end;
  
  if Ingredientes = '' then
    Ingredientes := 'Sem ingredientes cadastrados';
  
  FMemoIngredientes := TMemo.Create(Self);
  FMemoIngredientes.Parent := FPnlInfo;
  FMemoIngredientes.Align := alClient;
  FMemoIngredientes.Text := 'Ingredientes: ' + Ingredientes;
  FMemoIngredientes.Font.Size := 9;
  FMemoIngredientes.Font.Color := clGray;
  FMemoIngredientes.ReadOnly := True;
  FMemoIngredientes.BorderStyle := bsNone;
  FMemoIngredientes.Color := clWhite;
  FMemoIngredientes.WordWrap := True;
  FMemoIngredientes.ScrollBars := ssVertical;
end;

{ TFormMenu }

procedure TFormMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormMenu.FormCreate(Sender: TObject);
begin
  FProdutoDAO := TProdutoDAO.Create(DMDTO.FDConexao);
  FListaProdutos := TObjectList<TProduto>.Create;
  FListaPaineis := TList.Create;
  
  // Cotação padrão do dólar
  FCotacaoDolar := 5.50; // inserir busca API aqui

  CarregarProdutos;
  ExibirProdutos;
end;

procedure TFormMenu.FormDestroy(Sender: TObject);
begin
  LimparPaineis;
  FListaPaineis.Free;
  FListaProdutos.Free;
  FProdutoDAO.Free;
end;

procedure TFormMenu.FormResize(Sender: TObject);
begin
  // Reorganizar os painéis ao redimensionar
  if Assigned(FListaProdutos) and (FListaProdutos.Count > 0) then
    ExibirProdutos;
end;

procedure TFormMenu.CarregarProdutos;
begin
  FListaProdutos.Clear;
  FListaProdutos.Free;
  FListaProdutos := FProdutoDAO.ListarTodos(chkApenasAtivos.Checked);
end;

procedure TFormMenu.LimparPaineis;
var
  I: Integer;
  Painel: TPainelProduto;
begin
  for I := FListaPaineis.Count - 1 downto 0 do
  begin
    Painel := TPainelProduto(FListaPaineis[I]);
    Painel.Free;
  end;
  FListaPaineis.Clear;
end;

procedure TFormMenu.ExibirProdutos;
var
  I: Integer;
  Produto: TProduto;
  Painel: TPainelProduto;
  ColunasVisiveis: Integer;
  Linha, Coluna: Integer;
  EspacoHorizontal, EspacoVertical: Integer;
  LarguraPainel, AlturaPainel: Integer;
  Filtro: string;
begin
  LimparPaineis;
  
  Filtro := UpperCase(Trim(edtFiltro.Text));
  
  // Calcular quantas colunas cabem
  LarguraPainel := 350;
  AlturaPainel := 420;
  EspacoHorizontal := 15;
  EspacoVertical := 15;
  
  ColunasVisiveis := Max(1, (ScrollBox.Width - 20) div (LarguraPainel + EspacoHorizontal));
  
  Linha := 0;
  Coluna := 0;
  
  for I := 0 to FListaProdutos.Count - 1 do
  begin
    Produto := FListaProdutos[I];
    
    // Aplicar filtro
    if (Filtro <> '') and (Pos(Filtro, UpperCase(Produto.Nome)) = 0) then
      Continue;
    
    // Criar painel do produto
    Painel := TPainelProduto.Create(ScrollBox, Produto, FCotacaoDolar);
    Painel.Left := 10 + (Coluna * (LarguraPainel + EspacoHorizontal));
    Painel.Top := 10 + (Linha * (AlturaPainel + EspacoVertical));
    
    FListaPaineis.Add(Painel);
    
    // Calcular próxima posição
    Inc(Coluna);
    if Coluna >= ColunasVisiveis then
    begin
      Coluna := 0;
      Inc(Linha);
    end;
  end;
  
  // Ajustar altura do ScrollBox se necessário
  if FListaPaineis.Count > 0 then
  begin
    ScrollBox.VertScrollBar.Range := (Linha + 1) * (AlturaPainel + EspacoVertical) + 20;
  end;
end;

function TFormMenu.ConverterParaDolar(ValorReal: Double): Double;
begin
  if FCotacaoDolar > 0 then
    Result := ValorReal / FCotacaoDolar
  else
    Result := 0;
end;

procedure TFormMenu.AtualizarValoresDolar;
begin

end;

procedure TFormMenu.btnAtualizarClick(Sender: TObject);
begin
  CarregarProdutos;
  ExibirProdutos;
end;

procedure TFormMenu.edtFiltroChange(Sender: TObject);
begin
  ExibirProdutos;
end;

procedure TFormMenu.chkApenasAtivosClick(Sender: TObject);
begin
  CarregarProdutos;
  ExibirProdutos;
end;

end.
